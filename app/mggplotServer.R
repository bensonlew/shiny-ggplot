
#' @param input,output,session Standards \code{shiny} server arguments.
#' @param data A \code{reactiveValues} with at least a slot \code{data} containing a \code{data.frame}
#'  to use in the module. And a slot \code{name} corresponding to the name of the \code{data.frame}.
#' @param dataModule Data module to use, choose between \code{"GlobalEnv"}
#'  or \code{"ImportFile"}.
#' @param sizeDataModule Size for the modal window for selecting data.
#'
#' @export
#'
#' @rdname module-esquisse
#'
#' @importFrom shiny callModule reactiveValues observeEvent
#'  renderPlot stopApp plotOutput showNotification isolate reactiveValuesToList
#' @importFrom ggplot2 ggplot_build ggsave
#' @import ggplot2
#' @importFrom datamods import_modal import_server
#'
mggplotServer <- function(input,
                            output,
                            session,
                            data = NULL,
                            dataModule = c("GlobalEnv", "ImportFile"),
                            sizeDataModule = "m") {
  .Deprecated(new = "esquisse_server", package = "esquisse", old = "esquisserServer")
  ns <- session$ns
  ggplotCall <- reactiveValues(code = "")
  dataChart <- reactiveValues(data = NULL, name = NULL)
  # Settings modal (aesthetics choices)
  observeEvent(input$settings_pre, {
    showModal(pre_settings(view=input$view))
    
    # hideTab(inputId="mggplot", target="ui_aesthetics")
  })
  observeEvent(input$settings, {
    # saveRDS(input, "input.rds")
    showModal(modal_settings(aesthetics = input$aesthetics))
  })

  observeEvent(input$save_load, {
    showModal(save_load())
    
    # hideTab(inputId="mggplot", target="ui_aesthetics")
  })

  observeEvent(input$view, {
    # print(paste("input view", input$view))
     if("show_aes" %in% input$view){
      shinyjs::show(id="aes_show")
     }else{
      shinyjs::hide(id="aes_show")
     }
     if("show_table" %in% input$view){
      shinyjs::show(selector=".flexfill-item:first")
     }else{
      shinyjs::hide(selector=".flexfill-item:first")
     }
    #  if("show_img" %in% input$view){
    #   shinyjs::show(id="img_show")
    #  }else{
    #   shinyjs::hide(id="img_show")
    #  }

  })



  # Generate drag-and-drop input
  output$ui_aesthetics <- renderUI({
    if (is.null(input$aesthetics)) {
      aesthetics <- c("fill", "color", "size", "group", "facet")
    } else {
      aesthetics <- input$aesthetics
    }
    data <- isolate(dataChart$data)
    if (!is.null(data)) {
      var_choices <- setdiff(names(data), attr(data, "sf_column"))
      dragulaInput(
        inputId = ns("dragvars"),
        sourceLabel = "Variables",
        targetsLabels = c("X", "Y", aesthetics),
        targetsIds = c("xvar", "yvar", aesthetics),
        choiceValues = var_choices,
        choiceNames = badgeType(
          col_name = var_choices,
          col_type = col_type(data[, var_choices])
        ),
        selected = dropNulls(isolate(input$dragvars$target)),
        badge = FALSE,
        width = "100%",
        height = "70px",
        replace = TRUE
      )
    } else {
      dragulaInput(
        inputId = ns("dragvars"),
        sourceLabel = "Variables",
        targetsLabels = c("X", "Y", aesthetics),
        targetsIds = c("xvar", "yvar", aesthetics),
        choices = "",
        badge = FALSE,
        width = "100%",
        height = "70px",
        replace = TRUE
      )
    }
  })

  observeEvent(data$data, {
    dataChart$data <- data$data
    dataChart$name <- data$name
  }, ignoreInit = FALSE)



  # Launch import modal if no data at start
  if (is.null(isolate(data$data))) {
    datamods::import_modal(
      id = ns("import-data"),
      from = c("env", "file", "copypaste"),
      title = "Import data"
    )
  }

  # Launch import modal if button clicked
  observeEvent(input$launch_import_data, {
    datamods::import_modal(
      id = ns("import-data"),
      from = c("env", "file", "copypaste"),
      title = "Import data"
    )
  })

  # DAta imported and update rv used
  data_imported_r <- datamods::import_server("import-data", return_class = "tbl_df")
  observeEvent(data_imported_r$data(), {
    data <- data_imported_r$data()
    dataChart$data <- data
    dataChart$name <- data_imported_r$name()
  })

  # 修改数据表
  observeEvent(paramsChart$inputs$vol_scatter, {
    # browser()
    # print("change")
    # print(paste("paramsChart$inputs$vol_scatter", paramsChart$inputs$vol_scatter))
    data <- isolate(dataChart$data)
    if(paramsChart$inputs$vol_scatter){
      data[["log_padjust"]] = -log(data[["padjust"]])
      vol_logp_max = max(data[["log_padjust"]][data[["log_padjust"]]!=Inf])
      data[["log_padjust"]][data[["log_padjust"]]==Inf] = vol_logp_max
      dataChart$data <- data
    }
  })

  # 修改数据表
  observeEvent(paramsChart$inputs$vol_sig, {
    # browser()
    # print("change")
    # print(paste("paramsChart$inputs$vol_scatter", paramsChart$inputs$vol_scatter))
    data <- isolate(dataChart$data)
    if(paramsChart$inputs$vol_sig){
      data[["reg_sig"]] = data[["significant"]]
      data[["reg_sig"]][data[["reg_sig"]]=="no test"] = "no"
      data[["reg_sig"]][data[["reg_sig"]]=="yes" & data[["regulate"]]=="up"] = "up"
      data[["reg_sig"]][data[["reg_sig"]]=="yes" & data[["regulate"]]=="down"] = "down"
      dataChart$data <- data
    }
  })

  # Update drag-and-drop input when data changes
  observeEvent(dataChart$data, {
    data <- dataChart$data

    if (is.null(data)) {
      updateDragulaInput(
        session = session,
        inputId = "dragvars",
        status = NULL,
        choices = character(0),
        badge = FALSE
      )
    } else {
      # special case: geom_sf
      if (inherits(data, what = "sf")) {
        geom_possible$x <- c("sf", geom_possible$x)
      }
      var_choices <- setdiff(names(data), attr(data, "sf_column"))
      updateDragulaInput(
        session = session,
        inputId = "dragvars",
        status = NULL,
        choiceValues = var_choices,
        choiceNames = badgeType(
          col_name = var_choices,
          col_type = col_type(data[, var_choices])
        ),
        badge = FALSE
      )
    }
  }, ignoreNULL = FALSE)

  geom_possible <- reactiveValues(x = "auto")
  geom_controls <- reactiveValues(x = "auto")
  observeEvent(list(input$dragvars$target, input$geom), {
    geoms <- potential_geoms(
      data = dataChart$data,
      mapping = build_aes(
        data = dataChart$data,
        x = input$dragvars$target$xvar,
        y = input$dragvars$target$yvar
      )
    )
    geom_possible$x <- c("auto", geoms)

    geom_controls$x <- select_geom_controls(input$geom, geoms)

    if (!is.null(input$dragvars$target$fill) | !is.null(input$dragvars$target$color)) {
      geom_controls$palette <- TRUE
    } else {
      geom_controls$palette <- FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(geom_possible$x, {
    geoms <- c(
      "auto", "line", "area", "bar", "histogram",
      "point", "boxplot", "violin", "density",
      "tile", "sf"
    )
    updateDropInput(
      session = session,
      inputId = "geom",
      selected = setdiff(geom_possible$x, "auto")[1],
      disabled = setdiff(geoms, geom_possible$x)
    )
  })

  # Module chart controls : title, xlabs, colors, export...
  paramsChart <- reactiveValues(inputs = NULL)
  paramsChart <- controls_server(
    id = "controls",
    type = geom_controls,
    data_table = reactive(dataChart$data),
    data_name = reactive({
      req(dataChart$name)
      dataChart$name
    }),
    ggplot_rv = ggplotCall,
    aesthetics = reactive({
      dropNullsOrEmpty(input$dragvars$target)
    }),
    use_facet = reactive({
      !is.null(input$dragvars$target$facet) | !is.null(input$dragvars$target$facet_row) | !is.null(input$dragvars$target$facet_col)
    }),
    use_transX = reactive({
      if (is.null(input$dragvars$target$xvar))
        return(FALSE)
      identical(
        x = col_type(dataChart$data[[input$dragvars$target$xvar]]),
        y = "continuous"
      )
    }),
    use_transY = reactive({
      if (is.null(input$dragvars$target$yvar))
        return(FALSE)
      identical(
        x = col_type(dataChart$data[[input$dragvars$target$yvar]]),
        y = "continuous"
      )
    })
  )

  output$plotdata <- DT::renderDataTable({
        req(dataChart$data)
        dataChart$data
    },
    options = list(scrollX = TRUE)
  )

  # output$plotdata

  # render plot 增加下载功能
  render_ggplot("plooooooot", {
    
    # req(input$play_plot, cancelOutput = TRUE)
    req(dataChart$data)
    req(paramsChart$data)
    req(paramsChart$inputs)
    req(input$geom)
    # print(paramsChart$inputs)
    aes_input <- make_aes(input$dragvars$target)
    # hide(id="aes_show")
    req(unlist(aes_input) %in% names(dataChart$data))

    mapping <- build_aes(
      data = dataChart$data,
      .list = aes_input,
      geom = input$geom
    )

    geoms <- potential_geoms(
      data = dataChart$data,
      mapping = mapping
    )
    req(input$geom %in% geoms)

    data <- paramsChart$data

    scales <- which_pal_scale(
      mapping = mapping,
      palette = paramsChart$colors$colors,
      data = data,
      reverse = paramsChart$colors$reverse
    )

    if (identical(input$geom, "auto")) {
      geom <- "blank"
    } else {
      geom <- input$geom
    }

    geom_args <- match_geom_args(input$geom, paramsChart$inputs, mapping = mapping)

    if (isTRUE(paramsChart$smooth$add) & input$geom %in% c("point", "line")) {
      geom <- c(geom, "smooth")
      geom_args <- c(
        setNames(list(geom_args), input$geom),
        list(smooth = paramsChart$smooth$args)
      )
    }

    scales_args <- scales$args
    scales <- scales$scales

    if (isTRUE(paramsChart$transX$use)) {
      scales <- c(scales, "x_continuous")
      scales_args <- c(scales_args, list(x_continuous = paramsChart$transX$args))
    }

    if (isTRUE(paramsChart$transY$use)) {
      scales <- c(scales, "y_continuous")
      scales_args <- c(scales_args, list(y_continuous = paramsChart$transY$args))
    }

    if (isTRUE(paramsChart$limits$x)) {
      xlim <- paramsChart$limits$xlim
    } else {
      xlim <- NULL
    }
    if (isTRUE(paramsChart$limits$y)) {
      ylim <- paramsChart$limits$ylim
    } else {
      ylim <- NULL
    }

    if (isTRUE(paramsChart$inputs$vol_line)){
      hline <- paramsChart$inputs$vol_line_logp
      vline <- paramsChart$inputs$vol_line_logfc
    }else{
      hline <- NULL
      vline <- NULL
    }

    # if (isTRUE(paramsChart$inputs$vol_markgene)){
    #   vol_markgene_text <- paramsChart$inputs$vol_markgene_text
    #   gene_id_list <- strsplit(vol_markgene_text, "\n")
    #   choose_data[data[["gene_id"]] %in% gene_id_list, ]
    # }else{
    #   vol_markgene_text <- NULL
    # }

    choose_data_fun <- eventReactive(c(paramsChart$inputs$vol_markgene_text, dataChart$data), {
      if (paramsChart$inputs$vol_markgene != "no"){
        if(paramsChart$inputs$vol_markgene == "table"){
          print(input$plotdata_rows_selected)
          data <- dataChart$data
          choose_data <- data[input$plotdata_rows_selected, ]
        }else{
          
          vol_markgene_text <- paramsChart$inputs$vol_markgene_text
          print(vol_markgene_text)
          gene_id_list <- strsplit(vol_markgene_text, "\n")
          print(gene_id_list)
          
          data <- dataChart$data
          # chhose_data <- data[input$plotdata_rows_selected, ]
          choose_data <-  data[data[["gene_id"]] %in% as.vector(gene_id_list[[1]]), ]
        }
      }else{
        choose_data <- NULL
      }
      choose_data
    })

    choose_data_name <- NULL


    if (paramsChart$inputs$vol_markgene!="no"){
      choose_data_name <- "choose_data"
    }else{
      choose_data_name <- NULL
    }

    

    data_name <- dataChart$name %||% "data"
    choose_data <- choose_data_fun()
    gg_call <- ggcall(
      data = data_name,
      mapping = mapping,
      geom = geom,
      geom_args = geom_args,
      scales = scales,
      scales_args = scales_args,
      labs = paramsChart$labs,
      theme = paramsChart$theme$theme,
      theme_args = paramsChart$theme$args,
      coord = paramsChart$coord,
      facet = input$dragvars$target$facet,
      facet_row = input$dragvars$target$facet_row,
      facet_col = input$dragvars$target$facet_col,
      facet_args = paramsChart$facet,
      xlim = xlim,
      ylim = ylim,
      hline = hline,
      vline = vline,
      markgene_data = choose_data_name
    )

    ggplotCall$code <- deparse2(gg_call)
    ggplotCall$call <- gg_call

    ggplotCall$ggobj <- safe_ggplot(
      expr = gg_call,
      data = setNames(list(data, choose_data), c(data_name, "choose_data"))
    )
    ggplotCall$ggobj$plot
  }, filename = "mggplot")


  # Close addin
  observeEvent(input$close, shiny::stopApp())

  # Ouput of module (if used in Shiny)
  output_module <- reactiveValues(code_plot = NULL, code_filters = NULL, data = NULL)
  observeEvent(ggplotCall$code, {
    output_module$code_plot <- ggplotCall$code
  }, ignoreInit = TRUE)
  observeEvent(paramsChart$data, {
    output_module$code_filters <- paramsChart$code
    output_module$data <- paramsChart$data
  }, ignoreInit = TRUE)

  return(output_module)
}

