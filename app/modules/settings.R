
#' @importFrom htmltools tags tagList
#' @importFrom shiny getDefaultReactiveDomain modalDialog
#' @importFrom shinyWidgets alert prettyCheckboxGroup
modal_settings <- function(aesthetics = NULL, session = shiny::getDefaultReactiveDomain()) {
  ns <- session$ns
  modalDialog(
    title = tagList(
      mgi18n("Esquisse settings"),
      tags$button(
        ph("x", title = mgi18n("Close")),
        title = mgi18n("Close"),
        class = "btn btn-default",
        style = "border: 0 none; position: absolute; top: 5px; right: 5px;",
        `data-dismiss` = "modal",
        `data-bs-dismiss` = "modal"
      )
    ),
    tags$label(
      mgi18n("Select aesthetics to be used to build a graph:"),
      `for` = ns("aesthetics"),
      class = "control-label"
    ),
    shinyWidgets::alert(
      ph("info"),
      mgi18n("Aesthetic mappings describe how variables in the data are mapped to visual properties (aesthetics) of geoms."),
      status = "info"
    ),
    prettyCheckboxGroup(
      inputId = ns("aesthetics"),
      label = NULL,
      choiceNames = list(
        tagList(tags$b("fill:"), mgi18n("fill color for shapes")),
        tagList(tags$b("color:"), mgi18n("color points and lines")),
        tagList(tags$b("size:"), mgi18n("size of the points")),
        tagList(tags$b("shape:"), mgi18n("shape of the points")),
        tagList(tags$b("weight:"), mgi18n("frequency weights")),
        tagList(tags$b("group:"), mgi18n("identifies series of points with a grouping variable")),
        tagList(tags$b("ymin:"), mgi18n("used in ribbons charts with ymax to display an interval between two lines")),
        tagList(tags$b("ymax:"), mgi18n("used in ribbons charts with ymin to display an interval between two lines")),
        tagList(tags$b("facet:"), mgi18n("create small multiples")),
        tagList(tags$b("facet row:"), mgi18n("create small multiples by rows")),
        tagList(tags$b("facet col:"), mgi18n("create small multiples by columns"))
      ),
      choiceValues = c("fill", "color", "size", "shape", "weight", "group", "ymin", "ymax", "facet", "facet_row", "facet_col"),
      selected = aesthetics %||% c("fill", "color", "size", "facet"),
      status = "primary"
    ),
    easyClose = TRUE,
    footer = NULL,
    size = "m"
  )
}

pre_settings <- function(view = NULL, session = shiny::getDefaultReactiveDomain()) {
  ns <- session$ns
  modalDialog(
    title = tagList(
      mgi18n("显示设置"),
      tags$button(
        ph("x", title = mgi18n("Close")),
        title = mgi18n("Close"),
        class = "btn btn-default",
        style = "border: 0 none; position: absolute; top: 5px; right: 5px;",
        `data-dismiss` = "modal",
        `data-bs-dismiss` = "modal"
      )
    ),
    tags$label(
      mgi18n("Select setting of page"),
      `for` = ns("view"),
      class = "control-label"
    ),
    shinyWidgets::alert(
      ph("info"),
      mgi18n("settings"),
      status = "info"
    ),
    prettyCheckboxGroup(
      inputId = ns("view"),
      label = NULL,
      choiceNames = list(
        tagList(tags$b("映射"), mgi18n("展示映射关系")),
        tagList(tags$b("表格"), mgi18n("展示表格")),
        tagList(tags$b("图片"), mgi18n("展示图片")),
        tagList(tags$b("固定尺寸"), mgi18n("固定尺寸")),
        tagList(tags$b("输入数据"), mgi18n("输入数据"))
      ),
      choiceValues = c("show_aes", "show_table", "show_img", "fix_img_size", "show_importdata"),
      selected = view %||% c("show_aes", "show_table", "show_img", "fix_img_size", "show_importdata"),
      status = "primary"
    ),
    easyClose = TRUE,
    footer = NULL,
    size = "m"
  )
}

save_load <- function(save = NULL, session = shiny::getDefaultReactiveDomain()) {
  ns <- session$ns
  load_records <- c()
  modalDialog(
    title = tagList(
      mgi18n("save & load"),
      tags$button(
        ph("x", title = mgi18n("Save")),
        title = mgi18n("Save"),
        class = "btn btn-default",
        style = "border: 0 none; position: absolute; top: 5px; right: 5px;",
        `data-dismiss` = "modal",
        `data-bs-dismiss` = "modal"
      )
    ),
    tags$label(
      mgi18n("Select setting of page"),
      `for` = ns("view"),
      class = "control-label"
    ),
    shinyWidgets::alert(
      ph("info"),
      mgi18n("save"),
      status = "info"
    ),

    textInput(inputId = 'save_name', 
      label = 'save_name', 
      value="mggplot-result"),

    actionButton(
      inputId = "save", label = "save",
      icon = icon("file-powerpoint-o"),
      class = "btn-block btn-primary"
    ),
    selectInput(inputId = 'load_records',
      label = 'load_records',
      choices = names(load_records),
      selected = names(load_records)[[0]]),
    actionButton(
      inputId = "load", label = "load",
      icon = icon("file-powerpoint-o"),
      class = "btn-block btn-primary"
    ),

    easyClose = TRUE,
    footer = NULL,
    size = "m"
  )
}


save_load_server <- function(id,
                                ){
  callModule(
    id = id,
    module = function(input, output, session) {
      observeEvent(input$save,{
        
      })
      observeEvent(input$load,{

      })

    }
  )

}




