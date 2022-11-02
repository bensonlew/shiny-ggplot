# 保存当前环境参数到硬盘
# session.
saveState <- function(state) {
  id <- createUniqueId(8)

  # A function for saving the state object to disk, given a directory to save
  # to.
  saveState <- function(stateDir) {
    state$dir <- stateDir

    # Allow user-supplied onSave function to do things like add state$values, or
    # save data to state dir.
    if (!is.null(state$onSave))
      isolate(state$onSave(state))

    # Serialize values, possibly saving some extra data to stateDir
    exclude <- c(state$exclude, "._bookmark_")
    inputValues <- serializeReactiveValues(state$input, exclude, state$dir)
    saveRDS(inputValues, file.path(stateDir, "input.rds"))

    # If values were added, save them also.
    if (length(state$values) != 0)
      saveRDS(state$values, file.path(stateDir, "values.rds"))
  }

  # Pass the saveState function to the save interface function, which will
  # invoke saveState after preparing the directory.

  # Look for a save.interface function. This will be defined by the hosting
  # environment if it supports bookmarking.
  saveInterface <- getShinyOption("save.interface", default = NULL)

  if (is.null(saveInterface)) {
    if (inShinyServer()) {
      # We're in a version of Shiny Server/Connect that doesn't have
      # bookmarking support.
      saveInterface <- function(id, callback) {
        stop("The hosting environment does not support saved-to-server bookmarking.")
      }

    } else {
      # We're running Shiny locally.
      saveInterface <- saveInterfaceLocal
    }
  }

  saveInterface(id, saveState)

  paste0("_state_id_=", encodeURIComponent(id))
}


loadState = function(stateDir) {
    values <- parseQueryString(queryString, nested = TRUE)
    id <- values[["_state_id_"]]



    # This function is passed to the loadInterface function; given a
    # directory, it will load state from that directory
    loadFun <- function(stateDir) {
    self$dir <- stateDir

    if (!dirExists(stateDir)) {
        stop("Bookmarked state directory does not exist.")
    }

    tryCatch({
        inputValues <- readRDS(file.path(stateDir, "input.rds"))
        self$input <- RestoreInputSet$new(inputValues)
        },
        error = function(e) {
        stop("Error reading input values file.")
        }
    )

    valuesFile <- file.path(stateDir, "values.rds")
    if (file.exists(valuesFile)) {
        tryCatch({
            self$values <- readRDS(valuesFile)
        },
        error = function(e) {
            stop("Error reading values file.")
        }
        )
    }
    }

    # Look for a load.interface function. This will be defined by the hosting
    # environment if it supports bookmarking.
    loadInterface <- getShinyOption("load.interface", default = NULL)

    if (is.null(loadInterface)) {
        if (inShinyServer()) {
            # We're in a version of Shiny Server/Connect that doesn't have
            # bookmarking support.
            loadInterface <- function(id, callback) {
            stop("The hosting environment does not support saved-to-server bookmarking.")
            }

        } else {
            # We're running Shiny locally.
            loadInterface <- loadInterfaceLocal
        }
    }
    loadInterface(id, loadFun)

    invisible()
}

