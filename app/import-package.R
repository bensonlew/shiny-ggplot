library(shiny)
library(shinyjs)
library(shinyWidgets)
library(esquisse)
library(utils)
library(ggplot2)
library(htmltools)
library(stats)
library(scales)
library(datamods)
library(DT)
library(phosphoricons)
library(jsonlite)
library(rlang)
library("ggrepel")

# source("onLoad.R")
source("utils/utils.R")
source("utils/default-options.R")
source("utils/utils-shiny.R")

source("modules/module-controls.R")
source("modules/input-colors.R")
source("modules/export.R")
source("utils/mapping.R")
source("modules/settings.R")
source("modules/input-drop.R")
source("modules/input-dragula.R")
source("utils/ggcall.R")
source("utils/safe_ggplot.R")
source("utils/geometries.R")
source("utils/save.R")

mytrantab = read.csv("inst/i18n/cn.csv")
mgi18n <- function(a){
    i18n(a, mytrantab)
}

source("esquisse-ui.R")
source("mggplotUI.R")
source("mggplotServer.R")


shiny::registerInputHandler("esquisse.dragula", function(data, ...) {
    if (is.null(data)) {
        NULL
    } else {
        data$source <- unlist(data$source)
        data$target <- lapply(data$target, unlist, recursive = FALSE)
        names(data$target) <- idToChar(names(data$target))
        data
    }
}, force = TRUE)
