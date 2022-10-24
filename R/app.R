#---------------------------------------------------------------------
# Title:         IRIS - Shiny Application
# Author:        Brandon Monier
# Created:       2018-01-26 11:29:39 CDT
# Last Modified: 2018-05-25 14:54:04 CDT
#---------------------------------------------------------------------

## Set working directory (FOR LOCAL TESTING ONLY)

os_type = .Platform$OS.type
options(shiny.reactlog=TRUE) 

# database
library(shiny)
library(shinyWidgets)
library(esquisse)
library(utils)
library(ggplot2)
library(htmltools)
library(stats)
library(scales)
library(datamods)
library(DT)

source("utils.R")
source("default-options.R")
source("esquisserUI2.R")
source("utils-shiny.R")
source("module-controls.R")
source("input-colors.R")
source("esquisserServer2.R")
source("mapping.R")
source("settings.R")
  
my_data <- data.frame(
var1 = rnorm(100),
var2 = sample(letters[1:5], 100, TRUE)
)


ui <- fluidPage(
esquisserUI2(
    id = "esquisse", 
    container = esquisseContainer(fixed = TRUE)
)
)

server <- function(input, output, session) {

callModule(module = esquisserServer2, id = "esquisse")

}
  

  
shinyApp(ui, server)

