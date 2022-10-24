#---------------------------------------------------------------------
# Title:         IRIS - Shiny Application
# Author:        Brandon Monier
# Created:       2018-01-26 11:29:39 CDT
# Last Modified: 2018-05-25 14:54:04 CDT
#---------------------------------------------------------------------

## Set working directory (FOR LOCAL TESTING ONLY)

os_type = .Platform$OS.type
options(shiny.reactlog=TRUE)



# databasei18n
wd = getwd()
source("import-package.R")

set_i18n("cn", packages = c("datamods", "esquisse"))


my_data <- data.frame(
    var1 = rnorm(100),
    var2 = sample(letters[1:5], 100, TRUE)
)


ui <- fluidPage(
    useShinyjs(debug = TRUE),
    mggplotUI(
        id = "mggplot",
        container = esquisseContainer(fixed = TRUE)
    )
)

server <- function(input, output, session) {



callModule(module = mggplotServer, id = "mggplot")

}



shinyApp(ui, server)

