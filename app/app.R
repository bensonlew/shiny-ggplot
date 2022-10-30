#---------------------------------------------------------------------
# Title:         IRIS - Shiny Application
# Author:        Brandon Monier)
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

    data <- reactive({
        # http://127.0.0.1:4366/?data_path=test1.diff.txt
        print(session)
        query <- parseQueryString(session$clientData$url_search)
        if(is.null(query$data_path)) {
            query$data = NULL
        }else{
            if(file.exists(query$data_path)){
                print(c("QUERY", query))
                query$data = read.csv(query$data_path, sep="\t", header = TRUE)
            }else{
                query$data = NULL
            }   
        }
        query

    })
    #       #   print(c("geo", query[['geo']]))
    #       geo_default_id = query[['geo']]


    callModule(module = mggplotServer,  id = "mggplot", data = data())
    ## url bookmark
    # observe({
    #     reactiveValuesToList(input)
    #     session$doBookmark()
    # })
    # onBookmarked(updateQueryString)
    

}



shinyApp(ui, server, enableBookmarking = "server")

