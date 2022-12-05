
source("R/uploadStrava_module.R")
source("R/uploadMalhaPermanente_module.R")
source("R/uploadMalhaPDC_module.R")
source("R/uploadMalhaOperacional_module.R")
source("R/utils.R")

uploadInput <- function(id) {
    ns = NS(id) 
    tagList(
        fluidRow(
            box(width = 12,
                title = "Dados Strava",
                uploadStravaInput(ns("stravaTrafego"))
            )
        ),
        fluidRow(
            box(width = 12,
                title = "Malha Cicloviária Permanente", 
                uploadMalhaPermanenteInput(ns("malhaPermanente"))
            )
        ),
        fluidRow(
            box(width = 12,
                title = "Malha Plano Diretor Cicloviário", 
                uploadMalhaPDCInput(ns("malhaPDC"))
            )
        ),
        fluidRow(
            box(width = 12,
                title = "Malha Cicloviária Operacional", 
                uploadMalhaOperacionalInput(ns("malhaOperacional"))
            )
        )
    )
}

uploadServer <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            uploadMalhaPermanenteServer("malhaPermanente")
            uploadMalhaPDCServer("malhaPDC")
            uploadMalhaOperacionalServer("malhaOperacional")
            uploadStravaServer("stravaTrafego")
        }
    )
}

uploadApp <- function() {
    ui <- dashboardPage(
        header = dashboardHeader(),
        sidebar = dashboardSidebar(),
        body = dashboardBody(
            shinyWidgets::useSweetAlert(),
            waiter::use_waiter(),
            uploadInput("id1")
        )
    )
    server = function(input, output, session) {
        uploadServer("id1")
    }
    
    shinyApp(ui, server)
}

