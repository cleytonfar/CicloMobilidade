
uploadMalhaPermanenteInput <- function(id) {
    ns = NS(id)
    tagList(
        fileInput(
            inputId = ns("uploadMalhaPermanente"),
            label = "Carregar Arquivo Malha Permanente (geojson)",
            buttonLabel = "Procurar",
            accept = c(".geojson"),
            placeholder = "Nenhum arquivo selecionado",
            multiple = F
        ),
        actionButton(
            inputId = ns("adicionar"),
            label = "Adicionar"
        )
    )
}


uploadMalhaPermanenteServer <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            
            # # dados de trafego do strava:
            # rides2 = reactivePoll(
            #     intervalMillis = 1000, session = session,
            #     checkFunc = function() file.mtime("data/rides2.rds"),
            #     valueFunc = function() readRDS("data/rides2.rds")
            # )            

            observeEvent(input$adicionar, {
                
                req(input$uploadMalhaPermanente)
                
                # show the waiter
                waiter_show(
                    color = transparent(.5),
                    html = spin_3() # use a spinner
                )
                
                # reading:
                malhaPermanente = geojsonsf::geojson_sf(input$uploadMalhaPermanente$datapath)
                malhaPermanente = malhaPermanente[!st_is_empty(malhaPermanente),]
                # saving:
                saveRDS(malhaPermanente, "data/malhaPermanente.rds")
                
                # 2. merge espacial com strava processado:
                # reading dados de trafego do strava processado:
                rides2 = readRDS("data/rides2.rds")
                # spatial merge 
                rides3 = spatialMerge_stravaMalhaPermanente(
                    strava = rides2,
                    malhaPermanente = malhaPermanente
                )

                # saving:
                saveRDS(rides3, "data/strava.rds")
                
                # hide the waiter
                waiter_hide() 
                
                shinyWidgets::sendSweetAlert(
                    session = session,
                    title = "Sucesso!!",
                    text = "Arquivo salvo",
                    type = "success"
                )
                
            })
        }
    )
}

uploadMalhaPermanenteApp <- function(){
    ui = fluidPage(
        shinyWidgets::useSweetAlert(),
        waiter::use_waiter(),
        sidebarLayout(
            sidebarPanel(
                uploadMalhaPermanenteInput("id1")
            ),
            mainPanel()
        )
    )
    
    server = function(input, output, session) {
        uploadMalhaPermanenteServer("id1")
    }
    shinyApp(ui, server)
}
