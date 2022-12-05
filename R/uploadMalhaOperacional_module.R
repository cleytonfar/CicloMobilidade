


uploadMalhaOperacionalInput <- function(id) {
    ns = NS(id)
    tagList(
        fileInput(
            inputId = ns("uploadMalhaOperacional"),
            label = "Carregar Arquivo Malha Operacional (geojson)",
            buttonLabel = "Procurar",
            accept = c(".geojson"), 
            multiple = F
        ),
        actionButton(
            inputId = ns("adicionar"),
            label = "Adicionar"
        )
    )
}


uploadMalhaOperacionalServer <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            observeEvent(input$adicionar, {
                req(input$uploadMalhaOperacional)
                
                # show the waiter
                waiter_show(
                    color = transparent(.5),
                    html = spin_3() # use a spinner
                )
                
                # reading
                data = geojsonsf::geojson_sf(input$uploadMalhaOperacional$datapath)
                data = data[!st_is_empty(data),]
                # saving:
                saveRDS(data, "data/malhaOperacional.rds")
                
                # Interseção com malha permanente:
                malhaPermanente = readRDS("data/malhaPermanente.rds")
                # calculando intersecao
                intersecao = calcIntersectionMalhas(
                    malhaPermanente = malhaPermanente,
                    data
                )
                # saving:
                saveRDS(intersecao, "data/intersecao.rds")
                
                #  hide the waiter
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

uploadMalhaOperacionalApp <- function(){
    ui = fluidPage(
        shinyWidgets::useSweetAlert(),
        waiter::use_waiter(),
        sidebarLayout(
            sidebarPanel(
                uploadMalhaOperacionalInput("id1")
            ),
            mainPanel()
        )
    )
    server = function(input, output, session) {
        uploadMalhaOperacionalServer("id1")
    }
    shinyApp(ui, server)
}
