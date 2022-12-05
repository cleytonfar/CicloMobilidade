# Attaching packages:
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(tidyverse)
library(data.table)
library(stringr)
library(purrr)
library(DT)
library(leaflet)
library(sf)
library(lubridate)

load("data/mercados.rda")
load("data/pontosTuristicos.rda")
load("data/bikePE.rda")


analiseMalhaInput = function(id) {
    ns = NS(id)
    tagList(
        fluidRow(
            column(
                width = 4,
                offset = 4,
                align = "center",
                valueBoxOutput(
                    width = NULL,
                    outputId = ns("intersecaoKM")
                )
            )
            
        ),
        fluidRow(
            box(width = 12,
                title = "Malha Cicloviária da Cidade do Recife",
                solidHeader = T,
                status = "primary",
               
                leafletOutput(
                    outputId = ns("myMap"),
                    height = 800
                )
            )
        )
    )
}

analiseMalhaServer = function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            # shapefile da malha cicloviaria permanente:
            malhaPermanente <- reactivePoll(
                intervalMillis = 1000, session = session,
                checkFunc = function() file.mtime("data/malhaPermanente.rds"),
                valueFunc = function() readRDS("data/malhaPermanente.rds")
            )
            
            # shapefile da malha cicloviaria permanente:
            malhaOperacional <- reactivePoll(
                intervalMillis = 1000, session = session,
                checkFunc = function() file.mtime("data/malhaOperacional.rds"),
                valueFunc = function() readRDS("data/malhaOperacional.rds")
            )
            
            # shapefile da interseção:
            intersecao <- reactivePoll(
                intervalMillis = 1000, session = session,
                checkFunc = function() file.mtime("data/intersecao.rds"),
                valueFunc = function() readRDS("data/intersecao.rds")
            )
            
            # strava FDS:
            stravaFDS = readRDS("data/stravaFDS.rds")
            
            # intersecao:
            output$intersecaoKM = renderValueBox({
                # total KM:
                totalKM = intersecao() %>% 
                    filter(Sentido == Sentido.1) %>% 
                    summarize(total = sum(length)) %>% 
                    pull(total)
                # formatting
                totalKM = format(round(totalKM/1e3, 2), decimal.mark = ",", big.mark = ".", nsmall=2)
                
                valueBox(
                    value = str_c(totalKM, " KM"),
                    subtitle = "Extensão da interseção entre malhas",
                    color = 'red',
                    icon = icon("road", lib = "font-awesome", verify_fa = F)
                )
            })
            
            
            # Mapa com as malhas Permanente e Operacional:
            output$myMap = renderLeaflet({
                 # icon BikePE
                iconBikePE = makeIcon(
                    iconUrl = "www/faviconBikePE.ico",
                    iconWidth = 25,
                    iconHeight = 25
                )
                
                # icon MercadoPublico
                iconMercado = makeIcon(
                    iconUrl = "www/faviconMercadoPublico.ico",
                    #iconUrl = "deployable/www/faviconMercadoPublico.ico",
                    iconWidth = 25,
                    iconHeight = 25
                )
                
                # icon pontos turísticos
                iconSights = makeIcon(
                    iconUrl = "www/faviconSights.ico",
                    #iconUrl = "deployable/www/favi;conSights.ico",
                    iconWidth = 25,
                    iconHeight = 25
                )
                
                # criando paleta de cores:
                pal = colorFactor(
                    palette = c("#ef3b2c", "#cb181d", "#67000d"),
                    levels = c("baixo", "médio", "alto")
                )
                
                # plot
                leaflet() |> 
                    addTiles() |> 
                    setView(lat = -8.0663, lng = -34.9321, zoom = 13) %>% 
                    addLayersControl(
                        overlayGroups = c("Tráfego Reportado no Domingo (Strava)",
                                          "Malha Permanente", 
                                          "Malha Operacional",
                                          "Interseção entre malhas",
                                          "Estação Bike PE",
                                          "Mercados Públicos", 
                                          "Pontos Turísticos"),
                        options = layersControlOptions(collapsed = FALSE)
                    ) %>% 
                    addPolylines(
                        data = st_as_sf(stravaFDS),
                        color = ~pal(get("trip_count_cat")),
                        label = ~name,
                        popup = ~paste(name, "<br>", "Nível tráfego:", trip_count_cat),
                        labelOptions = labelOptions(direction = "top"),
                        group = "Tráfego Reportado no Domingo (Strava)"
                    ) %>% 
                    addPolylines(
                        data = malhaPermanente(), 
                        color = "green", 
                        group = "Malha Permanente",
                        label = ~Logradouro,
                        popup = ~paste(Nome, "<br>", "Sentido:", Sentido),
                    ) |> 
                    addPolylines(
                        data = malhaOperacional(), 
                        group = "Malha Operacional",
                        label = ~Nome,
                        popup = ~paste(Nome, "<br>", "Sentido:", Sentido),
                    ) |> 
                    addPolylines(
                        data = filter(intersecao(), Sentido == Sentido.1), 
                        color = "red", 
                        label = "Interseção",
                        popup = ~paste(Logradouro, "<br>", "Extensão: ", round(length/1e3, 2), "KM"),
                        group = "Interseção entre malhas"
                    ) |> 
                    addMarkers(
                        data = bikePE,
                        lng = ~longitude, lat = ~latitude,
                        label = ~nome2, popup = ~content,
                        icon = iconBikePE,
                        group = "Estação Bike PE"
                    ) %>% 
                    addMarkers(
                        data = mercados, icon = iconMercado,
                        label = ~nome,
                        lng = ~longitude,
                        lat = ~latitude,
                        group = "Mercados Públicos"
                    ) |>
                    addMarkers(
                        data = filter(pontosTuristicos, !str_detect(PTurist, "Mercado")),
                        icon = iconSights,
                        label = ~PTurist,
                        group = "Pontos Turísticos"
                    ) %>% 
                    hideGroup(
                        group = c("Mercados Públicos", 
                                  "Pontos Turísticos",
                                  "Estação Bike PE")
                    )
            })

        }
    )
}


analiseMalhaApp = function() {
    ui = dashboardPage(
        header = dashboardHeader(),
        sidebar = dashboardSidebar(
            sidebarMenu(
                menuItem(
                    text = "Análise das Malhas",
                    tabName = "analise_malha",
                    icon = icon("road", lib = "font-awesome", verify_fa=F)
                )
            )
        ),
        body = dashboardBody(
            waiter::use_waiter(),
            waiter::autoWaiter(
                color = transparent(.5),
                html = spin_3() # use a spinner
            ),
            tabItems(
                tabItem(
                    tabName = "analise_malha",
                    analiseMalhaInput("id1")
                )
            )
        )
    )
    server = function(input, output, session){
        analiseMalhaServer("id1")
    }
    shinyApp(ui, server)
}

