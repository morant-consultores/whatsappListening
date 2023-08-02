#' contenido_whats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import sf
#' @importFrom shiny NS tagList
mod_contenido_whats_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    fluidRow(
      column(3,
             selectInput(ns("nivel"), "Nivel", choices = c("Todos" = "",
                                                           "Distrito" = "distrito",
                                                           "Municipio" = "municipio"))
      ),
      column(3,
             dateInput(ns("fecha"),label = "Fecha", format = "dd-MM", language = "es")
      )
    ),
    fluidRow(id = ns("mapa_nivel"),
             column(12,
                    leafletOutput(ns("mapa"))
             )
    ),
    fluidRow(
      column(12,
             DTOutput(ns("tabla"))
      )
    )
  )
}

#' contenido_whats Server Functions
#'
#' @noRd
mod_contenido_whats_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    inicial <- reactive({
      tbl(pool, "resumen_conv_chis") |>
        collect() |>
        left_join(relacion, join_by(from)) |>
        tidyr::replace_na(list(nombre = "No identificado",
                               nivel = "No identificado",
                               unidad = "No identificada"))
    })

    observe({
      fecha <- range(inicial()$dia)
      updateDateInput(session = session, "fecha", value = fecha[[2]], min = fecha[[1]], max = fecha[[2]])
      if(input$nivel == ""){
        shinyjs::hide("mapa_nivel")
      } else{
        shinyjs::show("mapa_nivel")
      }
    })

    base <- reactive({
      a <- inicial()
      if(input$nivel != ""){
        a <- inicial() |>
          filter(nivel == input$nivel)
      }
      a <- a |>
        filter(dia == input$fecha)
    })

    shp <- eventReactive(input$nivel, {
      if(input$nivel == "distrito"){
        shp <- shp_df
      } else if (input$nivel == "municipio") {
        shp <- shp_mun
      } else {
        shp <- NULL
      }

      return(shp)
    })

    output$mapa <- renderLeaflet({
      req(input$nivel != "" & nrow(base()) > 0)
      a <- base() |>
        distinct(unidad, mensajes) |>
        mutate(mensajes = as.numeric(mensajes)) |>
        rename(!!rlang::sym(input$nivel) := unidad)

      aux <- shp() |>
        left_join(a)

      paleta <- colorRampPalette(c("#5e60ce", "white", "#48bfe3"))(5)

      pal <- colorNumeric(
        palette = paleta,
        domain = aux$mensajes
      )

      aux2 <- if_else(input$nivel == "distrito", "Distrito", "Municipio")

      lft <- leaflet(data = aux,
                     options = leafletOptions(zoomControl = FALSE)) %>%
        #setView(lng = -99.7612, lat = 19.395056, zoom = 8) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          weight = 1,
          stroke = TRUE,
          color = '#6c757d',
          fillColor = ~pal(mensajes),
          fillOpacity = 0.8,
          opacity = 0.5,
          popup = ~glue::glue('{aux2} {input$nivel} <br><br>
                            Mensajes escuchados: {mensajes}'),
          highlightOptions = highlightOptions(color = 'white', weight = 2,
                                              bringToFront = TRUE)
        ) %>%
        addLegend('bottomleft', pal = pal, values = ~mensajes,
                  title = glue::glue('Distribución de mensajes escuchados por {input$nivel}'))
    })

    output$tabla <- renderDT(server = FALSE, {
      validate(need(nrow(base() > 0), message = "En este día no se escucharon diálogos. Intenta con una nueva fecha"))

      base() |>
        select(nombre, dia, nivel, unidad, resumen) |>
        mutate(dia = format(dia, "%d de %B")) |>
        datatable(selection = 'none', rownames = FALSE, extensions =  'Buttons',
                  options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json',
                                                 searchPlaceholder = "Buscar...", sSearch = ""),
                                 lengthMenu = c(5, 10, 25, 50, 100),
                                 pageLength = 10,
                                 paging = TRUE,
                                 scrollX=TRUE,
                                 searching = TRUE,
                                 ordering = TRUE,
                                 buttons = c('csv', 'excel', "pdf"),
                                 dom = 'Bfrtip'),
                  escape = FALSE,
                  class = "display")
    })


  })
}

## To be copied in the UI
# mod_contenido_whats_ui("contenido_whats_1")

## To be copied in the server
# mod_contenido_whats_server("contenido_whats_1")
