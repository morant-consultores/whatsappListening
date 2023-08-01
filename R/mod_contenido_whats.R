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
    shinyjs::hidden(
      fluidRow(id = "mapa_nivel",
               column(12,
                      leafletOutput(ns("mapa"))
               )
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
    })

    observeEvent(input$nivel, {
      shinyjs::toggle("mapa_nivel", condition = input$nivel != "")
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

    # output$mapa <- renderLeaflet({
    #
    #   validate(need(sum(base()$mensajes) > 0, "Sin información suficiente"))
    #
    #   a <- base() |>
    #     distinct(unidad, mensajes) |>
    #     mutate(mensajes = as.numeric(mensajes))
    #
    #   aux <- shp_df |>
    #     left_join(a, by = c("distrito" = "unidad")) %>%
    #     tidyr::replace_na(list(mensajes = 0))
    #
    #   paleta <- colorRampPalette(c(complemento, "white", morena))(10)
    #
    #   pal <- colorNumeric(
    #     palette = paleta,
    #     domain = seq(min(aux$mensajes), max(aux$mensajes) , by = max(aux$mensajes)/10)
    #   )
    #
    #   lft <- leaflet(data = aux,
    #                  options = leafletOptions(zoomControl = FALSE)) %>%
    #     setView(lng = -99.7612, lat = 19.395056, zoom = 8) %>%
    #     addProviderTiles(providers$CartoDB.Positron) %>%
    #     addPolygons(
    #       weight = 1,
    #       stroke = TRUE,
    #       color = '#6c757d',
    #       fillColor = ~pal(mensajes),
    #       fillOpacity = 0.8,
    #       opacity = 0.5,
    #       popup = ~glue::glue('Entidad: Distrito {distrito} <br><br>
    #                         Mensajes escuchados: {mensajes}'),
    #       highlightOptions = highlightOptions(color = 'white', weight = 2,
    #                                           bringToFront = TRUE)
    #     ) %>%
    #     addLegend('bottomleft', pal = pal, values = ~mensajes,
    #               title = 'Distribución de mensajes escuchados por distrito')
    # })

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
