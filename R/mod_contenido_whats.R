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
      # column(3,
      #        selectInput(ns("nivel"), "Nivel", choices = c("Todos" = "",
      #                                                      "Distrito" = "distrito",
      #                                                      "Municipio" = "municipio",
      #                                                      "Sección" = "seccion"))
      # ),
      column(3,
             dateInput(ns("fecha"),label = "Fecha", format = "dd-MM", language = "es")
      )
    ),
    # fluidRow(id = ns("mapa_nivel"),
    #          column(12,
    #                 leafletOutput(ns("mapa"))
    #          )
    # ),
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
      tbl(pool, "resumen_conv_tuxtla") |>
        collect() |>
        filter(resumen != "") |>
        mutate(categorias = categorias1,
               participantes = as.numeric(cantidad_participantes) - 1,
               fecha_nueva = format(if_else(is.na(fecha_nueva), as_date("2023-12-07"), fecha_nueva), '%d/%m/%y')) |>
        arrange(desc(as.numeric(total_mensajes_hoy))) |>
        select(fecha = fecha_nueva, nombre, resumen, participantes, mensajes = total_mensajes_hoy, categorias)
    })

    observe({
      fecha <- range(inicial()$fecha)
      updateDateInput(session = session, "fecha", value = dmy(fecha[[2]]), min = dmy(fecha[[1]]), max = dmy(fecha[[2]]))
      # if(input$nivel == ""){
      #   shinyjs::hide("mapa_nivel")
      # } else{
      #   shinyjs::show("mapa_nivel")
      # }
    })

    base <- reactive({
      inicial() |>
        filter(fecha == format(input$fecha, '%d/%m/%y'))
    })
    #
    # shp <- eventReactive(input$nivel, {
    #   if(input$nivel == "distrito"){
    #     shp <- shp_df
    #   } else if (input$nivel == "municipio") {
    #     shp <- shp_mun
    #   } else if (input$nivel == "seccion") {
    #     shp <- shp_secc
    #   } else {
    #     shp <- NULL
    #   }
    #   return(shp)
    # })

    # output$mapa <- renderLeaflet({
    #   req(input$nivel != "" & nrow(base()) > 0)
    #   validate(need((input$nivel != "" & nrow(base()) > 0), message = "En este día no se escucharon diálogos. Intenta con una nueva fecha"))
    #   temp <- case_when(input$nivel == "distrito" ~ "nombre_distrito",
    #                     T ~ input$nivel)
    #   a <- base() |>
    #     distinct(unidad, mensajes) |>
    #     mutate(mensajes = as.numeric(mensajes)) |>
    #     rename(!!rlang::sym(input$nivel) := unidad)
    #
    #   aux <- shp() |>
    #     left_join(a) |>
    #     mutate(grupo = !!rlang::sym(temp))
    #
    #   paleta <- colorRampPalette(c("#5e60ce", "white", "#48bfe3"))(5)
    #
    #   pal <- colorNumeric(
    #     palette = paleta,
    #     domain = aux$mensajes
    #   )

    #   aux2 <- case_when(input$nivel == "distrito" ~ "Distrito",
    #                     input$nivel == "municipio" ~ "Municipio",
    #                     input$nivel == "seccion" ~ "Sección")
    #
    #   lft <- leaflet(data = aux,
    #                  options = leafletOptions(zoomControl = FALSE)) %>%
    #     #setView(lng = -99.7612, lat = 19.395056, zoom = 8) %>%
    #     addProviderTiles(providers$CartoDB.Positron) %>%
    #     addPolygons(
    #       weight = 1,
    #       stroke = TRUE,
    #       color = ~pal(mensajes),
    #       fillColor = ~pal(mensajes),
    #       fillOpacity = 0.8,
    #       opacity = 0.5,
    #       popup = ~glue::glue('{aux2}: {grupo} <br>
    #                         Mensajes escuchados: {mensajes}'),
    #       highlightOptions = highlightOptions(color = 'white', weight = 2,
    #                                           bringToFront = TRUE)
    #     ) %>%
    #     addLegend('bottomleft', pal = pal, values = ~mensajes,
    #               title = glue::glue('Número de mensajes por {input$nivel}'))
    # })

    output$tabla <- renderDT(server = FALSE, {
      validate(need(nrow(base() > 0), message = "En este día no se escucharon diálogos. Intenta con una nueva fecha"))
      base() |>
        rename_all(toupper) |>
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
