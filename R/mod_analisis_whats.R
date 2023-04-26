#' analisis_whats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @import leaflet forecast sf
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analisis_whats_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    tabsetPanel(type = "tabs",
                tabPanel("General",
                         fluidRow(
                           col_8(
                             h1("Análisis general")
                           )
                         ),
                         fluidRow(
                           column(3,
                                  shinyWidgets::prettyRadioButtons(ns("nivel"), "Nivel",
                                                                   choices = c("Distritos" = "distrito",
                                                                               "Municipios" = "municipio"), inline = T)
                           )
                         ),
                         fluidRow(
                           valueBoxOutput(ns("total_msg"), width = 4),
                           valueBoxOutput(ns("diario_msg"), width = 4),
                           valueBoxOutput(ns("prom_msg"), width = 4)
                         ),
                         fluidRow(
                           box(
                             width = 6,
                             status = 'primary',
                             title = 'Progreso de escucha de grupos',
                             uiOutput(ns('progreso_grupo'))
                           ),
                           valueBoxOutput(ns('fecha_meta'), width = 6)
                         ),
                         hr(),
                         fluidRow(
                           column(6,
                                  highchartOutput(ns("linea_msg"))
                           ),
                           column(6,
                                  highchartOutput(ns("linea_gpo"))
                           )
                         ),
                         hr(),
                         fluidRow(
                           column(12,
                                  leafletOutput(ns("mapa"))
                           )
                         ),
                         hr(),
                         fluidRow(
                           column(6,
                                  highchartOutput(ns("dist_grupo"))
                           ),
                           col_6(
                             gt::gt_output(ns("top"))
                           )
                         )
                ),
                tabPanel("Contenido",
                         mod_contenido_whats_ui(ns("contenido_whats_1"))
                )
    )
  )
}

#' analisis_whats Server Functions
#'
#' @noRd
mod_analisis_whats_server <- function(id, inicial){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_contenido_whats_server("contenido_whats_1")

    bd <- eventReactive(input$nivel, {
      aux <- ifelse(input$nivel == "distrito", "5215578721958@c.us", "5215568913223@c.us")

      inicial() |>
        filter(to == !!aux)
    })

    grupos <- reactive({
      clave |>
        inner_join(bd(), by = "from")
    })

    shp <- eventReactive(input$nivel,{
      if(input$nivel == "distrito"){
        shp_df
      }
      else if(input$nivel == "municipio") {
        shp_mun
      }
    })

    output$total_msg <- renderValueBox({
      a <- nrow(bd())

      valueBox(value = scales::comma(a), subtitle = "Total de mensajes recibidos", icon = icon("comments"))
    })

    output$diario_msg <-  renderValueBox({

      a <- calcular_var_diarios(bd()) %>%
        summarise(n = round(mean(n, na.rm = T), 0)) %>%
        pull()

      valueBox(value = scales::comma(a), subtitle = "Promedio de mensajes por día", icon = icon("envelope-open"))
    })

    output$prom_msg <-  renderValueBox({

      a <- bd() %>%
        count(author) %>%
        summarise(n = round(mean(n), 0)) %>%
        pull(n)

      valueBox(value = scales::comma(a), subtitle = "Promedio de mensajes por usuario", icon("comment-alt"))
    })

    output$fecha_meta <- renderValueBox({

      a <- calcular_var_diarios(bd(), grupo = T, from, .keep_all = T)

      dias <- (6534 - sum(a$n))/as_tibble(forecast(ets(a$n), h = 1))[[1]]

      fecha = format(Sys.Date() + dias, format = "%d de %B %Y")

      valueBox(subtitle = "Día en que se alcanzaría la meta", value = fecha, icon = icon("calendar-check"))

    })

    output$progreso_grupo <- renderUI({

      a <- distinct(bd(), grupo_wa) %>%
        tally() %>%
        pull()

      tags$div(
        progressGroup("Grupos escuchados", value = a, max = 6534, color = 'red'),
        hr(),
        hr()
      )
    })

    output$linea_msg <- renderHighchart({

      a <- calcular_var_diarios(bd())

      graficar_tendencia(a, titulo = "Mensajes enviados por día",
                         yaxis = "Mensajes recibidos")

    })

    output$linea_gpo <- renderHighchart({

      a <- calcular_var_diarios(bd(), grupo = T, dia, from)

      graficar_tendencia(a, titulo = "Grupos con mensajes enviados por día",
                         yaxis = "Grupos con mensajes enviados")
    })

    output$dist_grupo <- renderHighchart({

      a <- bd() %>%
        group_by(from) %>%
        summarise(n = n())

      dat <- a %>%
        data_to_boxplot(variable = n,
                        name = "Distribución de mensajes por grupo",
                        color = "#A30039",
                        add_outliers = TRUE)

      highchart() %>%
        hc_chart(inverted = TRUE, style = list(fontFamily = 'Poppins')) %>%
        hc_xAxis(title = list(text = ""),
                 labels = list(enabled = FALSE)) %>%
        hc_add_series_list(dat) %>%
        hc_legend(enabled = FALSE) %>%
        hc_yAxis(title = list(text = "Mensajes"),
                 labels = list(fontSize = '14px', width = '100px'),
                 gridLineWidth = 0) %>%
        hc_title(text = "Distribución de mensajes por grupo")


    })

    output$mapa <- renderLeaflet({

      seccion <- grupos() |>
        filter(nivel == !!input$nivel) |>
        distinct(unidad, from, nombre_distrito) |>
        count(!!input$nivel := unidad, nombre_distrito)

      aux <- shp() %>%
        left_join(seccion) %>%
        tidyr::replace_na(list(n = 0))

      paleta <- colorRampPalette(c(complemento, "white", morena))(10)

      pal <- colorNumeric(
        palette = paleta,
        domain = seq(min(aux$n), max(aux$n) , by = max(aux$n)/10)
      )

      lft <- leaflet(data = aux,
                     options = leafletOptions(zoomControl = FALSE)) %>%
        setView(lng = -99.7612, lat = 19.395056, zoom = 8) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          weight = 1,
          stroke = TRUE,
          color = '#6c757d',
          fillColor = ~pal(n),
          fillOpacity = 0.8,
          opacity = 0.5,
          popup = ~glue::glue('Entidad: {nombre_distrito} <br><br>
                            Grupos activos: {n}' ),
          highlightOptions = highlightOptions(color = 'white', weight = 2,
                                              bringToFront = TRUE)
        ) %>%
        addLegend('bottomleft', pal = pal, values = ~n,
                  title = 'Grupos activos por distrito')

      return(lft)

    })

    output$top <- gt::render_gt({
      obtener_mayor_participacion(bd()) %>%
        gt::gt() %>%
        tab_header(
          title = glue::glue("Top 10: Usuarios más activos")
        ) %>%
        cols_label(nombre = "NOMBRE (GRUPO)") %>%
        opt_all_caps() %>%
        tab_style(
          style = cell_borders(
            sides = "bottom", color = "transparent", weight = px(2)
          ),
          locations = cells_body(
            columns = TRUE#,
            #rows = nrow(bd_w())
          )
        )  %>%
        tab_options(
          table.width = px(500),
          heading.title.font.weight = "bold",
          heading.align = "left"
        )
    })

  })
}

## To be copied in the UI
# mod_analisis_whats_ui("analisis_whats_1")

## To be copied in the server
# mod_analisis_whats_server("analisis_whats_1")
