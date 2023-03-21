#' analisis_whats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
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
                           )
                         ),
                         hr(),
                         fluidRow(
                           column(12,
                                  highchartOutput(ns("linea_msg"))
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
                )
    )
  )
}

#' analisis_whats Server Functions
#'
#' @noRd
mod_analisis_whats_server <- function(id, bd){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$progreso_grupo <- renderUI({

      a <- distinct(bd(), grupo_wa) %>%
        tally() %>%
        pull()

      tags$div(
        progressGroup("Grupos escuchados", value = a, max = 6000, color = 'red'),
      )
    })

    output$total_msg <- renderValueBox({
      a <- nrow(bd())

      valueBox(value = a, subtitle = "Total de mensajes recibidos", icon = icon("comments"))
    })

    output$diario_msg <-  renderValueBox({

      a <- calcular_mensajes_diarios(bd()) %>%
        summarise(n = round(mean(n, na.rm = T), 0)) %>%
        pull()

      valueBox(value = a, subtitle = "Promedio de mensajes por día", icon = icon("envelope-open"))
    })

    output$prom_msg <-  renderValueBox({

      a <- bd() %>%
        count(author) %>%
        summarise(n = round(mean(n), 0)) %>%
        pull(n)

      valueBox(value = a, subtitle = "Promedio de mensajes por usuario", icon("comment-alt"))
    })

    output$linea_msg <- renderHighchart({

      a <- calcular_mensajes_diarios(bd())

      graficar_tendencia(a)

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
