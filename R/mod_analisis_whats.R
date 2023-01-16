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
  tagList(
    tabsetPanel(type = "tabs",
                tabPanel("General",
                         fluidRow(
                           col_8(
                           h1("Análisis general")
                         ),
                         column(3,
                                selectInput(ns("grupo"),
                                            label = "Grupos",
                                            choices = c("Todos" = ""),
                                            selected = ""))
                         ),
                         fluidRow(
                           col_1(),
                           valueBoxOutput(ns("periodo"), width = 5),
                           valueBoxOutput(ns("mensajes"), width = 5),
                           col_1()
                         ),
                         fluidRow(
                           col_6(
                             box(width = 12,
                                 gt::gt_output(ns("top"))
                                 )
                           ),
                           col_6(
                             box(width = 12,
                                 highchartOutput(ns("prom_dia"))
                                 )
                           )
                         ),
                         fluidRow(
                           col_4(
                             box(width = 12,
                                 highchartOutput(ns("prom_hora"))
                             )
                           ),
                           col_4(
                             box(width = 12,
                                 plotOutput(ns("semana_conteo"))
                                 )
                           ),
                           col_4(
                             box(width = 12,
                                 highchartOutput(ns("flujo_mensajes"))
                                 )
                           )
                         )
                ),
                tabPanel(title = "Comparativa",
                         fluidRow(col_6(
                           h1("Comparativa de grupos")
                         )
                         ),
                         fluidRow(
                           col_1(),
                           col_10(
                             box(width = 12,
                                 plotOutput(ns("red_asociacion"))
                             )
                           ),
                           col_1()
                         ),
                         fluidRow(
                           column(6,
                                  box(width = 6,
                                      )
                                  ),
                           column(6,
                                  box(width = 6,
                                      )
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

    observe({
      updateSelectInput(session = session, inputId = "grupo",
                        choices = c("Todos" = "", sort(bd() %>%
                                                         distinct(grupo_wa) %>%
                                                         pull())
                        ))
    })

    bd_w <- reactive({
      if(input$grupo == "") {
        bd()
      } else {
        bd() %>%
          filter(grupo_wa == input$grupo)
      }
    })

    output$periodo <- renderValueBox({
      a <- calcular_dias(bd_w())

      valueBox(value = a, subtitle = "Total de días completos")
    })

    output$mensajes <-  renderValueBox({
      a <- contar_mensajes(bd_w())

      valueBox(value = a, subtitle = "Total de mensajes")
    })

    output$top <- gt::render_gt({
      obtener_mayor_participacion(bd_w()) %>%
        gt::gt() %>%
        tab_header(
          title = glue::glue("Top 10: Usuarios más activos")
        ) %>%
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

    output$prom_hora <- renderHighchart({
      bd_w() %>%
        group_by(hora) %>%
        summarise(n = n()) %>%
        hchart(hcaes(x = hora, y = n),
               type = "column") %>%
        hc_colors(colors = "#800f2f") %>%
        hc_plotOptions(column = list(borderRadius = 3)) %>%
        hc_title(text ="Mensajes promedio por hora") %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_tooltip(pointFormat = "{point.n} mensajes")
    })

    output$prom_dia <- renderHighchart({
      bd_w() %>%
        mutate(fecha = format(floor_date(time, unit = "day"), format = "%d-%m-%y")) %>%
        group_by(fecha) %>%
        summarise(n = n()) %>%
        mutate(n2 = cumsum(n)) %>%
        hchart(hcaes(x = fecha, y = n2), type = "area") %>%
        hc_colors(colors = "#800f2f") %>%
        hc_plotOptions(column = list(borderRadius = 3)) %>%
        hc_title(text ="Mensajes acumulados por día") %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_tooltip(pointFormat = "{point.n2} mensajes")
    })

    output$semana_conteo <- renderPlot({

      bd_w() %>%
        count(dia_s, hora) %>%
        na.omit() %>%
        arrange(desc(hora)) %>%
        mutate(orden = row_number()) %>%
        ggplot(aes(y = forcats::fct_reorder(hora, orden), x = dia_s, fill = n)) +
        scale_fill_gradient2(low = "#FDED91",
                             mid = "#FFD60A",
                             high = "#168AAD") +
        geom_tile(color = "white",
                  lwd = .5,
                  linetype = 1) +
        coord_fixed() +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              legend.position = "none") +
        labs(x = NULL, y= NULL, fill = NULL) +
        geom_text(aes(label = n), size=3.5, color = "white") +
        scale_x_discrete(position = "top", guide = guide_axis(n.dodge = 2))
    })

    output$flujo_mensajes <-  renderHighchart({
      sent_corpus <- bd_w() %>%
        corpus(text_field = "body") %>%
        limpiar_corpus(quitar_puntuacion = T,
                       quitar_stopwords = T,
                       quitar_ht_menciones = T,
                       quitar_simbolos = T)

      pclave_dia <- analizar_palabras_clave(sent_corpus,
                                            top_palabras = 3,
                                            quitar_stopwords = TRUE,
                                            quitar_simbolos = TRUE,
                                            grupo = "fecha_hora",
                                            p_value = 1) %>%
        group_by(categoria) %>%
        summarise(feature = paste0(feature, collapse = ", "))

      bd_w() %>%
        left_join(pclave_dia, by = c("fecha_hora" = "categoria")) %>%
        count(dia, hora, feature) %>%
        mutate(feature = case_when(is.na(feature) ~"", T~feature)) %>%
        hchart(hcaes(x = dia, y = hora, value = n), type = "heatmap") %>%
        hc_colorAxis(minColor = "#fcbf49",
                     maxColor = "#d62828") %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_tooltip(headerFormat = "",
                   pointFormat = "<b>{point.fecha}<b/><br>Mensajes: {point.n}<br> <b>{point.feature}</b>") %>%
        hc_title(text = "Flujo de mensajes y palabras clave") %>%
        hc_legend(align= 'right',
                  verticalAlign= 'top',
                  layout= 'vertical',
                  x= 0,
                  y= 100, reversed= T  ) %>%
        hc_chart(style = list(fontFamily = "Montserrat"))
    })

    output$red_asociacion <- renderPlot({
      personaje_corpus <- bd() %>%
        select(grupo_wa, body) %>%
        corpus(text_field = "body")

      grupo_clave <- analizar_palabras_clave(personaje_corpus,
                                             quitar_ht_menciones = T,
                                             top_palabras = 20,
                                             "grupo_wa")


      emoji <- rwhatsapp::lookup_emoji(grupo_clave, text_field = "feature") %>%
        pull(emoji) %>%
        unlist()

      grupo_clave <- grupo_clave %>%
        filter(!feature %in% emoji)

      grupo_clave$categoria %>%
        unique() %>%
        purrr::map({~
            graficar_red_palabras_clave(grupo_clave, corpus = personaje_corpus,
                                        "grupo_wa", .x)

        })
    })

  })
}

## To be copied in the UI
# mod_analisis_whats_ui("analisis_whats_1")

## To be copied in the server
# mod_analisis_whats_server("analisis_whats_1")
