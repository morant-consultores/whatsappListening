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
    fluidRow(
      h1("Análisis de Whatsapp"),
      column(3,
             selectInput(ns("grupo"),
                         label = "Grupos",
                            choices = c("Todos" = ""),
                         selected = ""))
    ),
    fluidRow(
      valueBoxOutput(ns("periodo"), width = 4),
      valueBoxOutput(ns("mensajes"), width = 4),
      valueBoxOutput(ns("persona"), width = 4)
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
                                                         distinct(grupo) %>%
                                                         pull())
                        ))
    })

    bd_w <- reactive({
      if(input$grupo == "") {
        bd()
      } else {
        bd() %>%
          filter(grupo == input$grupo)
      }
    })

    output$periodo <- renderValueBox({
      a <- calcular_dias(bd_w())

      valueBox(value = a, subtitle = "Total de días")
    })

    output$mensajes <-  renderValueBox({
      a <- contar_mensajes(bd_w())

      valueBox(value = a, subtitle = "Total de mensajes")
    })

    output$persona <- renderValueBox({
      a <- obtener_mayor_participacion(bd_w())

      valueBox(value = a[1], subtitle = glue::glue("Fue quien más mensajes envió ({a[2]})"))
    })

  })
}

## To be copied in the UI
# mod_analisis_whats_ui("analisis_whats_1")

## To be copied in the server
# mod_analisis_whats_server("analisis_whats_1")
