#' contenido_whats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_contenido_whats_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(3,
             dateInput(ns("fecha"),label = "Fecha", format = "dd-MM", language = "es")
      )
    ),
    fluidRow(
      column(12,
             leafletOutput(ns("mapa"))
      )
    ),
    fluidRow(
      column(12,
             gt_output(ns("tabla"))
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
      tbl(pool, "resumen_conv") |>
        collect() |>
        left_join()
    })

    observe({
      fecha <- range(inicial()$dia)

      updateDateInput(session = session, "fecha", value = fecha[[2]], min = fecha[[1]], max = fecha[[2]])
    })

    base <- reactive({
      inicial() |>
        filter(dia == input$fecha)
    })

    output$tabla <- render_gt({
      base() |>
        gt()
    })


  })
}

## To be copied in the UI
# mod_contenido_whats_ui("contenido_whats_1")

## To be copied in the server
# mod_contenido_whats_server("contenido_whats_1")
