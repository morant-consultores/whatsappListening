#' solicitudes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import dplyr htmltools DT
#' @importFrom shiny NS tagList
mod_solicitudes_ui <- function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("tabla"))
  )
}

#' solicitudes Server Functions
#'
#' @noRd
mod_solicitudes_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tabla <- renderDT({
      tbl(pool, "K_ASISTENCIA") %>%
        left_join(tbl(pool, "K_DIRECTORIO"), by = c("ASI_DIR_ID"="DIR_ID")) %>%
        left_join(tbl(pool, "K_EVENTO"), by = c("ASI_EVE_ID"="EVE_ID")) %>%
        select(ASI_ID, DIR_NOMBRE, DIR_APATERNO, DIR_SEXO, DIR_NUMERO_CEL, DIR_FECHA_CREACION, ASI_CONTENIDO, EVE_NOMBRE) %>%
        collect() %>%
        mutate(boton = input_btns(inputId = ns("whatsapp"), ASI_ID, tooltip = "whatsapp", icon = icon("whatsapp"), status = "default", label = ""))
    }, selection = 'none',rownames = FALSE, extensions = 'Responsive',
    options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                   drawCallback = JS("function() {Shiny.bindAll(this.api().table().node());}"),
                   lengthMenu = c(5, 10, 25, 50, 100), pageLength = 5
    ),
    escape = F)



    observe({
      print(input$whatsapp)
      })

  })
}

## To be copied in the UI
# mod_solicitudes_ui("solicitudes_1")

## To be copied in the server
# mod_solicitudes_server("solicitudes_1")
