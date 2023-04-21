#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  bd <- reactive({
    tbl(pool, "K_ESCUCHA") %>%
      filter(to == "5219613657247@c.us",
             !pushname %in% c("Cielo Odette", "Memo Chavez", "Lisette Corzo"),
             type == "chat",
             #sql("LOWER(pushname) NOT LIKE '%delfina%'")
             ) %>%
      collect() %>%
      mutate(time = lubridate::as_datetime(time, tz = "America/Mexico_City"),
             dia = format(lubridate::floor_date(time, unit = "day"), format = "%d-%m-%y"),
             hora = format(lubridate::floor_date(time, unit = "hour"), format = "%H:%M"),
             fecha_hora = paste(dia, hora, sep = " "),
             dia_s = factor(substr(stringr::str_to_title(weekdays(time)), 1, 3),
                            levels = c("Lun", "Mar", "Mié", "Jue", "Vie", "Sab", "Dom")),
             grupo_wa = as.integer(factor(from, levels = unique(from))),
             author = substr(author, 4, 13)
      ) %>%
      janitor::clean_names()
  })

  # mod_solicitudes_server("solicitudes_1")
  mod_analisis_whats_server("analisis_whats_1", bd = bd)
}
