#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  bd <- reactive({
    bd <- tbl(pool, "K_ESCUCHA") %>%
      collect() %>%
      janitor::clean_names() %>%
      filter(to == "5219671858096@c.us" & grepl("@g",x = from) & type == "chat") %>%
      select(-c(media, from_me, self)) %>%
      mutate(time = lubridate::as_datetime(time),
             dia = format(floor_date(time, unit = "day"), format = "%d-%m-%y"),
             hora = format(floor_date(time, unit = "hour"), format = "%H:%M"),
             fecha_hora = paste(dia, hora, sep = " "),
             dia_s = factor(substr(stringr::str_to_title(weekdays(time)), 1, 3),
                            levels = c("Lun", "Mar", "Mié", "Jue", "Vie", "Sab", "Dom")))

    grupo <- bd %>%
      count(from, sort = TRUE) %>%
      mutate(grupo_wa = row_number()) %>%
      select(-n)

    bd <- bd %>%
      left_join(grupo)

    return(bd)
    })

  mod_solicitudes_server("solicitudes_1")
  mod_analisis_whats_server("analisis_whats_1", bd = bd)
}
