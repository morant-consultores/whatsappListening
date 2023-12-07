#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  bd <- reactive({
    tbl(pool, DBI::Id(schema = "tuxtla", name = "K_ESCUCHA")) %>%
      filter(#to  %in% c("5219671858096@c.us", "5219995531136@c.us"),
             !pushname %in% c("Cielo Odette", "Memo Chavez", "Lisette Corzo"),
             # type == "chat",
             #sql("LOWER(pushname) NOT LIKE '%delfina%'")
             ) %>%
      collect() %>%
      filter(grepl("@g", from_)) |>
      mutate(time = lubridate::as_datetime(time, tz = "America/Mexico_City"),
             dia = format(lubridate::floor_date(time, unit = "day"), format = "%d-%m-%y"),
             hora = format(lubridate::floor_date(time, unit = "hour"), format = "%H:%M"),
             fecha_hora = paste(dia, hora, sep = " "),
             dia_s = factor(substr(stringr::str_to_title(weekdays(time)), 1, 3),
                            levels = c("Lun", "Mar", "Mié", "Jue", "Vie", "Sab", "Dom")),
             grupo_wa = as.integer(factor(from_, levels = unique(from_))),
             author = substr(author, 4, 13)
      ) %>%
      janitor::clean_names() #|>
      # bind_rows(respaldo)
  })

  # mod_solicitudes_server("solicitudes_1")
  mod_analisis_whats_server("analisis_whats_1", bd = bd)
}
