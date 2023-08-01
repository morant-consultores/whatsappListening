## code to prepare `respaldo` dataset goes here

library(dplyr)

pool <- pool::dbPool(
  drv = odbc::odbc(),
  Driver= 'ODBC Driver 17 for SQL Server',
  Database = "RESPALDODIRECTORIO",
  Server = "tcp:morant.database.windows.net",
  UID = "emorones",
  PWD = "Presidevis-Emi",
  Port = 1433,
  timeout = 120
)


respaldo <- tbl(pool, "K_ESCUCHA") %>%
  filter(to == "5219613657247@c.us",
         !pushname %in% c("Cielo Odette", "Memo Chavez", "Lisette Corzo"),
         type == "chat",
         #sql("LOWER(pushname) NOT LIKE '%delfina%'")
  ) %>%
  collect() %>%
  filter(grepl("@g", from)) |>
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


usethis::use_data(respaldo, overwrite = TRUE)
