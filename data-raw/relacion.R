## code to prepare `relacion` dataset goes here
library(readr)
library(dplyr)

relacion <- read_csv("data-raw/relacion.csv") |>
  janitor::clean_names() |>
  rename(from = id,
         nombre = nombre_del_grupo,
         nivel = unidad,
         unidad = nivel) |>
  mutate(nivel = if_else(nivel == "Ditrito", "Distrito", nivel)) |>
  filter(nivel != "N/A") |>
  tidyr::pivot_wider(id_cols = c(from, nombre), names_from = nivel, values_from = unidad) |>
  janitor::clean_names() |>
  mutate(municipio = if_else(municipio == "Ocosiongo", "Ocosingo", municipio)) |>
  tidyr::pivot_longer(cols = c(municipio, distrito), names_to = "nivel", values_to = "unidad") |>
  filter(!is.na(unidad))

usethis::use_data(relacion, overwrite = TRUE)
