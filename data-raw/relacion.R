## code to prepare `relacion` dataset goes here
library(readr)
library(dplyr)
library(sf)

nombres_mun <- read_sf("data-raw/MUNICIPIO.shp") |>
  janitor::clean_names() |>
  as_tibble() |>
  select(nombre, municipio) |>
  mutate(municipio = as.character(municipio))

relacion <- read_csv("data-raw/relacion.csv") |>
  janitor::clean_names() |>
  rename(from = id,
         nombre = nombre_del_grupo,
         nivel = unidad,
         unidad = nivel) |>
  filter(nivel != "N/A") |>
  mutate(nivel = if_else(nivel == "Ditrito", "Distrito", nivel),
         unidad = if_else(nivel == "Municipio", toupper(stringi::stri_trans_general(unidad, id = "latin-ascii")), unidad)) |>
  left_join(nombres_mun, join_by(unidad == nombre)) |>
  mutate(unidad = if_else(nivel == "Municipio", municipio, unidad),
         nivel = tolower(nivel)) |>
  select(-municipio)

usethis::use_data(relacion, overwrite = TRUE)
