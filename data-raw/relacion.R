## code to prepare `relacion` dataset goes here
library(readr)
library(dplyr)
library(sf)
library(readxl)

nombres_mun <- read_sf("data-raw/MUNICIPIO.shp") |>
  janitor::clean_names() |>
  as_tibble() |>
  select(nombre, municipio) |>
  mutate(municipio = as.character(municipio))

relacion1 <- read_csv("data-raw/relacion.csv") |>
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

relacion2 <- read_csv("data-raw/Vinculacion whatsapp listening 17082023.csv") |>
  janitor::clean_names() |>
  rename(nivel = unidad,
         unidad = nivel,
         nombre = nombre_de_grupo) |>
  mutate(nivel = iconv(nivel, from = "ISO-8859-1", to = "UTF-8"),
         nombre = iconv(nombre, from = "ISO-8859-1", to = "UTF-8"),
         unidad = stringi::stri_trans_general(
           toupper(
             iconv(unidad, from = "ISO-8859-1", to = "UTF-8")
             ),
           id = "latin-ascii"),
         nivel = tolower(nivel),
         nivel = if_else(nivel == "colonia/fraccionamiento", "colonia", nivel),
         nivel = forcats::fct_lump_min(nivel, min = 20, other_level = "otros")
  ) |>
  naniar::replace_with_na(replace = list(nivel = "N/a",
                                         unidad = c("N/A", "n/a"))) |>
  left_join(nombres_mun, join_by(unidad == nombre)) |>
  mutate(unidad = if_else(nivel == "Municipio", municipio, unidad),
         nivel = tolower(nivel)) |>
  select(-municipio)

relacion3 <- read_excel("data-raw/base de whatsapp para chiapas.xlsx") |>
  janitor::clean_names() |>
  rename(nombre = "posible_grupo") |>
  mutate(nivel = tolower(nivel),
         unidad = as.character(unidad))

relacion <- bind_rows(relacion1, relacion2, relacion3) |>
  mutate(nivel = if_else(nivel == "seccional", "seccion", nivel))

usethis::use_data(relacion, overwrite = TRUE)





