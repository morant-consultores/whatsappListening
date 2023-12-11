## code to prepare `mapas` dataset goes here

library(sf)
library(dplyr)

nombres <- readxl::read_excel("data-raw/DatosAbiertos-derfe-pdln_edms_re_20230804.xlsx") |>
  janitor::clean_names() |>
  filter(clave_entidad == 7, clave_distrito != 0) |>
  distinct(distrito = clave_distrito, nombre_distrito, municipio = clave_municipio, nombre_municipio)

dist <- nombres |>
  distinct(distrito = as.character(distrito), nombre_distrito = paste0(distrito, "-", nombre_distrito))

shp_df <- sf::read_sf("data-raw/DISTRITO_FEDERAL.shp") |>
  sf::st_transform(crs = st_crs(4326)) |>
  janitor::clean_names() |>
  select(entidad, distrito, tipo) |>
  mutate(distrito = as.character(distrito)) |>
  left_join(dist)

usethis::use_data(shp_df, overwrite = TRUE)

shp_mun <- sf::read_sf("data-raw/MUNICIPIO.shp") |>
  sf::st_transform(crs = st_crs(4326)) |>
  janitor::clean_names() |>
  select(entidad, cve_mun = municipio, municipio = nombre)

usethis::use_data(shp_mun, overwrite = TRUE)

shp_secc <- sf::read_sf("data-raw/SECCION.shp") |>
  sf::st_transform(crs = st_crs(4326)) |>
  janitor::clean_names() |>
  mutate(seccion = sprintf("%04s", seccion))

usethis::use_data(shp_secc, overwrite = TRUE)
