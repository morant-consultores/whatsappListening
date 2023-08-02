## code to prepare `mapas` dataset goes here

library(sf)
library(dplyr)

shp_df <- sf::read_sf("data-raw/DISTRITO_FEDERAL.shp") |>
  sf::st_transform(crs = st_crs(4326)) |>
  janitor::clean_names() |>
  select(entidad, distrito, tipo) |>
  mutate(distrito = as.character(distrito))

usethis::use_data(shp_df, overwrite = TRUE)


shp_mun <- sf::read_sf("data-raw/MUNICIPIO.shp") |>
  sf::st_transform(crs = st_crs(4326)) |>
  janitor::clean_names() |>
  select(entidad, municipio, nombre) |>
  mutate(municipio = as.character(municipio))

usethis::use_data(shp_mun, overwrite = TRUE)
