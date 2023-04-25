## code to prepare `secciones` dataset goes here

library(dplyr)
library(aelectoral2)
library(sf)


# Carga de insumos --------------------------------------------------------

bd <- tbl(pool, "K_ESCUCHA") %>%
  filter(to == "5215578721958@c.us",
         from != "5215578107028@c.us",
         !pushname %in% c("Madre Promueve Delfina", "Cielo Odette", "Memo Chavez", "Medicalexp Bot"),
         type == "chat",
         sql("LOWER(pushname) NOT LIKE '%delfina%'")) %>%
  mutate(author = substr(author, 4, 13)) %>%
  distinct(author, from) %>%
  collect()

nombre_distrito <- readr::read_csv("data-raw/encarte_2023.csv") |>
  janitor::clean_names() |>
  select(distrito = id_distrito_federal, nombre_distrito = cabecera_distrital_federal) |>
  mutate(distrito = as.character(distrito)) |>
  distinct()

clave_2703 <- readxl::read_excel("data-raw/Contactos Edo Mex 27 marzo.xlsx") |>
  janitor::clean_names() |>
  mutate(author = substr(gsub(" ", "", numero), 5, 14)) |>
  tidyr::separate(col = nombre_del_grupo, into = c("nivel", "unidad"), sep = "-") |>
  select(-numero)

clave_0904 <- readxl::read_excel("data-raw/9 abril 23.xlsx") |>
  janitor::clean_names() |>
  transmute(nivel = "DISTRITO",
            unidad = as.character(distrito),
            author = substr(gsub(" ", "", celular), 5, 14))

clave_secc <-



# Revisar concordancia entre claves ---------------------------------------

clave <- clave_2703 |>
  bind_rows(anti_join(clave_0904, clave_2703))


# Procesamiento de clave --------------------------------------------------

clave <- clave |>
  inner_join(bd) |>
  group_by(from) |>
  count(nivel, unidad, sort = T) |>
  filter(n == max(n)) |>
  ungroup() |>
  distinct(from, .keep_all = T) |>
  left_join(nombre_distrito, by = c("unidad" = "distrito")) |>
  mutate(nombre_distrito = glue::glue("{unidad}. {nombre_distrito}")) |>
  select(-n)


# Unión de meta por distrito para cada distrito ---------------------------
shp <- ElectoralSHP$new("df_21", "mex")

shp_df <- shp$shp[[1]] %>%
  mutate(distritof_21 = as.character(as.numeric(gsub("15_", "", distritof_21)))) %>%
  rename(distrito = distritof_21) %>%
  st_transform(st_crs(4326))

shp <- ElectoralSHP$new("mun_21", "mex")

shp_mun <- shp$shp[[1]] |>
  mutate(municipio = as.numeric(gsub("15_", "", municipio_21)),
         nom_mun = nombre_municipio_21) |>
  select(-c(municipio_21, nombre_municipio_21)) |>
  st_transform(st_crs(4326))

shp_secc <- read_sf("data-raw/SECCION.shp") |>
  janitor::clean_names() |>
  st_transform(st_crs(4326))


# -------------------------------------------------------------------------

usethis::use_data(clave, overwrite = TRUE)
usethis::use_data(shp_df, overwrite = TRUE)
usethis::use_data(shp_mun, overwrite = TRUE)
usethis::use_data(shp_secc, overwrite = TRUE)
