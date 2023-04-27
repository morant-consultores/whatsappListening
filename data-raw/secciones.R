## code to prepare `secciones` dataset goes here

library(dplyr)
library(aelectoral2)
library(sf)


# Carga de insumos --------------------------------------------------------

bd <- tbl(pool, "K_ESCUCHA") %>%
  filter(to %in% c("5215578721958@c.us"),
         from != "5215578107028@c.us",
         !pushname %in% c("Madre Promueve Delfina", "Cielo Odette", "Memo Chavez", "Medicalexp Bot"),
         type == "chat",
         sql("LOWER(pushname) NOT LIKE '%delfina%'")) %>%
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

nombre_distrito <- readr::read_csv("data-raw/encarte_2023.csv") |>
  janitor::clean_names() |>
  select(distrito = id_distrito_federal, nombre_distrito = cabecera_distrital_federal) |>
  mutate(distrito = as.character(distrito)) |>
  distinct()

id_mun <- readr::read_csv("data-raw/encarte_2023.csv") |>
  janitor::clean_names() |>
  distinct(id_municipio, nombre_municipio) |>
  arrange(id_municipio)

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

# Revisar concordancia entre claves ---------------------------------------

clave <- clave_2703 |>
  bind_rows(anti_join(clave_0904, clave_2703))

# Clave municipal ---------------------------------------------------------

DIR <- tbl(pool, "K_DIRECTORIO") |>
  select(DIR_NUMERO_CEL, DIR_MUNICIPIO) |>
  collect()

clave_mun <- tbl(pool, "K_ESCUCHA") |>
  filter(to == "5215568913223@c.us",
         from != "5215578107028@c.us",
         !pushname %in% c("Madre Promueve Delfina", "Cielo Odette", "Memo Chavez", "Medicalexp Bot"),
         type == "chat",
         sql("LOWER(pushname) NOT LIKE '%delfina%'")) %>%
  collect() |>
  filter(grepl("g", from)) |>
  mutate(author = as.character(as.numeric(substr(gsub("@c.us", "", author), 4, 13)))) |>
  distinct(from, author) |>
  inner_join(DIR, by = c("author" = "DIR_NUMERO_CEL"))  |>
  group_by(DIR_MUNICIPIO, from) |>
  summarise(n = n(),
            nivel = "MUNICIPIO") |>
  mutate(nombre_distrito = stringi::stri_trans_general(toupper(DIR_MUNICIPIO), id = "LATIN-ASCII"),
         nombre_distrito = ifelse(nombre_distrito == "ACAMBAY", "ACAMBAY DE RUIZ CASTAÑEDA",
                                  nombre_distrito),
         ranking = rank(n, ties.method = "min")) |>
  filter(ranking == 1) |>
  ungroup() |>
  left_join(id_mun, by = c("nombre_distrito" = "nombre_municipio")) |>
  mutate(unidad = as.character(id_municipio)) |>
  select(from, nivel, unidad, nombre_distrito)

# Procesamiento de clave --------------------------------------------------

clave <- clave |>
  inner_join(bd, by = "author") |>
  group_by(from, unidad) |>
  summarise(n = n()) |>
  mutate(ranking = rank(-n, ties.method = "min")) |>
  filter(ranking == 1) |>
  ungroup() |>
  distinct(from, .keep_all = T) |>
  left_join(nombre_distrito, by = c("unidad" = "distrito")) |>
  mutate(nombre_distrito = glue::glue("{unidad}. {nombre_distrito}"),
         nivel = "DISTRITO") |>
  select(-n, -ranking) |>
  bind_rows(clave_mun)


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
