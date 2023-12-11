## code to prepare `relacion` dataset goes here
library(readr)
library(dplyr)
library(sf)
library(readxl)

## Nombres provenientes de la lista nominal del INE

##OJO: CUANDO UTILICEMOS LA CLAVE DE MUNICIPIO HAY QUE HACERLE UN UNIQUE O DISTINCT PORQUE SE REPITEN POR LOS DISTRITOS
nombres <- readxl::read_excel("data-raw/DatosAbiertos-derfe-pdln_edms_re_20230804.xlsx") |>
  janitor::clean_names() |>
  filter(clave_entidad == 7, clave_distrito != 0) |>
  distinct(distrito = clave_distrito, nombre_distrito, municipio = clave_municipio, nombre_municipio)

dist <- nombres |>
  distinct(distrito = as.character(distrito), nombre_nivel = nombre_distrito)

relacion1 <- read_csv("data-raw/relacion.csv") |>
  janitor::clean_names() |>
  rename(from = id,
         nombre = nombre_del_grupo,
         nivel = unidad,
         unidad = nivel) |>
  filter(nivel != "N/A") |>
  mutate(nivel = if_else(nivel == "Ditrito", "Distrito", nivel),
         unidad = if_else(nivel == "Municipio", toupper(stringi::stri_trans_general(unidad, id = "latin-ascii")), unidad)) |>
  left_join(dist, join_by(unidad == distrito)) |>
  mutate(nombre_nivel = if_else(nivel == "Distrito", paste0(unidad,"-", nombre_nivel), unidad))



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
  naniar::replace_with_na(replace = list(nivel = c("N/a", "n/a"),
                                         unidad = c("N/A", "n/a"))) |>
  mutate(nivel = if_else(nivel == "Ditrito", "Distrito", nivel),
         unidad = if_else(nivel == "Municipio", toupper(stringi::stri_trans_general(unidad, id = "latin-ascii")), unidad)) |>
  left_join(dist, join_by(unidad == distrito)) |>
  mutate(nombre_nivel = if_else(nivel == "distrito", paste0(unidad,"-", nombre_nivel), unidad))


relacion3 <- read_excel("data-raw/base de whatsapp para chiapas.xlsx") |>
  janitor::clean_names() |>
  rename(nombre = "posible_grupo") |>
  mutate(nivel = tolower(nivel),
         unidad = as.character(unidad)) |>
  mutate(nivel = if_else(nivel == "Distrito", "Distrito", nivel),
         unidad = if_else(nivel == "Municipio", toupper(stringi::stri_trans_general(unidad, id = "latin-ascii")), unidad)) |>
  left_join(dist, join_by(unidad == distrito)) |>
  mutate(nombre_nivel = if_else(nivel == "distrito", paste0(unidad,"-", nombre_nivel), unidad))

relacion4 <- read_excel("data-raw/Vinculacion whatsapp listening 26092023.xlsx") |>
  janitor::clean_names() |>
  rename(nombre = nombre_de_grupo,
         nivel = unidad,
         unidad = nivel) |>
  mutate(nivel = tolower(nivel),
         nivel = if_else(nivel == "seccional", "seccion", nivel),
         unidad = if_else(nivel == "Municipio", toupper(stringi::stri_trans_general(unidad, id = "latin-ascii")), unidad),
         unidad = if_else(nivel == "seccion", sprintf("%04s", unidad), unidad)) |>
  left_join(dist, join_by(unidad == distrito)) |>
  mutate(nombre_nivel = if_else(nivel == "distrito", paste0(unidad,"-", nombre_nivel), unidad))

relacion4 |>
  count(nivel)

relacion <- bind_rows(relacion1, relacion2, relacion3) |>
  filter(!is.na(from)) |>
  mutate(nivel = if_else(nivel == "seccional", "seccion", nivel))

usethis::use_data(relacion, overwrite = TRUE)


# Conexion a Ultra-MSG ----------------------------------------------------
library(httr)
library(jsonlite)

bd <- tbl(pool, "K_ESCUCHA") %>%
  filter(to  %in% c("5219671858096@c.us", "5219995531136@c.us"),
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
  janitor::clean_names() |>
  bind_rows(respaldo)

grupos <- setdiff(unique(bd$from), unique(relacion$from))

#Función
obtener_info_grupo <- function(i) {
  url <- "https://api.ultramsg.com/instance55729/groups/group"
  query_params <- list(
    token = "j5oonnyna8sufur3",
    groupId = i,
    priority = ""
  )
  response <- httr::GET(url, query = query_params)
  nombre <- fromJSON(httr::content(response, "text"))$name
  return(tibble(from = i, nombre = nombre))
}

relacion_umsg <- purrr::map_df(grupos, obtener_info_grupo)

r_umsg <- relacion_umsg |>
  mutate(nivel = case_when(grepl("secc", nombre, ignore.case = T) ~ "seccion",
                           grepl("dist", nombre, ignore.case = T) & !grepl("secc", nombre, ignore.case = T)  ~ "distrito",
                           T ~ "otro"),
         nivel = if_else(grepl("Brigadistas|Suma Dist", nombre), "otro", nivel),
         nombre = gsub("SECCIONAL  ", "SECCIONAL ", nombre),
         unidad = case_when(nivel == "distrito" ~ stringr::str_extract(nombre, "(?i)(?<=distrito\\s|dist\\s|dist\\.\\s|dist|Dtto\\.\\s|dtto\\.\\s)\\d+"),
                            nivel == "seccion" ~ sprintf("%04s", stringr::str_extract(nombre, "(?i)(?<=seccional\\s|SECCIONAL\\s)\\d+")),
                            T ~ NA_character_))

faltantes <- r_umsg |>
  filter(nivel == "otro" | grepl("NA", unidad))

listos <- r_umsg |>
  filter(nivel != "otro" & !grepl("NA", unidad))

faltantes_c <- read_csv("~/Desktop/faltantes_umsg.csv") |>
  mutate(nivel = if_else(nivel == "otro" & !is.na(unidad), "municipio", nivel),
         unidad = if_else(nivel == "seccion", sprintf("%4s", unidad), stringr::str_to_title(unidad)),
         nombre = if_else(is.na(nombre), "Sin nombre", nombre)) |>
  left_join(dist, join_by(unidad == distrito)) |>
  mutate(nombre_nivel = if_else(nivel == "distrito", paste0(unidad,"-", nombre_nivel), unidad))

relacion <- relacion |>
  left_join(dist, join_by(unidad == distrito)) |>
  mutate(nombre_nivel = if_else(nivel == "distrito", paste0(unidad,"-", nombre_nivel.y), unidad)) |>
  select(-c(nombre_nivel.y, nombre_nivel.x)) |>
  bind_rows(faltantes_c)

relacion <- relacion |>
  mutate(unidad = toupper(unidad),
         nivel = case_when(unidad == "VALLES ZOQUE" ~ "otro",
                           unidad == "PATRIA NUEVA" ~ "otro",
                           unidad == "MOJUVA" ~ "otro",
                           T ~ nivel),
         unidad = case_when(unidad == "OCOZOCOAUTLA" ~ "OCOZOCOAUTLA DE ESPINOSA",
                            unidad == "PLAN DE AYALA" ~ "OSTUACAN",
                            unidad == "MONTE CRISTO DE GUERRERO" ~ "MONTECRISTO DE GUERRERO",
                            unidad == "VILLACORZO" ~ "VILLA CORZO",
                            unidad == "VALLES ZOQUE" ~ "OTRO",
                            unidad == "PATRIA NUEVA" ~ "OTRO",
                            unidad == "ZINACATAN" ~ "ZINACANTAN",
                            unidad == "MOJUVA" ~ "OTRO",
                            unidad == "COMALTITLAN" ~ "VILLACOMALTITLAN",
                            T ~ unidad
                            ))

usethis::use_data(relacion, overwrite = TRUE)
