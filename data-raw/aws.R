## code to prepare `aws` dataset goes here

library(dplyr)
library(pool)
library(odbc)
library(DBI)
library(tibble)

pool <- dbPool(
  drv = odbc(),
  Driver= 'ODBC Driver 17 for SQL Server',
  Database = "DIRECTORIO",
  Server = "db.netdevelop.mx",
  UID = "empread",
  PWD = "empread",
  Port = 1433
)
bd <- tbl(pool, "K_ESCUCHA") %>% collect()
bd %>% view

dbListTables(pool) %>% .[1:30]





usethis::use_data(aws, overwrite = TRUE)
