#' pool
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @import gt
#' @noRd

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
