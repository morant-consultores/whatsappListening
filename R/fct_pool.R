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
  Database = "DIRECTORIO",
  Server = "db.netdevelop.mx",
  UID = "empread",
  PWD = "empread",
  Port = 1433
)
