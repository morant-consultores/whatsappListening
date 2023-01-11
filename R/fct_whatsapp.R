#' whatsapp
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @import lubridate
#' @noRd


calcular_dias <- function(bd) {
  bd %>%
    summarise(dia = paste(as.period(max(bd$time) - min(bd$time)) %>%
                  day(), "días", sep = " ")) %>%
    pull()
}

contar_mensajes <- function(bd) {
  bd %>%
    distinct(id) %>%
    count()
}

obtener_mayor_participacion <- function(bd) {
  a <- list(bd %>%
    group_by(author, pushname) %>%
    count(sort = TRUE) %>%
    head(1) %>%
    pull(pushname),
    bd %>%
      group_by(author, pushname) %>%
      count(sort = TRUE) %>%
      head(1) %>%
      pull(n))

  return(a)
}
