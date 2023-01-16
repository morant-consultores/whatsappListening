#' solicitudes
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


js_code <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}
"

