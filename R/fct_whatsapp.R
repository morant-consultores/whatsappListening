#' whatsapp
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @import lubridate highcharter ggplot2 quanteda quanteda.textmodels quanteda.textstats quanteda.textplots igraph ggraph shinyjs
#' @noRd

Sys.setlocale(locale = "es_ES.UTF-8")

morena <- "#E81D1D"
complemento <- "#1DE8B5"


progressBar <- function(
    value = 0,
    label = FALSE,
    color = "#B3282D",
    size = NULL,
    striped = FALSE,
    active = FALSE,
    vertical = FALSE
) {
  stopifnot(is.numeric(value))
  if (value < 0 || value > 100)
    stop("'value' should be in the range from 0 to 100.", call. = FALSE)
  # if (!(color %in% shinydashboard:::validColors || color %in% shinydashboard:::validStatuses))
  #   stop("'color' should be a valid status or color.", call. = FALSE)
  if (!is.null(size))
    size <- match.arg(size, c("sm", "xs", "xxs"))
  text_value <- paste0(value, "%")
  if (vertical)
    style <- htmltools::css(height = text_value, `min-height` = "2em")
  else
    style <- htmltools::css(width = text_value, `min-width` = "0em")
  tags$div(
    class = "progress",
    class = if (!is.null(size)) paste0("progress-", size),
    class = if (vertical) "vertical",
    class = if (active) "active",
    tags$div(
      class = "progress-bar",
      class = paste0("progress-bar-", color),
      class = if (striped) "progress-bar-striped",
      style = style,
      role = "progressbar",
      `aria-valuenow` = value,
      `aria-valuemin` = 0,
      `aria-valuemax` = 100,
      tags$span(class = if (!label) "sr-only", text_value)
    )
  )
}

#' Progress Group
progressGroup <- function(text, value, min = 0, max = value, color = "#B3282D") {
  stopifnot(is.character(text))
  stopifnot(is.numeric(value))
  if (value < min || value > max)
    stop(sprintf("'value' should be in the range from %d to %d.", min, max), call. = FALSE)

  tags$div(
    class = "progress-group",
    tags$span(class = "progress-text", text),
    tags$span(class = "progress-number", sprintf("%s / %s", scales::comma(value), scales::comma(max))),
    progressBar(round(value / max * 100), color = color, size = "sm")
  )
}

contar_mensajes <- function(bd) {
  bd %>%
    distinct(id) %>%
    count()
}

calcular_var_diarios <- function(bd, grupo = FALSE, ...) {
  bd %>% {
    if(grupo == T) {
      distinct(., ...)
    } else{.}
  } %>%
    count(dia) %>%
    mutate(dia = as.Date(dia, format = "%d-%m-%y")) %>%
    tidyr::complete(dia = seq(min(dia, na.rm = T), max(dia, na.rm = T), by = "day"), fill = list(n = 0)) %>%
    arrange(dia)
}

graficar_tendencia <- function(bd, titulo, yaxis) {
  highchart() %>%
    hc_chart(style = list(fontFamily = 'Poppins')) %>%
    hc_add_series(bd, hcaes(x = dia, y = n),
                  type = "spline",
                  name = "Mensajes escuchados",
                  color = "#A30039") %>%
    hc_yAxis(title = list(text = yaxis),
             labels = list(fontSize = '14px', width = '100px'),
             gridLineWidth = 0) %>%
    hc_xAxis(type = 'datetime',
             dateTimeLabelFormats = list(day = '%e %b'),
             title = list(text = "Fecha"),
             labels= list(style = list(fontSize = '14px',
                                       autoRotationLimit = 80))) %>%
    hc_legend(enabled = FALSE) %>%
    hc_title(text = titulo)
}

obtener_mayor_participacion <- function(bd) {
   a <- list(nombre = bd %>%
               mutate(pushname = glue::glue("{pushname} ({nombre_distrito})")) %>%
    group_by(author, pushname) %>%
    count(sort = TRUE) %>%
    head(10) %>%
    pull(pushname),
    mensajes = bd %>%
      group_by(author, pushname) %>%
      count(sort = TRUE) %>%
      head(10) %>%
      pull(n) %>%
     scales::comma()
   ) |>
    bind_cols() %>%
     mutate(nombre = stringr::str_to_title(nombre))
  return(a)
}

# Highcharts --------------------------------------------------------------

hcoptslang <- getOption("highcharter.lang")
hcoptslang$weekdays<- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
hcoptslang$shortMonths <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
hcoptslang$months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
hcoptslang$thousandsSep <- c(",")
options(highcharter.lang = hcoptslang)

#Análisis de palabras

limpiar_corpus <- function(corpus,
                          quitar_numeros=F,
                          quitar_puntuacion=F,
                          quitar_simbolos=F,
                          quitar_url=T,
                          quitar_stopwords=T,
                          quitar_ht_menciones=F){
  # Crear la version de tokens
  tuits_tok <- quanteda::tokens(corpus,
                                remove_numbers = quitar_numeros,
                                remove_punct = quitar_puntuacion,
                                remove_symbols = quitar_simbolos,
                                remove_url = quitar_url,
                                include_docvars = T) %>%
    tokens_tolower(keep_acronyms = F)
  if(quitar_stopwords){
    # Quitar Stopwords
    tuits_tok<-tokens_select(x = tuits_tok,
                             pattern =  stopwords("spanish"),
                             case_insensitive = T,
                             selection = "remove")
  }
  if(quitar_ht_menciones){
    # Quitar Hashtags y arrobas
    tuits_tok<-tokens_select(x = tuits_tok,
                             pattern =  c("#*","@*" ),
                             case_insensitive = T,
                             selection = "remove")
  }
  return(tuits_tok)
}



analizar_palabras_clave <- function(corpus, grupo, grupo_vector=NULL,
                                    p_value=0.05, top_palabras=10,
                                    quitar_numeros=T,
                                    quitar_puntuacion=T,
                                    quitar_simbolos=F,
                                    quitar_url=T,
                                    quitar_stopwords=T,
                                    quitar_ht_menciones=F){
  # Limpiar
  tuits_tok <- limpiar_corpus(corpus,
                              quitar_numeros,
                              quitar_puntuacion,
                              quitar_simbolos,
                              quitar_url,
                              quitar_stopwords,
                              quitar_ht_menciones
  )
  # Tuits dfm
  tuits_dfm<-dfm(tuits_tok)
  #
  if(is.null(grupo_vector)) {
    tuits_dfm_grupo <- dfm_group(tuits_dfm, groups = eval(sym(grupo)))
    categorias <- tuits_dfm %>% docvars()  %>% pull(!!sym(grupo))
  }
  else {
    tuits_dfm_grupo <- dfm_group(tuits_dfm, groups = grupo_vector)
    categorias <- grupo_vector
  }
  p_clave <- categorias %>% unique() %>%
    purrr::map_df(~{

      tb <- textstat_keyness(measure = "lr",
                             tuits_dfm_grupo,
                             target = .x) %>%
        as_tibble() %>%
        mutate(categoria=.x) %>%
        group_by(pos=G2>0) %>%
        arrange(desc(abs(G2))) %>%
        mutate(posicion=row_number()) %>%
        ungroup()
      return(tb)
    })

  res <- p_clave %>%
    filter(p<p_value, posicion<=top_palabras, n_target>1)
  return(res)
}



graficar_red_palabras_clave <- function(p_clave,
                                        corpus,
                                        variable,
                                        nivel,
                                        n_palabras=20,
                                        quitar_numeros=T,
                                        quitar_puntuacion=T,
                                        quitar_simbolos=F,
                                        quitar_url=T,
                                        quitar_stopwords=T,
                                        quitar_ht_menciones=F,
                                        acomodo="fr"
){
  # Palabras
  palabras_clave <- p_clave %>%
    filter(pos, categoria ==nivel) %>%
    pull(feature)
  # Tokens
  # Limpiar
  tuits_tok <- limpiar_corpus(corpus,
                              quitar_numeros,
                              quitar_puntuacion,
                              quitar_simbolos,
                              quitar_url,
                              quitar_stopwords,
                              quitar_ht_menciones

  )

  fcmat <- fcm(tokens_subset(tuits_tok,
                             eval(sym(variable)) ==nivel),
               context = "window",
               tri = FALSE)
  # Red
  fctib <- convert(fcmat, to = "data.frame") %>%
    as_tibble()

  # formato red
  una <- fctib %>%
    filter(doc_id %in% palabras_clave) %>%
    tidyr::pivot_longer(-doc_id) %>%
    rename(weight=value) %>%
    slice_max(n=n_palabras,order_by = weight)

  repeticiones <- fctib %>%
    summarise(across(where(is.numeric), ~sum(.x))) %>%
    tidyr::pivot_longer(everything()) %>%
    filter(name %in% unique(c(una$doc_id, una$name)))



  red <- igraph::graph_from_data_frame(una,
                               directed = F)
  vertex_attr(red, "clave") <- (igraph::V(red)$name %in% palabras_clave)

  vertex_attr(red, "uso") <- left_join(tibble(name = igraph::V(red)$name),
                                       repeticiones) %>%
    pull(value)

  ggraph::ggraph(red, layout = acomodo) +
    geom_edge_arc(aes(width=weight),linejoin = "round",
                  color="gray30", alpha=0.2) +
    geom_node_point(aes(color=clave,size=V(red)$uso)) +
    geom_node_text(aes(label = V(red)$name,
                       color=V(red)$clave), repel=T,
                   show.legend = F) +
    theme_void()+
    scale_size_area(guide="none")+
    scale_edge_width(guide="none") +
    scale_color_manual(values = c("#B382BD", "#c79840"),
                       name="Palabra clave",
                       labels=c("No", "Sí"))+
    labs(title = "Red asociaciones de palabras clave",
         subtitle = glue::glue("{nivel}: {max(p_clave$posicion)} palabras clave"))
}
