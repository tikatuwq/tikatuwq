#' Mapa interativo de pontos de coleta
#'
#' Gera um mapa Leaflet com os pontos de coleta que possuam coordenadas
#' de latitude e longitude validas. Mostra informacoes adicionais no popup.
#'
#' @param data Um data.frame contendo as colunas de coordenadas.
#'   Sao aceitos nomes "latitude"/"longitude" ou "lat"/"lon".
#' @param popup_cols Vetor de colunas a exibir no popup (ex.: c("rio","ponto","data","iqa")).
#'   Se NULL, usa colunas comuns se existirem.
#' @param cluster Agrupar marcadores proximos (TRUE/FALSE). Default = TRUE.
#' @param color_by Nome de coluna para colorir os pontos (opcional).
#'   Se for "iqa", aplica classes de qualidade da agua.
#' @param tiles Provedor de tiles (default = "OpenStreetMap").
#'
#' @return Objeto htmlwidget (mapa Leaflet).
#'
#' @examples
#' df <- data.frame(
#'   rio = c("Buranhem","Chamagunga"),
#'   ponto = c("P1","P2"),
#'   data = as.Date(c("2025-09-20","2025-09-21")),
#'   latitude = c(-16.435, -16.498),
#'   longitude = c(-39.062, -39.080),
#'   iqa = c(72, 58)
#' )
#' plot_map(df, popup_cols = c("rio","ponto","data","iqa"), color_by = "iqa")
#'
#' @export
plot_map <- function(data,
                     popup_cols = NULL,
                     cluster = TRUE,
                     color_by = NULL,
                     tiles = "OpenStreetMap") {

  stopifnot(is.data.frame(data))

  # Detectar colunas de coordenadas
  cols <- tolower(names(data))
  lat_idx <- match(c("latitude","lat"), cols, nomatch = 0)
  lon_idx <- match(c("longitude","lon"), cols, nomatch = 0)
  lat_col <- names(data)[lat_idx[lat_idx > 0][1]]
  lon_col <- names(data)[lon_idx[lon_idx > 0][1]]
  if (is.na(lat_col) || is.na(lon_col)) {
    stop("Colunas de latitude/longitude n\u00e3o encontradas. Use 'latitude'/'longitude' ou 'lat'/'lon'.")
  }

  df <- data
  df$latitude  <- suppressWarnings(as.numeric(df[[lat_col]]))
  df$longitude <- suppressWarnings(as.numeric(df[[lon_col]]))

  n_in <- nrow(df)
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  df <- df[df$latitude >= -90 & df$latitude <= 90 &
             df$longitude >= -180 & df$longitude <= 180, ]
  if (nrow(df) < n_in) {
    warning(n_in - nrow(df), " registros removidos por coordenadas inv\u00e1lidas.")
  }
  if (nrow(df) == 0) {
    stop("Nenhum ponto com coordenadas v\u00e1lidas para plotar.")
  }

  # Popup
  if (is.null(popup_cols)) {
    popup_cols <- intersect(c("rio","ponto","data","iqa"), names(df))
  }
  make_popup <- function(r) {
    if (length(popup_cols) == 0) return(NULL)
    paste(sprintf("<b>%s:</b> %s", popup_cols, r[popup_cols]), collapse = "<br>")
  }

  # Paleta/cores
  pal <- NULL
  class_col <- NULL
  if (!is.null(color_by) && color_by %in% names(df)) {
    vals <- df[[color_by]]
    if (identical(color_by, "iqa")) {
      breaks <- c(0, 25, 50, 70, 90, 100)
      labels <- c("P\u00e9ssimo","Ruim","Regular","Bom","\u00d3timo")
      cls <- cut(as.numeric(vals), breaks = breaks, labels = labels, include.lowest = TRUE)
      class_col <- cls
      pal <- leaflet::colorFactor("viridis", domain = levels(cls))
    } else if (is.numeric(vals)) {
      pal <- leaflet::colorNumeric("viridis", domain = vals)
    } else {
      pal <- leaflet::colorFactor("viridis", domain = vals)
    }
  }

  # Mapa
  m <- leaflet::leaflet(df) |>
    leaflet::addProviderTiles(tiles)

  if (isTRUE(cluster) && nrow(df) > 1) {
    m <- leaflet::addMarkers(
      m, lng = ~longitude, lat = ~latitude,
      popup = apply(df, 1, make_popup),
      label = if (length(popup_cols)) df[[popup_cols[1]]] else NULL,
      clusterOptions = leaflet::markerClusterOptions()
    )
  } else {
    m <- leaflet::addCircleMarkers(
      m, lng = ~longitude, lat = ~latitude,
      popup = apply(df, 1, make_popup),
      label = if (length(popup_cols)) df[[popup_cols[1]]] else NULL,
      fillColor = if (!is.null(pal)) pal(df[[color_by]]) else "#3182bd",
      color = "#222", radius = 6, stroke = TRUE, weight = 1, fillOpacity = 0.9
    )
  }

  if (!is.null(pal)) {
    vals_for_legend <- if (!is.null(class_col)) class_col else df[[color_by]]
    title <- toupper(color_by)
    m <- leaflet::addLegend(m, pal = pal, values = vals_for_legend, title = title, opacity = 1)
  }

  m
}