# R/plot_map.R
# Mapas interativos (Leaflet) usando SEMPRE o pin padrao.
# ASCII-only.

#' Plot interactive map of sampling points (default Leaflet pins)
#'
#' @description
#' Creates an interactive Leaflet map of sampling points using the **default
#' Leaflet marker** (blue pin). Latitude/longitude are autodetected from
#' columns \code{lat} and \code{lon}. If these columns are not present, but
#' \code{latitude} and/or \code{longitude} exist, they are automatically
#' copied to \code{lat} and \code{lon}. You may group layers with
#' \code{group_by} (e.g., "year") and show popups with \code{popup}.
#'
#' If \code{color_by} is provided, a legend is drawn to describe the values,
#' but **markers are not colorized** (the default \emph{Leaflet} pin has fixed
#' style).
#'
#' @details
#' The function expects coordinates in columns named \code{lat} and \code{lon}.
#' If those columns are not found, but \code{latitude} and/or \code{longitude}
#' are present, they are copied to \code{lat} and \code{lon} respectively
#' before plotting.
#'
#' @param df data.frame/tibble with coordinates; must contain \code{lat}/\code{lon}
#'   (or \code{latitude}/\code{longitude}, which will be mapped automatically).
#' @param color_by optional column used to build a legend (numeric or factor).
#'                 It does not change the marker color.
#' @param popup    optional column name with popup/tooltip text.
#' @param group_by optional column name to create overlay layers (e.g., "year").
#' @param legend_title optional legend title (used when \code{color_by} is set).
#' @param na_rm logical; if \code{TRUE} (default) remove rows with invalid
#'              coordinates.
#'
#' @return a \code{leaflet} htmlwidget.
#'
#' @examples
#' \dontrun{
#' d  <- read_wq("dataset-real.csv")
#' d2 <- iqa(d, na_rm = TRUE); d2$year <- as.integer(format(d2$data, "%Y"))
#'
#' # Marcadores padrao + legenda de IQA
#' plot_map(d2, color_by="IQA", group_by="year", popup="ponto",
#'          legend_title = "IQA (0–100)")
#' }
#' @importFrom leaflet leaflet leafletOptions addProviderTiles addMarkers
#' @importFrom leaflet addLayersControl layersControlOptions addLegend
#' @importFrom leaflet colorNumeric colorFactor fitBounds
#' @export
plot_map <- function(df,
                     color_by    = NULL,
                     popup       = NULL,
                     group_by    = NULL,
                     legend_title = NULL,
                     na_rm       = TRUE) {

  stopifnot(is.data.frame(df))

  # map alternative coordinate column names if needed
  nm <- names(df)
  if (!("lat" %in% nm) && ("latitude" %in% nm)) {
    df$lat <- df$latitude
  }
  if (!("lon" %in% nm) && ("longitude" %in% nm)) {
    df$lon <- df$longitude
  }

  if (!("lat" %in% names(df) && "lon" %in% names(df))) {
    stop("plot_map: columns 'lat' and 'lon' (latitude/longitude) are required.")
  }

  # ----- coords: coerce & validate
  df$lat <- suppressWarnings(as.numeric(df$lat))
  df$lon <- suppressWarnings(as.numeric(df$lon))
  ok <- is.finite(df$lat) & is.finite(df$lon) &
        df$lat >= -90 & df$lat <= 90 & df$lon >= -180 & df$lon <= 180

  if (na_rm) {
    bad <- sum(!ok, na.rm = TRUE)
    if (bad > 0) warning(bad, " registros removidos por coordenadas invalidas.")
    df <- df[ok, , drop = FALSE]
  } else {
    df$lat[!ok] <- NA_real_; df$lon[!ok] <- NA_real_
  }

  if (!nrow(df)) {
    return(leaflet::leaflet() |> leaflet::addTiles())
  }

  # ----- popup
  has_popup <- !is.null(popup) && popup %in% names(df)
  popup_txt <- if (has_popup) as.character(df[[popup]]) else NULL

  # ----- base map
  m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
       leaflet::addProviderTiles("CartoDB.Positron")

  # ----- add points (with/without grouping) - ALWAYS default markers
  has_group <- !is.null(group_by) && group_by %in% names(df)

  if (has_group) {
    groups <- unique(df[[group_by]])
    groups <- groups[!is.na(groups)]
    for (g in groups) {
      idx  <- which(df[[group_by]] == g)
      dsub <- df[idx, , drop = FALSE]
      pp   <- if (has_popup) popup_txt[idx] else NULL

      m <- leaflet::addMarkers(
        m, lng = dsub$lon, lat = dsub$lat,
        popup = pp, group = as.character(g)
      )
    }
    m <- leaflet::addLayersControl(
      m, overlayGroups = as.character(groups),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
  } else {
    m <- leaflet::addMarkers(
      m, lng = df$lon, lat = df$lat, popup = popup_txt
    )
  }

  # ----- legend (only to describe color_by values; pins remain default)
  if (!is.null(color_by) && color_by %in% names(df)) {
    v <- df[[color_by]]
    lt <- if (is.null(legend_title)) color_by else legend_title

    if (is.numeric(v)) {
      rng <- range(v, na.rm = TRUE); if (!all(is.finite(rng))) rng <- c(0, 1)
      pal <- leaflet::colorNumeric("viridis", domain = rng, na.color = "#cccccc")
      m <- leaflet::addLegend(m, pal = pal, values = v, opacity = 0.9, title = lt)
    } else {
      v <- as.factor(v)
      pal <- leaflet::colorFactor("Set2", domain = levels(v), na.color = "#cccccc")
      m <- leaflet::addLegend(m, pal = pal, values = as.integer(v), # legenda categ.
                              labels = levels(v), opacity = 0.9, title = lt)
    }
  }

  # ----- fit bounds
  m <- leaflet::fitBounds(
    m,
    lng1 = min(df$lon, na.rm = TRUE), lat1 = min(df$lat, na.rm = TRUE),
    lng2 = max(df$lon, na.rm = TRUE), lat2 = max(df$lat, na.rm = TRUE)
  )

  m
}
