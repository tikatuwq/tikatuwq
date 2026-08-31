# R/plot_map_quality.R
# Mapa coroplético interativo de qualidade da agua (IQA / IET / NSFWQI)
# ASCII-only no codigo

# Paletas e classificacoes internas por indice
.map_quality_breaks <- list(
  IQA = list(
    breaks = c(0, 19, 36, 51, 79, 100),
    labels_pt = c("Pessima","Ruim","Regular","Boa","Otima"),
    labels_en = c("Very Bad","Bad","Fair","Good","Excellent"),
    colors = c("#d7191c","#fdae61","#ffffbf","#a6d96a","#1a9641")
  ),
  IET_carlson = list(
    breaks = c(-Inf, 30, 40, 50, 70, Inf),
    labels_pt = c("Ultraoligotrofico","Oligotrofico","Mesotrofico",
                  "Eutrofico","Hipereutrofico"),
    labels_en = c("Ultraoligotrophic","Oligotrophic","Mesotrophic",
                  "Eutrophic","Hypereutrophic"),
    colors = c("#3288BD","#66C2A5","#FEE08B","#FC8D59","#D53E4F")
  ),
  IET_lamparelli = list(
    breaks = c(-Inf, 47, 52, 59, 63, 67, Inf),
    labels_pt = c("Ultraoligotrofico","Oligotrofico","Mesotrofico",
                  "Eutrofico","Supereutrofico","Hipereutrofico"),
    labels_en = c("Ultraoligotrophic","Oligotrophic","Mesotrophic",
                  "Eutrophic","Supereutrophic","Hypereutrophic"),
    colors = c("#3288BD","#66C2A5","#FEE08B","#FC8D59","#F46D43","#D53E4F")
  ),
  NSFWQI = list(
    breaks = c(0, 25, 50, 70, 90, 100),
    labels_pt = c("Muito Ruim","Ruim","Regular","Boa","Excelente"),
    labels_en = c("Very Bad","Bad","Fair","Good","Excellent"),
    colors = c("#d7191c","#fdae61","#ffffbf","#a6d96a","#1a9641")
  )
)

#' Mapa interativo de qualidade da agua por ponto de amostragem
#'
#' @description
#' Cria um mapa Leaflet interativo colorindo cada ponto de amostragem
#' de acordo com o valor do indice de qualidade da agua calculado
#' (IQA, IET ou NSF WQI). Util para identificar espacialmente pontos
#' criticos e gradientes de qualidade ao longo de uma bacia.
#'
#' @details
#' A funcao detecta automaticamente a coluna de indice quando
#' \code{index_col = NULL}, procurando por: \code{"IQA"},
#' \code{"IET"}, \code{"IET_Carlson"}, \code{"IET_Lamparelli"},
#' \code{"TSI"}, \code{"NSFWQI"}.
#'
#' Requer o pacote \pkg{leaflet} (listado em \code{Suggests}).
#' Se nao estiver instalado, a funcao emite uma mensagem e retorna
#' \code{invisible(NULL)}.
#'
#' Quando o data frame tiver multiplas linhas por ponto, e usada a
#' **media** do indice por ponto para representacao no mapa.
#'
#' @param df Data frame com ao menos as colunas de coordenadas
#'   (\code{lat_col}, \code{lon_col}) e o indice de qualidade.
#' @param index_col Character; nome da coluna do indice. Se \code{NULL}
#'   (default), detectada automaticamente.
#' @param index Character; nome do indice para determinacao da paleta:
#'   \code{"IQA"} (default), \code{"IET_carlson"},
#'   \code{"IET_lamparelli"} ou \code{"NSFWQI"}.
#'   Ignorado quando \code{index_col} esta especificado e o nome
#'   da coluna identifica o indice univocamente.
#' @param lat_col Character; coluna de latitude. Default \code{"lat"}.
#' @param lon_col Character; coluna de longitude. Default \code{"lon"}.
#' @param label_col Character; coluna para rotulo do popup.
#'   Default \code{"ponto"}.
#' @param radius Numeric; raio dos circulos em pixels. Default \code{10}.
#' @param locale Character; idioma dos rotulos de classificacao:
#'   \code{"pt"} (default) ou \code{"en"}.
#'
#' @returns Um objeto \code{leaflet} (HTML widget) ou
#'   \code{invisible(NULL)} se \pkg{leaflet} nao estiver disponivel.
#'
#' @seealso \code{\link[=plot_map]{plot_map()}},
#'   \code{\link[=iqa]{iqa()}}, \code{\link[=iet_carlson]{iet_carlson()}}
#'
#' @family visualization-tools
#'
#' @examples
#' \donttest{
#' if (requireNamespace("leaflet", quietly = TRUE)) {
#'   data("wq_demo", package = "tikatuwq")
#'   d <- iqa(wq_demo, na_rm = TRUE)
#'   plot_map_quality(d, index = "IQA")
#' }
#' }
#'
#' @export
plot_map_quality <- function(
  df,
  index_col = NULL,
  index     = c("IQA", "IET_carlson", "IET_lamparelli", "NSFWQI"),
  lat_col   = "lat",
  lon_col   = "lon",
  label_col = "ponto",
  radius    = 10,
  locale    = c("pt", "en")
) {
  index  <- match.arg(index)
  locale <- match.arg(locale)
  stopifnot(is.data.frame(df))

  if (!requireNamespace("leaflet", quietly = TRUE)) {
    message("O pacote 'leaflet' e necessario para plot_map_quality(). ",
            "Instale com: install.packages('leaflet')")
    return(invisible(NULL))
  }

  # Detectar coluna do indice
  if (is.null(index_col)) {
    candidates <- c("IQA", "NSFWQI", "IET_Lamparelli", "IET_Carlson",
                    "IET", "TSI", "iet", "tsi")
    index_col  <- candidates[candidates %in% names(df)][1]
    if (is.na(index_col)) {
      stop("Coluna de indice nao detectada. Fornecer 'index_col' ",
           "ou calcular o indice com iqa() / iet_carlson() primeiro.")
    }
    # Ajustar 'index' pelo nome detectado
    if (grepl("Lamparelli|lamparelli", index_col, ignore.case = TRUE)) {
      index <- "IET_lamparelli"
    } else if (grepl("Carlson|carlson|IET|TSI|iet|tsi", index_col,
                     ignore.case = TRUE)) {
      index <- "IET_carlson"
    } else if (grepl("NSF", index_col, ignore.case = TRUE)) {
      index <- "NSFWQI"
    } else {
      index <- "IQA"
    }
  }

  if (!index_col %in% names(df))
    stop("Coluna '", index_col, "' nao encontrada.")
  if (!lat_col %in% names(df))
    stop("Coluna '", lat_col, "' nao encontrada.")
  if (!lon_col %in% names(df))
    stop("Coluna '", lon_col, "' nao encontrada.")

  # Media por ponto quando ha multiplas linhas
  idx_vals  <- suppressWarnings(as.numeric(df[[index_col]]))
  lat_vals  <- suppressWarnings(as.numeric(df[[lat_col]]))
  lon_vals  <- suppressWarnings(as.numeric(df[[lon_col]]))
  lab_vals  <- if (label_col %in% names(df)) df[[label_col]] else rep("", nrow(df))

  pts <- data.frame(
    label     = lab_vals,
    lat       = lat_vals,
    lon       = lon_vals,
    idx_value = idx_vals,
    stringsAsFactors = FALSE
  )
  pts <- pts[is.finite(pts$lat) & is.finite(pts$lon), , drop = FALSE]

  # Agregar por ponto (media do indice)
  grp  <- pts$label
  agg  <- lapply(split(pts, grp), function(g) {
    data.frame(
      label     = g$label[1],
      lat       = mean(g$lat, na.rm = TRUE),
      lon       = mean(g$lon, na.rm = TRUE),
      idx_value = mean(g$idx_value, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  pts_agg <- do.call(rbind, agg)
  rownames(pts_agg) <- NULL

  if (!nrow(pts_agg)) stop("Nenhum ponto com coordenadas validas encontrado.")

  # Paleta
  pal_def  <- .map_quality_breaks[[index]]
  lbl_key  <- if (locale == "pt") "labels_pt" else "labels_en"
  brk      <- pal_def$breaks
  clr      <- pal_def$colors
  lbl_vals <- pal_def[[lbl_key]]

  # Cor por ponto
  cut_idx <- as.integer(cut(pts_agg$idx_value, breaks = brk,
                             include.lowest = TRUE, right = TRUE))
  pts_agg$color <- ifelse(is.na(cut_idx), "grey70", clr[cut_idx])
  pts_agg$classe <- ifelse(is.na(cut_idx), "N/D",
                            lbl_vals[cut_idx])

  # Popup
  idx_label <- switch(index,
    IQA           = "IQA",
    IET_carlson   = "IET (Carlson)",
    IET_lamparelli = "IET (Lamparelli)",
    NSFWQI        = "NSF WQI"
  )
  pts_agg$popup_txt <- paste0(
    "<b>", pts_agg$label, "</b><br>",
    idx_label, ": <b>", round(pts_agg$idx_value, 1), "</b><br>",
    if (locale == "pt") "Classe: " else "Class: ",
    "<b>", pts_agg$classe, "</b>"
  )

  # Legenda
  legend_title <- switch(index,
    IQA            = if (locale == "pt") "IQA" else "WQI",
    IET_carlson    = if (locale == "pt") "IET (Carlson)" else "TSI (Carlson)",
    IET_lamparelli = if (locale == "pt") "IET (Lamparelli)" else "TSI (Lamparelli)",
    NSFWQI         = "NSF WQI"
  )

  # Valores representativos das faixas para a legenda (ponto medio)
  brk_num <- brk[is.finite(brk)]
  brk_all <- c(if (is.infinite(brk[1])) brk_num[1] - 5 else brk[1], brk_num)
  pal_fn  <- leaflet::colorBin(
    palette   = clr,
    domain    = c(min(brk_all), max(brk_all)),
    bins      = brk_all,
    na.color  = "grey70"
  )

  leaflet::leaflet(pts_agg) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addCircleMarkers(
      lat         = ~lat,
      lng         = ~lon,
      color       = ~color,
      fillColor   = ~color,
      fillOpacity = 0.85,
      opacity     = 1,
      radius      = radius,
      popup       = ~popup_txt,
      label       = ~label
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      colors   = rev(clr),
      labels   = rev(lbl_vals),
      title    = legend_title,
      opacity  = 0.9
    )
}
