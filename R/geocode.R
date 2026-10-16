#' Convert Swiss Addresses to Coordinates
#'
#' Geocodes addresses with the search service of the Swiss federal geoportal
#' (api3.geo.admin.ch, origin `address`, based on the federal building and
#' dwelling register). Use it to place customers, patients or sites on a map
#' and, with [locate()], to assign them to municipalities.
#'
#' Coordinates are returned in LV95 (EPSG:2056) with `x` as easting and `y`
#' as northing, matching the maps of this package, and as WGS84 latitude and
#' longitude. Only the best match per address is kept; `n_treffer_n` tells
#' whether the address was ambiguous. Addresses without a match give a row
#' of `NA`.
#'
#' @param x character vector of addresses, e.g. `"Waldwis 4 Madetswil"`.
#' @param output `"data.frame"`, or `"sf"` for an `sf` point object (rows
#'   without a match dropped).
#'
#' @return One row per address with `adresse_x` (query), `treffer_x`
#'   (matched address), `n_treffer_n` (number of hits), `x`, `y` (LV95),
#'   `lat_n` and `lon_n`.
#'
#' @family swiss.spatial
#' @concept geocoding
#' @concept spatial-data
#' @seealso [locate()].
#' @examples
#' \dontrun{
#' adr <- geocode(c("Bundesplatz 3 Bern", "Quaderaweg 3 Trimmis"))
#' locate(adr$x, adr$y)
#' }
#' @export
geocode <- function(x, output = c("data.frame", "sf")) {
  output <- match.arg(output)
  if (!is.character(x) || !length(x))
    stop("Argument 'x' must be a non-empty character vector.", call. = FALSE)
  url <- "https://api3.geo.admin.ch/rest/services/api/SearchServer"
  rows <- lapply(x, function(a) {
    if (is.na(a)) return(.parseGeocode(list(), NA_character_))
    js <- .httpJson(url, query = list(searchText = a, origins = "address",
                                      type = "locations", sr = "2056"))
    .parseGeocode(js, a)
  })
  out <- .attachSourceMetadata(.resetRownames(do.call(rbind, rows)),
    .sourceMeta("swisstopo / geo.admin.ch SearchServer (origin address)", url))
  if (output == "sf") {
    meta <- sourceInfo(out)
    out <- sf::st_as_sf(out[!is.na(out$x), ], coords = c("x", "y"),
                        crs = 2056, remove = FALSE)
    out <- .attachSourceMetadata(out, meta)
  }
  out
}


.parseGeocode <- function(js, address) {
  res <- js$results %||% list()
  out <- data.frame(adresse_x = address, treffer_x = NA_character_,
                    n_treffer_n = length(res), x = NA_real_, y = NA_real_,
                    lat_n = NA_real_, lon_n = NA_real_,
                    stringsAsFactors = FALSE)
  if (!length(res)) return(out)
  a <- res[[1L]]$attrs
  num <- function(v) as.numeric(v %||% NA)
  # Swiss convention: attribute y = easting (2.4-2.9e6), x = northing.
  # Assign by magnitude so that a change of convention cannot swap them.
  e <- c(num(a$y), num(a$x))
  out$treffer_x <- gsub("<[^>]+>", "", a$label %||% a$detail %||% NA)
  out$x <- max(e)
  out$y <- min(e)
  out$lat_n <- num(a$lat)
  out$lon_n <- num(a$lon)
  out
}
