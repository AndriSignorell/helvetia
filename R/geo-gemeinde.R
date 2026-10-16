#' Get the Boundaries of Municipalities, Districts and Cantons
#'
#' Returns the polygons of the Swiss political municipalities, districts or
#' cantons for a defined stand, ready to be joined with statistics of the
#' same stand and drawn with [plotMap()].
#'
#' Two sources are available for municipalities:
#' \describe{
#'   \item{`"themakart"`}{the generalised BFS geometries for thematic maps
#'     (see [geoThemakart()]): small, fast and the official basis of
#'     statistical maps; total area including lakes.}
#'   \item{`"swissboundaries3d"`}{swisstopo's full-resolution boundaries,
#'     obtained through the STAC API of data.geo.admin.ch, for precise
#'     spatial analysis. The release valid at `date` is used. Communal
#'     condominia and lake areas are excluded, Z coordinates dropped, parts
#'     combined to one MULTIPOLYGON per municipality, and invalid geometries
#'     repaired with [sf::st_make_valid()] and reported (attributes
#'     `repairedGemId`, `emptyGemId`). The first download is large.}
#' }
#' Districts and cantons always come from ThemaKart. Geometries are in LV95
#' (EPSG:2056); another CRS is only used if `crs` is given.
#'
#' @param date stand of the boundaries, a `Date` or a string
#'   `"YYYY-MM-DD"`. The default `NULL` uses the most recent stand.
#' @param source source of the municipal boundaries, `"themakart"` or
#'   `"swissboundaries3d"`.
#' @param crs optional target CRS, an EPSG code or anything accepted by
#'   [sf::st_crs()]; `NULL` keeps EPSG:2056.
#' @param ... further arguments passed to [geoThemakart()], e.g.
#'   `area = "vegetation"` or `setYear`.
#' @inheritParams gemeinde
#'
#' @return An `sf` object with `stand_d`, the unit's id and name
#'   (`gem_id`/`gem_x`, `bezirk_id`/`bezirk_x`, or `kanton_id`/`kanton_x`
#'   and `kanton_c`) and `geometry`. The key is `stand_d` + id.
#'
#' @family swiss.geometry
#' @concept municipality
#' @concept spatial-data
#' @seealso [gemeinde()], [plotMap()].
#' @examples
#' \dontrun{
#' geo <- geoGemeinde(as.Date("2021-01-01"))
#' plotMap(geo)
#' kt <- geoKanton()
#' }
#' @name geo-boundaries
NULL


#' @rdname geo-boundaries
#' @export
geoGemeinde <- function(date = NULL,
                        source = c("themakart", "swissboundaries3d"),
                        crs = NULL, cache = TRUE, refresh = FALSE, ...) {
  source <- match.arg(source)
  out <- switch(source,
    themakart = geoThemakart("gemeinde", date = date, cache = cache,
                             refresh = refresh, ...),
    swissboundaries3d = .geoSwissboundaries(date, cache, refresh))
  .transformKeep(out, crs)
}


#' @rdname geo-boundaries
#' @export
geoBezirk <- function(date = NULL, crs = NULL, cache = TRUE,
                      refresh = FALSE, ...) {
  .transformKeep(geoThemakart("bezirk", date = date, cache = cache,
                              refresh = refresh, ...), crs)
}


#' @rdname geo-boundaries
#' @export
geoKanton <- function(date = NULL, crs = NULL, cache = TRUE,
                      refresh = FALSE, ...) {
  .transformKeep(geoThemakart("kanton", date = date, cache = cache,
                              refresh = refresh, ...), crs)
}


# st_transform without losing the package attributes.
.transformKeep <- function(x, crs) {
  if (is.null(crs)) return(x)
  keep <- attributes(x)[c("source", "themakart", "repairedGemId",
                          "emptyGemId")]
  x <- sf::st_transform(x, crs)
  for (a in names(keep)) if (!is.null(keep[[a]])) attr(x, a) <- keep[[a]]
  x
}
