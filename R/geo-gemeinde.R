#' Municipal boundaries for a defined stand
#'
#' Municipal boundaries (political municipalities, without communal
#' condominia and lake areas) from swisstopo's swissBOUNDARIES3D, obtained
#' through the STAC API of data.geo.admin.ch.
#'
#' The release valid at `stand_d` is used, i.e. the most recent release
#' dated on or before `stand_d`; its date is returned in `stand_d` (and
#' reported if it differs from the requested date). Geometries are returned
#' in the source CRS (LV95, EPSG:2056) and only transformed if `crs` is
#' given. Z coordinates are dropped. Municipalities consisting of several
#' parts are combined into one MULTIPOLYGON per `gem_id`. Invalid
#' geometries are repaired with [sf::st_make_valid()]; repaired and empty
#' geometries are reported and listed in the attributes `repaired_gem_id`
#' and `empty_gem_id`.
#'
#' The first download of a release is large (the full-resolution data set,
#' several hundred MB unpacked); afterwards the parsed object is served from
#' the cache. The zip file itself is not kept.
#'
#' @param stand_d boundary stand (`Date` or `"YYYY-MM-DD"`); `NULL` uses the
#'   most recent release
#' @param crs optional target CRS (EPSG code or anything accepted by
#'   [sf::st_crs()]); `NULL` keeps EPSG:2056
#' @inheritParams gemeinde
#'
#' @return An `sf` object with the columns `stand_d`, `gem_id`, `gem_x` and
#'   `geometry`; key `stand_d` + `gem_id`.
#'
#' @seealso [gemeinde()]
#' @examples
#' \dontrun{
#' geo <- geo_gemeinde(as.Date("2021-01-01"))
#' plot(sf::st_geometry(geo))
#' }
#' @export
geo_gemeinde <- function(stand_d = NULL, crs = NULL, cache = TRUE,
                         refresh = FALSE) {
  items <- .cached("geometry", "stac", "swissboundaries3d", cache = cache,
                   refresh = refresh, mode = "remote-first",
                   fetch = .stac_boundaries)
  item <- .pick_release(items, stand_d)

  out <- .cached("geometry", item$id, "gemeinde", cache = cache,
                 refresh = refresh, fetch = function() {
    dir <- if (cache) dirname(.cache_path("geometry", item$id, "x"))
           else tempfile("helvetia_")
    dl <- .download_checked(item$href, file.path(dir, basename(item$href)))
    on.exit(unlink(c(dl$path, file.path(dir, "unzipped")), recursive = TRUE),
            add = TRUE)
    files <- utils::unzip(dl$path, exdir = file.path(dir, "unzipped"))
    gpkg <- grep("\\.gpkg$", files, value = TRUE)
    if (length(gpkg) != 1L)
      stop("expected exactly one GeoPackage in ", basename(item$href),
           call. = FALSE)
    res <- .read_boundaries(gpkg, item$stand_d)
    .attach_source_metadata(res, .source_meta(
      "swisstopo swissBOUNDARIES3D (Hoheitsgebiet), via STAC data.geo.admin.ch",
      item$href, retrieved_at = dl$retrieved_at, checksum = dl$md5,
      version = item$id))
  })

  if (!is.null(stand_d) && .as_stand_d(stand_d) != item$stand_d)
    message(sprintf("boundaries of release %s (stand %s) used for %s",
                    item$id, item$stand_d, .as_stand_d(stand_d)))
  if (!is.null(crs)) {
    meta <- source_info(out)
    out <- .attach_source_metadata(sf::st_transform(out, crs), meta)
  }
  out
}


.stac_url <- paste0("https://data.geo.admin.ch/api/stac/v0.9/collections/",
                    "ch.swisstopo.swissboundaries3d/items")

# All releases with their GeoPackage asset; follows "next" links.
.stac_boundaries <- function() {
  url <- .stac_url
  feats <- list()
  while (!is.null(url)) {
    js <- .http_json(url)
    feats <- c(feats, js$features)
    nxt <- Filter(function(l) identical(l$rel, "next"), js$links %||% list())
    url <- if (length(nxt)) nxt[[1L]]$href else NULL
  }
  .parse_stac_items(feats)
}

.parse_stac_items <- function(feats) {
  rows <- lapply(feats, function(f) {
    a <- Filter(function(x) grepl("geopackage", x$type %||% ""), f$assets)
    if (!length(a)) return(NULL)
    data.frame(id = f$id,
               stand_d = as.Date(substr(f$properties$datetime, 1L, 10L)),
               href = a[[1L]]$href, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out) || !nrow(out))
    stop("no swissBOUNDARIES3D GeoPackage releases found via STAC",
         call. = FALSE)
  out[order(out$stand_d), , drop = FALSE]
}


.pick_release <- function(items, stand_d) {
  if (is.null(stand_d)) return(items[nrow(items), , drop = FALSE])
  stand_d <- .as_stand_d(stand_d)
  ok <- which(items$stand_d <= stand_d)
  if (!length(ok))
    stop(sprintf("no boundary release on or before %s; earliest is %s",
                 stand_d, min(items$stand_d)), call. = FALSE)
  items[max(ok), , drop = FALSE]
}


.read_boundaries <- function(gpkg, stand_d) {
  layers <- sf::st_layers(gpkg)$name
  lyr <- grep("hoheitsgebiet", layers, ignore.case = TRUE, value = TRUE)
  if (length(lyr) != 1L)
    stop(sprintf("municipal layer not found; layers: %s",
                 paste(layers, collapse = ", ")), call. = FALSE)
  g <- sf::st_read(gpkg, layer = lyr, quiet = TRUE)
  .prepare_boundaries(g, stand_d)
}


.prepare_boundaries <- function(g, stand_d) {
  names(g)[names(g) != attr(g, "sf_column")] <-
    tolower(names(g)[names(g) != attr(g, "sf_column")])
  .require_cols(g, c("bfs_nummer", "name"), "swissBOUNDARIES3D Hoheitsgebiet")
  if ("objektart" %in% names(g)) {
    g <- g[g$objektart %in% "Gemeindegebiet", ]
  } else {
    warning("column 'objektart' missing; no filter on municipal areas applied",
            call. = FALSE)
  }
  if (!nrow(g)) stop("no municipal areas (Gemeindegebiet) found", call. = FALSE)

  g <- sf::st_zm(g)
  geom <- sf::st_geometry(g)

  invalid <- !sf::st_is_valid(geom)
  invalid[is.na(invalid)] <- TRUE
  repaired <- integer()
  if (any(invalid)) {
    geom[invalid] <- sf::st_make_valid(geom[invalid])
    repaired <- sort(unique(as.integer(g$bfs_nummer[invalid])))
    message(sprintf("%d invalid geometr%s repaired (gem_id %s)",
                    sum(invalid), if (sum(invalid) == 1L) "y" else "ies",
                    paste(utils::head(repaired, 10), collapse = ", ")))
  }

  id <- as.integer(g$bfs_nummer)
  parts <- split(seq_along(id), id)
  geom_u <- do.call(c, lapply(parts, function(i)
    sf::st_cast(sf::st_union(geom[i]), "MULTIPOLYGON")))

  out <- sf::st_sf(stand_d = rep(as.Date(stand_d), length(parts)),
                   gem_id = as.integer(names(parts)),
                   gem_x = g$name[vapply(parts, `[`, 1L, 1L)],
                   geometry = sf::st_set_crs(geom_u, sf::st_crs(g)))

  empty <- out$gem_id[sf::st_is_empty(out)]
  if (length(empty))
    warning(sprintf("empty geometries for gem_id %s",
                    paste(empty, collapse = ", ")), call. = FALSE)
  attr(out, "repaired_gem_id") <- repaired
  attr(out, "empty_gem_id") <- empty
  .validate_unique_key(out, c("stand_d", "gem_id"), "geo_gemeinde()")
  rownames(out) <- NULL
  out
}
