# swissBOUNDARIES3D (swisstopo, full resolution) ----------------------------
# Used by geoGemeinde(source = "swissboundaries3d").

.geoSwissboundaries <- function(date, cache, refresh) {
  items <- .cached("geometry", "stac", "swissboundaries3d", cache = cache,
                   refresh = refresh, mode = "remote-first",
                   fetch = .stacBoundaries)
  item <- .pickRelease(items, date)

  out <- .cached("geometry", item$id, "gemeinde", cache = cache,
                 refresh = refresh, fetch = function() {
    dir <- if (cache) dirname(.cachePath("geometry", item$id, "x"))
           else tempfile("helvetia_")
    dl <- .downloadChecked(item$href, file.path(dir, basename(item$href)))
    on.exit(unlink(c(dl$path, file.path(dir, "unzipped")), recursive = TRUE),
            add = TRUE)
    files <- unzip(dl$path, exdir = file.path(dir, "unzipped"))
    gpkg <- grep("\\.gpkg$", files, value = TRUE)
    if (length(gpkg) != 1L)
      stop("Expected exactly one GeoPackage in ", basename(item$href), ".",
           call. = FALSE)
    res <- .readBoundaries(gpkg, item$date)
    .attachSourceMetadata(res, .sourceMeta(
      "swisstopo swissBOUNDARIES3D (Hoheitsgebiet), via STAC data.geo.admin.ch",
      item$href, retrievedAt = dl$retrievedAt, checksum = dl$md5,
      version = item$id))
  })

  if (!is.null(date) && .asDate(date) != item$date)
    message(sprintf("Boundaries of release %s (stand %s) used for %s.",
                    item$id, item$date, .asDate(date)))
  out
}


.stacUrl <- paste0("https://data.geo.admin.ch/api/stac/v0.9/collections/",
                   "ch.swisstopo.swissboundaries3d/items")

# All releases with their GeoPackage asset; follows "next" links.
.stacBoundaries <- function() {
  url <- .stacUrl
  feats <- list()
  while (!is.null(url)) {
    js <- .httpJson(url)
    feats <- c(feats, js$features)
    nxt <- Filter(function(l) identical(l$rel, "next"), js$links %||% list())
    url <- if (length(nxt)) nxt[[1L]]$href else NULL
  }
  .parseStacItems(feats)
}

.parseStacItems <- function(feats) {
  rows <- lapply(feats, function(f) {
    a <- Filter(function(x) grepl("geopackage", x$type %||% ""), f$assets)
    if (!length(a)) return(NULL)
    data.frame(id = f$id,
               date = as.Date(substr(f$properties$datetime, 1L, 10L)),
               href = a[[1L]]$href, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out) || !nrow(out))
    stop("No swissBOUNDARIES3D GeoPackage releases found via STAC.",
         call. = FALSE)
  out[order(out$date), , drop = FALSE]
}


.pickRelease <- function(items, date) {
  if (is.null(date)) return(items[nrow(items), , drop = FALSE])
  date <- .asDate(date)
  ok <- which(items$date <= date)
  if (!length(ok))
    stop(sprintf(paste("Argument 'date': no boundary release on or before",
                       "%s; the earliest is %s."),
                 date, min(items$date)), call. = FALSE)
  items[max(ok), , drop = FALSE]
}


.readBoundaries <- function(gpkg, date) {
  layers <- sf::st_layers(gpkg)$name
  lyr <- grep("hoheitsgebiet", layers, ignore.case = TRUE, value = TRUE)
  if (length(lyr) != 1L)
    stop(sprintf("Municipal layer not found; the layers are %s.",
                 paste(layers, collapse = ", ")), call. = FALSE)
  g <- sf::st_read(gpkg, layer = lyr, quiet = TRUE)
  .prepareBoundaries(g, date)
}


.prepareBoundaries <- function(g, date) {
  attrCols <- names(g) != attr(g, "sf_column")
  names(g)[attrCols] <- tolower(names(g)[attrCols])
  .requireCols(g, c("bfs_nummer", "name"), "swissBOUNDARIES3D Hoheitsgebiet")
  if ("objektart" %in% names(g)) {
    g <- g[g$objektart %in% "Gemeindegebiet", ]
  } else {
    warning(paste("Column 'objektart' is missing; no filter on municipal",
                  "areas applied."), call. = FALSE)
  }
  if (!nrow(g))
    stop("No municipal areas (Gemeindegebiet) found.", call. = FALSE)

  g <- sf::st_zm(g)
  geom <- sf::st_geometry(g)

  invalid <- !sf::st_is_valid(geom)
  invalid[is.na(invalid)] <- TRUE
  repaired <- integer()
  if (any(invalid)) {
    geom[invalid] <- sf::st_make_valid(geom[invalid])
    repaired <- sort(unique(as.integer(g$bfs_nummer[invalid])))
    message(sprintf("%d invalid geometr%s repaired (gem_id %s).",
                    sum(invalid), if (sum(invalid) == 1L) "y" else "ies",
                    paste(head(repaired, 10), collapse = ", ")))
  }

  id <- as.integer(g$bfs_nummer)
  parts <- split(seq_along(id), id)
  geomUnion <- do.call(c, lapply(parts, function(i)
    sf::st_cast(sf::st_union(geom[i]), "MULTIPOLYGON")))

  out <- sf::st_sf(stand_d = rep(as.Date(date), length(parts)),
                   gem_id = as.integer(names(parts)),
                   gem_x = g$name[vapply(parts, `[`, 1L, 1L)],
                   geometry = sf::st_set_crs(geomUnion, sf::st_crs(g)))

  empty <- out$gem_id[sf::st_is_empty(out)]
  if (length(empty))
    warning(sprintf("Empty geometries for gem_id %s.",
                    paste(empty, collapse = ", ")), call. = FALSE)
  attr(out, "repairedGemId") <- repaired
  attr(out, "emptyGemId") <- empty
  .validateUniqueKey(out, c("stand_d", "gem_id"), "geoGemeinde()")
  rownames(out) <- NULL
  out
}
