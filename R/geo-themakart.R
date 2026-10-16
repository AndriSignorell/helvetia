# BFS ThemaKart: generalised geometries for small-scale thematic maps --------
#
# The yearly sets ("Kartengeometrien ThemaKart - Set YYYY") are discovered
# through the search API of the BFS data catalogue (dam-api), downloaded
# once per set into the cache and read from the zip in place (GDAL
# /vsizip/), without unpacking. Layers are found by
# parsing the file names (K4<code><yyyymmdd><variant>_ch2007<Poly|Pnts>),
# not through a hand-maintained path table.

.damApi <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets"

.helvetiaEnv <- new.env(parent = emptyenv())


#' List the Available ThemaKart Map Sets
#'
#' Lists the yearly sets of generalised geometries for small-scale thematic
#' maps ("Kartengeometrien ThemaKart") published by the BFS. Use it to see
#' which map years can be requested with `setYear` in [geoThemakart()].
#'
#' The sets are found through the search API of the BFS data catalogue.
#' Only the yearly sets from 2020 onwards are listed; the older BASIS/PRO
#' sets have a different structure and are not supported.
#'
#' @inheritParams gemeinde
#'
#' @return A data.frame with the columns `set_n` (year of the set),
#'   `dam_id` (catalogue id), `published_d` and `url` (download link).
#'
#' @family swiss.geometry
#' @concept map
#' @concept spatial-data
#' @seealso [geoThemakart()].
#' @examples
#' \dontrun{
#' themakartSets()
#' }
#' @export
themakartSets <- function(cache = TRUE, refresh = FALSE) {
  .cached("themakart", "sets", "index", cache = cache, refresh = refresh,
          mode = "remote-first", fetch = .fetchThemakartSets)
}


.fetchThemakartSets <- function() {
  items <- list()
  skip <- 0L
  repeat {
    js <- .httpJson(.damApi, query = list(
      language = "de", title = "Kartengeometrien ThemaKart", skip = skip))
    items <- c(items, js$data)
    skip <- skip + length(js$data)
    if (!length(js$data) || skip >= (js$total %||% 0L) || skip > 500L) break
  }
  .parseThemakartSets(items)
}


.parseThemakartSets <- function(items) {
  rows <- lapply(items, function(a) {
    title <- a$description$titles$main %||% ""
    m <- regmatches(title, regexec(
      "^Kartengeometrien ThemaKart - Set ([0-9]{4})$", title))[[1L]]
    if (!length(m)) return(NULL)
    master <- Filter(function(l) identical(l$rel, "master"), a$links)
    if (!length(master)) return(NULL)
    data.frame(set_n = as.integer(m[2L]),
               dam_id = as.integer(a$ids$damId),
               published_d = as.Date(substr(a$bfs$embargo %||% NA, 1L, 10L)),
               url = master[[1L]]$href, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out) || !nrow(out))
    stop("No ThemaKart sets found in the BFS data catalogue.", call. = FALSE)
  out <- out[!duplicated(out$set_n), , drop = FALSE]
  .resetRownames(out[order(out$set_n), , drop = FALSE])
}


# Spatial units: user-facing name -> ThemaKart code and output columns.
.tkUnits <- data.frame(
  unit = c("schweiz", "kanton", "bezirk", "gemeinde", "grossregion",
           "ms-region", "sprachgebiet", "agglomeration", "metropolitanraum",
           "tourismusregion", "arbeitsmarktregion", "arbeitsmarktgrossregion"),
  code = c("suis", "kant", "bezk", "polg", "greg", "msre", "sprg", "aggl",
           "metr", "tour", "amre", "amgr"),
  id   = c("region_id", "kanton_id", "bezirk_id", "gem_id",
           rep("region_id", 8)),
  x    = c("region_x", "kanton_x", "bezirk_x", "gem_x", rep("region_x", 8)),
  stringsAsFactors = FALSE)


# Index of all layers of a set, parsed from the file names: either the
# downloaded zip (read in place through GDAL's /vsizip/) or a locally
# unpacked directory.
.themakartIndex <- function(src) {
  if (!is.null(src$zip)) {
    names <- .zipEntries(src$zip)
    zipPath <- normalizePath(src$zip, winslash = "/", mustWork = TRUE)
    files <- paste0("/vsizip/", zipPath, "/",
                    names[grepl("\\.(shp|gpkg)$", names, ignore.case = TRUE)])
  } else {
    files <- list.files(src$dir, pattern = "\\.(shp|gpkg)$",
                        recursive = TRUE, full.names = TRUE,
                        ignore.case = TRUE)
  }
  .parseThemakartFiles(files)
}


# Entry names of a zip as UTF-8. Names without the UTF-8 flag are stored in
# the DOS code page CP437 ("Gesamtfl\x84che"); GDAL's /vsizip/ decodes them
# the same way, so the converted names address the entries. Nothing is
# extracted: R on Windows cannot create files or directories from such
# names.
.zipEntries <- function(zip) {
  names <- unzip(zip, list = TRUE)$Name
  bad <- !validUTF8(names)
  if (any(bad)) names[bad] <- iconv(names[bad], "CP437", "UTF-8")
  if (anyNA(names))
    stop("The entry names of the ThemaKart archive could not be decoded.",
         call. = FALSE)
  names
}


.parseThemakartFiles <- function(files) {
  re <- paste0("^k4_?([a-z0-9]{4})_?([0-9]{8}|yyyymmdd)",
               "([a-z]{2}|[0-9]{2})(_ch2007)?(poly|pnts)?\\.(shp|gpkg)$")
  # directory names contain umlauts ("Gesamtflaeche"): avoid re-encoding
  b <- tolower(sub(".*[/\\\\]", "", files, useBytes = TRUE))
  m <- regmatches(b, regexec(re, b))
  ok <- lengths(m) > 0L
  if (!any(ok))
    stop("No ThemaKart layers recognised; files found: ",
         paste(head(b, 10), collapse = ", "), ".", call. = FALSE)
  m <- do.call(rbind, m[ok])
  variant <- m[, 4L]
  date <- ifelse(m[, 3L] == "yyyymmdd", NA_character_, m[, 3L])
  data.frame(
    path     = files[ok],
    code     = m[, 2L],
    date     = as.Date(date, format = "%Y%m%d"),
    variant  = variant,
    area     = ifelse(variant %in% c("vf", "zv"), "vegetation", "total"),
    output   = ifelse(m[, 6L] == "pnts" | variant %in% c("zg", "zv", "kk"),
                      "point", "polygon"),
    category = suppressWarnings(as.integer(substr(variant, 1L, 1L))),
    stringsAsFactors = FALSE)
}


# Location of a set: the downloaded zip, kept in the cache and read in
# place, or with options(helvetia.themakartDir = <path>) a locally unpacked
# set (e.g. from an existing bfsMaps installation).
.themakartDir <- function(setYear, cache, refresh) {
  local <- getOption("helvetia.themakartDir")
  if (!is.null(local)) {
    if (!dir.exists(local))
      stop("Option 'helvetia.themakartDir' points to a missing directory: ",
           local, call. = FALSE)
    return(list(dir = local, set = .tkSetInfo(setYear, local = TRUE)))
  }
  set <- .tkSetInfo(setYear, cache = cache, refresh = refresh)
  dir <- if (cache) file.path(.cacheDir(), "themakart", set$set_n)
         else file.path(tempdir(), "helvetia_themakart", set$set_n)
  zip <- file.path(dir, "set.zip")
  metaFile <- file.path(dir, "set.rds")

  have <- file.exists(metaFile) && file.exists(zip)
  if (refresh || !have) {
    # download next to the old copy, verify, then replace: a failed refresh
    # leaves the existing set untouched
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    new <- file.path(dir, "set.zip.new")
    on.exit(unlink(new), add = TRUE)
    dl <- tryCatch({
      d <- .downloadChecked(set$url, new)
      if (!length(.zipEntries(new)))
        stop("the downloaded file is not a readable zip archive")
      d
    }, error = function(e) e)
    if (inherits(dl, "error")) {
      if (!have) stop(dl)
      warning(sprintf(paste("ThemaKart set %s could not be refreshed (%s);",
                            "using the cached copy."),
                      set$set_n, conditionMessage(dl)), call. = FALSE)
    } else {
      .moveFile(new, zip)
      saveRDS(list(url = dl$url, retrievedAt = dl$retrievedAt,
                   md5 = unname(tools::md5sum(zip))), metaFile)
    }
  }
  list(zip = zip, set = set, download = readRDS(metaFile))
}


.tkSetInfo <- function(setYear, cache = TRUE, refresh = FALSE,
                       local = FALSE) {
  if (local)
    return(list(set_n = as.integer(setYear %||%
                  getOption("helvetia.themakartSet", NA_integer_)),
                url = getOption("helvetia.themakartDir")))
  sets <- themakartSets(cache = cache, refresh = refresh)
  if (is.null(setYear)) return(as.list(sets[nrow(sets), ]))
  i <- match(as.integer(setYear), sets$set_n)
  if (is.na(i))
    stop(sprintf("Argument 'setYear': set %s not available; available are %s.",
                 setYear, paste(sets$set_n, collapse = ", ")), call. = FALSE)
  as.list(sets[i, ])
}


#' Get Generalised Geometries for Thematic Maps (ThemaKart)
#'
#' Reads a layer of the BFS map geometry sets for small-scale thematic maps
#' ("Kartengeometrien ThemaKart"). These generalised geometries are the
#' official basis for statistical maps of Switzerland: small, fast to draw
#' and available for many spatial units besides the institutional ones.
#'
#' A set is downloaded once (about 12 MB) as zip archive into the cache and
#' read from the archive in place, without unpacking. `refresh = TRUE`
#' downloads it anew and replaces the cached archive only after the new one
#' has been verified; on failure the cached set is kept with a warning.
#' A set holds the geometries of its own stand (and possibly a few older
#' ones). Without `setYear`, the sets are searched for the most recent stand
#' on or before `date`: first the set of the year of `date`, then the newer
#' sets, then the older ones; each set searched is downloaded once. Without
#' `date` the most recent set is used. The stand found is returned in
#' `stand_d` and reported by a message if it differs from `date`. To use an already
#' unpacked set, e.g. from a former bfsMaps installation, set
#' `options(helvetia.themakartDir = <path>)` and optionally
#' `options(helvetia.themakartSet = <year>)`.
#'
#' Maps published with these geometries must carry the source note
#' "Kartengrundlage: (c) BFS, ThemaKart"; see [stampBfs()].
#'
#' @param unit spatial unit: one of `"gemeinde"`, `"bezirk"`, `"kanton"`,
#'   `"schweiz"`, `"grossregion"`, `"ms-region"`, `"sprachgebiet"`,
#'   `"agglomeration"`, `"metropolitanraum"`, `"tourismusregion"`,
#'   `"arbeitsmarktregion"`, `"arbeitsmarktgrossregion"`, or a four-letter
#'   ThemaKart code such as `"deur"`.
#' @param date stand of the boundaries, a `Date` or a string
#'   `"YYYY-MM-DD"`. The default `NULL` uses the most recent stand in the
#'   set.
#' @param area `"total"` for the total area or `"vegetation"` for the
#'   vegetation area, i.e. the inhabitable area without lakes and high
#'   mountains.
#' @param output `"polygon"` for the areas or `"point"` for the official
#'   label points of the units.
#' @param setYear year of the ThemaKart set; see Details.
#' @param crs target CRS. The default harmonises all layers to LV95
#'   (EPSG:2056), since not all layers of a set share one CRS.
#' @inheritParams gemeinde
#'
#' @return An `sf` object with `stand_d`, an id and a name column
#'   (`gem_id`/`gem_x`, `bezirk_id`/`bezirk_x`, `kanton_id`/`kanton_x` plus
#'   `kanton_c`, otherwise `region_id`/`region_x`) and the geometry. The
#'   attribute `"themakart"` records set, unit and stand, so that
#'   [plotMap()] can find the matching vegetation area and label points.
#'
#' @family swiss.geometry
#' @concept map
#' @concept spatial-data
#' @seealso [geoGemeinde()], [plotMap()], [geoGewaesser()].
#' @examples
#' \dontrun{
#' kt <- geoThemakart("kanton")
#' plot(sf::st_geometry(kt))
#' }
#' @export
geoThemakart <- function(unit = "gemeinde", date = NULL,
                         area = c("total", "vegetation"),
                         output = c("polygon", "point"),
                         setYear = NULL, crs = 2056,
                         cache = TRUE, refresh = FALSE) {
  area <- match.arg(area)
  output <- match.arg(output)
  u <- .tkUnit(unit)
  if (!is.null(date)) date <- .asDate(date)
  setYear <- .asYear(setYear, "setYear")

  # a set holds only its own stand (and possibly a few older ones), so the
  # stand valid at `date` is searched over the sets
  tried <- character()
  for (sy in .tkCandidateSets(setYear, date, cache, refresh)) {
    src <- .themakartDir(sy, cache, refresh)
    idx <- .themakartIndex(src)
    lyr <- .tkFindLayer(idx, u$code, date, area, output)
    if (!is.null(lyr)) break
    tried <- c(tried, as.character(src$set$set_n))
  }
  if (is.null(lyr))
    stop(sprintf(paste("Argument 'date': no %s layer (%s area, %s) on or",
                       "before %s in ThemaKart set(s) %s."),
                 u$code, area, output, format(date %||% Sys.Date()),
                 paste(tried, collapse = ", ")), call. = FALSE)
  if (!is.null(date) && !is.na(lyr$date) && lyr$date != date)
    message(sprintf("%s: stand %s of set %s used for %s.", u$code, lyr$date,
                    src$set$set_n, date))

  g <- sf::st_read(lyr$path, quiet = TRUE)
  out <- .tkNormalise(g, u, lyr$date, crs)

  .helvetiaEnv$lastSet <- src$set$set_n
  attr(out, "themakart") <- list(unit = u$unit, code = u$code,
                                 setYear = src$set$set_n, date = lyr$date,
                                 area = area, output = output)
  .attachSourceMetadata(out, .sourceMeta(
    sprintf("BFS Kartengeometrien ThemaKart, Set %s", src$set$set_n),
    c(src$set$url, lyr$path),
    retrievedAt = src$download$retrievedAt %||% Sys.time(),
    checksum = src$download$md5 %||% NA_character_,
    version = paste0(lyr$code, format(lyr$date, "%Y%m%d"), lyr$variant)))
}


# Sets to search, in order: a given set only; else the set of the year of
# `date`, then the newer sets, then the older ones; without `date` the most
# recent set. A locally unpacked set (option) is the only candidate.
.tkCandidateSets <- function(setYear, date, cache, refresh) {
  if (!is.null(getOption("helvetia.themakartDir")))
    return(list(setYear %||% getOption("helvetia.themakartSet")))
  if (!is.null(setYear)) return(list(setYear))
  sets <- themakartSets(cache = cache, refresh = refresh)$set_n
  if (is.null(date)) return(list(max(sets)))
  y <- as.integer(format(date, "%Y"))
  as.list(c(sort(sets[sets >= y]), sort(sets[sets < y], decreasing = TRUE)))
}


.tkUnit <- function(unit) {
  if (!is.character(unit) || length(unit) != 1L)
    stop("Argument 'unit' must be a single string.", call. = FALSE)
  i <- match(tolower(unit), .tkUnits$unit)
  if (is.na(i)) i <- match(tolower(unit), .tkUnits$code)
  if (!is.na(i)) return(as.list(.tkUnits[i, ]))
  if (grepl("^[a-z0-9]{4}$", tolower(unit)))
    return(list(unit = tolower(unit), code = tolower(unit), id = "region_id",
                x = "region_x"))
  stop(sprintf("Argument 'unit': unknown unit %s; use one of %s or a %s.",
               sQuote(unit, FALSE), paste(.tkUnits$unit, collapse = ", "),
               "four-letter ThemaKart code"), call. = FALSE)
}


# Most recent layer on or before `date` in one set; NULL if there is none.
.tkFindLayer <- function(idx, code, date, area, output) {
  cand <- idx[idx$code == code & idx$area == area & idx$output == output, ,
              drop = FALSE]
  if (!is.null(date) && any(!is.na(cand$date)))
    cand <- cand[!is.na(cand$date) & cand$date <= date, , drop = FALSE]
  if (!nrow(cand)) return(NULL)
  cand <- cand[order(cand$date, decreasing = TRUE, na.last = TRUE), ,
               drop = FALSE]
  cand[1L, ]
}


.tkNormalise <- function(g, u, date, crs) {
  attrCols <- setdiff(names(g), attr(g, "sf_column"))
  names(g)[match(attrCols, names(g))] <- tolower(attrCols)
  g <- .fixEncoding(g)
  if (!"id" %in% names(g))
    stop(sprintf(paste("ThemaKart layer %s has no id column (available: %s);",
                       "the file format may have changed."), u$code,
                 paste(attrCols, collapse = ", ")), call. = FALSE)
  if (anyNA(.asInt(g$id)))
    stop(sprintf("ThemaKart layer %s has missing or non-numeric ids.", u$code),
         call. = FALSE)
  g <- .tkUnionById(g, u$code)
  id <- g$id
  nm <- if ("name" %in% names(g)) g$name else rep(NA_character_, nrow(g))

  df <- data.frame(stand_d = rep(date, nrow(g)),
                   id = .asInt(id), x = as.character(nm),
                   stringsAsFactors = FALSE)
  names(df)[2:3] <- c(u$id, u$x)
  if (u$code == "kant") df$kanton_c <- .kantonAbbrev[df$kanton_id]

  geom <- sf::st_geometry(g)
  if (is.na(sf::st_crs(geom)))
    stop("ThemaKart layer without CRS.", call. = FALSE)
  if (!is.null(crs) && !is.na(crs) && sf::st_crs(geom) != sf::st_crs(crs))
    geom <- sf::st_transform(geom, crs)
  out <- sf::st_sf(df, geometry = geom)
  out <- out[order(out[[u$id]]), ]
  rownames(out) <- NULL
  out
}


# A unit must be one row. Polygons split into several rows (multipart
# stored as single parts) are unioned; repeated points, or one id with
# different names, are an error.
.tkUnionById <- function(g, code) {
  if (!anyDuplicated(g$id)) return(g)
  dup <- unique(g$id[duplicated(g$id)])
  poly <- all(sf::st_geometry_type(g) %in% c("POLYGON", "MULTIPOLYGON"))
  nm <- if ("name" %in% names(g)) g$name else rep(NA_character_, nrow(g))
  nNames <- tapply(nm, g$id, function(v) length(unique(v)))
  if (!poly || any(nNames > 1L))
    stop(sprintf("ThemaKart layer %s: id(s) %s occur repeatedly.", code,
                 paste(head(dup, 10), collapse = ", ")), call. = FALSE)
  first <- g[!duplicated(g$id), , drop = FALSE]
  geom <- sf::st_geometry(g)
  u <- lapply(first$id, function(i) sf::st_union(geom[g$id == i]))
  sf::st_geometry(first) <- sf::st_set_crs(do.call(c, u), sf::st_crs(g))
  first
}


# Older shapefiles come without .cpg and in latin1.
.fixEncoding <- function(x) {
  for (n in names(x)) {
    v <- x[[n]]
    if (is.character(v) && any(!validUTF8(v[!is.na(v)])))
      x[[n]] <- iconv(v, "latin1", "UTF-8")
  }
  x
}


#' Get Lakes and Rivers
#'
#' Returns the water bodies of the topographic layers of a ThemaKart set:
#' lakes in two size categories, rivers in five. They make thematic maps
#' readable as maps of Switzerland, and lakes explain gaps in maps drawn on
#' the vegetation area.
#'
#' @param type `"see"` for lakes or `"fluss"` for rivers.
#' @param category integer vector of size categories, 1 being the largest;
#'   `NULL` for all.
#' @inheritParams geoThemakart
#'
#' @return An `sf` object with the columns `kategorie_n` and `geometry`.
#'
#' @family swiss.geometry
#' @concept map
#' @concept hydrography
#' @seealso [addGewaesser()] to draw them.
#' @examples
#' \dontrun{
#' lakes <- geoGewaesser("see", category = 1)
#' }
#' @export
geoGewaesser <- function(type = c("see", "fluss"), category = NULL,
                         setYear = NULL, crs = 2056, cache = TRUE,
                         refresh = FALSE) {
  type <- match.arg(type)
  setYear <- .asYear(setYear, "setYear") %||% .helvetiaEnv$lastSet
  code <- c(see = "seen", fluss = "flus")[[type]]
  src <- .themakartDir(setYear, cache, refresh)
  idx <- .themakartIndex(src)
  idx <- idx[idx$code == code & !is.na(idx$category), , drop = FALSE]
  if (!is.null(category)) idx <- idx[idx$category %in% category, ]
  if (!nrow(idx))
    stop(sprintf("Argument 'category': no %s layers for the requested %s.",
                 type, "categories"), call. = FALSE)
  idx <- idx[order(idx$category), , drop = FALSE]
  parts <- lapply(seq_len(nrow(idx)), function(i) {
    geom <- sf::st_geometry(sf::st_read(idx$path[i], quiet = TRUE))
    if (!is.null(crs) && sf::st_crs(geom) != sf::st_crs(crs))
      geom <- sf::st_transform(geom, crs)
    sf::st_sf(kategorie_n = rep(idx$category[i], length(geom)),
              geometry = geom)
  })
  out <- do.call(rbind, parts)
  .attachSourceMetadata(out, .sourceMeta(
    sprintf("BFS Kartengeometrien ThemaKart, Set %s", src$set$set_n),
    c(src$set$url, idx$path)))
}
