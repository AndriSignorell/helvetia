# Map tools (from bfsMaps): plotting, source note, waters, union,
# neighbours, point location.

# Name of the id column of a map returned by the geo*() functions.
.mapKey <- function(map) {
  pref <- c("gem_id", "bezirk_id", "kanton_id", "region_id", "groups")
  key <- intersect(pref, names(map))
  if (!length(key)) key <- grep("_id$", names(map), value = TRUE)
  if (!length(key))
    stop(paste("Argument 'map' has no id column (expected gem_id,",
               "bezirk_id, kanton_id, groups, ...)."), call. = FALSE)
  key[1L]
}


.checkMap <- function(map) {
  if (!inherits(map, "sf"))
    stop("Argument 'map' must be an sf object or a unit name.", call. = FALSE)
  invisible(map)
}


# Translate ids given as canton abbreviations to the map's numeric ids.
.mapIds <- function(map, id, key) {
  if (is.character(id) && "kanton_c" %in% names(map))
    return(map[[key]][match(id, map$kanton_c)])
  id
}


# Companion layer (vegetation area or label points) of a ThemaKart map.
.mapCompanion <- function(map, area = "total", output = "polygon") {
  tk <- attr(map, "themakart")
  if (is.null(tk)) return(NULL)
  tryCatch(
    suppressMessages(geoThemakart(tk$code, date = tk$date, area = area,
                                  output = output, setYear = tk$setYear,
                                  crs = sf::st_crs(map))),
    error = function(e) NULL)
}


# Resolve a "value or list" argument (design rules 3.3 I): the bare value,
# or a list with the value as the unnamed element or as `text`, plus
# further named elements, which must be in `allowed`.
.valueOrList <- function(x, arg, allowed) {
  if (!is.list(x)) return(list(value = x, args = list()))
  nm <- names(x) %||% rep("", length(x))
  value <- if ("text" %in% nm) x[["text"]]
           else if (any(nm == "")) x[[which(nm == "")[1L]]]
           else TRUE
  args <- x[nm != "" & nm != "text"]
  bad <- setdiff(names(args), allowed)
  if (length(bad))
    stop(sprintf("Argument '%s': unknown element(s) %s.", arg,
                 paste(sQuote(bad, FALSE), collapse = ", ")), call. = FALSE)
  list(value = value, args = args)
}


# Flexible element pattern (design rules 6.3): TRUE draws with defaults,
# FALSE/NULL/NA draws nothing, a list draws with its settings.
.elementSpec <- function(x, arg, defaults) {
  if (is.null(x) || identical(x, FALSE) || identical(x, NA)) return(NULL)
  if (isTRUE(x)) return(defaults)
  if (!is.list(x))
    stop(sprintf("Argument '%s' must be TRUE, FALSE, NA or a list.", arg),
         call. = FALSE)
  bad <- setdiff(names(x), names(defaults))
  if (length(bad))
    stop(sprintf("Argument '%s': unknown element(s) %s.", arg,
                 paste(sQuote(bad, FALSE), collapse = ", ")), call. = FALSE)
  modifyList(defaults, x)
}


#' Map of Swiss Spatial Units
#'
#' Draws spatial units (municipalities, districts, cantons, regions) and
#' colours a selection of them. This is the quick way to a choropleth map
#' of municipal or cantonal statistics, and to highlight single units.
#'
#' Units not listed in `id` are drawn with the fill colour `bg` and a grey
#' border (unless `add = TRUE`). With a ThemaKart map the vegetation area
#' can be overlaid and labels are placed at the official label points;
#' otherwise at [sf::st_point_on_surface()]. ThemaKart maps get the
#' mandatory source note (see [stampBfs()]).
#'
#' @param map an `sf` object from [geoGemeinde()], [geoKanton()],
#'   [geoThemakart()] etc., or a unit name passed to [geoThemakart()], e.g.
#'   `"kanton"`.
#' @param id ids of the units to colour, cantons also by abbreviation (e.g.
#'   `"ZH"`); `NULL` for all units.
#' @param main plot title; `NULL`, `""`, `NA` or `FALSE` draw none.
#' @param col fill colours, recycled along `id`.
#' @param border border colours, recycled along `id`.
#' @param lwd border line width.
#' @param bg fill colour of the units not in `id`.
#' @param labels `TRUE` to label the units in `id` with their ids, a
#'   character vector of labels along `id`, or a list holding such a value
#'   (unnamed or as `text`) and further arguments of [text()] (`cex`, `col`,
#'   `font`, `adj`, `pos`, `offset`); `FALSE` draws none.
#' @param vegetation the vegetation area of ThemaKart maps: `TRUE` draws it
#'   with default colours, `FALSE`, `NULL` or `NA` draw none, a list
#'   `list(col = , border = , lwd = )` sets its style.
#' @param add logical; if `TRUE`, the map is added to the current plot.
#' @param stamp source note of ThemaKart maps: `TRUE` draws it, `FALSE`,
#'   `NULL` or `NA` suppress it, a list passes arguments to [stampBfs()].
#'   Ignored with `add = TRUE` and for maps of other sources.
#' @param ... further graphical parameters passed to [plot()] for the
#'   geometry.
#'
#' @return Invisibly, a data.frame with the id and the label coordinates
#'   `x`, `y` of the units in `id`, for use with [text()].
#'
#' @family swiss.map
#' @concept map
#' @concept choropleth
#' @seealso [addGewaesser()], [combineRegions()].
#' @examples
#' \dontrun{
#' plotMap("kanton", id = c("ZH", "BE"), col = c("steelblue", "orange"),
#'         labels = TRUE)
#' addGewaesser()
#'
#' # choropleth of the population by municipality
#' geo <- geoGemeinde(as.Date("2021-01-01"))
#' pop <- aggregate(ch_n ~ gem_id, data = popGemeinde(2021), FUN = sum)
#' br  <- cut(log10(pop$ch_n), 6)
#' plotMap(geo, id = pop$gem_id,
#'         col = hcl.colors(6, "Blues 3", rev = TRUE)[br], border = NA)
#' }
#' @export
plotMap <- function(map, id = NULL,
                    # Labels
                    main = NULL,
                    # Style
                    col = NA, border = "grey", lwd = 1, bg = "white",
                    # Features
                    labels = FALSE, vegetation = FALSE,
                    # Framework
                    add = FALSE, stamp = TRUE, ...) {
  if (is.character(map)) map <- geoThemakart(map)
  .checkMap(map)
  .checkFlag(add)
  lab <- .valueOrList(labels, "labels",
                      c("cex", "col", "font", "adj", "pos", "offset"))
  veg <- .elementSpec(vegetation, "vegetation",
                      list(col = "grey85", border = NA, lwd = lwd))
  stampSpec <- .elementSpec(stamp, "stamp",
                            list(x = NULL, y = NULL, year = NULL,
                                 text = NULL, cex = 0.6, adj = c(1, 0)))

  key <- .mapKey(map)
  ids <- map[[key]]
  id <- .mapIds(map, id %||% ids, key)
  idx <- match(id, ids)
  if (anyNA(idx))
    warning(sprintf("Argument 'id': unit(s) %s not in the map.",
                    paste(head(id[is.na(idx)], 10), collapse = ", ")),
            call. = FALSE)
  ok <- !is.na(idx)

  fill <- rep(if (add) NA else bg, nrow(map))
  bord <- rep(if (add) NA else "grey", nrow(map))
  fill[idx[ok]] <- rep_len(col, length(id))[ok]
  bord[idx[ok]] <- rep_len(border, length(id))[ok]
  plot(sf::st_geometry(map), col = fill, border = bord, lwd = lwd,
       add = add, ...)

  if (!is.null(veg)) {
    vf <- .mapCompanion(map, area = "vegetation")
    if (is.null(vf)) {
      warning("Argument 'vegetation': no vegetation area for this map.",
              call. = FALSE)
    } else {
      j <- match(id[ok], vf[[key]])
      plot(sf::st_geometry(vf)[j[!is.na(j)]], col = veg$col,
           border = veg$border, lwd = veg$lwd, add = TRUE)
    }
  }

  xy <- .labelPoints(map, key, id[ok])
  if (!identical(lab$value, FALSE) && !is.null(lab$value)) {
    txt <- if (isTRUE(lab$value)) id[ok] else rep_len(lab$value, length(id))[ok]
    do.call(text, c(list(x = xy$x, y = xy$y, labels = txt), lab$args))
  }

  if (!add) {
    if (!is.null(stampSpec) && !is.null(attr(map, "themakart"))) {
      stampSpec$year <- stampSpec$year %||% attr(map, "themakart")$setYear
      do.call(stampBfs, stampSpec)
    }
    if (!(is.null(main) || identical(main, FALSE) || identical(main, NA)))
      title(main = main)
  }
  invisible(xy)
}


.labelPoints <- function(map, key, id) {
  pts <- .mapCompanion(map, output = "point")
  if (is.null(pts)) pts <- suppressWarnings(sf::st_point_on_surface(map))
  j <- match(id, pts[[key]])
  co <- sf::st_coordinates(sf::st_geometry(pts))[j, 1:2, drop = FALSE]
  out <- data.frame(id = id, x = co[, 1L], y = co[, 2L])
  names(out)[1L] <- key
  .resetRownames(out)
}


#' Add Lakes and Rivers to a Map
#'
#' Overlays the water bodies of the ThemaKart set on the current plot. Lakes
#' and the larger rivers make a thematic map recognisable as Switzerland at
#' a glance.
#'
#' @param lakes lake size categories to draw, 1 (largest) or 2; `NULL` for
#'   none.
#' @param rivers river size categories to draw, 1 (largest) to 5; `NULL`
#'   for none.
#' @param col fill colour of the lakes. The default `NULL` uses a lighter,
#'   opaque variant of `border`.
#' @param border colour of the rivers and the lake shores.
#' @param lwd line width.
#' @param setYear ThemaKart set; by default the set of the last map read.
#' @param ... further graphical parameters passed to [plot()].
#'
#' @return `NULL`, invisibly.
#'
#' @family swiss.map
#' @concept map
#' @concept hydrography
#' @seealso [geoGewaesser()] for the geometries.
#' @examples
#' \dontrun{
#' plotMap("kanton")
#' addGewaesser(lakes = 1, rivers = 1:2)
#' }
#' @export
addGewaesser <- function(lakes = 1, rivers = 1:5, col = NULL,
                         border = "lightskyblue3", lwd = 1, setYear = NULL,
                         ...) {
  if (is.null(col)) col <- .lighten(border, 0.4)
  if (length(rivers))
    plot(sf::st_geometry(geoGewaesser("fluss", rivers, setYear)),
         col = border, lwd = lwd, add = TRUE, ...)
  if (length(lakes))
    plot(sf::st_geometry(geoGewaesser("see", lakes, setYear)),
         col = col, border = border, lwd = lwd, add = TRUE, ...)
  invisible(NULL)
}


# Opaque mix of a colour with white (amount = share of white).
.lighten <- function(col, amount) {
  m <- col2rgb(col) / 255
  m <- m * (1 - amount) + amount
  rgb(m[1, ], m[2, ], m[3, ])
}


#' Add the ThemaKart Source Note to a Map
#'
#' Writes the source note "Kartengrundlage: (c) BFS, ThemaKart, <year>"
#' into the current plot. The note is mandatory for published maps based on
#' the BFS ThemaKart geometries; [plotMap()] adds it by default.
#'
#' @param x position of the text: `NULL` for the lower right corner of a
#'   map of Switzerland in LV95, `"bottomright"` for the lower right corner
#'   of the plot region, or an x coordinate.
#' @param y y coordinate if `x` is numeric.
#' @param year year of the ThemaKart set. By default the set of the last
#'   map read, else the current year.
#' @param text text to use instead of the default note.
#' @param cex text size.
#' @param adj text adjustment, see [text()].
#' @param ... further arguments passed to [text()].
#'
#' @return `NULL`, invisibly.
#'
#' @family swiss.map
#' @concept map
#' @concept annotation
#' @seealso [plotMap()].
#' @examples
#' \dontrun{
#' plot(sf::st_geometry(geoKanton()))
#' stampBfs()
#' }
#' @export
stampBfs <- function(x = NULL, y = NULL, year = NULL, text = NULL,
                     cex = 0.6, adj = c(1, 0), ...) {
  year <- .asYear(year) %||% .helvetiaEnv$lastSet %||%
    as.integer(format(Sys.Date(), "%Y"))
  if (is.null(text))
    text <- sprintf("Kartengrundlage:\n\u00a9 BFS, ThemaKart, %s", year)
  if (is.null(x)) {
    x <- 2835000
    y <- 1075000
  } else if (identical(x, "bottomright")) {
    usr <- par("usr")
    x <- usr[2L] - cex * strwidth("m")
    y <- usr[3L] + cex * strheight("m")
  } else if (!is.numeric(x) || !is.numeric(y)) {
    stop("Arguments 'x' and 'y' must be numeric coordinates.", call. = FALSE)
  }
  graphics::text(x, y, labels = text, cex = cex, adj = adj, ...)
  invisible(NULL)
}


#' Merge Spatial Units into Larger Regions
#'
#' Unions the geometries of a map by a grouping, e.g. municipalities to
#' premium regions or cantons to language areas. Use it to map any regional
#' division that is defined as a set of municipalities or cantons.
#'
#' @param map an `sf` object, e.g. from [geoGemeinde()] or [geoKanton()].
#' @param groups grouping, along the rows of `map` or, if `id` is given,
#'   along `id`; units with `NA` are left out.
#' @param id optional ids of the units `groups` refers to, cantons also by
#'   abbreviation.
#'
#' @return An `sf` object with one row per group: the column `groups`
#'   (sorted) and the unioned geometry.
#'
#' @note Based on code by Juerg Guggenbuehl, adapted to conform to package
#'   standards.
#'
#' @family swiss.spatial
#' @concept spatial-data
#' @concept geometry
#' @seealso [praemienregion()], [raumgliederung()].
#' @examples
#' \dontrun{
#' preg <- praemienregion(2023)
#' geo  <- geoGemeinde(as.Date("2023-01-01"))
#' reg  <- combineRegions(geo, preg$preg_x, id = preg$gem_id)
#' plotMap(reg)
#' }
#' @export
combineRegions <- function(map, groups, id = NULL) {
  .checkMap(map)
  key <- .mapKey(map)
  if (!is.null(id)) {
    id <- .mapIds(map, id, key)
    if (length(id) != length(groups))
      stop("Arguments 'id' and 'groups' must have the same length.",
           call. = FALSE)
    groups <- groups[match(map[[key]], id)]
  }
  if (length(groups) != nrow(map))
    stop(paste("Argument 'groups' must have one element per row of 'map'",
               "(or use 'id')."), call. = FALSE)
  keep <- !is.na(groups)
  lev <- sort(unique(groups[keep]))
  geom <- sf::st_geometry(map)[keep]
  parts <- split(seq_along(geom), factor(groups[keep], levels = lev))
  u <- do.call(c, lapply(parts, function(i) sf::st_union(geom[i])))
  sf::st_sf(groups = lev, geometry = sf::st_set_crs(u, sf::st_crs(map)))
}


#' Find Neighbouring Spatial Units
#'
#' Finds the units sharing a boundary or a point with the given units, e.g.
#' to define comparison regions or to check spatial autocorrelation.
#'
#' @param map an `sf` object, e.g. from [geoGemeinde()].
#' @param id ids of the units whose neighbours are wanted, cantons also by
#'   abbreviation; `NULL` for all units.
#'
#' @return A data.frame with one row per pair: the id column of the map
#'   (e.g. `gem_id`) and the neighbour's id (e.g. `nb_gem_id`). A unit is
#'   not its own neighbour. For the outer neighbours of a group of units,
#'   drop the rows whose neighbour is in the group.
#'
#' @family swiss.spatial
#' @concept spatial-data
#' @concept geometry
#' @examples
#' \dontrun{
#' neighbours(geoKanton(), "ZH")
#' }
#' @export
neighbours <- function(map, id = NULL) {
  .checkMap(map)
  key <- .mapKey(map)
  ids <- map[[key]]
  sel <- if (is.null(id)) seq_along(ids) else match(.mapIds(map, id, key), ids)
  if (anyNA(sel))
    stop(sprintf("Argument 'id': unit(s) %s not in the map.",
                 paste(id[is.na(sel)], collapse = ", ")), call. = FALSE)
  hit <- sf::st_intersects(sf::st_geometry(map)[sel], sf::st_geometry(map))
  out <- data.frame(a = rep(ids[sel], lengths(hit)), b = ids[unlist(hit)])
  out <- out[out$a != out$b, , drop = FALSE]
  names(out) <- c(key, paste0("nb_", key))
  .resetRownames(out)
}


#' Find the Spatial Units Containing Points
#'
#' Determines the spatial unit (municipality, district, ...) containing
#' given points, or points clicked in the current map. Use it to assign
#' geocoded addresses to municipalities, or to identify a unit on a map.
#'
#' @param x,y coordinates in the CRS of `map`, in any form accepted by
#'   [xy.coords()]. With `x = NULL` the points are clicked in the current
#'   plot with [locator()]; finish with Esc or a right click.
#' @param map an `sf` object; `NULL` uses [geoGemeinde()].
#' @param nPoints maximum number of points to click.
#'
#' @return A data.frame with `x`, `y` and the attributes of the unit
#'   containing each point (`NA` outside all units).
#'
#' @family swiss.spatial
#' @concept spatial-data
#' @concept geocoding
#' @seealso [geocode()].
#' @examples
#' \dontrun{
#' locate(2683000, 1248000)
#' plotMap("kanton")
#' locate()
#' }
#' @export
locate <- function(x = NULL, y = NULL, map = NULL, nPoints = 512) {
  if (is.null(map)) map <- geoGemeinde()
  .checkMap(map)
  if (is.null(x)) {
    if (!interactive())
      stop("Clicking points needs an interactive session; give 'x' and 'y'.",
           call. = FALSE)
    x <- locator(nPoints)
    if (is.null(x)) return(NULL)
  }
  xy <- xy.coords(x, y)
  pts <- data.frame(x = xy$x, y = xy$y)
  geom <- sf::st_as_sf(pts, coords = c("x", "y"), crs = sf::st_crs(map))
  hit <- sf::st_intersects(geom, map)
  i <- vapply(hit, function(h) if (length(h)) h[1L] else NA_integer_, 0L)
  .resetRownames(cbind(pts, sf::st_drop_geometry(map)[i, , drop = FALSE]))
}
