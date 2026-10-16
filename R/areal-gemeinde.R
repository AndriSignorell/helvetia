#' Get the Land Use Statistics by Municipality
#'
#' Returns the area in hectares by municipality and NOAS04 class from the
#' Swiss land use statistics (Arealstatistik). Use it for settlement,
#' agricultural or forest shares per municipality, and for their change
#' between survey periods.
#'
#' The data come from the dataflow `CH1.AREA/DF_AREA_NOAS` of the BFS SDMX
#' dissemination service (stats.swiss). Three time concepts are kept apart:
#' `period_x` is the survey period (e.g. `"2013/18"`), not a publication or
#' boundary year; `gem_stand_d` is the municipality boundary stand the
#' results are reported on; the year of the aerial photographs a value is
#' based on is kept in `flight_year_x` where the source delivers it.
#'
#' Missing, zero and suppressed values are distinguished in
#' `value_status_c`: `"observed"` (including true zeros), `"suppressed"`
#' (confidentiality), `"not-applicable"` (the value cannot exist) and
#' `"missing"` (not delivered, including cells absent from the response).
#' The raw SDMX observation status is kept in `obs_status_c`.
#'
#' With `checkSums = TRUE` the sum over the classes of the requested level
#' is compared, per municipality, with the sum over the four main domains;
#' differences above `threshold` hectares raise a warning.
#'
#' @param period survey period, e.g. `"2013/18"`. The default `NULL` uses
#'   the most recent period; note that a period still in progress covers only
#'   part of Switzerland.
#' @param nClasses level of the nomenclature, given by its number of
#'   classes: `4`, `17`, `27` or `72`.
#' @param checkSums logical; if `TRUE`, the class sums are checked (see
#'   Details).
#' @param threshold difference in hectares above which `checkSums` warns.
#' @inheritParams gemeinde
#'
#' @return A data.frame in long format with the columns `period_x`,
#'   `gem_stand_d`, `gem_id`, `noas_c`, `noas_n`, `noas_x`, `area_ha`,
#'   `value_status_c`, `obs_status_c` and `flight_year_x`. The key is
#'   `period_x` + `gem_stand_d` + `gem_id` + `noas_c`.
#'
#' @family swiss.statistics
#' @concept land-use
#' @concept municipality
#' @concept missing-value
#' @seealso [noas()] for the nomenclature.
#' @examples
#' \dontrun{
#' area <- arealGemeinde(period = "2013/18", nClasses = 17)
#' head(area)
#' }
#' @export
arealGemeinde <- function(period = NULL, nClasses = 17, checkSums = TRUE,
                          threshold = 0.5, cache = TRUE, refresh = FALSE) {
  nClasses <- .checkNClasses(nClasses)
  if (length(nClasses) != 1L)
    stop("Argument 'nClasses' must be a single level.", call. = FALSE)
  .checkFlag(checkSums)
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) ||
      threshold < 0)
    stop("Argument 'threshold' must be a single non-negative number.",
         call. = FALSE)

  st <- .cached("sdmx", "DF_AREA_NOAS", "structure", cache = cache,
                refresh = refresh, mode = "remote-first",
                fetch = .areaStructure)
  per <- .areaPeriod(st$periods, period)
  nom <- noas(c(4L, nClasses), cache = cache, refresh = refresh)

  out <- .cached("area", per$x, c("level", nClasses), cache = cache,
                 refresh = refresh, fetch = function() {
    key <- list(NOAS = nom$noas_c, PERIOD = per$code)
    raw <- .sdmxData(.areaFlow, key, st$dims$id)
    res <- .parseArea(raw, nom, per$x, st$gemStand)
    .attachSourceMetadata(res, .sourceMeta(
      "BFS Arealstatistik, SDMX dataflow CH1.AREA/DF_AREA_NOAS 1.1.0",
      attr(raw, "url"), version = per$x,
      notes = sprintf("region codelist %s", st$regionCodelist)))
  })

  dom <- out[out$noas_c %in% nom$noas_c[nom$level_n == 4L], , drop = FALSE]
  res <- out[out$noas_c %in% nom$noas_c[nom$level_n == nClasses], ,
             drop = FALSE]
  if (checkSums && nClasses != 4L) .checkAreaSums(res, dom, threshold)

  .validateUniqueKey(res, c("period_x", "gem_stand_d", "gem_id", "noas_c"),
                     "arealGemeinde()")
  .attachSourceMetadata(.resetRownames(res), sourceInfo(out))
}


.areaFlow <- "CH1.AREA,DF_AREA_NOAS,1.1.0"


.areaStructure <- function() {
  dims <- .sdmxDimensions("CH1.AREA", "DSD_AREA_NOAS", "1.1.0")
  need <- c("NOAS", "PERIOD", "REGION")
  .requireCols(setNames(as.list(dims$id), dims$id), need,
               "Arealstatistik data structure")
  perCl <- dims$codelist[dims$id == "PERIOD"]
  regCl <- dims$codelist[dims$id == "REGION"]
  periods <- .sdmxCodelist(perCl)
  list(dims = dims, periods = periods, regionCodelist = regCl,
       gemStand = .standFromCodelist(regCl))
}


# The region codelist is versioned by municipality stand, e.g.
# "CL_AREA_HIER_REGION_2026_DIFF" -> 2026-01-01.
.standFromCodelist <- function(id) {
  y <- regmatches(id, regexpr("(19|20)[0-9]{2}", id))
  if (!length(y)) {
    warning(sprintf(paste("The municipality stand could not be derived from",
                          "the region codelist '%s'; gem_stand_d is NA."), id),
            call. = FALSE)
    return(as.Date(NA))
  }
  as.Date(paste0(y, "-01-01"))
}


# Map "2013/18" <-> SDMX period code (labels like "2013-2018").
.areaPeriod <- function(periods, period) {
  lab <- periods$name_de
  y1 <- substr(lab, 1L, 4L)
  y2 <- substr(lab, nchar(lab) - 1L, nchar(lab))
  px <- paste0(y1, "/", y2)
  if (is.null(period)) {
    i <- which.max(as.integer(y1))
  } else {
    if (!is.character(period) || length(period) != 1L)
      stop("Argument 'period' must be a single survey period, e.g. \"2013/18\".",
           call. = FALSE)
    i <- match(period, px)
    if (is.na(i))
      stop(sprintf("Argument 'period': '%s' not available; available are %s.",
                   period, paste(px, collapse = ", ")), call. = FALSE)
  }
  list(code = periods$code[i], x = px[i])
}


# Region codes: 8100 (CH), CH01-CH07 (Grossregionen), canton abbreviations,
# B_* (districts). Everything else ending in digits is a municipality.
.areaGemId <- function(region) {
  higher <- grepl("^(8100|CH0[0-9]|[A-Z]{2}|B_[0-9]+)$", region)
  num <- sub("^.*?([0-9]+)$", "\\1", region)
  ifelse(!higher & grepl("[0-9]+$", region), .asInt(num), NA_integer_)
}


.parseArea <- function(raw, nom, period, gemStand) {
  .requireCols(raw, c("NOAS", "PERIOD", "REGION", "OBS_VALUE"),
               "Arealstatistik SDMX response")
  raw$gem_id <- .areaGemId(raw$REGION)
  raw <- raw[!is.na(raw$gem_id), , drop = FALSE]
  if (!nrow(raw))
    stop(paste("The Arealstatistik response contains no municipality-level",
               "regions; the region coding may have changed."), call. = FALSE)

  # complete grid: one row per municipality and requested class
  grid <- expand.grid(noas_c = nom$noas_c, gem_id = sort(unique(raw$gem_id)),
                      stringsAsFactors = FALSE)
  m <- match(paste(grid$gem_id, grid$noas_c), paste(raw$gem_id, raw$NOAS))
  col <- function(n) if (is.null(raw[[n]])) rep(NA_character_, length(m))
                     else raw[[n]][m]
  val <- suppressWarnings(as.numeric(col("OBS_VALUE")))
  status <- col("OBS_STATUS")
  status[!is.na(status) & status == ""] <- NA_character_

  vstat <- ifelse(!is.na(val), "observed",
           ifelse(!is.na(status) & status == "Q", "suppressed",
           ifelse(!is.na(status) & status == "M", "not-applicable",
                  "missing")))
  fj <- col("FJ")
  fj[!is.na(fj) & fj == ""] <- NA_character_

  out <- data.frame(
    period_x       = period,
    gem_stand_d    = rep(gemStand, nrow(grid)),
    gem_id         = grid$gem_id,
    noas_c         = grid$noas_c,
    noas_n         = nom$noas_n[match(grid$noas_c, nom$noas_c)],
    noas_x         = nom$noas_x_de[match(grid$noas_c, nom$noas_c)],
    area_ha        = val,
    value_status_c = vstat,
    obs_status_c   = status,
    flight_year_x  = fj,
    stringsAsFactors = FALSE)
  out[order(out$gem_id, out$noas_c), , drop = FALSE]
}


.checkAreaSums <- function(res, dom, threshold) {
  sumLevel <- tapply(res$area_ha, res$gem_id, sum, na.rm = TRUE)
  sumDomain <- tapply(dom$area_ha, dom$gem_id, sum, na.rm = TRUE)
  ids <- intersect(names(sumLevel), names(sumDomain))
  d <- abs(sumLevel[ids] - sumDomain[ids])
  bad <- ids[d > threshold]
  if (length(bad))
    warning(sprintf(paste("Class sums differ from the main-domain total by",
                          "more than %g ha in %d municipalities, e.g. %s."),
                    threshold, length(bad),
                    paste(head(bad, 10), collapse = ", ")),
            call. = FALSE)
  invisible(bad)
}
