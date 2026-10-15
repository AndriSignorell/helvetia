#' Land use statistics by municipality
#'
#' Area in hectares by municipality and NOAS04 class from the Swiss land use
#' statistics (Arealstatistik), dataflow `CH1.AREA/DF_AREA_NOAS` of the BFS
#' SDMX dissemination service (stats.swiss).
#'
#' Three time concepts are kept apart: `period_x` is the survey period
#' (e.g. `"2013/18"`), not a publication or boundary year; `gem_stand_d` is
#' the municipality boundary stand the results are reported on; the year
#' of the aerial photographs a value is based on is kept in
#' `flight_year_x` where the source delivers it.
#'
#' Missing, zero and suppressed values are distinguished in
#' `value_status_c`: `"observed"` (including true zeros), `"suppressed"`
#' (confidentiality), `"not-applicable"` (value cannot exist) and
#' `"missing"` (not delivered, including cells absent from the response).
#' The raw SDMX observation status is kept in `obs_status_c`.
#'
#' With `check = TRUE` the sum over the classes of the requested level is
#' compared, per municipality, with the sum over the four main domains;
#' differences above `tol` hectares raise a warning.
#'
#' @param period_x survey period, e.g. `"2013/18"`; `NULL` uses the most
#'   recent period. Note that a period still in progress covers only part of
#'   Switzerland.
#' @param level_n nomenclature level: `4`, `17` (default), `27` or `72`
#' @param check logical; check the class sums (see Details)
#' @param tol tolerance in hectares for `check`
#' @inheritParams gemeinde
#'
#' @return A data.frame in long format with the columns `period_x`,
#'   `gem_stand_d`, `gem_id`, `noas_c`, `noas_n`, `noas_x`, `area_ha`,
#'   `value_status_c`, `obs_status_c` and `flight_year_x`. The key is
#'   `period_x` + `gem_stand_d` + `gem_id` + `noas_c`.
#'
#' @seealso [noas()], [gemeinde()]
#' @examples
#' \dontrun{
#' area <- areal_gemeinde(period_x = "2013/18", level_n = 17)
#' head(area)
#' }
#' @export
areal_gemeinde <- function(period_x = NULL, level_n = 17, check = TRUE,
                           tol = 0.5, cache = TRUE, refresh = FALSE) {
  level_n <- .check_noas_level(level_n)
  if (length(level_n) != 1L)
    stop("'level_n' must be a single level", call. = FALSE)
  .checkFlag(check)

  st <- .cached("sdmx", "DF_AREA_NOAS", "structure", cache = cache,
                refresh = refresh, mode = "remote-first",
                fetch = .area_structure)
  per <- .area_period(st$periods, period_x)
  nom <- noas(c(4L, level_n), cache = cache, refresh = refresh)

  out <- .cached("area", per$x, c("level", level_n), cache = cache,
                 refresh = refresh, fetch = function() {
    key <- list(NOAS = nom$noas_c, PERIOD = per$code)
    raw <- .sdmx_data(.area_flow, key, st$dims$id)
    res <- .parse_area(raw, nom, per$x, st$gem_stand_d)
    .attach_source_metadata(res, .source_meta(
      "BFS Arealstatistik, SDMX dataflow CH1.AREA/DF_AREA_NOAS 1.1.0",
      attr(raw, "url"), version = per$x,
      notes = sprintf("region codelist %s", st$region_codelist)))
  })

  dom <- out[out$noas_c %in% nom$noas_c[nom$level_n == 4L], , drop = FALSE]
  res <- out[out$noas_c %in% nom$noas_c[nom$level_n == level_n], ,
             drop = FALSE]
  if (check && level_n != 4L) .check_area_sums(res, dom, tol)

  .validate_unique_key(res, c("period_x", "gem_stand_d", "gem_id", "noas_c"),
                       "areal_gemeinde()")
  .attach_source_metadata(.reset_rownames(res), source_info(out))
}


.area_flow <- "CH1.AREA,DF_AREA_NOAS,1.1.0"


.area_structure <- function() {
  dims <- .sdmx_dimensions("CH1.AREA", "DSD_AREA_NOAS", "1.1.0")
  need <- c("NOAS", "PERIOD", "REGION")
  .require_cols(stats::setNames(as.list(dims$id), dims$id), need,
                "Arealstatistik data structure")
  per_cl <- dims$codelist[dims$id == "PERIOD"]
  reg_cl <- dims$codelist[dims$id == "REGION"]
  periods <- .sdmx_codelist(per_cl)
  list(dims = dims, periods = periods, region_codelist = reg_cl,
       gem_stand_d = .stand_from_codelist(reg_cl))
}


# The region codelist is versioned by municipality stand, e.g.
# "CL_AREA_HIER_REGION_2026_DIFF" -> 2026-01-01.
.stand_from_codelist <- function(id) {
  y <- regmatches(id, regexpr("(19|20)[0-9]{2}", id))
  if (!length(y)) {
    warning(sprintf(paste("municipality stand could not be derived from",
                          "region codelist '%s'; gem_stand_d is NA"), id),
            call. = FALSE)
    return(as.Date(NA))
  }
  as.Date(paste0(y, "-01-01"))
}


# Map "2013/18" <-> SDMX period code (labels like "2013-2018").
.area_period <- function(periods, period_x) {
  lab <- periods$name_de
  y1 <- substr(lab, 1L, 4L)
  y2 <- substr(lab, nchar(lab) - 1L, nchar(lab))
  px <- paste0(y1, "/", y2)
  if (is.null(period_x)) {
    i <- which.max(as.integer(y1))
  } else {
    if (length(period_x) != 1L)
      stop("'period_x' must be a single survey period", call. = FALSE)
    i <- match(period_x, px)
    if (is.na(i))
      stop(sprintf("survey period '%s' not available; available: %s",
                   period_x, paste(px, collapse = ", ")), call. = FALSE)
  }
  list(code = periods$code[i], x = px[i])
}


# Region codes: 8100 (CH), CH01-CH07 (Grossregionen), canton abbreviations,
# B_* (districts). Everything else ending in digits is a municipality.
.area_gem_id <- function(region) {
  higher <- grepl("^(8100|CH0[0-9]|[A-Z]{2}|B_[0-9]+)$", region)
  num <- sub("^.*?([0-9]+)$", "\\1", region)
  ifelse(!higher & grepl("[0-9]+$", region), .as_int(num), NA_integer_)
}


.parse_area <- function(raw, nom, period_x, gem_stand_d) {
  .require_cols(raw, c("NOAS", "PERIOD", "REGION", "OBS_VALUE"),
                "Arealstatistik SDMX response")
  raw$gem_id <- .area_gem_id(raw$REGION)
  raw <- raw[!is.na(raw$gem_id), , drop = FALSE]
  if (!nrow(raw))
    stop(paste("Arealstatistik response contains no municipality-level",
               "regions; the region coding may have changed"), call. = FALSE)

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
    period_x       = period_x,
    gem_stand_d    = rep(gem_stand_d, nrow(grid)),
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


.check_area_sums <- function(res, dom, tol) {
  s_lvl <- tapply(res$area_ha, res$gem_id, sum, na.rm = TRUE)
  s_dom <- tapply(dom$area_ha, dom$gem_id, sum, na.rm = TRUE)
  ids <- intersect(names(s_lvl), names(s_dom))
  d <- abs(s_lvl[ids] - s_dom[ids])
  bad <- ids[d > tol]
  if (length(bad))
    warning(sprintf(paste("class sums differ from the main-domain total by",
                          "more than %g ha in %d municipalities, e.g. %s"),
                    tol, length(bad),
                    paste(utils::head(bad, 10), collapse = ", ")),
            call. = FALSE)
  invisible(bad)
}
