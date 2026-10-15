#' Assignment of municipalities to official spatial classifications
#'
#' Returns, for each municipality valid at `stand_d`, its code in the
#' official spatial classifications (Raumgliederungen) of the BFS, such as
#' language regions (`SPRGEB2020`), municipality typology (`GDETYP2020_9`,
#' `GDETYP2020_25`), agglomerations (`AGGL2020`) or the urban-rural
#' typology (`STALAN2020`), from the AGVCH levels service.
#'
#' The set of available classifications depends on the reference date;
#' classifications are selected by name, never by column position.
#'
#' @note Experimental. The levels service documents its output columns only
#'   partially (they "vary with language, date and mapping configuration");
#'   the result therefore keeps every classification column as delivered.
#'   Planned for a later version according to the specification.
#'
#' @param typ optional character vector of classification codes or code
#'   prefixes (case-insensitive), e.g. `"GDETYP2020"`; `NULL` returns all
#' @param lang language of the labels, one of `"de"`, `"fr"`, `"it"`, `"en"`
#' @inheritParams gemeinde
#'
#' @return A data.frame in long format with the columns `stand_d`,
#'   `gem_id`, `gem_x`, `typ_c` (classification column) and `value_x`
#'   (code or label as delivered).
#'
#' @seealso [gemeinde()]
#' @examples
#' \dontrun{
#' rg <- raumgliederung(as.Date("2021-01-01"), typ = "GDETYP2020_9")
#' }
#' @export
raumgliederung <- function(stand_d = NULL, typ = NULL,
                           lang = c("de", "fr", "it", "en"),
                           cache = TRUE, refresh = FALSE) {
  stand_d <- .as_stand_d(stand_d)
  lang <- match.arg(lang)
  wide <- .cached("agvch", "levels", c(format(stand_d), lang),
                  cache = cache, refresh = refresh, fetch = function() {
    raw <- .agvch_request("levels", labelLanguages = lang,
                          date = .agvch_date(stand_d))
    .attach_source_metadata(raw, .agvch_meta(attr(raw, "url"),
                                             format(stand_d)))
  })
  .parse_agvch_levels(wide, stand_d, typ)
}


.agvch_level_base <- c("HistoricalCode", "BfsCode", "Name", "CantonId",
                       "Canton", "DistrictId", "District")

.parse_agvch_levels <- function(raw, stand_d, typ = NULL) {
  .require_cols(raw, c("BfsCode", "Name"), "AGVCH levels")
  cols <- setdiff(names(raw), .agvch_level_base)
  if (!is.null(typ)) {
    keep <- Reduce(`|`, lapply(typ, function(t)
      startsWith(toupper(cols), toupper(t))), FALSE)
    if (!any(keep))
      stop(sprintf("classification(s) %s not available at %s.\nAvailable: %s",
                   paste(sQuote(typ, FALSE), collapse = ", "), stand_d,
                   paste(cols, collapse = ", ")), call. = FALSE)
    cols <- cols[keep]
  }
  n <- nrow(raw)
  out <- data.frame(
    stand_d = rep(stand_d, n * length(cols)),
    gem_id  = rep(.as_int(raw$BfsCode), times = length(cols)),
    gem_x   = rep(raw$Name, times = length(cols)),
    typ_c   = rep(cols, each = n),
    value_x = unlist(raw[cols], use.names = FALSE),
    stringsAsFactors = FALSE)
  out$value_x[out$value_x == ""] <- NA_character_
  .validate_unique_key(out, c("stand_d", "gem_id", "typ_c"),
                       "raumgliederung()")
  out <- .attach_source_metadata(.reset_rownames(out), source_info(raw))
  out
}
