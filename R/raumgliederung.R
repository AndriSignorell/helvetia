#' Get the Spatial Classifications of Municipalities
#'
#' Returns, for each municipality valid at a date, its code in the official
#' spatial classifications (Raumgliederungen) of the BFS: language regions
#' (`SPRGEB2020`), municipality typology (`GDETYP2020_9`, `GDETYP2020_25`),
#' agglomerations (`AGGL2020`), the urban-rural typology (`STALAN2020`) and
#' others. Use it to aggregate municipal data to regions or to stratify an
#' analysis by settlement type.
#'
#' The data come from the AGVCH levels service. The classifications
#' available depend on the date; they are selected by name, never by column
#' position.
#'
#' @note Experimental. The levels service documents its output columns only
#'   partially (they "vary with language, date and mapping configuration");
#'   the result therefore keeps every classification column as delivered.
#'
#' @param typology optional character vector of classification codes or
#'   code prefixes, matched case-insensitively, e.g. `"GDETYP2020"`. The
#'   default `NULL` returns all classifications.
#' @param lang language of the labels, one of `"de"`, `"fr"`, `"it"`,
#'   `"en"`.
#' @inheritParams gemeinde
#'
#' @return A data.frame in long format with the columns `stand_d`,
#'   `gem_id`, `gem_x`, `typ_c` (classification) and `value_x` (code or
#'   label as delivered). The key is `stand_d` + `gem_id` + `typ_c`.
#'
#' @family swiss.register
#' @concept municipality
#' @concept regional-classification
#' @seealso [gemeinde()], [combineRegions()] to draw the regions.
#' @examples
#' \dontrun{
#' rg <- raumgliederung(as.Date("2021-01-01"), typology = "GDETYP2020_9")
#' }
#' @export
raumgliederung <- function(date = NULL, typology = NULL,
                           lang = c("de", "fr", "it", "en"),
                           cache = TRUE, refresh = FALSE) {
  lang <- match.arg(lang)
  date <- .asDate(date)
  wide <- .cached("agvch", "levels", c(format(date), lang),
                  cache = cache, refresh = refresh, fetch = function() {
    raw <- .agvchRequest("levels", labelLanguages = lang,
                         date = .agvchDate(date))
    .attachSourceMetadata(raw, .agvchMeta(attr(raw, "url"), format(date)))
  })
  .parseAgvchLevels(wide, date, typology)
}


.agvchLevelBase <- c("HistoricalCode", "BfsCode", "Name", "CantonId",
                     "Canton", "DistrictId", "District")

.parseAgvchLevels <- function(raw, date, typology = NULL) {
  .requireCols(raw, c("BfsCode", "Name"), "AGVCH levels")
  cols <- setdiff(names(raw), .agvchLevelBase)
  if (!is.null(typology)) {
    keep <- Reduce(`|`, lapply(typology, function(t)
      startsWith(toupper(cols), toupper(t))), FALSE)
    if (!any(keep))
      stop(sprintf(paste("Argument 'typology': %s not available at %s.",
                         "Available: %s."),
                   paste(sQuote(typology, FALSE), collapse = ", "), date,
                   paste(cols, collapse = ", ")), call. = FALSE)
    cols <- cols[keep]
  }
  n <- nrow(raw)
  out <- data.frame(
    stand_d = rep(date, n * length(cols)),
    gem_id  = rep(.asInt(raw$BfsCode), times = length(cols)),
    gem_x   = rep(raw$Name, times = length(cols)),
    typ_c   = rep(cols, each = n),
    value_x = unlist(raw[cols], use.names = FALSE),
    stringsAsFactors = FALSE)
  out$value_x[out$value_x == ""] <- NA_character_
  .validateUniqueKey(out, c("stand_d", "gem_id", "typ_c"),
                     "raumgliederung()")
  .attachSourceMetadata(.resetRownames(out), sourceInfo(raw))
}
