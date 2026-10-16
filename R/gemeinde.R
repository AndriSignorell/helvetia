#' Get the Municipalities Valid at a Reference Date
#'
#' Returns the stock of Swiss political municipalities valid at a given date,
#' together with their district and canton, from the historicised
#' municipality register of the Swiss Federal Statistical Office (AGVCH).
#' This is the reference against which statistics and geometries of the
#' same stand are joined, and the starting point for every analysis that
#' crosses a municipality merger.
#'
#' The hierarchy canton -> district -> municipality is derived from the
#' `Parent` references of the AGVCH snapshot service, not from positional or
#' level codes. Each municipality occurs at most once per date; this is
#' checked and a violation is an error.
#'
#' @param date reference date, a `Date` or a string `"YYYY-MM-DD"`. The
#'   default `NULL` uses today.
#' @param cache logical; if `TRUE`, results are read from and written to the
#'   local cache (see [cacheList()]).
#' @param refresh logical; if `TRUE`, the source is queried again even if a
#'   cached copy exists.
#'
#' @return A data.frame with one row per municipality and the columns
#'   \describe{
#'     \item{`stand_d`}{date of the register stand.}
#'     \item{`gem_id`}{BFS municipality number.}
#'     \item{`gem_x`}{municipality name.}
#'     \item{`kanton_c`}{canton abbreviation, e.g. `"ZH"`.}
#'     \item{`kanton_id`}{BFS canton number, 1 to 26.}
#'     \item{`bezirk_id`}{BFS number of the district or equivalent unit.}
#'     \item{`bezirk_x`}{district name.}
#'     \item{`hist_id`}{AGVCH historical number of the municipality version.}
#'     \item{`valid_from_d`, `valid_to_d`}{validity of that version.}
#'   }
#'   The key is `stand_d` + `gem_id`; source metadata via [sourceInfo()].
#'
#' @family swiss.register
#' @concept municipality
#' @concept historicisation
#' @seealso [gemeindeMutationen()] for the changes between two stands.
#' @examples
#' \dontrun{
#' gem <- gemeinde(as.Date("2021-01-01"))
#' table(gem$kanton_c)
#' }
#' @export
gemeinde <- function(date = NULL, cache = TRUE, refresh = FALSE) {
  date <- .asDate(date)
  .cached("agvch", "snapshot", format(date), cache = cache,
          refresh = refresh, fetch = function() {
    raw <- .agvchRequest("snapshot", date = .agvchDate(date))
    out <- .parseAgvchSnapshot(raw, date)
    .attachSourceMetadata(out, .agvchMeta(attr(raw, "url"), format(date)))
  })
}


.parseAgvchSnapshot <- function(raw, date) {
  .requireCols(raw, c("HistoricalCode", "BfsCode", "ValidFrom", "ValidTo",
                      "Parent", "Name"), "AGVCH snapshot")
  parent <- raw$Parent
  parent[parent == ""] <- NA_character_

  isKt  <- is.na(parent)
  isBz  <- !isKt & parent %in% raw$HistoricalCode[isKt]
  isGem <- !isKt & !isBz & parent %in% raw$HistoricalCode[isBz]

  if (!any(isGem))
    stop(paste("AGVCH snapshot: no municipalities found below district",
               "level; the hierarchy (Parent) format may have changed."),
         call. = FALSE)
  if (any(!(isKt | isBz | isGem)))
    warning(sprintf(paste("AGVCH snapshot: %d row(s) outside the",
                          "canton/district/municipality hierarchy ignored."),
                    sum(!(isKt | isBz | isGem))), call. = FALSE)

  kt <- raw[isKt, , drop = FALSE]
  bz <- raw[isBz, , drop = FALSE]
  gm <- raw[isGem, , drop = FALSE]

  b <- match(gm$Parent, bz$HistoricalCode)
  k <- match(bz$Parent[b], kt$HistoricalCode)
  kantonId <- .asInt(kt$BfsCode[k])

  out <- data.frame(
    stand_d      = rep(date, nrow(gm)),
    gem_id       = .asInt(gm$BfsCode),
    gem_x        = gm$Name,
    kanton_c     = .kantonAbbrev[kantonId],
    kanton_id    = kantonId,
    bezirk_id    = .asInt(bz$BfsCode[b]),
    bezirk_x     = bz$Name[b],
    hist_id      = .asInt(gm$HistoricalCode),
    valid_from_d = .parseDateAny(gm$ValidFrom),
    valid_to_d   = .parseDateAny(gm$ValidTo),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$gem_id), , drop = FALSE]
  .validateUniqueKey(out, c("stand_d", "gem_id"), "gemeinde()")
  .resetRownames(out)
}
