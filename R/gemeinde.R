#' Municipalities valid at a reference date
#'
#' Returns the stock of Swiss political municipalities (Gemeinden) valid at
#' a given reference date, together with their district and canton, from
#' the historicised municipality register of the Swiss Federal Statistical
#' Office (AGVCH, snapshot service).
#'
#' The hierarchy is derived from the `Parent` references of the snapshot
#' (canton -> district -> municipality), not from positional or level
#' codes. Each municipality occurs at most once per reference date; this is
#' checked.
#'
#' @param stand_d reference date (`Date` or `"YYYY-MM-DD"`); the default
#'   `NULL` uses today
#' @param cache logical; read from and write to the local cache (see
#'   [cache_list()])
#' @param refresh logical; force a new download even if a cached copy exists
#'
#' @return A data.frame with one row per municipality and the columns
#'   \describe{
#'     \item{`stand_d`}{reference date of the register stand}
#'     \item{`gem_id`}{BFS municipality number}
#'     \item{`gem_x`}{municipality name}
#'     \item{`kanton_c`}{canton abbreviation, e.g. `"ZH"`}
#'     \item{`kanton_id`}{BFS canton number (1--26)}
#'     \item{`bezirk_id`}{BFS number of the district or equivalent unit}
#'     \item{`bezirk_x`}{district name}
#'     \item{`hist_id`}{AGVCH historical number of the municipality version}
#'     \item{`valid_from_d`, `valid_to_d`}{validity of that version}
#'   }
#'   Source metadata are available via [source_info()].
#'
#' @seealso [gemeinde_mutationen()], [raumgliederung()]
#' @examples
#' \dontrun{
#' gem <- gemeinde(as.Date("2021-01-01"))
#' table(gem$kanton_c)
#' }
#' @export
gemeinde <- function(stand_d = NULL, cache = TRUE, refresh = FALSE) {
  stand_d <- .as_stand_d(stand_d)
  .cached("agvch", "snapshot", format(stand_d), cache = cache,
          refresh = refresh, fetch = function() {
    raw <- .agvch_request("snapshot", date = .agvch_date(stand_d))
    out <- .parse_agvch_snapshot(raw, stand_d)
    .attach_source_metadata(out, .agvch_meta(attr(raw, "url"),
                                             format(stand_d)))
  })
}


.parse_agvch_snapshot <- function(raw, stand_d) {
  .require_cols(raw, c("HistoricalCode", "BfsCode", "ValidFrom", "ValidTo",
                       "Parent", "Name"), "AGVCH snapshot")
  parent <- raw$Parent
  parent[parent == ""] <- NA_character_

  is_kt  <- is.na(parent)
  is_bz  <- !is_kt & parent %in% raw$HistoricalCode[is_kt]
  is_gem <- !is_kt & !is_bz & parent %in% raw$HistoricalCode[is_bz]

  if (!any(is_gem))
    stop(paste("AGVCH snapshot: no municipalities found below district",
               "level; the hierarchy (Parent) format may have changed"),
         call. = FALSE)
  if (any(!(is_kt | is_bz | is_gem)))
    warning(sprintf(paste("AGVCH snapshot: %d row(s) outside the",
                          "canton/district/municipality hierarchy ignored"),
                    sum(!(is_kt | is_bz | is_gem))), call. = FALSE)

  kt <- raw[is_kt, , drop = FALSE]
  bz <- raw[is_bz, , drop = FALSE]
  gm <- raw[is_gem, , drop = FALSE]

  b <- match(gm$Parent, bz$HistoricalCode)
  k <- match(bz$Parent[b], kt$HistoricalCode)
  kanton_id <- .as_int(kt$BfsCode[k])

  out <- data.frame(
    stand_d      = rep(stand_d, nrow(gm)),
    gem_id       = .as_int(gm$BfsCode),
    gem_x        = gm$Name,
    kanton_c     = .kanton_abbrev[kanton_id],
    kanton_id    = kanton_id,
    bezirk_id    = .as_int(bz$BfsCode[b]),
    bezirk_x     = bz$Name[b],
    hist_id      = .as_int(gm$HistoricalCode),
    valid_from_d = .parse_date_any(gm$ValidFrom),
    valid_to_d   = .parse_date_any(gm$ValidTo),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$gem_id), , drop = FALSE]
  .validate_unique_key(out, c("stand_d", "gem_id"), "gemeinde()")
  .reset_rownames(out)
}
