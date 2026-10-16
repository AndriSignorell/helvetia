#' Get the Mutations of Municipalities
#'
#' Returns the change history of the Swiss political municipalities between
#' two dates: mergers, splits, creations, abolitions, renamings,
#' renumberings and reassignments to another district. It is needed to
#' bring data of different municipality stands onto a common stand, and to
#' explain why a `gem_id` appears in one year and not in another.
#'
#' The history comes from the AGVCH mutation service and is kept separate
#' from the municipality stock returned by [gemeinde()]. Each row links one
#' predecessor version (`old_*`) to one successor version (`new_*`) of a
#' mutation; a merger of three municipalities therefore yields three rows
#' sharing the same `mutation_id`. Several predecessors or successors are
#' never collapsed into one field.
#'
#' The service delivers no type code, so `mutation_type_c` is derived from
#' the structure of each mutation:
#' \describe{
#'   \item{`"creation"`, `"abolition"`}{no predecessor or no successor.}
#'   \item{`"merger"`}{several predecessors, one successor.}
#'   \item{`"split"`}{one predecessor, several successors.}
#'   \item{`"territory-exchange"`}{several municipalities that all persist.}
#'   \item{`"reorganisation"`}{any other many-to-many mutation.}
#'   \item{`"rename"`, `"renumber"`, `"reassign"`}{one-to-one change of
#'     name, BFS number or district; combinations are joined with a
#'     hyphen in this order, e.g. `"rename-reassign"`.}
#'   \item{`"other"`}{one-to-one without a detectable change, e.g. a pure
#'     territory change when `territoryExchange = TRUE`.}
#' }
#' `"reassign"` refers to the district *version*: a district that is itself
#' renamed gives all its municipalities a new district version.
#'
#' @param from,to start and end of the period, each a `Date` or a string
#'   `"YYYY-MM-DD"`. The defaults are 1960-01-01, the start of the register,
#'   and today.
#' @param territoryExchange logical; if `TRUE`, mutations that consist only
#'   of territory exchanges between municipalities are included.
#' @inheritParams gemeinde
#'
#' @return A data.frame with one row per predecessor-successor pair and the
#'   columns `mutation_id`, `mutation_d`, `mutation_type_c`, `old_gem_id`,
#'   `new_gem_id`, `old_gem_x`, `new_gem_x`, `old_hist_id`, `new_hist_id`,
#'   `old_bezirk_x`, `new_bezirk_x`, `old_bezirk_hist_id`,
#'   `new_bezirk_hist_id`, `old_step_c` and `new_step_c`. The last two are
#'   the source fields `InitialStep` and `TerminalStep`, passed through
#'   unchanged.
#'
#' @family swiss.register
#' @concept municipality
#' @concept historicisation
#' @seealso [gemeinde()] for the stock at one date.
#' @examples
#' \dontrun{
#' mut <- gemeindeMutationen(from = "2020-01-01", to = "2021-01-01")
#' subset(mut, mutation_type_c == "merger")
#' }
#' @export
gemeindeMutationen <- function(from = NULL, to = NULL,
                               territoryExchange = FALSE,
                               cache = TRUE, refresh = FALSE) {
  from <- .asDate(from, "from", default = as.Date("1960-01-01"))
  to   <- .asDate(to, "to")
  .checkFlag(territoryExchange)
  if (from > to)
    stop("Argument 'from' must not be after 'to'.", call. = FALSE)

  .cached("agvch", "mutations",
          c(format(from), format(to), if (territoryExchange) "territory"),
          cache = cache, refresh = refresh, fetch = function() {
    raw <- .agvchRequest("mutations",
                          includeTerritoryExchange = tolower(territoryExchange),
                          startPeriod = .agvchDate(from),
                          endPeriod = .agvchDate(to))
    out <- .parseAgvchMutations(raw)
    .attachSourceMetadata(out, .agvchMeta(
      attr(raw, "url"), paste(format(from), format(to), sep = "/")))
  })
}


.parseAgvchMutations <- function(raw) {
  pre <- c("Initial", "Terminal")
  fld <- c("HistoricalCode", "Code", "Name", "ParentHistoricalCode",
           "ParentName", "Step")
  .requireCols(raw, c("MutationNumber", "MutationDate",
                       as.vector(outer(pre, fld, paste0))),
                "AGVCH mutations")
  side <- function(p) {
    v <- function(f) {
      x <- raw[[paste0(p, f)]]
      x[x == ""] <- NA_character_
      x
    }
    data.frame(gem_id = .asInt(v("Code")), gem_x = v("Name"),
               hist_id = .asInt(v("HistoricalCode")),
               bezirk_x = v("ParentName"),
               bezirk_hist_id = .asInt(v("ParentHistoricalCode")),
               step_c = v("Step"), stringsAsFactors = FALSE)
  }
  old <- side("Initial")
  new <- side("Terminal")
  names(old) <- paste0("old_", names(old))
  names(new) <- paste0("new_", names(new))

  out <- data.frame(mutation_id = .asInt(raw$MutationNumber),
                    mutation_d = .parseDateAny(raw$MutationDate),
                    mutation_type_c = NA_character_,
                    old, new, stringsAsFactors = FALSE)
  if (nrow(out)) {
    grp <- split(seq_len(nrow(out)), out$mutation_id)
    for (i in grp) out$mutation_type_c[i] <- .classifyMutation(out[i, ])
  }

  ord <- c("mutation_id", "mutation_d", "mutation_type_c",
           "old_gem_id", "new_gem_id", "old_gem_x", "new_gem_x",
           "old_hist_id", "new_hist_id", "old_bezirk_x", "new_bezirk_x",
           "old_bezirk_hist_id", "new_bezirk_hist_id",
           "old_step_c", "new_step_c")
  out <- out[order(out$mutation_d, out$mutation_id, out$old_gem_id,
                   out$new_gem_id), ord, drop = FALSE]
  .resetRownames(out)
}


# Type of one mutation (all rows sharing a mutation_id).
.classifyMutation <- function(d) {
  old <- unique(d$old_hist_id[!is.na(d$old_hist_id)])
  new <- unique(d$new_hist_id[!is.na(d$new_hist_id)])
  nOld <- length(old)
  nNew <- length(new)

  if (nOld == 0L && nNew == 0L) return("other")
  if (nOld == 0L) return("creation")
  if (nNew == 0L) return("abolition")
  if (nOld > 1L && nNew == 1L) return("merger")
  if (nOld == 1L && nNew > 1L) return("split")
  if (nOld > 1L || nNew > 1L) {
    persist <- setequal(na.omit(d$old_gem_id),
                        na.omit(d$new_gem_id))
    return(if (persist) "territory-exchange" else "reorganisation")
  }
  r <- d[1L, ]
  chg <- c(rename   = !identical(r$old_gem_x, r$new_gem_x),
           renumber = !identical(r$old_gem_id, r$new_gem_id),
           reassign = !identical(r$old_bezirk_hist_id, r$new_bezirk_hist_id))
  if (!any(chg)) "other" else paste(names(chg)[chg], collapse = "-")
}
