#' Municipality mutations
#'
#' Change history of Swiss political municipalities (mergers, splits,
#' creations, abolitions, renamings, renumberings and reassignments to
#' another district) from the AGVCH mutation service, kept separate from
#' the municipality stock returned by [gemeinde()].
#'
#' Each row links one predecessor version (`old_*`) to one successor
#' version (`new_*`) of a mutation. A merger of three municipalities
#' therefore yields three rows sharing the same `mutation_id`; several
#' predecessors or successors are never collapsed into one field.
#'
#' `mutation_type_c` is derived from the structure of each mutation, since
#' the service itself does not deliver a type code:
#' \describe{
#'   \item{`"creation"`, `"abolition"`}{no predecessor resp. successor}
#'   \item{`"merger"`}{several predecessors, one successor}
#'   \item{`"split"`}{one predecessor, several successors}
#'   \item{`"territory-exchange"`}{several municipalities that all persist}
#'   \item{`"reorganisation"`}{any other many-to-many mutation}
#'   \item{`"rename"`, `"renumber"`, `"reassign"`}{one-to-one changes of
#'     name, BFS number, or district; combinations are joined with `"+"`,
#'     e.g. `"rename+reassign"`}
#'   \item{`"other"`}{one-to-one without a detectable change (e.g. a pure
#'     territory change when `territory = TRUE`)}
#' }
#'
#' @param from,to start and end of the period (`Date` or `"YYYY-MM-DD"`);
#'   defaults are 1960-01-01 (start of the register) and today
#' @param territory logical; include mutations that consist only of
#'   territory exchanges between municipalities
#' @inheritParams gemeinde
#'
#' @return A data.frame with the columns `mutation_id`, `mutation_d`,
#'   `mutation_type_c`, `old_gem_id`, `new_gem_id`, `old_gem_x`,
#'   `new_gem_x`, `old_hist_id`, `new_hist_id`, `old_bezirk_x`,
#'   `new_bezirk_x`, `old_bezirk_hist_id`, `new_bezirk_hist_id`,
#'   `old_step_c` and `new_step_c` (the last two are the source fields
#'   `InitialStep`/`TerminalStep`, passed through unchanged).
#'
#' @seealso [gemeinde()]
#' @examples
#' \dontrun{
#' mut <- gemeinde_mutationen(from = "2020-01-01", to = "2021-01-01")
#' subset(mut, mutation_type_c == "merger")
#' }
#' @export
gemeinde_mutationen <- function(from = NULL, to = NULL, territory = FALSE,
                                cache = TRUE, refresh = FALSE) {
  from <- .as_stand_d(from, "from", default = as.Date("1960-01-01"))
  to   <- .as_stand_d(to, "to")
  .checkFlag(territory)
  if (from > to) stop("'from' must not be after 'to'", call. = FALSE)

  .cached("agvch", "mutations",
          c(format(from), format(to), if (territory) "territory"),
          cache = cache, refresh = refresh, fetch = function() {
    raw <- .agvch_request("mutations",
                          includeTerritoryExchange = tolower(territory),
                          startPeriod = .agvch_date(from),
                          endPeriod = .agvch_date(to))
    out <- .parse_agvch_mutations(raw)
    .attach_source_metadata(out, .agvch_meta(
      attr(raw, "url"), paste(format(from), format(to), sep = "/")))
  })
}


.parse_agvch_mutations <- function(raw) {
  pre <- c("Initial", "Terminal")
  fld <- c("HistoricalCode", "Code", "Name", "ParentHistoricalCode",
           "ParentName", "Step")
  .require_cols(raw, c("MutationNumber", "MutationDate",
                       as.vector(outer(pre, fld, paste0))),
                "AGVCH mutations")
  side <- function(p) {
    v <- function(f) {
      x <- raw[[paste0(p, f)]]
      x[x == ""] <- NA_character_
      x
    }
    data.frame(gem_id = .as_int(v("Code")), gem_x = v("Name"),
               hist_id = .as_int(v("HistoricalCode")),
               bezirk_x = v("ParentName"),
               bezirk_hist_id = .as_int(v("ParentHistoricalCode")),
               step_c = v("Step"), stringsAsFactors = FALSE)
  }
  old <- side("Initial")
  new <- side("Terminal")
  names(old) <- paste0("old_", names(old))
  names(new) <- paste0("new_", names(new))

  out <- data.frame(mutation_id = .as_int(raw$MutationNumber),
                    mutation_d = .parse_date_any(raw$MutationDate),
                    mutation_type_c = NA_character_,
                    old, new, stringsAsFactors = FALSE)
  if (nrow(out)) {
    grp <- split(seq_len(nrow(out)), out$mutation_id)
    for (i in grp) out$mutation_type_c[i] <- .classify_mutation(out[i, ])
  }

  ord <- c("mutation_id", "mutation_d", "mutation_type_c",
           "old_gem_id", "new_gem_id", "old_gem_x", "new_gem_x",
           "old_hist_id", "new_hist_id", "old_bezirk_x", "new_bezirk_x",
           "old_bezirk_hist_id", "new_bezirk_hist_id",
           "old_step_c", "new_step_c")
  out <- out[order(out$mutation_d, out$mutation_id, out$old_gem_id,
                   out$new_gem_id), ord, drop = FALSE]
  .reset_rownames(out)
}


# Type of one mutation (all rows sharing a mutation_id).
.classify_mutation <- function(d) {
  old <- unique(d$old_hist_id[!is.na(d$old_hist_id)])
  new <- unique(d$new_hist_id[!is.na(d$new_hist_id)])
  n_old <- length(old)
  n_new <- length(new)

  if (n_old == 0L && n_new == 0L) return("other")
  if (n_old == 0L) return("creation")
  if (n_new == 0L) return("abolition")
  if (n_old > 1L && n_new == 1L) return("merger")
  if (n_old == 1L && n_new > 1L) return("split")
  if (n_old > 1L || n_new > 1L) {
    persist <- setequal(stats::na.omit(d$old_gem_id),
                        stats::na.omit(d$new_gem_id))
    return(if (persist) "territory-exchange" else "reorganisation")
  }
  r <- d[1L, ]
  chg <- c(rename   = !identical(r$old_gem_x, r$new_gem_x),
           renumber = !identical(r$old_gem_id, r$new_gem_id),
           reassign = !identical(r$old_bezirk_hist_id, r$new_bezirk_hist_id))
  if (!any(chg)) "other" else paste(names(chg)[chg], collapse = "+")
}
