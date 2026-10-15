#' NOAS04 land use nomenclature
#'
#' The hierarchical standard nomenclature NOAS04 of the Swiss land use
#' statistics (Arealstatistik), separate from the measured values: 4 main
#' domains, 17 base categories, 27 aggregated categories and 72 basic
#' categories.
#'
#' The nomenclature is read from the SDMX codelist `CL_AREA_NOAS` of the BFS
#' dissemination service, which carries labels in German, French, Italian
#' and English. If the service is unreachable and no cached copy exists, a
#' German-only copy shipped with the package is used.
#'
#' Codes are hierarchical: the main domain is one digit, each finer level
#' appends the two-digit class number of that level, e.g. `"1"` ->
#' `"102"` (base category 2) -> `"10202"` (aggregated category 2) ->
#' `"1020203"` (basic category 3).
#'
#' @param level_n nomenclature level(s), any of `4`, `17`, `27`, `72`
#' @inheritParams gemeinde
#'
#' @return A data.frame with the columns `level_n`, `noas_c` (hierarchical
#'   code), `noas_n` (class number within the level, e.g. 1--17),
#'   `parent_c` (code of the parent class, `NA` for the main domains),
#'   `noas_x_de`, `noas_x_fr`, `noas_x_it` and `noas_x_en`.
#'
#' @seealso [areal_gemeinde()]
#' @examples
#' \dontrun{
#' noas(17)
#' # parent of each of the 72 basic categories at the 17-class level
#' n72 <- noas(72); n27 <- noas(27)
#' n72$noas17_c <- n27$parent_c[match(n72$parent_c, n27$noas_c)]
#' }
#' @export
noas <- function(level_n = 17, cache = TRUE, refresh = FALSE) {
  level_n <- .check_noas_level(level_n)
  cl <- tryCatch(
    .cached("sdmx", "CL_AREA_NOAS", "latest", cache = cache,
            refresh = refresh, fetch = function() {
      raw <- .sdmx_codelist("CL_AREA_NOAS")
      .attach_source_metadata(raw, .source_meta(
        "BFS SDMX codelist CL_AREA_NOAS (NOAS04)", attr(raw, "url")))
    }),
    error = function(e) {
      message("NOAS04 codelist not reachable (", conditionMessage(e),
              "); using the German copy shipped with helvetia")
      .noas_internal()
    })
  out <- .parse_noas(cl)
  out <- out[out$level_n %in% level_n, , drop = FALSE]
  .attach_source_metadata(.reset_rownames(out), source_info(cl))
}


.check_noas_level <- function(level_n) {
  level_n <- as.integer(level_n)
  if (!length(level_n) || anyNA(level_n) ||
      !all(level_n %in% c(4L, 17L, 27L, 72L)))
    stop("'level_n' must be one or more of 4, 17, 27, 72", call. = FALSE)
  unique(level_n)
}


.noas_internal <- function() {
  f <- system.file("extdata", "noas04.csv", package = "helvetia",
                   mustWork = TRUE)
  cl <- utils::read.csv2(f, colClasses = "character", encoding = "UTF-8",
                         na.strings = "")
  .attach_source_metadata(cl, .source_meta(
    "NOAS04 copy shipped with helvetia (from BFS codelist CL_AREA_NOAS 1.0.0)",
    f, retrieved_at = as.POSIXct("2026-10-15", tz = "UTC")))
}


.parse_noas <- function(cl) {
  .require_cols(cl, c("code", "parent", "name_de"), "NOAS04 codelist")
  len <- nchar(cl$code)
  lvl <- c(`1` = 4L, `3` = 17L, `5` = 27L, `7` = 72L)[as.character(len)]
  if (anyNA(lvl))
    stop("NOAS04 codelist contains codes of unexpected length",
         call. = FALSE)
  nm <- function(l) if (is.null(cl[[l]])) NA_character_ else cl[[l]]
  out <- data.frame(
    level_n   = unname(lvl),
    noas_c    = cl$code,
    noas_n    = .as_int(ifelse(len == 1L, cl$code,
                               substring(cl$code, len - 1L))),
    parent_c  = ifelse(is.na(cl$parent) | cl$parent == "", NA_character_,
                       cl$parent),
    noas_x_de = nm("name_de"), noas_x_fr = nm("name_fr"),
    noas_x_it = nm("name_it"), noas_x_en = nm("name_en"),
    stringsAsFactors = FALSE)
  # hierarchy must be consistent: parent = code without the last two digits
  exp_parent <- ifelse(len == 1L, NA_character_,
                       substring(cl$code, 1L, len - 2L))
  if (!identical(exp_parent, out$parent_c))
    stop("NOAS04 codelist: parent references do not match the code hierarchy",
         call. = FALSE)
  counts <- table(factor(out$level_n, c(4, 17, 27, 72)))
  if (!all(counts == c(4, 17, 27, 72)))
    warning(sprintf("NOAS04 codelist has %s classes per level (expected 4/17/27/72)",
                    paste(counts, collapse = "/")), call. = FALSE)
  out[order(out$level_n, out$noas_c), , drop = FALSE]
}
