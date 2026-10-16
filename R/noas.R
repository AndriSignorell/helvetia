#' Get the NOAS04 Land Use Nomenclature
#'
#' Returns the hierarchical standard nomenclature NOAS04 of the Swiss land
#' use statistics (Arealstatistik), separate from the measured values: 4
#' main domains, 17 base categories, 27 aggregated categories and 72 basic
#' categories. Use it to label land use classes in four languages and to
#' aggregate fine classes to coarser ones reproducibly.
#'
#' The nomenclature is read from the SDMX codelist `CL_AREA_NOAS` of the BFS
#' dissemination service. If the service is unreachable and no cached copy
#' exists, a German-only copy shipped with the package is used and a message
#' says so.
#'
#' Codes are hierarchical: the main domain is one digit, each finer level
#' appends the two-digit class number of that level, e.g. `"1"` -> `"102"`
#' (base category 2) -> `"10202"` (aggregated category 2) -> `"1020203"`
#' (basic category 3).
#'
#' @param nClasses level(s) of the nomenclature, given by their number of
#'   classes: any of `4`, `17`, `27`, `72`.
#' @inheritParams gemeinde
#'
#' @return A data.frame with the columns `level_n` (number of classes of
#'   the level), `noas_c` (hierarchical code), `noas_n` (class number within
#'   the level, e.g. 1 to 17), `parent_c` (code of the parent class, `NA`
#'   for the main domains), `noas_x_de`, `noas_x_fr`, `noas_x_it` and
#'   `noas_x_en`.
#'
#' @family swiss.statistics
#' @concept land-use
#' @concept nomenclature
#' @seealso [arealGemeinde()] for the measured areas.
#' @examples
#' \dontrun{
#' noas(17)
#'
#' # base category (17 classes) of each of the 72 basic categories
#' n72 <- noas(72)
#' n27 <- noas(27)
#' n72$noas17_c <- n27$parent_c[match(n72$parent_c, n27$noas_c)]
#' }
#' @export
noas <- function(nClasses = 17, cache = TRUE, refresh = FALSE) {
  nClasses <- .checkNClasses(nClasses)
  cl <- tryCatch(
    .cached("sdmx", "CL_AREA_NOAS", "latest", cache = cache,
            refresh = refresh, fetch = function() {
      raw <- .sdmxCodelist("CL_AREA_NOAS")
      .attachSourceMetadata(raw, .sourceMeta(
        "BFS SDMX codelist CL_AREA_NOAS (NOAS04)", attr(raw, "url")))
    }),
    error = function(e) {
      message("The NOAS04 codelist is not reachable (", conditionMessage(e),
              "); using the German copy shipped with helvetia.")
      .noasInternal()
    })
  out <- .parseNoas(cl)
  out <- out[out$level_n %in% nClasses, , drop = FALSE]
  .attachSourceMetadata(.resetRownames(out), sourceInfo(cl))
}


.checkNClasses <- function(nClasses) {
  if (!is.numeric(nClasses) || !length(nClasses) || anyNA(nClasses) ||
      !all(nClasses %in% c(4, 17, 27, 72)))
    stop("Argument 'nClasses' must be one or more of 4, 17, 27, 72.",
         call. = FALSE)
  unique(as.integer(nClasses))
}


.noasInternal <- function() {
  f <- system.file("extdata", "noas04.csv", package = "helvetia",
                   mustWork = TRUE)
  cl <- read.csv2(f, colClasses = "character", encoding = "UTF-8",
                  na.strings = "")
  .attachSourceMetadata(cl, .sourceMeta(
    "NOAS04 copy shipped with helvetia (from BFS codelist CL_AREA_NOAS 1.0.0)",
    f, retrievedAt = as.POSIXct("2026-10-15", tz = "UTC")))
}


.parseNoas <- function(cl) {
  .requireCols(cl, c("code", "parent", "name_de"), "NOAS04 codelist")
  len <- nchar(cl$code)
  lvl <- c(`1` = 4L, `3` = 17L, `5` = 27L, `7` = 72L)[as.character(len)]
  if (anyNA(lvl))
    stop("The NOAS04 codelist contains codes of unexpected length.",
         call. = FALSE)
  nm <- function(l) if (is.null(cl[[l]])) NA_character_ else cl[[l]]
  out <- data.frame(
    level_n   = unname(lvl),
    noas_c    = cl$code,
    noas_n    = .asInt(ifelse(len == 1L, cl$code,
                              substring(cl$code, len - 1L))),
    parent_c  = ifelse(is.na(cl$parent) | cl$parent == "", NA_character_,
                       cl$parent),
    noas_x_de = nm("name_de"), noas_x_fr = nm("name_fr"),
    noas_x_it = nm("name_it"), noas_x_en = nm("name_en"),
    stringsAsFactors = FALSE)
  # hierarchy must be consistent: parent = code without the last two digits
  expParent <- ifelse(len == 1L, NA_character_,
                      substring(cl$code, 1L, len - 2L))
  if (!identical(expParent, out$parent_c))
    stop(paste("NOAS04 codelist: the parent references do not match the",
               "code hierarchy."), call. = FALSE)
  counts <- table(factor(out$level_n, c(4, 17, 27, 72)))
  if (!all(counts == c(4, 17, 27, 72)))
    warning(sprintf(paste("The NOAS04 codelist has %s classes per level",
                          "(expected 4/17/27/72)."),
                    paste(counts, collapse = "/")), call. = FALSE)
  out[order(out$level_n, out$noas_c), , drop = FALSE]
}
