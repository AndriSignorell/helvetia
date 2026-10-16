#' Get the Swiss Cantons
#'
#' Returns the 26 Swiss cantons with their official BFS number, abbreviation
#' and name in German, French and Italian. Use it to label cantonal results
#' or to translate between the numeric code and the abbreviation.
#'
#' The table is shipped with the package; the cantonal numbering has been
#' stable since the founding of the canton of Jura in 1979.
#'
#' @return A data.frame with the columns `kanton_id`, `kanton_c`,
#'   `kanton_x_de`, `kanton_x_fr` and `kanton_x_it`.
#'
#' @family swiss.register
#' @concept canton
#' @concept nomenclature
#' @seealso [geoKanton()].
#' @examples
#' kanton()
#' @export
kanton <- function() {
  f <- system.file("extdata", "kanton.csv", package = "helvetia",
                   mustWork = TRUE)
  out <- read.csv2(f, colClasses = c("integer", rep("character", 4)),
                   encoding = "UTF-8")
  .attachSourceMetadata(out, .sourceMeta(
    "Official cantonal numbering and names (BFS)", f,
    retrievedAt = as.POSIXct("2026-10-16", tz = "UTC")))
}
