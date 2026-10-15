# Source metadata and key validation ----------------------------------------

# Build the metadata record attached to every returned object.
.source_meta <- function(source_x, source_url, retrieved_at = Sys.time(),
                         checksum = NA_character_, version = NA_character_,
                         notes = NULL) {
  structure(
    list(source_x     = source_x,
         source_url   = source_url,
         retrieved_at = as.POSIXct(retrieved_at),
         checksum     = checksum,
         version      = version,
         notes        = notes),
    class = "helvetia_source"
  )
}


.attach_source_metadata <- function(x, metadata) {
  attr(x, "source") <- metadata
  x
}


#' Source metadata of a helvetia result
#'
#' Every object returned by the package carries a record of where it came
#' from: source description, exact request URL(s), retrieval time, file
#' checksum (where a file was downloaded) and the source version or stand.
#'
#' @param x an object returned by one of the data functions, e.g.
#'   [gemeinde()] or [pop_gemeinde()]
#'
#' @return An object of class `helvetia_source` (a list with elements
#'   `source_x`, `source_url`, `retrieved_at`, `checksum`, `version`,
#'   `notes`), or `NULL` if `x` carries no metadata.
#'
#' @examples
#' \dontrun{
#' gem <- gemeinde(as.Date("2021-01-01"))
#' source_info(gem)
#' }
#' @export
source_info <- function(x) attr(x, "source", exact = TRUE)


#' @export
print.helvetia_source <- function(x, ...) {
  cat("Source:     ", x$source_x, "\n", sep = "")
  cat("URL:        ", paste(x$source_url, collapse = "\n            "), "\n",
      sep = "")
  cat("Retrieved:  ", format(x$retrieved_at, "%Y-%m-%d %H:%M:%S %Z"), "\n",
      sep = "")
  if (!all(is.na(x$version)))
    cat("Version:    ", paste(x$version, collapse = ", "), "\n", sep = "")
  if (!all(is.na(x$checksum)))
    cat("MD5:        ", paste(x$checksum, collapse = ", "), "\n", sep = "")
  for (n in x$notes) cat("Note:       ", n, "\n", sep = "")
  invisible(x)
}


# Error if the combination of key columns is not unique.
.validate_unique_key <- function(x, key, what = "result") {
  .require_cols(x, key, what)
  k <- as.data.frame(x)[, key, drop = FALSE]
  dup <- duplicated(k)
  if (any(dup)) {
    ex <- utils::head(unique(k[dup, , drop = FALSE]), 5)
    stop(sprintf(paste0("key (%s) of %s is not unique: %d duplicated ",
                        "row(s), e.g.\n%s"),
                 paste(key, collapse = ", "), what, sum(dup),
                 paste(utils::capture.output(print(ex, row.names = FALSE)),
                       collapse = "\n")),
         call. = FALSE)
  }
  invisible(TRUE)
}
