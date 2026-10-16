# Source metadata and key validation ----------------------------------------

# Build the metadata record attached to every returned object.
.sourceMeta <- function(sourceText, url, retrievedAt = Sys.time(),
                        checksum = NA_character_, version = NA_character_,
                        query = NULL, notes = NULL) {
  structure(
    list(source      = sourceText,
         url         = url,
         retrievedAt = as.POSIXct(retrievedAt),
         checksum    = checksum,
         version     = version,
         query       = query,
         notes       = notes[!is.na(notes)]),
    class = "SourceInfo"
  )
}


.attachSourceMetadata <- function(x, metadata) {
  attr(x, "source") <- metadata
  x
}


#' Get the Source Metadata of a Result
#'
#' Returns the provenance record that every data object of helvetia
#' carries. Use it to cite a source correctly, to see which stand or version
#' a cached result is based on, or to reproduce a download exactly.
#'
#' The record is stored in the attribute `"source"` and survives the cache:
#' a cached result reports the time of its original download, not of the
#' cache hit.
#'
#' @param x an object returned by one of the data functions, e.g.
#'   [gemeinde()] or [popGemeinde()].
#'
#' @return An object of class `SourceInfo`, a list with the elements
#'   \describe{
#'     \item{`source`}{description of the source.}
#'     \item{`url`}{exact request URL(s).}
#'     \item{`retrievedAt`}{time of the download.}
#'     \item{`checksum`}{MD5 checksum of a downloaded file, else `NA`.}
#'     \item{`version`}{source version, stand or period, else `NA`.}
#'     \item{`query`}{POST selections sent to the source (PXWeb), else
#'       `NULL`; the URL alone does not describe such a request.}
#'     \item{`notes`}{further remarks, or `NULL`.}
#'   }
#'   `NULL` if `x` carries no metadata.
#'
#' @family swiss.source
#' @concept source-metadata
#' @concept reproducibility
#' @examples
#' sourceInfo(kanton())
#' @export
sourceInfo <- function(x) attr(x, "source", exact = TRUE)


#' @export
print.SourceInfo <- function(x, ...) {
  cat("Source:     ", x$source, "\n", sep = "")
  cat("URL:        ", paste(x$url, collapse = "\n            "), "\n",
      sep = "")
  cat("Retrieved:  ",
      paste(format(x$retrievedAt, "%Y-%m-%d %H:%M:%S %Z"), collapse = ", "),
      "\n", sep = "")
  if (!all(is.na(x$version)))
    cat("Version:    ", paste(x$version, collapse = ", "), "\n", sep = "")
  if (!all(is.na(x$checksum)))
    cat("MD5:        ", paste(x$checksum, collapse = ", "), "\n", sep = "")
  if (length(x$query))
    cat("Query:      ", length(x$query), " POST selection(s), see $query\n",
        sep = "")
  for (n in x$notes) cat("Note:       ", n, "\n", sep = "")
  invisible(x)
}


# Error if the combination of key columns is not unique.
.validateUniqueKey <- function(x, key, what = "result", allowNA = NULL) {
  .requireCols(x, key, what)
  k <- as.data.frame(x)[, key, drop = FALSE]
  na <- vapply(setdiff(key, allowNA), function(n) anyNA(k[[n]]), TRUE)
  if (any(na))
    stop(sprintf("Key column(s) %s of %s contain missing values.",
                 paste(names(na)[na], collapse = ", "), what), call. = FALSE)
  dup <- duplicated(k)
  if (any(dup)) {
    ex <- head(unique(k[dup, , drop = FALSE]), 5)
    stop(sprintf(paste0("Key (%s) of %s is not unique: %d duplicated ",
                        "row(s), e.g.\n%s"),
                 paste(key, collapse = ", "), what, sum(dup),
                 paste(capture.output(print(ex, row.names = FALSE)),
                       collapse = "\n")),
         call. = FALSE)
  }
  invisible(TRUE)
}
