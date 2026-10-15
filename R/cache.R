# Local cache ---------------------------------------------------------------
#
# Layout:  <cache>/<source>/<version>/<key>.rds
# Every cached object is the *parsed* result including its source metadata,
# so a cache hit returns exactly what the original download returned.

.cache_dir <- function() {
  getOption("helvetia.cache_dir", tools::R_user_dir("helvetia", "cache"))
}


.cache_path <- function(source, version, ...) {
  clean <- function(x) gsub("[^A-Za-z0-9._-]", "_", as.character(x))
  key <- paste(clean(c(...)), collapse = "_")
  if (!nzchar(key)) key <- "data"
  file.path(.cache_dir(), clean(source), clean(version), paste0(key, ".rds"))
}


# Fetch through the cache.
#   mode = "cache-first":  use a cached copy if present (unless refresh).
#   mode = "remote-first": always try the source, fall back to the cache
#                          if the source is unreachable (used for small,
#                          frequently changing metadata).
# With cache = FALSE nothing is read from or written to the cache.
.cached <- function(source, version, key, fetch, cache = TRUE, refresh = FALSE,
                    mode = c("cache-first", "remote-first")) {
  mode <- match.arg(mode)
  .checkFlag(cache)
  .checkFlag(refresh)
  path <- .cache_path(source, version, key)
  have <- cache && file.exists(path)

  if (have && !refresh && mode == "cache-first")
    return(readRDS(path))

  res <- tryCatch(fetch(), error = function(e) e)

  if (inherits(res, "error")) {
    if (have) {
      warning(sprintf(
        "%s could not be retrieved (%s); using cached copy from %s",
        source, conditionMessage(res),
        format(file.mtime(path), "%Y-%m-%d %H:%M")), call. = FALSE)
      return(readRDS(path))
    }
    stop(res)
  }

  if (cache) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(res, path)
  }
  res
}


#' Inspect and clear the local cache
#'
#' Downloads are cached below [tools::R_user_dir()]`("helvetia", "cache")`.
#' The location can be changed with `options(helvetia.cache_dir = <path>)`.
#'
#' @param source optional source name to restrict the operation to
#'   (`"agvch"`, `"pxweb"`, `"geometry"`, `"area"`, `"sdmx"`); `NULL` for all
#'
#' @return `cache_list()` returns a data.frame with columns `source`,
#'   `version`, `file`, `size_mb` and `modified`. `cache_clear()` returns
#'   the number of removed files invisibly.
#'
#' @examples
#' cache_list()
#' @export
cache_list <- function(source = NULL) {
  root <- .cache_dir()
  dir <- if (is.null(source)) root else file.path(root, source)
  files <- list.files(dir, pattern = "\\.rds$", recursive = TRUE,
                      full.names = TRUE)
  if (!length(files))
    return(data.frame(source = character(), version = character(),
                      file = character(), size_mb = numeric(),
                      modified = as.POSIXct(character())))
  rel <- substring(files, nchar(root) + 2L)
  parts <- strsplit(rel, "[/\\\\]")
  info <- file.info(files)
  data.frame(source   = vapply(parts, `[`, "", 1L),
             version  = vapply(parts, `[`, "", 2L),
             file     = vapply(parts, function(p) p[length(p)], ""),
             size_mb  = round(info$size / 2^20, 2),
             modified = info$mtime)
}


#' @rdname cache_list
#' @export
cache_clear <- function(source = NULL) {
  root <- .cache_dir()
  dir <- if (is.null(source)) root else file.path(root, source)
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  n <- sum(file.remove(files))
  message(sprintf("%d cached file(s) removed", n))
  invisible(n)
}
