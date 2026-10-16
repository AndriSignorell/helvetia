# Local cache ---------------------------------------------------------------
#
# Layout:  <cache>/<source>/<version>/<key>.rds
# Every cached object is the *parsed* result including its source metadata,
# so a cache hit returns exactly what the original download returned.

#' @importFrom tools R_user_dir
.cacheDir <- function() {
  getOption("helvetia.cacheDir", R_user_dir("helvetia", "cache"))
}


.cachePath <- function(source, version, ...) {
  clean <- function(x) gsub("[^A-Za-z0-9._-]", "_", as.character(x))
  key <- paste(clean(c(...)), collapse = "_")
  if (!nzchar(key)) key <- "data"
  file.path(.cacheDir(), clean(source), clean(version), paste0(key, ".rds"))
}


# Fetch through the cache.
#   mode = "cache-first":  use a cached copy if present (unless refresh).
#   mode = "remote-first": always try the source, fall back to the cache
#                          if the source is unreachable (used for small,
#                          frequently changing metadata).
# With cache = FALSE nothing is read from or written to the cache.
#   validate: optional function(x) -> TRUE/FALSE; a cached entry that fails
#             it (e.g. written by an older package version) is ignored, as
#             if absent, and never returned as fallback.
#   store:    optional function(x) -> TRUE/FALSE; a fresh result failing it
#             is returned but not written, so it cannot replace a good entry.
# A cached copy returned because the source failed carries the attribute
# "cacheFallback" = TRUE, so callers can avoid combining it with new data.
.cached <- function(source, version, key, fetch, cache = TRUE, refresh = FALSE,
                    mode = c("cache-first", "remote-first"), validate = NULL,
                    store = NULL) {
  mode <- match.arg(mode)
  .checkFlag(cache)
  .checkFlag(refresh)
  path <- .cachePath(source, version, key)
  old <- if (cache && file.exists(path)) .readCached(path, validate)
  have <- !is.null(old)

  if (have && !refresh && mode == "cache-first")
    return(old)

  res <- tryCatch(fetch(), error = function(e) e)

  if (inherits(res, "error")) {
    if (have) {
      warning(sprintf(
        "%s could not be retrieved (%s); using the cached copy from %s.",
        source, conditionMessage(res),
        format(file.mtime(path), "%Y-%m-%d %H:%M")), call. = FALSE)
      attr(old, "cacheFallback") <- TRUE
      return(old)
    }
    stop(res)
  }
  if (!is.null(validate) && !isTRUE(validate(res)))
    stop(sprintf("Unexpected structure of the %s result.", source),
         call. = FALSE)

  if (cache && (is.null(store) || isTRUE(store(res)))) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(res, path)
  }
  res
}


#' List and Clear the Local Cache
#'
#' Downloads are kept in a local cache so that repeated calls are fast and
#' work offline. `cacheList()` shows what is stored, e.g. to see which stands
#' are available without network access; `cacheClear()` removes entries,
#' e.g. to free disk space or to force a complete new download.
#'
#' The cache lives in [tools::R_user_dir()]`("helvetia", "cache")`; set
#' `options(helvetia.cacheDir = <path>)` to use another directory. A
#' single result is refreshed with the argument `refresh = TRUE` of the
#' respective data function.
#'
#' @param source optional source name to restrict the operation to, one of
#'   `"agvch"`, `"pxweb"`, `"sdmx"`, `"area"`, `"geometry"`, `"themakart"`,
#'   `"priminfo"`; `NULL` for all sources.
#'
#' @return `cacheList()` returns a data.frame with the columns `source`,
#'   `version`, `file`, `sizeMb` and `modified`. `cacheClear()` returns the
#'   number of removed files invisibly.
#'
#' @family swiss.source
#' @concept caching
#' @concept reproducibility
#' @examples
#' cacheList()
#' @export
cacheList <- function(source = NULL) {
  root <- .cacheDir()
  dir <- .cacheSourceDir(source)
  files <- list.files(dir, pattern = "\\.rds$", recursive = TRUE,
                      full.names = TRUE)
  if (!length(files))
    return(data.frame(source = character(), version = character(),
                      file = character(), sizeMb = numeric(),
                      modified = as.POSIXct(character())))
  rel <- substring(files, nchar(root) + 2L)
  parts <- strsplit(rel, "[/\\\\]")
  info <- file.info(files)
  data.frame(source   = vapply(parts, `[`, "", 1L),
             version  = vapply(parts, `[`, "", 2L),
             file     = vapply(parts, function(p) p[length(p)], ""),
             sizeMb   = round(info$size / 2^20, 2),
             modified = info$mtime)
}


#' @rdname cacheList
#' @export
cacheClear <- function(source = NULL) {
  root <- normalizePath(.cacheDir(), winslash = "/", mustWork = FALSE)
  dir <- .cacheSourceDir(source)
  files <- list.files(dir, recursive = TRUE, full.names = TRUE)
  files <- normalizePath(files, winslash = "/", mustWork = FALSE)
  inside <- startsWith(files, paste0(root, "/"))
  if (!all(inside))
    stop("Refusing to remove files outside the cache directory.",
         call. = FALSE)
  n <- sum(file.remove(files))
  message(sprintf("%d cached file(s) removed.", n))
  invisible(n)
}


# Read a cache entry; unreadable or invalid entries count as absent.
.readCached <- function(path, validate = NULL) {
  x <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(x) || (!is.null(validate) && !isTRUE(validate(x)))) return(NULL)
  x
}


# The cache sources; cacheList()/cacheClear() accept only these names, so a
# value like "../x" can never address a directory outside the cache.
.cacheSources <- c("agvch", "pxweb", "sdmx", "area", "geometry", "themakart",
                   "priminfo")

.cacheSourceDir <- function(source) {
  root <- .cacheDir()
  if (is.null(source)) return(root)
  if (!is.character(source) || length(source) != 1L || is.na(source) ||
      !source %in% .cacheSources)
    stop(sprintf("Argument 'source' must be NULL or one of %s.",
                 paste(sprintf("\"%s\"", .cacheSources), collapse = ", ")),
         call. = FALSE)
  file.path(root, source)
}
