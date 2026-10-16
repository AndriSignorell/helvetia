# HTTP layer (curl) -----------------------------------------------------------

.userAgent <- function() {
  sprintf("helvetia/%s (R package; https://github.com/AndriSignorell/helvetia)",
          packageVersion("helvetia"))
}


.buildUrl <- function(url, query = NULL) {
  if (!length(query)) return(url)
  q <- paste(names(query),
             vapply(query, function(v) curl::curl_escape(as.character(v)), ""),
             sep = "=", collapse = "&")
  paste0(url, if (grepl("?", url, fixed = TRUE)) "&" else "?", q)
}


# Timeouts (seconds): connecting, and the whole transfer. Large downloads
# (ThemaKart, swissBOUNDARIES3D) get the longer transfer limit.
.httpTimeouts <- function(download = FALSE) {
  list(connecttimeout = getOption("helvetia.connectTimeout", 30L),
       timeout = getOption("helvetia.timeout", if (download) 1800L else 120L))
}

.newHandle <- function(download = FALSE, ...) {
  h <- curl::new_handle(useragent = .userAgent(), followlocation = TRUE, ...)
  do.call(curl::handle_setopt, c(list(h), .httpTimeouts(download)))
  h
}


# Perform a request with retries on 429/5xx and on connection errors
# (timeouts, resets). Returns the curl response.
.http <- function(url, headers = NULL, postJson = NULL, tries = 3L) {
  for (i in seq_len(tries)) {
    h <- .newHandle()
    if (length(headers)) curl::handle_setheaders(h, .list = headers)
    if (!is.null(postJson)) {
      curl::handle_setheaders(h, "Content-Type" = "application/json")
      curl::handle_setopt(h, copypostfields = postJson)
    }
    res <- tryCatch(curl::curl_fetch_memory(url, handle = h),
                    error = function(e) e)
    if (inherits(res, "error")) {
      if (i == tries)
        stop(sprintf("Connection to %s failed: %s", url,
                     conditionMessage(res)), call. = FALSE)
      Sys.sleep(2^i)
      next
    }
    if (res$status_code < 400L) return(res)
    if (!res$status_code %in% c(429L, 500L, 502L, 503L, 504L) || i == tries)
      break
    Sys.sleep(2^i)
  }
  body <- substr(rawToChar(res$content), 1L, 300L)
  stop(sprintf("HTTP %d for %s%s", res$status_code, url,
               if (nzchar(body)) paste0("\n", body) else ""), call. = FALSE)
}


.httpText <- function(url, query = NULL, headers = NULL, postJson = NULL) {
  full <- .buildUrl(url, query)
  res <- .http(full, headers = headers, postJson = postJson)
  raw <- res$content
  bom <- as.raw(c(0xef, 0xbb, 0xbf))
  if (length(raw) >= 3L && identical(raw[1:3], bom)) raw <- raw[-(1:3)]
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  txt
}


.httpJson <- function(url, query = NULL, post = NULL) {
  body <- if (!is.null(post)) jsonlite::toJSON(post, auto_unbox = TRUE)
  jsonlite::fromJSON(.httpText(url, query, postJson = body),
                     simplifyVector = FALSE)
}


# Download a file and record URL, retrieval time and MD5.
#' @importFrom tools md5sum
.downloadChecked <- function(url, dest, tries = 3L) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(dest, ".part")
  on.exit(unlink(tmp), add = TRUE)
  for (i in seq_len(tries)) {
    h <- .newHandle(download = TRUE, failonerror = TRUE)
    ok <- tryCatch({
      curl::curl_download(url, tmp, handle = h, quiet = !interactive())
      TRUE
    }, error = function(e) e)
    if (isTRUE(ok)) break
    # HTTP errors (failonerror) other than 429/5xx are not retried
    if (i == tries || grepl("HTTP error 4(0[0-9]|1[0-9]|2[0-8])", conditionMessage(ok)))
      stop(ok)
    Sys.sleep(2^i)
  }
  if (!file.exists(tmp) || file.size(tmp) == 0)
    stop(sprintf("The download of %s produced an empty file.", url),
         call. = FALSE)
  .moveFile(tmp, dest)
  list(path = dest, url = url, retrievedAt = Sys.time(),
       md5 = unname(md5sum(dest)))
}


# Try several candidate URLs in turn; the first successful download wins.
.downloadFirst <- function(urls, dest) {
  errs <- character()
  for (u in urls) {
    res <- tryCatch(.downloadChecked(u, dest), error = function(e) e)
    if (!inherits(res, "error")) return(res)
    errs <- c(errs, sprintf("%s: %s", u, conditionMessage(res)))
  }
  stop("None of the source URLs could be downloaded:\n",
       paste(errs, collapse = "\n"), call. = FALSE)
}


# file.rename() can fail on Windows while a virus scanner still holds the
# new file; fall back to copying.
.moveFile <- function(from, to) {
  if (suppressWarnings(file.rename(from, to))) return(invisible(TRUE))
  if (!file.copy(from, to, overwrite = TRUE))
    stop(sprintf("The downloaded file could not be moved to %s.", to),
         call. = FALSE)
  unlink(from)
  invisible(TRUE)
}
