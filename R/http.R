# HTTP layer (curl) -----------------------------------------------------------

.user_agent <- function() {
  sprintf("helvetia/%s (R package; https://github.com/AndriSignorell/helvetia)",
          utils::packageVersion("helvetia"))
}


.build_url <- function(url, query = NULL) {
  if (!length(query)) return(url)
  q <- paste(names(query),
             vapply(query, function(v) curl::curl_escape(as.character(v)), ""),
             sep = "=", collapse = "&")
  paste0(url, if (grepl("?", url, fixed = TRUE)) "&" else "?", q)
}


# Perform a request with retries on 429/5xx. Returns the curl response.
.http <- function(url, headers = NULL, post_json = NULL, tries = 3L) {
  for (i in seq_len(tries)) {
    h <- curl::new_handle(useragent = .user_agent(), followlocation = TRUE)
    if (length(headers)) curl::handle_setheaders(h, .list = headers)
    if (!is.null(post_json)) {
      curl::handle_setheaders(h, "Content-Type" = "application/json")
      curl::handle_setopt(h, copypostfields = post_json)
    }
    res <- curl::curl_fetch_memory(url, handle = h)
    if (res$status_code < 400L) return(res)
    if (!res$status_code %in% c(429L, 500L, 502L, 503L, 504L) || i == tries)
      break
    Sys.sleep(2^i)
  }
  body <- substr(rawToChar(res$content), 1L, 300L)
  stop(sprintf("HTTP %d for %s%s", res$status_code, url,
               if (nzchar(body)) paste0("\n", body) else ""), call. = FALSE)
}


.http_text <- function(url, query = NULL, headers = NULL, post_json = NULL) {
  full <- .build_url(url, query)
  res <- .http(full, headers = headers, post_json = post_json)
  raw <- res$content
  bom <- as.raw(c(0xef, 0xbb, 0xbf))
  if (length(raw) >= 3L && identical(raw[1:3], bom)) raw <- raw[-(1:3)]
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  txt
}


.http_json <- function(url, query = NULL, post = NULL) {
  body <- if (!is.null(post)) jsonlite::toJSON(post, auto_unbox = TRUE)
  jsonlite::fromJSON(.http_text(url, query, post_json = body),
                     simplifyVector = FALSE)
}


# Download a file and record URL, retrieval time and MD5.
.download_checked <- function(url, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(dest, ".part")
  on.exit(unlink(tmp), add = TRUE)
  h <- curl::new_handle(useragent = .user_agent(), followlocation = TRUE)
  curl::curl_download(url, tmp, handle = h, quiet = !interactive())
  if (!file.exists(tmp) || file.size(tmp) == 0)
    stop(sprintf("download of %s produced an empty file", url), call. = FALSE)
  file.rename(tmp, dest)
  list(path = dest, url = url, retrieved_at = Sys.time(),
       md5 = unname(tools::md5sum(dest)))
}
