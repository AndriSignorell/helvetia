# AGVCH REST services (Applikation der Schweizer Gemeinden) -----------------
# Documentation: https://www.agvchapp.bfs.admin.ch/documents/rest_api_de.pdf

.agvch_base <- "https://www.agvchapp.bfs.admin.ch/api/communes/"

.agvch_date <- function(d) format(as.Date(d), "%d-%m-%Y")


# GET an AGVCH endpoint as CSV and return an all-character data.frame.
.agvch_request <- function(endpoint, ...) {
  query <- list(...)
  url <- .build_url(paste0(.agvch_base, endpoint), query)
  txt <- .http_text(url)
  df <- .read_csv_text(txt)
  attr(df, "url") <- url
  df
}


.read_csv_text <- function(txt) {
  if (!nzchar(trimws(txt)))
    return(data.frame())
  utils::read.csv(text = txt, colClasses = "character", check.names = TRUE,
                  na.strings = character(), encoding = "UTF-8",
                  strip.white = TRUE)
}


.agvch_meta <- function(url, version) {
  .source_meta("BFS, Applikation der Schweizer Gemeinden (AGVCH), REST",
               url, version = version)
}
