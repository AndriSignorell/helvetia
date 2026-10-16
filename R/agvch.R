# AGVCH REST services (Applikation der Schweizer Gemeinden) -----------------
# Documentation: https://www.agvchapp.bfs.admin.ch/documents/rest_api_de.pdf

.agvchBase <- "https://www.agvchapp.bfs.admin.ch/api/communes/"

.agvchDate <- function(d) format(as.Date(d), "%d-%m-%Y")


# GET an AGVCH endpoint as CSV and return an all-character data.frame.
.agvchRequest <- function(endpoint, ...) {
  query <- list(...)
  url <- .buildUrl(paste0(.agvchBase, endpoint), query)
  txt <- .httpText(url)
  df <- .readCsvText(txt)
  attr(df, "url") <- url
  df
}


.readCsvText <- function(txt) {
  if (!nzchar(trimws(txt)))
    return(data.frame())
  read.csv(text = txt, colClasses = "character", check.names = TRUE,
                  na.strings = character(), encoding = "UTF-8",
                  strip.white = TRUE)
}


.agvchMeta <- function(url, version) {
  .sourceMeta("BFS, Applikation der Schweizer Gemeinden (AGVCH), REST",
               url, version = version)
}
