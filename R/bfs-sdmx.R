# BFS SDMX dissemination API (stats.swiss, .Stat Suite) ---------------------
# Used for the Arealstatistik dataflow CH1.AREA/DF_AREA_NOAS and its
# codelists (NOAS04 hierarchy, survey periods, regions).

.sdmxBase <- "https://disseminate.stats.swiss/rest"


.sdmxXml <- function(path) {
  url <- paste0(.sdmxBase, path)
  list(doc = xml2::read_xml(.httpText(url)), url = url)
}


# Namespace-agnostic XPath step: .el("Code") -> *[local-name()='Code']
.el <- function(name, cond = NULL) {
  sprintf("*[local-name()='%s'%s]", name,
          if (is.null(cond)) "" else paste0(" and ", cond))
}


# Codelist as data.frame(code, parent, name_de, name_fr, name_it, name_en).
.sdmxCodelist <- function(id, agency = "all", version = "latest") {
  res <- .sdmxXml(sprintf("/codelist/%s/%s/%s", agency, id, version))
  cl <- .parseSdmxCodelist(res$doc, id)
  attr(cl, "url") <- res$url
  cl
}

.parseSdmxCodelist <- function(doc, id) {
  lists <- xml2::xml_find_all(
    doc, paste0("//", .el("Codelist", sprintf("@id='%s'", id))))
  if (!length(lists))
    stop(sprintf("SDMX codelist '%s' not found in response", id),
         call. = FALSE)
  codes <- xml2::xml_find_all(lists[[1L]], paste0("./", .el("Code")))
  name_in <- function(node, lang) {
    n <- xml2::xml_find_first(
      node, paste0("./", .el("Name", sprintf("@xml:lang='%s'", lang))))
    if (inherits(n, "xml_missing")) NA_character_ else xml2::xml_text(n)
  }
  parent <- vapply(codes, function(n) {
    p <- xml2::xml_find_first(n, paste0("./", .el("Parent"), "/",
                                        .el("Ref")))
    if (inherits(p, "xml_missing")) NA_character_ else xml2::xml_attr(p, "id")
  }, "")
  out <- data.frame(code = xml2::xml_attr(codes, "id"), parent = parent,
                    stringsAsFactors = FALSE)
  for (l in c("de", "fr", "it", "en"))
    out[[paste0("name_", l)]] <- vapply(codes, name_in, "", lang = l)
  out
}


# Dimensions of a data structure: data.frame(id, position, codelist).
.sdmxDimensions <- function(agency, id, version) {
  res <- .sdmxXml(sprintf("/datastructure/%s/%s/%s", agency, id, version))
  .parseSdmxDimensions(res$doc)
}

.parseSdmxDimensions <- function(doc) {
  dims <- xml2::xml_find_all(doc, paste0("//", .el("DimensionList"), "/",
                                       .el("Dimension")))
  cl <- vapply(dims, function(d) {
    r <- xml2::xml_find_first(d, paste0(".//", .el("Enumeration"), "/",
                                         .el("Ref")))
    if (inherits(r, "xml_missing")) NA_character_ else xml2::xml_attr(r, "id")
  }, "")
  out <- data.frame(id = xml2::xml_attr(dims, "id"),
                    position = as.integer(xml2::xml_attr(dims, "position")),
                    codelist = cl, stringsAsFactors = FALSE)
  out[order(out$position), , drop = FALSE]
}


# Data query; `key` is a named list dimension -> codes (empty = all).
# Returns the SDMX-CSV table with all columns as character.
.sdmxData <- function(flow, key, dims) {
  k <- paste(vapply(dims, function(d) paste(key[[d]] %||% "", collapse = "+"),
                    ""), collapse = ".")
  url <- sprintf("%s/data/%s/%s?dimensionAtObservation=AllDimensions",
                 .sdmxBase, flow, k)
  txt <- .httpText(url, headers = list(
    Accept = "application/vnd.sdmx.data+csv;version=1.0.0"))
  df <- .readCsvText(txt)
  attr(df, "url") <- url
  df
}
