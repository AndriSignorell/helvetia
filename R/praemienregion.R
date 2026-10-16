# Health insurance premium regions (BAG, priminfo.admin.ch) -----------------

.priminfoBase <- "https://www.priminfo.admin.ch/downloads/"

# The current year is published as praemienregionen-YYYY.xlsx, archived
# years as praemienregionen_YYYY.xlsx.
.priminfoUrls <- function(year) {
  paste0(.priminfoBase, sprintf(c("praemienregionen-%d.xlsx",
                                  "praemienregionen_%d.xlsx"), year))
}


#' Get the Health Insurance Premium Regions
#'
#' Returns the assignment of municipalities to the premium regions of the
#' mandatory health insurance for a premium year, as published by the
#' Federal Office of Public Health (BAG) on priminfo.admin.ch. Premiums
#' differ between up to three regions within a canton, so the region is
#' needed to compare premiums or claims across municipalities, and to map
#' the regions with [combineRegions()].
#'
#' The regions are defined in the ordinance of the EDI, which remains the
#' binding source; the files on priminfo are its machine-readable
#' companion and are available from 2004. Cantons without regional
#' differentiation have region `0`. The file of a year lists the
#' municipalities and localities valid in that year; join it with
#' municipality data of the same stand.
#'
#' The layout of the files is not formally documented. Columns are found by
#' their headers (BFS number, region, canton, municipality, postcode,
#' locality), not by position; a change of the layout is an error naming
#' the headers found. Where the regions follow locality rather than
#' municipality boundaries, a municipality can lie in two regions; it then
#' appears once per region with `split_l = TRUE`.
#'
#' @param year premium year; the default `NULL` uses the current year.
#' @param output `"gemeinde"` for one row per municipality and region, or
#'   `"ortschaft"` for the rows as delivered (one per postcode and
#'   locality).
#' @inheritParams gemeinde
#'
#' @return For `output = "gemeinde"`, a data.frame with the columns
#'   `jahr_n`, `gem_id`, `gem_x`, `kanton_c`, `preg_c` (region within the
#'   canton, 0 to 3), `preg_x` (e.g. `"ZH3"`) and `split_l`; the key is
#'   `jahr_n` + `gem_id` + `preg_c`. For `output = "ortschaft"`, the
#'   columns `plz_n` and `ort_x` are added and `split_l` is dropped.
#'
#' @family swiss.register
#' @concept health-insurance
#' @concept municipality
#' @concept regional-classification
#' @seealso [combineRegions()] to draw the regions.
#' @examples
#' \dontrun{
#' preg <- praemienregion(2026)
#' table(preg$preg_x[preg$kanton_c == "ZH"])
#' }
#' @export
praemienregion <- function(year = NULL, output = c("gemeinde", "ortschaft"),
                           cache = TRUE, refresh = FALSE) {
  output <- match.arg(output)
  year <- .asYear(year, "year", default = as.integer(format(Sys.Date(), "%Y")))
  if (year < 2004L)
    stop("Argument 'year' must be 2004 or later.", call. = FALSE)

  full <- .cached("priminfo", year, "praemienregionen", cache = cache,
                  refresh = refresh, fetch = function() {
    dest <- tempfile(fileext = ".xlsx")
    on.exit(unlink(dest), add = TRUE)
    dl <- .downloadFirst(.priminfoUrls(year), dest)
    res <- .parsePriminfo(dest, year)
    .attachSourceMetadata(res, .sourceMeta(
      "BAG, Praemienregionen (priminfo.admin.ch)", dl$url,
      retrievedAt = dl$retrievedAt, checksum = dl$md5,
      version = as.character(year)))
  })
  meta <- sourceInfo(full)

  if (output == "ortschaft") {
    if (!all(c("plz_n", "ort_x") %in% names(full)))
      stop(paste("Argument 'output': the file of this year has no locality",
                 "columns; use output = \"gemeinde\"."), call. = FALSE)
    return(full)
  }

  keep <- c("jahr_n", "gem_id", "gem_x", "kanton_c", "preg_c", "preg_x")
  out <- unique(as.data.frame(full)[, keep, drop = FALSE])
  out <- out[!duplicated(out[c("jahr_n", "gem_id", "preg_c")]), ,
             drop = FALSE]
  out$split_l <- out$gem_id %in% out$gem_id[duplicated(out$gem_id)]
  out <- out[order(out$gem_id, out$preg_c), , drop = FALSE]
  .validateUniqueKey(out, c("jahr_n", "gem_id", "preg_c"),
                     "praemienregion()")
  .attachSourceMetadata(.resetRownames(out), meta)
}


# Find the header row and the columns by name; returns the delivered rows.
.parsePriminfo <- function(path, year) {
  sheets <- readxl::excel_sheets(path)
  for (sh in sheets) {
    raw <- suppressMessages(readxl::read_excel(
      path, sheet = sh, col_names = FALSE, col_types = "text",
      .name_repair = "minimal"))
    hdr <- .findHeaderRow(raw, c("bfs", "region"))
    if (!is.na(hdr)) break
  }
  if (is.na(hdr))
    stop(sprintf(paste("No sheet of the premium region file %d has a header",
                       "row with a BFS number and a region column."), year),
         call. = FALSE)

  heads <- tolower(trimws(as.character(unlist(raw[hdr, ]))))
  body <- as.data.frame(raw[-seq_len(hdr), ], stringsAsFactors = FALSE)
  col <- function(pattern, required = FALSE) {
    j <- grep(pattern, heads)
    if (!length(j) && required)
      stop(sprintf(paste("Column matching '%s' not found in the premium",
                         "region file %d; headers: %s."),
                   pattern, year, paste(heads, collapse = ", ")),
           call. = FALSE)
    if (length(j)) body[[j[1L]]] else NULL
  }

  gemId <- .asInt(gsub("[^0-9]", "", col("bfs", required = TRUE)))
  region <- .asInt(gsub("[^0-9]", "", col("region", required = TRUE)))
  canton <- .priminfoCanton(col("kanton|canton"))
  name <- col("gemeinde|commune")
  plz <- col("plz|npa|postleit")
  ort <- col("ortschaft|localit|^ort")

  ok <- !is.na(gemId) & !is.na(region)
  out <- data.frame(jahr_n = rep(year, sum(ok)), gem_id = gemId[ok],
                    gem_x = if (is.null(name)) NA_character_ else name[ok],
                    kanton_c = if (is.null(canton)) NA_character_
                               else canton[ok],
                    preg_c = region[ok], stringsAsFactors = FALSE)
  if (!is.null(plz)) out$plz_n <- .asInt(plz[ok])
  if (!is.null(ort)) out$ort_x <- ort[ok]
  if (!nrow(out))
    stop(sprintf("The premium region file %d contains no data rows.", year),
         call. = FALSE)
  if (anyNA(out$kanton_c))
    warning(sprintf("Canton missing for %d row(s) of the premium region file.",
                    sum(is.na(out$kanton_c))), call. = FALSE)
  out$preg_x <- paste0(out$kanton_c, out$preg_c)
  .resetRownames(out[order(out$gem_id), , drop = FALSE])
}


# First row (of the first 30) containing all patterns, else NA.
.findHeaderRow <- function(raw, patterns) {
  n <- min(nrow(raw), 30L)
  for (i in seq_len(n)) {
    cells <- tolower(as.character(unlist(raw[i, ])))
    cells <- cells[!is.na(cells)]
    if (all(vapply(patterns, function(p) any(grepl(p, cells)), TRUE)))
      return(i)
  }
  NA_integer_
}


# Canton as abbreviation, BFS number or German name -> abbreviation.
.priminfoCanton <- function(x) {
  if (is.null(x)) return(NULL)
  x <- trimws(x)
  out <- ifelse(toupper(x) %in% .kantonAbbrev, toupper(x), NA_character_)
  num <- .asInt(x)
  out[is.na(out) & !is.na(num) & num %in% 1:26] <-
    .kantonAbbrev[num[is.na(out) & !is.na(num) & num %in% 1:26]]
  kt <- kanton()
  byName <- kt$kanton_c[match(tolower(x), tolower(kt$kanton_x_de))]
  out[is.na(out)] <- byName[is.na(out)]
  out
}
