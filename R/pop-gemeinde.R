#' Get the Population by Municipality, Sex and Age
#'
#' Returns the permanent (or non-permanent) resident population on 31
#' December by municipality, sex and single year of age, from the BFS
#' population statistics STATPOP. It is the standard denominator for rates
#' per municipality and the basis for age-standardisation.
#'
#' The data come from the BFS PXWeb cube `px-x-0102010000_101`. All
#' selections are made through the cube metadata: variables are found by
#' name, municipalities by their level marker in the value labels (not by
#' position), the requested years are validated against the available ones
#' and sex is mapped explicitly (`1 = "m"`, `2 = "w"`). The open top age
#' class ("100 Jahre und mehr") is kept as `age_n = 100` and flagged with
#' `age_open_l = TRUE`; it must not be read as the exact age 100.
#'
#' **Spatial reference.** The reference date of the counts (31 December,
#' `jahr_n`) and the municipality stand of the cube are two different
#' things. The BFS publishes the whole time series on *one* municipality
#' stand (e.g. "Raumbezug: Gemeinden / 06.04.2025"), not on the stand of the
#' respective year. That stand is read from the table page and returned in
#' `gem_stand_d`; join geometries and registers of that stand, e.g.
#' `geoGemeinde(unique(pop$gem_stand_d))`. If the page cannot be read,
#' `gem_stand_d` is `NA` and a warning is given. An unchanged BFS number does
#' not guarantee unchanged boundaries, so always join on the stand, not on
#' the year.
#'
#' Large requests are split automatically to respect the cell limit of the
#' API. Each year is cached for the whole of Switzerland and per spatial
#' reference; `id` only filters the result. Cells the source marks as
#' unavailable are `NA`. [sourceInfo()] keeps the POST selections and the
#' time of the original download of each year.
#'
#' @param year vector of statistical years (whole numbers). The default
#'   `NULL` uses the most recent year available.
#' @param id optional vector of BFS municipality numbers (whole numbers) to
#'   keep; `NULL` returns all municipalities.
#' @param population population type, `"permanent"` (ständige
#'   Wohnbevölkerung) or `"non-permanent"` (nichtständige
#'   Wohnbevölkerung).
#' @inheritParams gemeinde
#'
#' @return A data.frame with the columns `jahr_n`, `gem_stand_d`, `gem_id`,
#'   `sex_c` (`"m"`, `"w"`), `age_n`, `age_open_l` and `ch_n` (number of
#'   persons). The key is `jahr_n` + `gem_stand_d` + `gem_id` + `sex_c` +
#'   `age_n`.
#'
#' @family swiss.statistics
#' @concept population
#' @concept demographics
#' @concept municipality
#' @seealso [gemeinde()], [geoGemeinde()].
#' @examples
#' \dontrun{
#' pop <- popGemeinde(year = 2021)
#' unique(pop$gem_stand_d)               # spatial reference of the cube
#' popTotal <- aggregate(ch_n ~ gem_id, data = pop, FUN = sum)
#' geo <- geoGemeinde(unique(pop$gem_stand_d))
#' }
#' @export
popGemeinde <- function(year = NULL, id = NULL,
                        population = c("permanent", "non-permanent"),
                        cache = TRUE, refresh = FALSE) {
  population <- match.arg(population)
  .checkWhole(year, "year")
  .checkWhole(id, "id")

  px  <- "px-x-0102010000_101"
  url <- .bfsUrl(px)
  meta <- .cached("pxweb", px, "metadata", cache = cache, refresh = refresh,
                  mode = "remote-first", fetch = function() {
    m <- .bfsMetadata(url)
    m$spatial <- .bfsSpatialRef(px)
    if (is.na(m$spatial$date)) {           # page unreachable: keep the known
      old <- tryCatch(readRDS(.cachePath("pxweb", px, "metadata")),
                      error = function(e) NULL)
      if (!is.null(old$spatial)) m$spatial <- old$spatial
    }
    m
  })
  sel <- .popSelection(meta, year, population)
  stand <- meta$spatial$date
  if (is.na(stand))
    warning(paste("The spatial reference (municipality stand) of the cube",
                  "could not be read from the table page; gem_stand_d is NA."),
            call. = FALSE)

  if (!is.null(id)) {
    id <- as.integer(id)
    miss <- setdiff(id, as.integer(sel$region))
    if (length(miss))
      stop(sprintf("Argument 'id': municipality number(s) %s not in the cube.",
                   paste(head(miss, 10), collapse = ", ")), call. = FALSE)
  }

  parts <- lapply(sel$years, function(y) {
    .cached("pxweb", px, c(population, y, format(stand)), cache = cache,
            refresh = refresh, fetch = function() {
      query <- setNames(
        list(y, sel$region, sel$popType, sel$nat, sel$sex, sel$age),
        c(sel$var$year, sel$var$region, sel$var$popType, sel$var$nat,
          sel$var$sex, sel$var$age))
      raw <- .bfsQuery(url, query)
      .attachSourceMetadata(.parsePop(raw, sel, stand), .sourceMeta(
        sprintf("BFS PXWeb %s: %s", px, meta$title), url, version = y,
        query = attr(raw, "queries")))
    })
  })
  metas <- lapply(parts, sourceInfo)
  out <- do.call(rbind, parts)
  if (!is.null(id)) out <- out[out$gem_id %in% id, , drop = FALSE]
  .validateUniqueKey(out, c("jahr_n", "gem_stand_d", "gem_id", "sex_c",
                            "age_n"), "popGemeinde()")

  .attachSourceMetadata(.resetRownames(out), .sourceMeta(
    metas[[1L]]$source, url,
    retrievedAt = do.call(c, lapply(metas, `[[`, "retrievedAt")),
    version = vapply(metas, `[[`, "", "version"),
    query = unlist(lapply(metas, `[[`, "query")),
    notes = c(sprintf("population type: %s", sel$popTypeText),
              "reference date of the counts: 31 December",
              meta$spatial$text %||% NA_character_)))
}


# Resolve all selections against the cube metadata.
.popSelection <- function(meta, year, population) {
  vYear <- .bfsVariable(meta, "^Jahr$")
  vReg  <- .bfsVariable(meta, "Gemeinde")
  vPop  <- .bfsVariable(meta, "lkerungstyp")
  vNat  <- .bfsVariable(meta, "Staatsangeh")
  vSex  <- .bfsVariable(meta, "^Geschlecht$")
  vAge  <- .bfsVariable(meta, "^Alter$")

  avail <- as.integer(vYear$values)
  years <- if (is.null(year)) max(avail) else as.integer(year)
  bad <- setdiff(years, avail)
  if (length(bad))
    stop(sprintf("Argument 'year': %s not available; available are %s.",
                 paste(bad, collapse = ", "),
                 paste(range(avail), collapse = "-")), call. = FALSE)

  # municipalities are marked with "......" in the value labels
  isGem <- startsWith(vReg$valueTexts, "......") &
    grepl("^[0-9]+$", vReg$values)
  if (!any(isGem))
    stop("No municipality level found in the PXWeb region variable.",
         call. = FALSE)

  # ASCII-only patterns: "Staendige" vs. "Nichtstaendige Wohnbevoelkerung"
  popType <- .bfsValue(vPop, if (population == "permanent") "^St" else "^Nicht")

  sex <- c(.bfsValue(vSex, "^Mann$"), .bfsValue(vSex, "^Frau$"))

  ageTot <- vAge$values == .bfsTotal(vAge)
  age <- vAge$values[!ageTot]
  ageText <- vAge$valueTexts[!ageTot]
  ageNum <- .asInt(age)
  if (anyNA(ageNum))
    stop("Unexpected non-numeric age codes in the PXWeb metadata.",
         call. = FALSE)
  open <- grepl("und mehr|oder mehr|\\+|plus", ageText, ignore.case = TRUE)
  if (!open[which.max(ageNum)])
    warning("The top age class is not labelled as open-ended; check the cube.",
            call. = FALSE)

  list(var = list(year = vYear$code, region = vReg$code,
                  popType = vPop$code, nat = vNat$code,
                  sex = vSex$code, age = vAge$code),
       years = as.character(years),
       region = vReg$values[isGem],
       popType = popType,
       popTypeText = vPop$valueTexts[vPop$values == popType],
       nat = .bfsTotal(vNat),
       sex = sex, sexMap = setNames(c("m", "w"), sex),
       age = age, ageOpen = setNames(open, age))
}


.parsePop <- function(df, sel, stand = as.Date(NA)) {
  v <- sel$var
  .requireCols(df, c(v$year, v$region, v$sex, v$age, "value"),
               "PXWeb population response")
  out <- data.frame(
    jahr_n      = .asInt(df[[v$year]]),
    gem_stand_d = rep(as.Date(stand), nrow(df)),
    gem_id      = .asInt(df[[v$region]]),
    sex_c      = unname(sel$sexMap[df[[v$sex]]]),
    age_n      = .asInt(df[[v$age]]),
    age_open_l = unname(sel$ageOpen[df[[v$age]]]),
    ch_n       = as.integer(round(df$value)),
    stringsAsFactors = FALSE)
  if (anyNA(out$sex_c))
    stop("Unmapped sex code in the PXWeb response.", call. = FALSE)
  out[order(out$jahr_n, out$gem_id, out$sex_c, out$age_n), , drop = FALSE]
}
