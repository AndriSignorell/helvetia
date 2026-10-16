#' Get the Population by Municipality, Sex and Age
#'
#' Returns the permanent (or non-permanent) resident population at the end
#' of the year by municipality, sex and single year of age, from the BFS
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
#' Large requests are split automatically to respect the cell limit of the
#' API. Each year is cached for the whole of Switzerland; `id` only filters
#' the result. Cells the source marks as unavailable are `NA`.
#'
#' @param year integer vector of statistical years. The default `NULL` uses
#'   the most recent year available.
#' @param id optional integer vector of BFS municipality numbers to keep;
#'   `NULL` returns all municipalities.
#' @param population population type, `"permanent"` (ständige
#'   Wohnbevölkerung) or `"non-permanent"` (nichtständige
#'   Wohnbevölkerung).
#' @inheritParams gemeinde
#'
#' @return A data.frame with the columns `jahr_n`, `gem_id`, `sex_c`
#'   (`"m"`, `"w"`), `age_n`, `age_open_l` and `ch_n` (number of persons).
#'   The key is `jahr_n` + `gem_id` + `sex_c` + `age_n`. Municipality numbers
#'   refer to the municipality stock of 31 December of the respective year.
#'
#' @family swiss.statistics
#' @concept population
#' @concept demographics
#' @concept municipality
#' @seealso [gemeinde()], [geoGemeinde()].
#' @examples
#' \dontrun{
#' pop <- popGemeinde(year = 2021)
#' aggregate(ch_n ~ gem_id, data = pop, FUN = sum)
#' }
#' @export
popGemeinde <- function(year = NULL, id = NULL,
                        population = c("permanent", "non-permanent"),
                        cache = TRUE, refresh = FALSE) {
  population <- match.arg(population)
  if (!is.null(year) && (!is.numeric(year) || anyNA(year)))
    stop("Argument 'year' must be a numeric vector without NA.",
         call. = FALSE)
  if (!is.null(id) && (!is.numeric(id) || anyNA(id)))
    stop("Argument 'id' must be a numeric vector without NA.", call. = FALSE)

  px  <- "px-x-0102010000_101"
  url <- .bfsUrl(px)
  meta <- .cached("pxweb", px, "metadata", cache = cache, refresh = refresh,
                  mode = "remote-first", fetch = function() .bfsMetadata(url))
  sel <- .popSelection(meta, year, population)

  if (!is.null(id)) {
    id <- as.integer(id)
    miss <- setdiff(id, as.integer(sel$region))
    if (length(miss))
      stop(sprintf("Argument 'id': municipality number(s) %s not in the cube.",
                   paste(head(miss, 10), collapse = ", ")), call. = FALSE)
  }

  parts <- lapply(sel$years, function(y) {
    .cached("pxweb", px, c(population, y), cache = cache, refresh = refresh,
            fetch = function() {
      query <- setNames(
        list(y, sel$region, sel$popType, sel$nat, sel$sex, sel$age),
        c(sel$var$year, sel$var$region, sel$var$popType, sel$var$nat,
          sel$var$sex, sel$var$age))
      .parsePop(.bfsQuery(url, query), sel)
    })
  })
  out <- do.call(rbind, parts)
  if (!is.null(id)) out <- out[out$gem_id %in% id, , drop = FALSE]
  .validateUniqueKey(out, c("jahr_n", "gem_id", "sex_c", "age_n"),
                     "popGemeinde()")

  .attachSourceMetadata(.resetRownames(out), .sourceMeta(
    sprintf("BFS PXWeb %s: %s", px, meta$title), url,
    version = paste(sel$years, collapse = ", "),
    notes = sprintf("population type: %s", sel$popTypeText)))
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

  ageTot <- grepl("Total", vAge$valueTexts, ignore.case = TRUE)
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
       nat = .bfsValue(vNat, "^Total$"),
       sex = sex, sexMap = setNames(c("m", "w"), sex),
       age = age, ageOpen = setNames(open, age))
}


.parsePop <- function(df, sel) {
  v <- sel$var
  .requireCols(df, c(v$year, v$region, v$sex, v$age, "value"),
               "PXWeb population response")
  out <- data.frame(
    jahr_n     = .asInt(df[[v$year]]),
    gem_id     = .asInt(df[[v$region]]),
    sex_c      = unname(sel$sexMap[df[[v$sex]]]),
    age_n      = .asInt(df[[v$age]]),
    age_open_l = unname(sel$ageOpen[df[[v$age]]]),
    ch_n       = as.integer(round(df$value)),
    stringsAsFactors = FALSE)
  if (anyNA(out$sex_c))
    stop("Unmapped sex code in the PXWeb response.", call. = FALSE)
  out[order(out$jahr_n, out$gem_id, out$sex_c, out$age_n), , drop = FALSE]
}
