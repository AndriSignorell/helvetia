#' Population by municipality, sex and age
#'
#' Permanent (or non-permanent) resident population at the end of the year
#' by municipality, sex and single year of age, from the BFS PXWeb cube
#' `px-x-0102010000_101` (STATPOP).
#'
#' All selections are made through the cube metadata: variables are found by
#' name, municipalities by their level marker in the value labels (not by
#' position), requested years are validated against the available years and
#' sex is mapped explicitly (`1 = "m"`, `2 = "w"`). The open top age class
#' ("100 Jahre und mehr") is kept as `age_n = 100` and flagged with
#' `age_open_l = TRUE`; it must not be read as exact age 100.
#'
#' Large requests are split automatically to respect the cell limit of the
#' API. Each year is cached separately for the whole of Switzerland;
#' `gem_id` only filters the result.
#'
#' @param jahr_n integer vector of statistical years; `NULL` uses the most
#'   recent year available
#' @param gem_id optional integer vector of BFS municipality numbers to
#'   keep; `NULL` returns all municipalities
#' @param population `"permanent"` (ständige Wohnbevölkerung, default) or
#'   `"non-permanent"` (nichtständige Wohnbevölkerung)
#' @inheritParams gemeinde
#'
#' @return A data.frame with the columns `jahr_n`, `gem_id`, `sex_c`
#'   (`"m"`, `"w"`), `age_n`, `age_open_l` and `ch_n` (number of persons).
#'   The key is `jahr_n` + `gem_id` + `sex_c` + `age_n`. Municipality
#'   numbers refer to the municipality stock of the respective year
#'   (31 December).
#'
#' @seealso [gemeinde()], [geo_gemeinde()]
#' @examples
#' \dontrun{
#' pop <- pop_gemeinde(jahr_n = 2021)
#' aggregate(ch_n ~ gem_id, data = pop, FUN = sum)
#' }
#' @export
pop_gemeinde <- function(jahr_n = NULL, gem_id = NULL,
                         population = c("permanent", "non-permanent"),
                         cache = TRUE, refresh = FALSE) {
  population <- match.arg(population)
  px  <- "px-x-0102010000_101"
  url <- .bfs_url(px)

  meta <- .cached("pxweb", px, "metadata", cache = cache, refresh = refresh,
                  mode = "remote-first", fetch = function() .bfs_metadata(url))
  sel <- .pop_selection(meta, jahr_n, population)

  if (!is.null(gem_id)) {
    gem_id <- as.integer(gem_id)
    miss <- setdiff(gem_id, as.integer(sel$region))
    if (length(miss))
      stop(sprintf("municipality number(s) not in the cube: %s",
                   paste(utils::head(miss, 10), collapse = ", ")),
           call. = FALSE)
  }

  parts <- lapply(sel$years, function(y) {
    .cached("pxweb", px, c(population, y), cache = cache, refresh = refresh,
            fetch = function() {
      query <- stats::setNames(
        list(y, sel$region, sel$poptype, sel$nat, sel$sex, sel$age),
        c(sel$var$year, sel$var$region, sel$var$poptype, sel$var$nat,
          sel$var$sex, sel$var$age))
      .parse_pop(.bfs_query(url, query), sel)
    })
  })
  out <- do.call(rbind, parts)
  if (!is.null(gem_id)) out <- out[out$gem_id %in% gem_id, , drop = FALSE]
  .validate_unique_key(out, c("jahr_n", "gem_id", "sex_c", "age_n"),
                       "pop_gemeinde()")

  .attach_source_metadata(.reset_rownames(out), .source_meta(
    sprintf("BFS PXWeb %s: %s", px, meta$title), url,
    version = paste(sel$years, collapse = ", "),
    notes = sprintf("population type: %s", sel$poptype_x)))
}


# Resolve all selections against the cube metadata.
.pop_selection <- function(meta, jahr_n, population) {
  v_year <- .bfs_variable(meta, "^Jahr$")
  v_reg  <- .bfs_variable(meta, "Gemeinde")
  v_pop  <- .bfs_variable(meta, "lkerungstyp")
  v_nat  <- .bfs_variable(meta, "Staatsangeh")
  v_sex  <- .bfs_variable(meta, "^Geschlecht$")
  v_age  <- .bfs_variable(meta, "^Alter$")

  years <- if (is.null(jahr_n)) max(as.integer(v_year$values)) else
    as.integer(jahr_n)
  bad <- setdiff(years, as.integer(v_year$values))
  if (length(bad))
    stop(sprintf("year(s) %s not available; available: %s",
                 paste(bad, collapse = ", "),
                 paste(range(v_year$values), collapse = "-")), call. = FALSE)

  # municipalities are marked with "......" in the value labels
  is_gem <- startsWith(v_reg$valueTexts, "......") &
    grepl("^[0-9]+$", v_reg$values)
  if (!any(is_gem))
    stop("no municipality level found in the PXWeb region variable",
         call. = FALSE)

  # ASCII-only patterns: "Ständige" vs. "Nichtständige Wohnbevölkerung"
  pop_pat <- if (population == "permanent") "^St" else "^Nicht"
  poptype <- .bfs_value(v_pop, pop_pat)

  sex <- c(.bfs_value(v_sex, "^Mann$"), .bfs_value(v_sex, "^Frau$"))
  sex_map <- stats::setNames(c("m", "w"), sex)

  age_tot <- grepl("Total", v_age$valueTexts, ignore.case = TRUE)
  age <- v_age$values[!age_tot]
  age_txt <- v_age$valueTexts[!age_tot]
  age_n <- .as_int(age)
  if (anyNA(age_n))
    stop("unexpected non-numeric age codes in PXWeb metadata", call. = FALSE)
  open <- grepl("und mehr|oder mehr|\\+|plus", age_txt, ignore.case = TRUE)
  if (!open[which.max(age_n)])
    warning("top age class is not labelled as open-ended; check the cube",
            call. = FALSE)

  list(var = list(year = v_year$code, region = v_reg$code,
                  poptype = v_pop$code, nat = v_nat$code,
                  sex = v_sex$code, age = v_age$code),
       years = as.character(years),
       region = v_reg$values[is_gem],
       poptype = poptype,
       poptype_x = v_pop$valueTexts[v_pop$values == poptype],
       nat = .bfs_value(v_nat, "^Total$"),
       sex = sex, sex_map = sex_map,
       age = age, age_open = stats::setNames(open, age))
}


.parse_pop <- function(df, sel) {
  v <- sel$var
  .require_cols(df, c(v$year, v$region, v$sex, v$age, "value"),
                "PXWeb population response")
  out <- data.frame(
    jahr_n     = .as_int(df[[v$year]]),
    gem_id     = .as_int(df[[v$region]]),
    sex_c      = unname(sel$sex_map[df[[v$sex]]]),
    age_n      = .as_int(df[[v$age]]),
    age_open_l = unname(sel$age_open[df[[v$age]]]),
    ch_n       = as.integer(round(df$value)),
    stringsAsFactors = FALSE)
  if (anyNA(out$sex_c))
    stop("unmapped sex code in PXWeb response", call. = FALSE)
  out[order(out$jahr_n, out$gem_id, out$sex_c, out$age_n), , drop = FALSE]
}
