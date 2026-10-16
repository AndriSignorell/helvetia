meta <- function() helvetia:::.parseBfsMetadata(read_fixture_json("pxweb_meta.json"))

test_that("selection is resolved through metadata, not positions", {
  s <- helvetia:::.popSelection(meta(), NULL, "permanent")
  expect_identical(s$years, "2021")
  expect_identical(s$region, c("0001", "0002"))
  expect_identical(s$popType, "1")
  expect_identical(s$nat, "-99999")
  expect_identical(unname(s$sexMap[c("1", "2")]), c("m", "w"))
  expect_identical(s$age, c("0", "1", "100"))
  expect_identical(unname(s$ageOpen), c(FALSE, FALSE, TRUE))

  expect_identical(helvetia:::.popSelection(meta(), 2020, "non-permanent")$popType,
                   "2")
  expect_error(helvetia:::.popSelection(meta(), 1999, "permanent"),
               "not available")
})

test_that("response is mapped to the data model", {
  s <- helvetia:::.popSelection(meta(), 2021, "permanent")
  df <- helvetia:::.parseBfsData(read_fixture_json("pxweb_data.json"))
  p <- helvetia:::.parsePop(df, s, as.Date("2025-04-06"))

  expect_named(p, c("jahr_n", "gem_stand_d", "gem_id", "sex_c", "age_n",
                    "age_open_l", "ch_n"))
  expect_identical(unique(p$gem_stand_d), as.Date("2025-04-06"))
  expect_identical(p$sex_c, c("m", "m", "w", "w"))
  expect_identical(p$age_open_l, c(FALSE, TRUE, FALSE, TRUE))
  expect_identical(p$ch_n, c(7L, 1L, 9L, NA))     # "..." -> NA
})

test_that("variables are found by name and ambiguity is an error", {
  m <- meta()
  expect_identical(helvetia:::.bfsVariable(m, "Gemeinde")$code,
                   "Kanton (-) / Bezirk (>>) / Gemeinde (......)")
  expect_error(helvetia:::.bfsVariable(m, "Raumtyp"), "not found")
  expect_error(helvetia:::.bfsVariable(m, "e"), "ambiguous")
})

test_that("queries are chunked under the cell limit", {
  q <- list(a = 1:3, b = as.character(1:1000), c = 1:2)
  ch <- helvetia:::.chunkQuery(q, 1000)
  expect_true(all(vapply(ch, function(x) prod(lengths(x)), 0) <= 1000))
  expect_identical(unlist(lapply(ch, `[[`, "b"), use.names = FALSE), q$b)
  expect_length(helvetia:::.chunkQuery(q, 1e6), 1L)
})

test_that("the spatial reference is read from the table page", {
  html <- paste("<div><span>Stichtag: 31. Dezember</span>",
                "<span>Raumbezug:</span> <span>Gemeinden / 06.04.2025</span>",
                "</div>")
  r <- helvetia:::.parseSpatialRef(html)
  expect_identical(r$date, as.Date("2025-04-06"))
  expect_match(r$text, "Gemeinden / 06.04.2025")
  expect_true(is.na(helvetia:::.parseSpatialRef("<p>nothing</p>")$date))
})

local_pop_mocks <- function(env = parent.frame()) {
  withr::local_options(helvetia.cacheDir = withr::local_tempdir(.local_envir = env),
                       .local_envir = env)
  calls <- new.env()
  calls$n <- 0L
  local_mocked_bindings(
    .bfsMetadata = function(url) meta(),
    .bfsSpatialRef = function(px) list(date = as.Date("2025-04-06"),
                                       text = "Raumbezug: Gemeinden / 06.04.2025"),
    .bfsQuery = function(url, query) {
      calls$n <- calls$n + 1L
      df <- helvetia:::.parseBfsData(read_fixture_json("pxweb_data.json"))
      attr(df, "queries") <- "{\"query\":[]}"
      df
    }, .env = env)
  calls
}

test_that("popGemeinde: stand column, query and original retrievedAt kept", {
  calls <- local_pop_mocks()
  p1 <- popGemeinde(2021)
  expect_identical(unique(p1$gem_stand_d), as.Date("2025-04-06"))
  si1 <- sourceInfo(p1)
  expect_identical(si1$query, "{\"query\":[]}")
  expect_true(any(grepl("06.04.2025", si1$notes)))
  Sys.sleep(1.1)
  p2 <- popGemeinde(2021)                               # cache hit
  expect_identical(calls$n, 1L)
  expect_identical(sourceInfo(p2)$retrievedAt, si1$retrievedAt)
})

test_that("popGemeinde: years and ids must be whole numbers", {
  local_pop_mocks()
  expect_error(popGemeinde(year = 2021.9), "'year'")
  expect_error(popGemeinde(id = 1.5), "'id'")
  expect_error(popGemeinde(id = NA_real_), "'id'")
  expect_error(popGemeinde(2021, id = 9999), "'id'")
})

test_that("key validation rejects missing key values", {
  x <- data.frame(a = c(1, NA), b = 1:2)
  expect_error(helvetia:::.validateUniqueKey(x, c("a", "b")), "missing")
  expect_true(helvetia:::.validateUniqueKey(x, c("a", "b"), allowNA = "a"))
})

test_that("live: popGemeinde()", {
  skip_if_not_live()
  withr::local_options(helvetia.cacheDir = withr::local_tempdir())
  p <- popGemeinde(2021, id = 261)
  expect_identical(unique(p$gem_id), 261L)
  expect_true(any(p$age_open_l))
  expect_false(anyNA(p$gem_stand_d))
  expect_gt(unique(p$gem_stand_d), as.Date("2021-12-31"))
})
