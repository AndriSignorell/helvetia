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
  p <- helvetia:::.parsePop(df, s)

  expect_named(p, c("jahr_n", "gem_id", "sex_c", "age_n", "age_open_l", "ch_n"))
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

test_that("live: popGemeinde()", {
  skip_if_not_live()
  withr::local_options(helvetia.cacheDir = withr::local_tempdir())
  p <- popGemeinde(2021, id = 261)
  expect_identical(unique(p$gem_id), 261L)
  expect_true(any(p$age_open_l))
})
