meta <- function() helvetia:::.parse_bfs_metadata(read_fixture_json("pxweb_meta.json"))

test_that("selection is resolved through metadata, not positions", {
  s <- helvetia:::.pop_selection(meta(), NULL, "permanent")
  expect_identical(s$years, "2021")
  expect_identical(s$region, c("0001", "0002"))
  expect_identical(s$poptype, "1")
  expect_identical(s$nat, "-99999")
  expect_identical(unname(s$sex_map[c("1", "2")]), c("m", "w"))
  expect_identical(s$age, c("0", "1", "100"))
  expect_identical(unname(s$age_open), c(FALSE, FALSE, TRUE))

  expect_identical(helvetia:::.pop_selection(meta(), 2020, "non-permanent")$poptype,
                   "2")
  expect_error(helvetia:::.pop_selection(meta(), 1999, "permanent"),
               "not available")
})

test_that("response is mapped to the data model", {
  s <- helvetia:::.pop_selection(meta(), 2021, "permanent")
  df <- helvetia:::.parse_bfs_data(read_fixture_json("pxweb_data.json"))
  p <- helvetia:::.parse_pop(df, s)

  expect_named(p, c("jahr_n", "gem_id", "sex_c", "age_n", "age_open_l", "ch_n"))
  expect_identical(p$sex_c, c("m", "m", "w", "w"))
  expect_identical(p$age_open_l, c(FALSE, TRUE, FALSE, TRUE))
  expect_identical(p$ch_n, c(7L, 1L, 9L, NA))     # "..." -> NA
})

test_that("variables are found by name and ambiguity is an error", {
  m <- meta()
  expect_identical(helvetia:::.bfs_variable(m, "Gemeinde")$code,
                   "Kanton (-) / Bezirk (>>) / Gemeinde (......)")
  expect_error(helvetia:::.bfs_variable(m, "Raumtyp"), "not found")
  expect_error(helvetia:::.bfs_variable(m, "e"), "ambiguous")
})

test_that("queries are chunked under the cell limit", {
  q <- list(a = 1:3, b = as.character(1:1000), c = 1:2)
  ch <- helvetia:::.chunk_query(q, 1000)
  expect_true(all(vapply(ch, function(x) prod(lengths(x)), 0) <= 1000))
  expect_identical(unlist(lapply(ch, `[[`, "b"), use.names = FALSE), q$b)
  expect_length(helvetia:::.chunk_query(q, 1e6), 1L)
})

test_that("live: pop_gemeinde()", {
  skip_if_not_live()
  withr::local_options(helvetia.cache_dir = withr::local_tempdir())
  p <- pop_gemeinde(2021, gem_id = 261)
  expect_identical(unique(p$gem_id), 261L)
  expect_true(any(p$age_open_l))
})
