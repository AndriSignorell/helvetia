test_that("premium region file is parsed by header names", {
  r <- helvetia:::.parsePriminfo(fixture("praemienregionen.xlsx"), 2026L)
  expect_named(r, c("jahr_n", "gem_id", "gem_x", "kanton_c", "preg_c",
                    "plz_n", "ort_x", "preg_x"))
  expect_identical(nrow(r), 8L)                    # footnote row dropped
  expect_identical(r$preg_x[r$gem_id == 6621], "GE0")
  expect_identical(r$gem_x[r$gem_id == 261][1], "Zürich")
})

test_that("municipality level: one row per region, split flagged", {
  withr::local_options(helvetia.cacheDir = withr::local_tempdir())
  local_mocked_bindings(
    .downloadFirst = function(urls, dest) {
      file.copy(fixture("praemienregionen.xlsx"), dest, overwrite = TRUE)
      list(path = dest, url = urls[1], retrievedAt = Sys.time(), md5 = "x")
    })
  p <- praemienregion(2026)
  expect_identical(p$gem_id, c(1L, 2L, 261L, 351L, 999L, 999L, 6621L))
  expect_identical(p$split_l, c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(sourceInfo(p)$url,
                   "https://www.priminfo.admin.ch/downloads/praemienregionen-2026.xlsx")
  o <- praemienregion(2026, output = "ortschaft")
  expect_identical(nrow(o), 8L)
  expect_error(praemienregion(2003), "2004")
  expect_error(praemienregion(output = "kanton"), "should be one of")
})

test_that("canton given as name or number is mapped", {
  expect_identical(helvetia:::.priminfoCanton(c("zh", "2", "Graubünden", "xx")),
                   c("ZH", "BE", "GR", NA))
})

test_that("live: praemienregion()", {
  skip_if_not_live()
  withr::local_options(helvetia.cacheDir = withr::local_tempdir())
  p <- praemienregion(2025)
  expect_gt(length(unique(p$gem_id)), 2000)
  expect_true(all(p$preg_c %in% 0:3))
})
