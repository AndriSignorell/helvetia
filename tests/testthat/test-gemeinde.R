test_that("snapshot is parsed via the Parent hierarchy", {
  raw <- read_fixture_csv("agvch_snapshot.csv")
  g <- helvetia:::.parse_agvch_snapshot(raw, as.Date("2021-01-01"))

  expect_identical(g$gem_id, c(1L, 2L, 2761L))
  expect_identical(g$kanton_c, c("ZH", "ZH", "BL"))
  expect_identical(g$bezirk_id, c(101L, 101L, 1301L))
  expect_identical(g$gem_x[1], "Aeugst am Albis")
  expect_s3_class(g$valid_from_d, "Date")
  expect_true(all(is.na(g$valid_to_d)))
  expect_true(all(g$stand_d == as.Date("2021-01-01")))
})

test_that("duplicate municipalities per stand are rejected", {
  raw <- read_fixture_csv("agvch_snapshot.csv")
  raw <- rbind(raw, raw[raw$BfsCode == "1" & raw$Parent == "10053", ])
  expect_error(helvetia:::.parse_agvch_snapshot(raw, Sys.Date()),
               "not unique")
})

test_that("schema changes give an informative error", {
  raw <- read_fixture_csv("agvch_snapshot.csv")
  raw$Parent <- NULL
  expect_error(helvetia:::.parse_agvch_snapshot(raw, Sys.Date()),
               "Parent")
})

test_that("mutations: one row per predecessor/successor, typed", {
  raw <- read_fixture_csv("agvch_mutations.csv")
  m <- helvetia:::.parse_agvch_mutations(raw)

  expect_identical(nrow(m), 4L)
  expect_identical(unique(m$mutation_type_c[m$mutation_id == 3700]), "merger")
  expect_identical(m$old_gem_id[m$mutation_id == 3700], c(4021L, 4022L))
  expect_identical(m$mutation_type_c[m$mutation_id == 3701],
                   "rename+reassign")
  expect_identical(m$mutation_type_c[m$mutation_id == 3702], "abolition")
  expect_true(is.na(m$new_gem_id[m$mutation_id == 3702]))
})

test_that("levels are returned in long format and filtered by name", {
  raw <- data.frame(HistoricalCode = c("1", "2"), BfsCode = c("1", "2"),
                    Name = c("A", "B"), GDETYP2020_9 = c("11", "12"),
                    SPRGEB2020 = c("1", ""), stringsAsFactors = FALSE)
  r <- helvetia:::.parse_agvch_levels(raw, as.Date("2021-01-01"),
                                      typ = "gdetyp")
  expect_identical(unique(r$typ_c), "GDETYP2020_9")
  expect_identical(r$value_x, c("11", "12"))
  all <- helvetia:::.parse_agvch_levels(raw, as.Date("2021-01-01"))
  expect_true(is.na(all$value_x[all$typ_c == "SPRGEB2020" & all$gem_id == 2]))
  expect_error(helvetia:::.parse_agvch_levels(raw, Sys.Date(), "XYZ"),
               "not available")
})

test_that("dates and AGVCH request format", {
  expect_identical(helvetia:::.agvch_date(as.Date("2021-01-01")), "01-01-2021")
  expect_identical(helvetia:::.parse_date_any(c("12.09.1848", "", "2021-01-01")),
                   as.Date(c("1848-09-12", NA, "2021-01-01")))
  expect_error(helvetia:::.parse_date_any("31.31.2020"), "unparseable")
  expect_error(gemeinde(stand_d = "kein Datum"), "stand_d")
})

test_that("live: gemeinde() and gemeinde_mutationen()", {
  skip_if_not_live()
  withr::local_options(helvetia.cache_dir = withr::local_tempdir())
  g <- gemeinde(as.Date("2021-01-01"))
  expect_gt(nrow(g), 2000)
  expect_setequal(unique(g$kanton_c), helvetia:::.kanton_abbrev)
  m <- gemeinde_mutationen("2020-01-01", "2021-01-01")
  expect_true(all(m$mutation_d >= as.Date("2020-01-01")))
})
