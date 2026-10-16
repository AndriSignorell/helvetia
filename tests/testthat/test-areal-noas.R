test_that("internal NOAS04 copy has the full hierarchy", {
  n <- helvetia:::.parseNoas(helvetia:::.noasInternal())
  expect_identical(as.vector(table(n$level_n)), c(4L, 17L, 27L, 72L))
  n17 <- n[n$level_n == 17, ]
  expect_identical(n17$noas_n, 1:17)
  expect_identical(n17$noas_x_de[2], "Gebäudeareal")
  expect_true(all(n$parent_c[n$level_n > 4] %in% n$noas_c))
})

test_that("SDMX codelist XML is parsed with languages and parents", {
  doc <- xml2::read_xml(fixture("sdmx_codelist.xml"))
  cl <- helvetia:::.parseSdmxCodelist(doc, "CL_AREA_NOAS")
  expect_identical(cl$code, c("1", "101", "102"))
  expect_identical(cl$parent, c(NA, "1", "1"))
  expect_identical(cl$name_en[1], "Settlement areas")
  expect_true(is.na(cl$name_fr[1]))
})

test_that("inconsistent hierarchy is detected", {
  cl <- helvetia:::.noasInternal()
  cl$parent[cl$code == "101"] <- "2"
  expect_error(helvetia:::.parseNoas(cl), "hierarchy")
})

test_that("Arealstatistik: municipality filter, status, complete grid", {
  nom <- helvetia:::.parseNoas(helvetia:::.noasInternal())
  nom <- nom[nom$noas_c %in% c("1", "101", "102"), ]
  raw <- read_fixture_csv("sdmx_area.csv")
  a <- helvetia:::.parseArea(raw, nom, "2013/18", as.Date("2026-01-01"))

  expect_setequal(unique(a$gem_id), c(1L, 2L))           # ZH, B_101 dropped
  x <- a[a$gem_id == 2, ]
  expect_identical(x$value_status_c[x$noas_c == "101"], "suppressed")
  expect_identical(x$value_status_c[x$noas_c == "102"], "missing")
  y <- a[a$gem_id == 1, ]
  expect_identical(y$area_ha[y$noas_c == "101"], 0)      # true zero
  expect_identical(y$value_status_c[y$noas_c == "101"], "observed")
  expect_identical(unique(a$flight_year_x[a$gem_id == 1]), "2014")
})

test_that("class sums are checked against the main domains", {
  res <- data.frame(gem_id = c(1, 1), area_ha = c(40, 50))
  dom <- data.frame(gem_id = 1, area_ha = 100)
  expect_warning(sc <- helvetia:::.checkAreaSums(res, dom, 0.5), "differ")
  expect_identical(sc$check_c, "difference")
  dom$area_ha <- 90
  expect_silent(sc <- helvetia:::.checkAreaSums(res, dom, 0.5))
  expect_identical(sc$check_c, "ok")
})

test_that("incomplete or missing sums are not checkable, not 'ok'", {
  res <- data.frame(gem_id = c(1, 1, 2, 2), area_ha = c(NA, NA, 40, NA))
  dom <- data.frame(gem_id = c(1, 2), area_ha = c(NA, 90))
  expect_warning(sc <- helvetia:::.checkAreaSums(res, dom, 0.5),
                 "could not be checked")
  expect_identical(sc$check_c, c("not-checkable", "not-checkable"))
})

test_that("duplicated raw observations are an error, not a silent pick", {
  nom <- helvetia:::.parseNoas(helvetia:::.noasInternal())
  nom <- nom[nom$noas_c %in% c("1", "101", "102"), ]
  raw <- read_fixture_csv("sdmx_area.csv")
  dup <- raw[raw$REGION == "1" & raw$NOAS == "101", ]
  dup$OBS_VALUE <- 999
  expect_error(helvetia:::.parseArea(rbind(raw, dup), nom, "2013/18",
                                     as.Date("2026-01-01")), "duplicated")
})

test_that("period labels and municipality stand", {
  per <- data.frame(code = c("A4", "A5"), name_de = c("2013-2018", "2020-2025"))
  expect_identical(helvetia:::.areaPeriod(per, "2013/18")$code, "A4")
  expect_identical(helvetia:::.areaPeriod(per, NULL)$x, "2020/25")
  expect_error(helvetia:::.areaPeriod(per, "2013"), "not available")
  expect_identical(
    helvetia:::.standFromCodelist("CL_AREA_HIER_REGION_2026_DIFF"),
    as.Date("2026-01-01"))
  expect_identical(helvetia:::.areaGemId(c("8100", "CH04", "ZH", "B_101", "261")),
                   c(NA, NA, NA, NA, 261L))
})

test_that("live: arealGemeinde() and noas()", {
  skip_if_not_live()
  withr::local_options(helvetia.cacheDir = withr::local_tempdir())
  expect_identical(nrow(noas(72)), 72L)
  a <- arealGemeinde("2013/18", nClasses = 17)
  expect_gt(length(unique(a$gem_id)), 2000)
})

test_that("SDMX dimensions are read in position order", {
  doc <- xml2::read_xml(paste0(
    '<s:Structure xmlns:s="urn:s"><s:DimensionList>',
    '<s:Dimension id="REGION" position="3"><s:LocalRepresentation><s:Enumeration>',
    '<Ref id="CL_AREA_HIER_REGION_2026_DIFF"/></s:Enumeration></s:LocalRepresentation></s:Dimension>',
    '<s:Dimension id="NOAS" position="1"><s:LocalRepresentation><s:Enumeration>',
    '<Ref id="CL_AREA_NOAS"/></s:Enumeration></s:LocalRepresentation></s:Dimension>',
    '</s:DimensionList></s:Structure>'))
  d <- helvetia:::.parseSdmxDimensions(doc)
  expect_identical(d$id, c("NOAS", "REGION"))
  expect_identical(d$codelist[2], "CL_AREA_HIER_REGION_2026_DIFF")
})
