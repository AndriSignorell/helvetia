test_that("ThemaKart file names are parsed into layers", {
  f <- c("01_INST/Gesamtfläche_gf/K4_polg20230101_gf/K4polg20230101gf_ch2007Poly.shp",
         "x/K4kant20220101vf_ch2007Pnts.shp",
         "x/K4bezk20001205zg_ch2007Pnts.shp",
         "x/K4amgr_20180101gf_ch2007Poly.shp",
         "x/K4_bgbr20190101gf_ch2007Poly.shp",
         "x/k4flusyyyymmdd33_ch2007.shp",
         "x/K4voge20200101gf.shp",
         "x/readme.shp")
  i <- helvetia:::.parseThemakartFiles(f)
  expect_identical(i$code, c("polg", "kant", "bezk", "amgr", "bgbr", "flus", "voge"))
  expect_identical(i$area[1:2], c("total", "vegetation"))
  expect_identical(i$output[1:3], c("polygon", "point", "point"))
  expect_identical(i$date[1], as.Date("2023-01-01"))
  expect_true(is.na(i$date[6]))
  expect_identical(i$category[6], 3L)
})

test_that("catalogue search results are reduced to the yearly sets", {
  item <- function(title, id, emb)
    list(description = list(titles = list(main = title)),
         ids = list(damId = id), bfs = list(embargo = emb),
         links = list(list(rel = "master",
                           href = sprintf("https://x/%s/master", id))))
  s <- helvetia:::.parseThemakartSets(list(
    item("Kartengeometrien ThemaKart - Set 2026", 36390072, "2026-02-02T07:30:00Z"),
    item("Kartengeometrien ThemaKart - Set «PRO» 2019", 7546178, "2019-02-25"),
    item("Kartengeometrien ThemaKart - Set 2025", 33807959, "2025-02-14T07:30:00Z")))
  expect_identical(s$set_n, c(2025L, 2026L))
  expect_identical(s$url[2], "https://x/36390072/master")
})

test_that("geoThemakart reads the layer valid at date", {
  local_themakart()
  kt <- geoThemakart("kanton")
  expect_s3_class(kt, "sf")
  expect_named(sf::st_drop_geometry(kt),
               c("stand_d", "kanton_id", "kanton_x", "kanton_c"))
  expect_identical(kt$kanton_c, c("ZH", "BE", "LU"))
  expect_identical(kt$kanton_x[1], "Zürich")
  expect_identical(unique(kt$stand_d), as.Date("2023-01-01"))
  expect_identical(attr(kt, "themakart")$setYear, 2023L)

  expect_message(old <- geoThemakart("kant", date = "2022-06-30"),
                 "2022-01-01")
  expect_identical(unique(old$stand_d), as.Date("2022-01-01"))
  expect_identical(nrow(geoThemakart("kanton", output = "point")), 3L)
  expect_error(geoThemakart("kanton", date = "2010-01-01"), "available stands")
  expect_error(geoThemakart("bezirk"), "No bezk layer")
  expect_error(geoThemakart("nirgendwo"), "unknown unit")
  expect_error(geoThemakart("kanton", area = "wald"), "should be one of")

  g <- geoGemeinde()
  expect_identical(g$gem_id, c(1L, 2L, 351L))
  expect_identical(sf::st_crs(geoKanton(crs = 4326))$epsg, 4326L)
  expect_false(is.null(attr(geoKanton(crs = 4326), "themakart")))
})

test_that("waters are read and harmonised to LV95", {
  local_themakart()
  fl <- geoGewaesser("fluss")
  expect_identical(sf::st_crs(fl)$epsg, 2056L)
  expect_identical(fl$kategorie_n, 1L)
  expect_error(geoGewaesser("see", category = 2), "no see layers")
})

test_that("plotMap, addGewaesser and stampBfs draw without error", {
  local_themakart()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  xy <- plotMap("kanton", id = c("ZH", "LU"), col = c("red", "blue"),
                vegetation = TRUE, labels = list(TRUE, cex = 0.7),
                main = "Test")
  expect_identical(xy$kanton_id, c(1L, 3L))
  expect_equal(xy$x[1], 2605000)                      # official label point
  expect_silent(addGewaesser(lakes = 1, rivers = 1))
  expect_warning(plotMap(geoKanton(), id = 99), "not in the map")
  expect_error(plotMap(geoKanton(), labels = list(TRUE, size = 2)),
               "unknown element")
  expect_error(plotMap(geoKanton(), vegetation = "yes"), "TRUE, FALSE")
  expect_silent(plotMap(geoKanton(), stamp = list(x = "bottomright")))
  expect_silent(stampBfs("bottomright", year = 2023))
  expect_error(plotMap(data.frame(a = 1)), "sf object")
})

test_that("combineRegions, neighbours and locate", {
  local_themakart()
  kt <- geoKanton()
  r <- combineRegions(kt, c("a", "a", "b"))
  expect_identical(r$groups, c("a", "b"))
  expect_equal(as.numeric(sf::st_area(r)), c(2e8, 1e8))
  grDevices::pdf(NULL)
  expect_identical(plotMap(r, labels = TRUE)$groups, c("a", "b"))
  grDevices::dev.off()
  r2 <- combineRegions(kt, groups = c(2, 1), id = c("BE", "ZH"))
  expect_identical(r2$groups, c(1, 2))

  nb <- neighbours(kt, "BE")
  expect_identical(nb$nb_kanton_id, c(1L, 3L))
  expect_named(neighbours(kt), c("kanton_id", "nb_kanton_id"))
  expect_error(neighbours(kt, 99), "not in the map")

  loc <- locate(c(2615000, 2500000), c(1205000, 1205000), map = kt)
  expect_identical(loc$kanton_c, c("BE", NA))
})

test_that("geocode response is parsed with easting as x", {
  js <- list(results = list(list(attrs = list(
    label = "<b>Bundesplatz 3</b> 3011 Bern", detail = "bundesplatz 3 3011 bern",
    lat = 46.9466, lon = 7.4441, y = 2600421.6, x = 1199580.8))))
  g <- helvetia:::.parseGeocode(js, "Bundesplatz 3 Bern")
  expect_identical(g$treffer_x, "Bundesplatz 3 3011 Bern")
  expect_equal(c(g$x, g$y), c(2600421.6, 1199580.8))
  expect_true(is.na(helvetia:::.parseGeocode(list(results = list()), "x")$x))
  expect_error(geocode(1), "character")
})

test_that("canton table", {
  k <- kanton()
  expect_identical(k$kanton_c, helvetia:::.kantonAbbrev)
  expect_identical(k$kanton_x_fr[k$kanton_c == "GE"], "Genève")
})

test_that("live: ThemaKart sets", {
  skip_if_not_live()
  s <- themakartSets(cache = FALSE)
  expect_true(all(2020:2026 %in% s$set_n))
})
