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
  expect_error(geoThemakart("kanton", date = "2010-01-01"), "on or before 2010-01-01")
  expect_error(geoThemakart("bezirk"), "no bezk layer")
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
  # on.exit() in test code would replace the withr cleanup of
  # local_themakart() and leak its options into later tests
  withr::local_pdf(NULL)
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
  withr::with_pdf(NULL,
    expect_identical(plotMap(r, labels = TRUE)$groups, c("a", "b")))
  r2 <- combineRegions(kt, groups = c(2, 1), id = c("BE", "ZH"))
  expect_identical(r2$groups, c(1, 2))

  nb <- neighbours(kt, "BE")
  expect_identical(nb$nb_kanton_id, c(1L, 3L))
  expect_named(neighbours(kt), c("kanton_id", "nb_kanton_id"))
  expect_error(neighbours(kt, 99), "not in the map")

  loc <- locate(c(2615000, 2500000), c(1205000, 1205000), map = kt)
  expect_identical(loc$kanton_c, c("BE", NA))
  expect_identical(loc$n_treffer_n, c(1L, 0L))
})

test_that("combineRegions rejects units assigned to several groups", {
  local_themakart()
  kt <- geoKanton()
  # premium regions: a split unit appears with two different regions
  expect_error(combineRegions(kt, c("R1", "R2", "R1", "R3"),
                              id = c(1, 1, 2, 3)), "'groups'")
  # identical duplicates are harmless
  r <- combineRegions(kt, c("R1", "R1", "R1", "R2"), id = c(1, 1, 2, 3))
  expect_identical(r$groups, c("R1", "R2"))
  expect_equal(as.numeric(sf::st_area(r)), c(2e8, 1e8))
})

test_that("locate keeps missing coordinates and handles boundaries", {
  local_themakart()
  kt <- geoKanton()
  x <- c(2615000, NA, 2610000)           # 2610000: common edge of ZH and BE
  y <- c(1205000, NA, 1205000)
  loc <- locate(x, y, map = kt)
  expect_identical(nrow(loc), 3L)
  expect_identical(loc$kanton_c, c("BE", NA, "ZH"))
  expect_identical(loc$n_treffer_n, c(1L, NA, 2L))
  expect_identical(locate(x, y, map = kt, multiple = "na")$kanton_c,
                   c("BE", NA, NA))
  expect_error(locate(x, y, map = kt, multiple = "error"), "'multiple'")
  expect_error(locate(x, y, map = kt, multiple = "all"), "should be one of")
})

test_that("addGewaesser follows the CRS of the plotted map", {
  local_themakart()
  withr::local_pdf(NULL)
  plotMap(geoKanton(crs = 4326))
  usr <- par("usr")
  addGewaesser(lakes = 1, rivers = 1)
  expect_identical(par("usr"), usr)
  expect_identical(helvetia:::.helvetiaEnv$lastCrs, sf::st_crs(4326))
  expect_lt(usr[2L], 100)                    # degrees, not LV95 metres
})

test_that("a failed ThemaKart refresh keeps the cached set", {
  local_no_themakart_dir()
  withr::local_options(helvetia.cacheDir = withr::local_tempdir())
  set <- list(set_n = 2023L, url = "https://x/set")
  src <- local_themakart()
  withr::local_options(helvetia.themakartDir = NULL)
  zipSrc <- file.path(withr::local_tempdir(), "s.zip")
  withr::with_dir(src, utils::zip(zipSrc, ".", flags = "-rq"))
  ok <- TRUE
  local_mocked_bindings(
    .tkSetInfo = function(...) set,
    .downloadChecked = function(url, dest, ...) {
      if (!ok) stop("offline")
      file.copy(zipSrc, dest)
      list(path = dest, url = url, retrievedAt = Sys.time(), md5 = "x")
    })
  a <- helvetia:::.themakartDir(2023L, cache = TRUE, refresh = FALSE)
  md5 <- unname(tools::md5sum(a$zip))
  ok <- FALSE
  expect_warning(b <- helvetia:::.themakartDir(2023L, TRUE, TRUE),
                 "could not be refreshed")
  expect_identical(unname(tools::md5sum(b$zip)), md5)
  expect_identical(b$download$retrievedAt, a$download$retrievedAt)
  expect_false(file.exists(paste0(b$zip, ".new")))
})

test_that("ThemaKart layers need unique ids", {
  u <- helvetia:::.tkUnit("kanton")
  g <- sf::st_sf(id = c(1L, 1L, 2L), name = c("A", "A", "B"),
                 geometry = square_sfc(2600000 + c(0, 1e4, 2e4), rep(12e5, 3)))
  out <- helvetia:::.tkNormalise(g, u, as.Date("2023-01-01"), 2056)
  expect_identical(out$kanton_id, 1:2)
  expect_equal(as.numeric(sf::st_area(out)), c(2e8, 1e8))
  g$name[2] <- "Z"
  expect_error(helvetia:::.tkNormalise(g, u, as.Date("2023-01-01"), 2056),
               "repeatedly")
  names(g)[1] <- "objid"
  expect_error(helvetia:::.tkNormalise(g, u, as.Date("2023-01-01"), 2056),
               "no id column")
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
  local_no_themakart_dir()
  s <- themakartSets(cache = FALSE)
  expect_true(all(2020:2026 %in% s$set_n))
})

test_that("sets are read from the zip, names in CP437 decoded", {
  local_themakart()
  src <- getOption("helvetia.themakartDir")
  zip <- file.path(withr::local_tempdir(), "set.zip")
  # zip with a CP437-encoded directory name, as delivered by the BFS: zip
  # with an ASCII placeholder, then patch the name bytes (0x84 = CP437 a-umlaut)
  withr::with_dir(src, utils::zip(zip, "01_INST", flags = "-rq"))
  withr::with_dir(src, {
    file.rename("01_INST", "Gesamtfl#che")
    utils::zip(zip, "Gesamtfl#che", flags = "-rq")
  })
  b <- readBin(zip, "raw", file.size(zip))
  pat <- charToRaw("Gesamtfl#che")
  for (i in which(b == pat[1L])) {
    j <- i + seq_along(pat) - 1L
    if (max(j) <= length(b) && identical(b[j], pat)) b[i + 8L] <- as.raw(0x84)
  }
  writeBin(b, zip)
  raw <- unzip(zip, list = TRUE)$Name
  expect_false(all(validUTF8(raw)))                  # really CP437 bytes

  names <- helvetia:::.zipEntries(zip)
  expect_true(any(startsWith(names, "Gesamtfl\u00e4che/")))
  idx <- helvetia:::.themakartIndex(list(zip = zip))
  expect_true(all(startsWith(idx$path, "/vsizip/")))
  lyr <- idx$path[idx$code == "kant" & idx$area == "total" &
                    idx$output == "polygon" & idx$date == as.Date("2023-01-01")]
  expect_length(lyr, 2L)                             # plain and CP437 copy
  cp <- grep("Gesamtfl", lyr, value = TRUE)
  # (a non-UTF-8 test locale warns about the path; reading works regardless)
  expect_identical(nrow(suppressWarnings(sf::st_read(cp, quiet = TRUE))), 3L)
})

test_that("moving a download falls back to copying", {
  d <- withr::local_tempdir()
  from <- file.path(d, "a.part")
  writeLines("x", from)
  expect_true(helvetia:::.moveFile(from, file.path(d, "a")))
  expect_true(file.exists(file.path(d, "a")))
  expect_false(file.exists(from))
})


test_that("the stand valid at date is searched over the sets", {
  local_no_themakart_dir()
  root <- withr::local_tempdir()
  mk <- function(stand) {
    d <- file.path(root, stand)
    dir.create(d)
    sf::st_write(sf::st_sf(id = 1:2, name = c("A", "B"),
                           geometry = square_sfc(2600000 + c(0, 1e4), c(12e5, 12e5))),
                 file.path(d, sprintf("K4polg%sgf_ch2007Poly.shp", stand)),
                 quiet = TRUE)
    d
  }
  dirs <- list(`2022` = mk("20220101"), `2023` = mk("20230101"))
  local_mocked_bindings(
    themakartSets = function(...) data.frame(set_n = c(2022L, 2023L)),
    .themakartDir = function(setYear, cache, refresh)
      list(dir = dirs[[as.character(setYear)]],
           set = list(set_n = setYear, url = "x"), download = NULL))
  expect_message(g <- geoThemakart("gemeinde", "2022-03-01"), "set 2022")
  expect_identical(unique(g$stand_d), as.Date("2022-01-01"))
  expect_identical(attr(geoThemakart("gemeinde"), "themakart")$setYear, 2023L)
  expect_message(g <- geoThemakart("gemeinde", "2024-05-01"), "set 2023")
  expect_error(geoThemakart("gemeinde", "2021-06-01"), "set\\(s\\) 2022, 2023")
})
