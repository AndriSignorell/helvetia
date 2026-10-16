square <- function(x0, y0, z = TRUE) {
  m <- cbind(c(x0, x0 + 1, x0 + 1, x0, x0), c(y0, y0, y0 + 1, y0 + 1, y0))
  if (z) m <- cbind(m, 0)
  sf::st_polygon(list(m))
}

test_that("boundaries: filter, Z drop, union of parts, repair", {
  bow <- sf::st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 1, 0, 1, 0), 0)))
  g <- sf::st_sf(
    BFS_NUMMER = c(1, 1, 2, 9999, 3),
    NAME = c("A", "A", "B", "See", "C"),
    OBJEKTART = c("Gemeindegebiet", "Gemeindegebiet", "Gemeindegebiet",
                  "Kantonsgebiet", "Gemeindegebiet"),
    geom = sf::st_sfc(square(0, 0), square(5, 5), square(10, 0),
                      square(20, 0), bow, crs = 2056))
  expect_message(
    out <- helvetia:::.prepareBoundaries(g, as.Date("2021-01-01")),
    "repaired")
  expect_s3_class(out, "sf")
  expect_identical(out$gem_id, 1:3)
  expect_identical(sf::st_crs(out)$epsg, 2056L)
  expect_false(any(grepl("Z", sf::st_geometry_type(out, by_geometry = FALSE))))
  expect_identical(attr(out, "repairedGemId"), 3L)
  expect_true(all(sf::st_is_valid(out)))
  expect_named(sf::st_drop_geometry(out), c("stand_d", "gem_id", "gem_x"))
})

test_that("release selection uses the latest release on or before date", {
  items <- data.frame(id = c("r2020", "r2021", "r2021b"),
                      date = as.Date(c("2020-01-01", "2021-01-01",
                                          "2021-04-18")),
                      href = "x")
  expect_identical(helvetia:::.pickRelease(items, "2021-03-01")$id, "r2021")
  expect_identical(helvetia:::.pickRelease(items, NULL)$id, "r2021b")
  expect_error(helvetia:::.pickRelease(items, "2019-01-01"), "earliest")
})

test_that("STAC items are parsed", {
  feats <- list(list(id = "swissboundaries3d_2021-01",
    properties = list(datetime = "2021-01-01T00:00:00Z"),
    assets = list(
      a = list(href = "https://x/a.shp.zip", type = "application/x.shapefile+zip"),
      b = list(href = "https://x/a.gpkg.zip", type = "application/x.geopackage+zip"))))
  it <- helvetia:::.parseStacItems(feats)
  expect_identical(it$href, "https://x/a.gpkg.zip")
  expect_identical(it$date, as.Date("2021-01-01"))
})

test_that("cache: hit, refresh, offline fallback, cache = FALSE", {
  withr::local_options(helvetia.cacheDir = withr::local_tempdir())
  n <- 0
  f <- function() { n <<- n + 1; n }
  expect_identical(helvetia:::.cached("agvch", "v", "k", f), 1)
  expect_identical(helvetia:::.cached("agvch", "v", "k", f), 1)            # hit
  expect_identical(helvetia:::.cached("agvch", "v", "k", f, refresh = TRUE), 2)
  expect_warning(
    r <- helvetia:::.cached("agvch", "v", "k", function() stop("offline"),
                            refresh = TRUE), "cached copy")
  expect_identical(r, 2)
  expect_identical(helvetia:::.cached("agvch", "v", "k2", f, cache = FALSE), 3)
  expect_false(file.exists(helvetia:::.cachePath("agvch", "v", "k2")))
  expect_identical(nrow(cacheList("agvch")), 1L)
  expect_message(cacheClear("agvch"), "1 cached")
})

test_that("cacheClear() and cacheList() accept only known sources", {
  root <- withr::local_tempdir()
  cache <- file.path(root, "cache")
  dir.create(file.path(cache, "agvch"), recursive = TRUE)
  outside <- file.path(root, "keep.txt")
  writeLines("x", outside)
  withr::local_options(helvetia.cacheDir = cache)
  for (bad in list("..", "../", "agvch/..", "/", "x", c("agvch", "pxweb"), NA, 1))
    expect_error(cacheClear(bad), "'source'")
  expect_error(cacheList(".."), "'source'")
  expect_true(file.exists(outside))
  expect_message(cacheClear("agvch"), "0 cached")
})

test_that("connection errors are retried, then reported", {
  n <- 0L
  local_mocked_bindings(curl_fetch_memory = function(url, handle) {
    n <<- n + 1L
    stop("Timeout was reached")
  }, .package = "curl")
  expect_error(helvetia:::.http("https://example.org", tries = 2L),
               "Connection to")
  expect_identical(n, 2L)
})

test_that("source metadata is attached and printable", {
  x <- helvetia:::.attachSourceMetadata(
    data.frame(a = 1), helvetia:::.sourceMeta("Test", "https://example.org"))
  expect_s3_class(sourceInfo(x), "SourceInfo")
  expect_output(print(sourceInfo(x)), "example.org")
})

test_that("live: geoGemeinde()", {
  skip_if_not_live()
  skip_on_cran()
  local_no_themakart_dir()
  g <- geoGemeinde(as.Date("2021-01-01"))
  expect_gt(nrow(g), 2000)
  expect_identical(sf::st_crs(g)$epsg, 2056L)
})
