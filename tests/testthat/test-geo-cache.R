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
    out <- helvetia:::.prepare_boundaries(g, as.Date("2021-01-01")),
    "repaired")
  expect_s3_class(out, "sf")
  expect_identical(out$gem_id, 1:3)
  expect_identical(sf::st_crs(out)$epsg, 2056L)
  expect_false(any(grepl("Z", sf::st_geometry_type(out, by_geometry = FALSE))))
  expect_identical(attr(out, "repaired_gem_id"), 3L)
  expect_true(all(sf::st_is_valid(out)))
  expect_named(sf::st_drop_geometry(out), c("stand_d", "gem_id", "gem_x"))
})

test_that("release selection uses the latest release on or before stand_d", {
  items <- data.frame(id = c("r2020", "r2021", "r2021b"),
                      stand_d = as.Date(c("2020-01-01", "2021-01-01",
                                          "2021-04-18")),
                      href = "x")
  expect_identical(helvetia:::.pick_release(items, "2021-03-01")$id, "r2021")
  expect_identical(helvetia:::.pick_release(items, NULL)$id, "r2021b")
  expect_error(helvetia:::.pick_release(items, "2019-01-01"), "earliest")
})

test_that("STAC items are parsed", {
  feats <- list(list(id = "swissboundaries3d_2021-01",
    properties = list(datetime = "2021-01-01T00:00:00Z"),
    assets = list(
      a = list(href = "https://x/a.shp.zip", type = "application/x.shapefile+zip"),
      b = list(href = "https://x/a.gpkg.zip", type = "application/x.geopackage+zip"))))
  it <- helvetia:::.parse_stac_items(feats)
  expect_identical(it$href, "https://x/a.gpkg.zip")
  expect_identical(it$stand_d, as.Date("2021-01-01"))
})

test_that("cache: hit, refresh, offline fallback, cache = FALSE", {
  withr::local_options(helvetia.cache_dir = withr::local_tempdir())
  n <- 0
  f <- function() { n <<- n + 1; n }
  expect_identical(helvetia:::.cached("t", "v", "k", f), 1)
  expect_identical(helvetia:::.cached("t", "v", "k", f), 1)            # hit
  expect_identical(helvetia:::.cached("t", "v", "k", f, refresh = TRUE), 2)
  expect_warning(
    r <- helvetia:::.cached("t", "v", "k", function() stop("offline"),
                            refresh = TRUE), "cached copy")
  expect_identical(r, 2)
  expect_identical(helvetia:::.cached("t", "v", "k2", f, cache = FALSE), 3)
  expect_false(file.exists(helvetia:::.cache_path("t", "v", "k2")))
  expect_identical(nrow(cache_list("t")), 1L)
  expect_message(cache_clear("t"), "1 cached")
})

test_that("source metadata is attached and printable", {
  x <- helvetia:::.attach_source_metadata(
    data.frame(a = 1), helvetia:::.source_meta("Test", "https://example.org"))
  expect_s3_class(source_info(x), "helvetia_source")
  expect_output(print(source_info(x)), "example.org")
})

test_that("live: geo_gemeinde()", {
  skip_if_not_live()
  skip_on_cran()
  g <- geo_gemeinde(as.Date("2021-01-01"))
  expect_gt(nrow(g), 2000)
  expect_identical(sf::st_crs(g)$epsg, 2056L)
})
