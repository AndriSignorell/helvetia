# A miniature ThemaKart set on disk, following the official file naming.
square_sfc <- function(x0, y0, size = 10000, crs = 2056) {
  sq <- function(x, y) sf::st_polygon(list(cbind(
    c(x, x + size, x + size, x, x), c(y, y, y + size, y + size, y))))
  sf::st_sfc(mapply(sq, x0, y0, SIMPLIFY = FALSE), crs = crs)
}

local_themakart <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  inst <- file.path(dir, "01_INST", "gf")
  topo <- file.path(dir, "00_TOPO")
  dir.create(inst, recursive = TRUE)
  dir.create(topo, recursive = TRUE)
  x0 <- 2600000 + c(0, 10000, 20000)
  y0 <- rep(1200000, 3)
  write <- function(geom, file, data) {
    sf::st_write(sf::st_sf(data, geometry = geom), file, quiet = TRUE)
  }
  kt <- data.frame(id = 1:3, name = c("Zürich", "Bern", "Luzern"))
  write(square_sfc(x0, y0), file.path(inst, "K4kant20230101gf_ch2007Poly.shp"), kt)
  write(square_sfc(x0, y0), file.path(inst, "K4kant20220101gf_ch2007Poly.shp"), kt)
  write(square_sfc(x0 + 2000, y0 + 2000, 5000),
        file.path(inst, "K4kant20230101vf_ch2007Poly.shp"), kt)
  write(sf::st_centroid(square_sfc(x0, y0)),
        file.path(inst, "K4kant20230101gf_ch2007Pnts.shp"), kt)
  gm <- data.frame(id = c(1L, 2L, 351L), name = c("A", "B", "C"))
  write(square_sfc(x0, y0), file.path(inst, "K4polg20230101gf_ch2007Poly.shp"), gm)
  write(square_sfc(2605000, 1195000, 3000),
        file.path(topo, "k4seenyyyymmdd11_ch2007Poly.shp"),
        data.frame(id = 1L))
  line <- sf::st_sfc(sf::st_linestring(cbind(c(2600000, 2630000),
                                             c(1205000, 1205000))), crs = 2056)
  write(sf::st_transform(line, 21781),
        file.path(topo, "k4flusyyyymmdd11_ch2007.shp"), data.frame(id = 1L))
  withr::local_options(helvetia.themakartDir = dir,
                       helvetia.themakartSet = 2023L, .local_envir = env)
  dir
}


# Tests of the set search must not see a local set left in the session's
# options (devtools::test() runs in the user's session).
local_no_themakart_dir <- function(env = parent.frame()) {
  withr::local_options(helvetia.themakartDir = NULL,
                       helvetia.themakartSet = NULL, .local_envir = env)
}
