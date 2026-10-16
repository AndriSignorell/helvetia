## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## -----------------------------------------------------------------------------
#  library(helvetia)
#  
#  pop   <- popGemeinde(year = 2021)
#  stand <- unique(pop$gem_stand_d)        # spatial reference of the cube
#  gem   <- gemeinde(date = stand)         # register of exactly that stand
#  geo   <- geoGemeinde(date = stand)      # geometries valid on that date
#  
#  # check the join before using it
#  setdiff(unique(pop$gem_id), gem$gem_id)
#  setdiff(unique(pop$gem_id), geo$gem_id)
#  
#  popTotal <- aggregate(ch_n ~ gem_id, data = pop, FUN = sum)
#  br <- cut(log10(popTotal$ch_n), 6)
#  plotMap(geo, id = popTotal$gem_id,
#          col = hcl.colors(6, "Blues 3", rev = TRUE)[br], border = NA)

## -----------------------------------------------------------------------------
#  area <- arealGemeinde(period = "2013/18", nClasses = 17)
#  unique(area$gem_stand_d)
#  table(attr(area, "sumCheck")$check_c)   # completeness of the class sums

## -----------------------------------------------------------------------------
#  plotMap("kanton", id = c("ZH", "BE", "TI"),
#          col = c("steelblue", "orange", "darkseagreen"), labels = TRUE)
#  addGewaesser()
#  
#  # premium regions of the mandatory health insurance; municipalities split
#  # over several regions must be resolved explicitly
#  preg <- praemienregion(2026)
#  preg$preg_x[preg$split_l] <- "mehrere"
#  reg  <- combineRegions(geoGemeinde("2026-01-01"), preg$preg_x, preg$gem_id)
#  plotMap(reg, col = hcl.colors(nrow(reg), "Set 3"), labels = TRUE)

## -----------------------------------------------------------------------------
#  sourceInfo(pop)        # incl. the POST selections and original download time
#  sourceInfo(area)
#  cacheList()

