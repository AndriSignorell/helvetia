## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## -----------------------------------------------------------------------------
#  library(helvetia)
#  
#  gem  <- gemeinde(date = as.Date("2021-01-01"))
#  pop  <- popGemeinde(year = 2021)
#  geo  <- geoGemeinde(date = as.Date("2021-01-01"))
#  area <- arealGemeinde(period = "2013/18", nClasses = 17)
#  
#  # aggregate the statistics before joining them to the map
#  popTotal <- aggregate(ch_n ~ gem_id, data = pop, FUN = sum)
#  br <- cut(log10(popTotal$ch_n), 6)
#  plotMap(geo, id = popTotal$gem_id,
#          col = hcl.colors(6, "Blues 3", rev = TRUE)[br], border = NA)

## -----------------------------------------------------------------------------
#  plotMap("kanton", id = c("ZH", "BE", "TI"),
#          col = c("steelblue", "orange", "darkseagreen"), labels = TRUE)
#  addGewaesser()
#  
#  # premium regions of the mandatory health insurance
#  preg <- praemienregion(2026)
#  reg  <- combineRegions(geoGemeinde("2026-01-01"), preg$preg_x, preg$gem_id)
#  plotMap(reg, col = hcl.colors(nrow(reg), "Set 3"), labels = TRUE)

## -----------------------------------------------------------------------------
#  setdiff(unique(pop$gem_id), gem$gem_id)

## -----------------------------------------------------------------------------
#  sourceInfo(area)
#  cacheList()

