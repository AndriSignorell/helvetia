# Changelog

## helvetia 0.1.1

### Source metadata (second review)

- [`popGemeinde()`](https://andrisignorell.github.io/helvetia/reference/popGemeinde.md):
  an unreadable spatial reference gives `gem_stand_d = NA` with a
  warning, as documented (it used to fail in the key check). Such
  results and their metadata are not cached.
- [`popGemeinde()`](https://andrisignorell.github.io/helvetia/reference/popGemeinde.md)
  never labels new data with an older, unverified stand: metadata and
  spatial reference are cached as one unit, and if only a cached copy of
  the metadata is available, only year blocks cached with that stand are
  returned.
- Cached cube metadata carry a format version; entries of 0.1.0 (without
  spatial reference) are ignored instead of failing.
- Whole-number arguments reject `Inf` and values outside the integer
  range.

### Hardening after the first review

- [`popGemeinde()`](https://andrisignorell.github.io/helvetia/reference/popGemeinde.md)
  returns `gem_stand_d`, the municipality stand of the cube
  (“Raumbezug”), read from the BFS table page; the counts refer to 31
  December of `jahr_n`, the municipalities to `gem_stand_d`. Years and
  ids must be whole numbers.
- [`sourceInfo()`](https://andrisignorell.github.io/helvetia/reference/sourceInfo.md)
  keeps the original download time per cached block and the PXWeb POST
  selections (`query`).
- [`combineRegions()`](https://andrisignorell.github.io/helvetia/reference/combineRegions.md)
  stops if a unit is assigned to different groups; identical duplicates
  are dropped.
- [`cacheClear()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md)/[`cacheList()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md)
  accept only the known source names.
- [`arealGemeinde()`](https://andrisignorell.github.io/helvetia/reference/arealGemeinde.md):
  duplicated raw observations are an error; incomplete class sums are
  reported as not checkable (`attr(, "sumCheck")`); the cache key
  includes the region codelist (municipality stand).
- ThemaKart: a refresh replaces the cached set only after a verified
  download; layers without id column or with conflicting duplicate ids
  are an error, multipart rows are unioned.
- [`locate()`](https://andrisignorell.github.io/helvetia/reference/locate.md)
  keeps points with missing coordinates, reports `n_treffer_n` and has
  `multiple = c("first", "na", "error")`.
- [`addGewaesser()`](https://andrisignorell.github.io/helvetia/reference/addGewaesser.md)
  follows the CRS of the last map drawn (or `crs`);
  [`stampBfs()`](https://andrisignorell.github.io/helvetia/reference/stampBfs.md)
  defaults to `"bottomright"`.
- HTTP requests use explicit timeouts (options
  `helvetia.connectTimeout`, `helvetia.timeout`) and retry on connection
  errors.

## helvetia 0.1.0

First version.

- Registers:
  [`gemeinde()`](https://andrisignorell.github.io/helvetia/reference/gemeinde.md),
  [`gemeindeMutationen()`](https://andrisignorell.github.io/helvetia/reference/gemeindeMutationen.md),
  [`raumgliederung()`](https://andrisignorell.github.io/helvetia/reference/raumgliederung.md)
  (experimental),
  [`praemienregion()`](https://andrisignorell.github.io/helvetia/reference/praemienregion.md),
  [`kanton()`](https://andrisignorell.github.io/helvetia/reference/kanton.md).
- Statistics:
  [`popGemeinde()`](https://andrisignorell.github.io/helvetia/reference/popGemeinde.md),
  [`arealGemeinde()`](https://andrisignorell.github.io/helvetia/reference/arealGemeinde.md),
  [`noas()`](https://andrisignorell.github.io/helvetia/reference/noas.md).
- Geometries:
  [`geoGemeinde()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md),
  [`geoBezirk()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md),
  [`geoKanton()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md),
  [`geoThemakart()`](https://andrisignorell.github.io/helvetia/reference/geoThemakart.md),
  [`geoGewaesser()`](https://andrisignorell.github.io/helvetia/reference/geoGewaesser.md),
  [`themakartSets()`](https://andrisignorell.github.io/helvetia/reference/themakartSets.md).
- Maps and spatial tools:
  [`plotMap()`](https://andrisignorell.github.io/helvetia/reference/plotMap.md),
  [`addGewaesser()`](https://andrisignorell.github.io/helvetia/reference/addGewaesser.md),
  [`stampBfs()`](https://andrisignorell.github.io/helvetia/reference/stampBfs.md),
  [`combineRegions()`](https://andrisignorell.github.io/helvetia/reference/combineRegions.md),
  [`neighbours()`](https://andrisignorell.github.io/helvetia/reference/neighbours.md),
  [`locate()`](https://andrisignorell.github.io/helvetia/reference/locate.md),
  [`geocode()`](https://andrisignorell.github.io/helvetia/reference/geocode.md).
- Sources:
  [`sourceInfo()`](https://andrisignorell.github.io/helvetia/reference/sourceInfo.md),
  [`cacheList()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md),
  [`cacheClear()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md).

Names and signatures follow the suite’s design rules (v0.7.1):
lowerCamelCase functions and arguments, kebab-case enum values, one
`@family` and two to four `@concept` tags per topic.

### Integration of bfsMaps

helvetia supersedes bfsMaps (1.99.4). The ThemaKart sets are found in
the BFS data catalogue and downloaded into the cache automatically;
`DownloadBfSMaps()`, `maps.csv` and the option `bfsMaps.base` are no
longer needed. An existing unpacked set can still be used with
`options(helvetia.themakartDir = <path>, helvetia.themakartSet = <year>)`.

| bfsMaps | helvetia |
|----|----|
| `GetMap("polg.map")`, `GetMap("kant.map")`, … | `geoThemakart("gemeinde")`, [`geoGemeinde()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md), [`geoKanton()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md), [`geoBezirk()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md) |
| `GetMap("kantvf.map")`, `GetMap("kant.pnt")` | `geoThemakart("kanton", area = "vegetation")`, `geoThemakart("kanton", output = "point")` |
| `PlotKant()`, `PlotBezk()`, `PlotPolg()`, `PlotGreg()`, `PlotMSRe()`, `PlotCH()` | `plotMap("kanton", ...)`, `plotMap("grossregion", ...)`, `plotMap("schweiz")` |
| `col.vf`, `border.vf` | `plotMap(vegetation = list(col = , border = ))` |
| `tmtxt` | `plotMap(stamp = )` |
| `AddLakes()`, `AddRivers()`, `AddWaters()` | `addGewaesser(lakes = , rivers = )`; data via [`geoGewaesser()`](https://andrisignorell.github.io/helvetia/reference/geoGewaesser.md) |
| `BfSStamp()` | [`stampBfs()`](https://andrisignorell.github.io/helvetia/reference/stampBfs.md) |
| `CombinePolygons()`, `CombineKant()`, `CombinePolg()` | `combineRegions(map, groups, id)` |
| `PlotPremReg()` | `plotMap(combineRegions(geoGemeinde(date), preg$preg_x, preg$gem_id))` with `preg <- praemienregion(year)` |
| `Neighbours()` | [`neighbours()`](https://andrisignorell.github.io/helvetia/reference/neighbours.md) (data.frame of pairs) |
| `SwissLocator()` | [`locate()`](https://andrisignorell.github.io/helvetia/reference/locate.md) (also for given coordinates) |
| `GeoCode()` | [`geocode()`](https://andrisignorell.github.io/helvetia/reference/geocode.md) |
| `d.bfsrg` | [`gemeinde()`](https://andrisignorell.github.io/helvetia/reference/gemeinde.md) + [`raumgliederung()`](https://andrisignorell.github.io/helvetia/reference/raumgliederung.md) for any stand; premium regions via [`praemienregion()`](https://andrisignorell.github.io/helvetia/reference/praemienregion.md) |
| `kt` | [`kanton()`](https://andrisignorell.github.io/helvetia/reference/kanton.md) |
| `tkart` (session cache) | local cache, see [`cacheList()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md) |
| `PlotMapDot()` | dropped: a plain [`layout()`](https://rdrr.io/r/graphics/layout.html) call |
| `DownloadBfSMaps()` | dropped: automatic download |

Behavioural changes compared to bfsMaps:

- Premium regions are read from the BAG files on priminfo.admin.ch for
  any year from 2004, instead of a fixed snapshot of 2023.
- [`stampBfs()`](https://andrisignorell.github.io/helvetia/reference/stampBfs.md)
  honours `cex` and `adj` and uses the `"bottomright"` position (both
  were ignored in `BfSStamp()`); the default year is the set of the last
  map read instead of the current year.
- The stand of a layer is chosen by `date` instead of a fixed file.
- [`neighbours()`](https://andrisignorell.github.io/helvetia/reference/neighbours.md)
  returns all neighbours except the unit itself; `Neighbours()`
  additionally dropped all members of `id`.
- No dependency on DescTools and httr any more.
