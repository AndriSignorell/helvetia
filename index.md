# helvetia

Swiss municipalities, population, land use and maps from official
sources (BFS, swisstopo, BAG/priminfo), with an explicit time reference,
local cache and source metadata on every result.

## Installation

``` r

# install.packages("remotes")
remotes::install_github("AndriSignorell/helvetia")
```

## Example

``` r

library(helvetia)

pop   <- popGemeinde(year = 2021)          # STATPOP, 31.12.2021
stand <- unique(pop$gem_stand_d)           # municipality stand of the cube
geo   <- geoGemeinde(date = stand)         # geometries of that stand

setdiff(unique(pop$gem_id), geo$gem_id)    # check the join

popTotal <- aggregate(ch_n ~ gem_id, data = pop, FUN = sum)
plotMap(geo, id = popTotal$gem_id,
        col = hcl.colors(6, "Blues 3", rev = TRUE)[cut(log10(popTotal$ch_n), 6)],
        border = NA)
addGewaesser()

sourceInfo(pop)
```

## Time references

| Field         | Meaning                                         |
|---------------|-------------------------------------------------|
| `stand_d`     | exact date of a register or geometry stand      |
| `jahr_n`      | statistical year (reference date of the counts) |
| `period_x`    | multi-year survey period, e.g. `"2013/18"`      |
| `gem_stand_d` | municipality stand a result is reported on      |

The year of a statistic and the municipality stand it is reported on are
different things: STATPOP counts of 2021 are published on a later
municipality stand (e.g. 6 April 2025). Always join registers,
statistics and geometries on the stand, never on the year alone. An
unchanged BFS number does not guarantee unchanged boundaries.

## Main functions

| Area | Functions |
|----|----|
| Registers | [`gemeinde()`](https://andrisignorell.github.io/helvetia/reference/gemeinde.md), [`gemeindeMutationen()`](https://andrisignorell.github.io/helvetia/reference/gemeindeMutationen.md), [`kanton()`](https://andrisignorell.github.io/helvetia/reference/kanton.md), [`raumgliederung()`](https://andrisignorell.github.io/helvetia/reference/raumgliederung.md) |
| Statistics | [`popGemeinde()`](https://andrisignorell.github.io/helvetia/reference/popGemeinde.md), [`arealGemeinde()`](https://andrisignorell.github.io/helvetia/reference/arealGemeinde.md), [`noas()`](https://andrisignorell.github.io/helvetia/reference/noas.md), [`praemienregion()`](https://andrisignorell.github.io/helvetia/reference/praemienregion.md) |
| Geometries | [`geoGemeinde()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md), [`geoBezirk()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md), [`geoKanton()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md), [`geoThemakart()`](https://andrisignorell.github.io/helvetia/reference/geoThemakart.md), [`geoGewaesser()`](https://andrisignorell.github.io/helvetia/reference/geoGewaesser.md) |
| Maps | [`plotMap()`](https://andrisignorell.github.io/helvetia/reference/plotMap.md), [`addGewaesser()`](https://andrisignorell.github.io/helvetia/reference/addGewaesser.md), [`stampBfs()`](https://andrisignorell.github.io/helvetia/reference/stampBfs.md), [`combineRegions()`](https://andrisignorell.github.io/helvetia/reference/combineRegions.md), [`neighbours()`](https://andrisignorell.github.io/helvetia/reference/neighbours.md), [`locate()`](https://andrisignorell.github.io/helvetia/reference/locate.md), [`geocode()`](https://andrisignorell.github.io/helvetia/reference/geocode.md) |
| Sources | [`sourceInfo()`](https://andrisignorell.github.io/helvetia/reference/sourceInfo.md), [`cacheList()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md), [`cacheClear()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md), [`themakartSets()`](https://andrisignorell.github.io/helvetia/reference/themakartSets.md) |

Data are downloaded on first use and cached in
`tools::R_user_dir("helvetia", "cache")` (option `helvetia.cacheDir`).
