# helvetia

Swiss municipalities, population, land use and maps from official sources
(BFS, swisstopo, BAG/priminfo), with an explicit time reference, local
cache and source metadata on every result.

## Installation

```r
# install.packages("remotes")
remotes::install_github("AndriSignorell/helvetia")
```

## Example

```r
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

| Field | Meaning |
|---|---|
| `stand_d` | exact date of a register or geometry stand |
| `jahr_n` | statistical year (reference date of the counts) |
| `period_x` | multi-year survey period, e.g. `"2013/18"` |
| `gem_stand_d` | municipality stand a result is reported on |

The year of a statistic and the municipality stand it is reported on are
different things: STATPOP counts of 2021 are published on a later
municipality stand (e.g. 6 April 2025). Always join registers, statistics
and geometries on the stand, never on the year alone. An unchanged BFS
number does not guarantee unchanged boundaries.

## Main functions

| Area | Functions |
|---|---|
| Registers | `gemeinde()`, `gemeindeMutationen()`, `kanton()`, `raumgliederung()` |
| Statistics | `popGemeinde()`, `arealGemeinde()`, `noas()`, `praemienregion()` |
| Geometries | `geoGemeinde()`, `geoBezirk()`, `geoKanton()`, `geoThemakart()`, `geoGewaesser()` |
| Maps | `plotMap()`, `addGewaesser()`, `stampBfs()`, `combineRegions()`, `neighbours()`, `locate()`, `geocode()` |
| Sources | `sourceInfo()`, `cacheList()`, `cacheClear()`, `themakartSets()` |

Data are downloaded on first use and cached in
`tools::R_user_dir("helvetia", "cache")` (option `helvetia.cacheDir`).
