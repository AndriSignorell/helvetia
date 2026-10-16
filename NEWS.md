# helvetia 0.1.0

First version.

* Registers: `gemeinde()`, `gemeindeMutationen()`, `raumgliederung()`
  (experimental), `praemienregion()`, `kanton()`.
* Statistics: `popGemeinde()`, `arealGemeinde()`, `noas()`.
* Geometries: `geoGemeinde()`, `geoBezirk()`, `geoKanton()`,
  `geoThemakart()`, `geoGewaesser()`, `themakartSets()`.
* Maps and spatial tools: `plotMap()`, `addGewaesser()`, `stampBfs()`,
  `combineRegions()`, `neighbours()`, `locate()`, `geocode()`.
* Sources: `sourceInfo()`, `cacheList()`, `cacheClear()`.

Names and signatures follow the suite's design rules (v0.7.1):
lowerCamelCase functions and arguments, kebab-case enum values, one
`@family` and two to four `@concept` tags per topic.

## Integration of bfsMaps

helvetia supersedes bfsMaps (1.99.4). The ThemaKart sets are found in the
BFS data catalogue and downloaded into the cache automatically;
`DownloadBfSMaps()`, `maps.csv` and the option `bfsMaps.base` are no longer
needed. An existing unpacked set can still be used with
`options(helvetia.themakartDir = <path>, helvetia.themakartSet = <year>)`.

| bfsMaps | helvetia |
|---|---|
| `GetMap("polg.map")`, `GetMap("kant.map")`, ... | `geoThemakart("gemeinde")`, `geoGemeinde()`, `geoKanton()`, `geoBezirk()` |
| `GetMap("kantvf.map")`, `GetMap("kant.pnt")` | `geoThemakart("kanton", area = "vegetation")`, `geoThemakart("kanton", output = "point")` |
| `PlotKant()`, `PlotBezk()`, `PlotPolg()`, `PlotGreg()`, `PlotMSRe()`, `PlotCH()` | `plotMap("kanton", ...)`, `plotMap("grossregion", ...)`, `plotMap("schweiz")` |
| `col.vf`, `border.vf` | `plotMap(vegetation = list(col = , border = ))` |
| `tmtxt` | `plotMap(stamp = )` |
| `AddLakes()`, `AddRivers()`, `AddWaters()` | `addGewaesser(lakes = , rivers = )`; data via `geoGewaesser()` |
| `BfSStamp()` | `stampBfs()` |
| `CombinePolygons()`, `CombineKant()`, `CombinePolg()` | `combineRegions(map, groups, id)` |
| `PlotPremReg()` | `plotMap(combineRegions(geoGemeinde(date), preg$preg_x, preg$gem_id))` with `preg <- praemienregion(year)` |
| `Neighbours()` | `neighbours()` (data.frame of pairs) |
| `SwissLocator()` | `locate()` (also for given coordinates) |
| `GeoCode()` | `geocode()` |
| `d.bfsrg` | `gemeinde()` + `raumgliederung()` for any stand; premium regions via `praemienregion()` |
| `kt` | `kanton()` |
| `tkart` (session cache) | local cache, see `cacheList()` |
| `PlotMapDot()` | dropped: a plain `layout()` call |
| `DownloadBfSMaps()` | dropped: automatic download |

Behavioural changes compared to bfsMaps:

* Premium regions are read from the BAG files on priminfo.admin.ch for any
  year from 2004, instead of a fixed snapshot of 2023.
* `stampBfs()` honours `cex` and `adj` and uses the `"bottomright"`
  position (both were ignored in `BfSStamp()`); the default year is the set
  of the last map read instead of the current year.
* The stand of a layer is chosen by `date` instead of a fixed file.
* `neighbours()` returns all neighbours except the unit itself;
  `Neighbours()` additionally dropped all members of `id`.
* No dependency on DescTools and httr any more.
