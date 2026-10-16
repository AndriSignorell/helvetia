# helvetia: Swiss Public Statistics, Regional Classifications and Spatial Data

helvetia gives a small, stable API to official Swiss sources and maps
them to one historicisable data model. The functions fall into six
groups:

**Registers** – which units exist at a given date:

|  |  |  |
|----|----|----|
| Function | Purpose | Key |
| [`gemeinde()`](https://andrisignorell.github.io/helvetia/reference/gemeinde.md) | municipalities valid at a date (AGVCH) | `stand_d` + `gem_id` |
| [`gemeindeMutationen()`](https://andrisignorell.github.io/helvetia/reference/gemeindeMutationen.md) | mergers, splits, renamings (AGVCH) | `mutation_id` + old/new version |
| [`raumgliederung()`](https://andrisignorell.github.io/helvetia/reference/raumgliederung.md) | spatial classifications (AGVCH) | `stand_d` + `gem_id` + `typ_c` |
| [`praemienregion()`](https://andrisignorell.github.io/helvetia/reference/praemienregion.md) | health insurance premium regions (BAG) | `jahr_n` + `gem_id` + `preg_c` |
| [`kanton()`](https://andrisignorell.github.io/helvetia/reference/kanton.md) | cantons with numbers and names | `kanton_id` |

**Statistics** – values per municipality:

|  |  |  |
|----|----|----|
| Function | Purpose | Key |
| [`popGemeinde()`](https://andrisignorell.github.io/helvetia/reference/popGemeinde.md) | population by sex and age (STATPOP) | `jahr_n` + `gem_stand_d` + `gem_id` + `sex_c` + `age_n` |
| [`arealGemeinde()`](https://andrisignorell.github.io/helvetia/reference/arealGemeinde.md) | land use (Arealstatistik) | `period_x` + `gem_stand_d` + `gem_id` + `noas_c` |
| [`noas()`](https://andrisignorell.github.io/helvetia/reference/noas.md) | NOAS04 nomenclature | `noas_c` |

**Geometries** – polygons and points:

|  |  |
|----|----|
| Function | Purpose |
| [`geoGemeinde()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md), [`geoBezirk()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md), [`geoKanton()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md) | institutional boundaries |
| [`geoThemakart()`](https://andrisignorell.github.io/helvetia/reference/geoThemakart.md) | any ThemaKart layer, incl. vegetation area and label points |
| [`geoGewaesser()`](https://andrisignorell.github.io/helvetia/reference/geoGewaesser.md) | lakes and rivers |
| [`themakartSets()`](https://andrisignorell.github.io/helvetia/reference/themakartSets.md) | available ThemaKart sets |

**Maps** (formerly bfsMaps):
[`plotMap()`](https://andrisignorell.github.io/helvetia/reference/plotMap.md),
[`addGewaesser()`](https://andrisignorell.github.io/helvetia/reference/addGewaesser.md),
[`stampBfs()`](https://andrisignorell.github.io/helvetia/reference/stampBfs.md).

**Spatial tools**:
[`combineRegions()`](https://andrisignorell.github.io/helvetia/reference/combineRegions.md),
[`neighbours()`](https://andrisignorell.github.io/helvetia/reference/neighbours.md),
[`locate()`](https://andrisignorell.github.io/helvetia/reference/locate.md),
[`geocode()`](https://andrisignorell.github.io/helvetia/reference/geocode.md).

**Sources**:
[`sourceInfo()`](https://andrisignorell.github.io/helvetia/reference/sourceInfo.md),
[`cacheList()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md),
[`cacheClear()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md).

Column names encode the type by suffix: `_id` identifiers, `_x` text,
`_c` codes, `_n` numbers, `_d` dates, `_l` logicals. Register stands
(`stand_d`), statistical years (`jahr_n`), survey periods (`period_x`)
and boundary stands (`gem_stand_d`) are never mixed: a bare `gem_id` is
not sufficient for historical analyses. The column names follow the
attribute conventions of the source databases and are therefore in
`snake_case`, unlike function arguments and list components, which are
lowerCamelCase.

## See also

Useful links:

- <https://github.com/AndriSignorell/helvetia>

- Report bugs at <https://github.com/AndriSignorell/helvetia/issues>

## Author

**Maintainer**: Andri Signorell <andri@signorell.net>
([ORCID](https://orcid.org/0000-0003-4311-1969))

Authors:

- Andri Signorell <andri@signorell.net>
  ([ORCID](https://orcid.org/0000-0003-4311-1969))

Other contributors:

- Juerg Guggenbuehl (map union functions, from bfsMaps) \[contributor\]
