# Changelog

## helvetia 0.1.0

### Hardening after review

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
