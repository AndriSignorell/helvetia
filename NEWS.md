# helvetia 0.1.0

## Hardening after review

* `popGemeinde()` returns `gem_stand_d`, the municipality stand of the cube
  ("Raumbezug"), read from the BFS table page; the counts refer to
  31 December of `jahr_n`, the municipalities to `gem_stand_d`. Years and
  ids must be whole numbers.
* `sourceInfo()` keeps the original download time per cached block and the
  PXWeb POST selections (`query`).
* `combineRegions()` stops if a unit is assigned to different groups;
  identical duplicates are dropped.
* `cacheClear()`/`cacheList()` accept only the known source names.
* `arealGemeinde()`: duplicated raw observations are an error; incomplete
  class sums are reported as not checkable (`attr(, "sumCheck")`); the cache
  key includes the region codelist (municipality stand).
* ThemaKart: a refresh replaces the cached set only after a verified
  download; layers without id column or with conflicting duplicate ids are
  an error, multipart rows are unioned.
* `locate()` keeps points with missing coordinates, reports `n_treffer_n`
  and has `multiple = c("first", "na", "error")`.
* `addGewaesser()` follows the CRS of the last map drawn (or `crs`);
  `stampBfs()` defaults to `"bottomright"`.
* HTTP requests use explicit timeouts (options `helvetia.connectTimeout`,
  `helvetia.timeout`) and retry on connection errors.
