# Get the Health Insurance Premium Regions

Returns the assignment of municipalities to the premium regions of the
mandatory health insurance for a premium year, as published by the
Federal Office of Public Health (BAG) on priminfo.admin.ch. Premiums
differ between up to three regions within a canton, so the region is
needed to compare premiums or claims across municipalities, and to map
the regions with
[`combineRegions()`](https://andrisignorell.github.io/helvetia/reference/combineRegions.md).

## Usage

``` r
praemienregion(
  year = NULL,
  output = c("gemeinde", "ortschaft"),
  cache = TRUE,
  refresh = FALSE
)
```

## Arguments

- year:

  premium year; the default `NULL` uses the current year.

- output:

  `"gemeinde"` for one row per municipality and region, or `"ortschaft"`
  for the rows as delivered (one per postcode and locality).

- cache:

  logical; if `TRUE`, results are read from and written to the local
  cache (see
  [`cacheList()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md)).

- refresh:

  logical; if `TRUE`, the source is queried again even if a cached copy
  exists.

## Value

For `output = "gemeinde"`, a data.frame with the columns `jahr_n`,
`gem_id`, `gem_x`, `kanton_c`, `preg_c` (region within the canton, 0 to
3), `preg_x` (e.g. `"ZH3"`) and `split_l`; the key is `jahr_n` +
`gem_id` + `preg_c`. For `output = "ortschaft"`, the columns `plz_n` and
`ort_x` are added and `split_l` is dropped.

## Details

The regions are defined in the ordinance of the EDI, which remains the
binding source; the files on priminfo are its machine-readable companion
and are available from 2004. Cantons without regional differentiation
have region `0`. The file of a year lists the municipalities and
localities valid in that year; join it with municipality data of the
same stand.

The data are read from the sheet `A_COM` of the file (other sheets hold
information and mutations). Columns are found by their headers (BFS
number, region, canton, municipality, postcode, locality), not by
position; a change of the layout is an error naming the headers found.
Where the regions follow locality rather than municipality boundaries, a
municipality can lie in two regions; it then appears once per region
with `split_l = TRUE`.

## See also

[`combineRegions()`](https://andrisignorell.github.io/helvetia/reference/combineRegions.md)
to draw the regions.

Other swiss.register:
[`gemeinde()`](https://andrisignorell.github.io/helvetia/reference/gemeinde.md),
[`gemeindeMutationen()`](https://andrisignorell.github.io/helvetia/reference/gemeindeMutationen.md),
[`kanton()`](https://andrisignorell.github.io/helvetia/reference/kanton.md),
[`raumgliederung()`](https://andrisignorell.github.io/helvetia/reference/raumgliederung.md)

## Examples

``` r
if (FALSE) { # \dontrun{
preg <- praemienregion(2026)
table(preg$preg_x[preg$kanton_c == "ZH"])
} # }
```
