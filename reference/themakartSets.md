# List the Available ThemaKart Map Sets

Lists the yearly sets of generalised geometries for small-scale thematic
maps ("Kartengeometrien ThemaKart") published by the BFS. Use it to see
which map years can be requested with `setYear` in
[`geoThemakart()`](https://andrisignorell.github.io/helvetia/reference/geoThemakart.md).

## Usage

``` r
themakartSets(cache = TRUE, refresh = FALSE)
```

## Arguments

- cache:

  logical; if `TRUE`, results are read from and written to the local
  cache (see
  [`cacheList()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md)).

- refresh:

  logical; if `TRUE`, the source is queried again even if a cached copy
  exists.

## Value

A data.frame with the columns `set_n` (year of the set), `dam_id`
(catalogue id), `published_d` and `url` (download link).

## Details

The sets are found through the search API of the BFS data catalogue.
Only the yearly sets from 2020 onwards are listed; the older BASIS/PRO
sets have a different structure and are not supported.

## See also

[`geoThemakart()`](https://andrisignorell.github.io/helvetia/reference/geoThemakart.md).

Other swiss.geometry:
[`geo-boundaries`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md),
[`geoGewaesser()`](https://andrisignorell.github.io/helvetia/reference/geoGewaesser.md),
[`geoThemakart()`](https://andrisignorell.github.io/helvetia/reference/geoThemakart.md)

## Examples

``` r
if (FALSE) { # \dontrun{
themakartSets()
} # }
```
