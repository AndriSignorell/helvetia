# Convert Swiss Addresses to Coordinates

Geocodes addresses with the search service of the Swiss federal
geoportal (api3.geo.admin.ch, origin `address`, based on the federal
building and dwelling register). Use it to place customers, patients or
sites on a map and, with
[`locate()`](https://andrisignorell.github.io/helvetia/reference/locate.md),
to assign them to municipalities.

## Usage

``` r
geocode(x, output = c("data.frame", "sf"))
```

## Arguments

- x:

  character vector of addresses, e.g. `"Waldwis 4 Madetswil"`.

- output:

  `"data.frame"`, or `"sf"` for an `sf` point object (rows without a
  match dropped).

## Value

One row per address with `adresse_x` (query), `treffer_x` (matched
address), `n_treffer_n` (number of hits), `x`, `y` (LV95), `lat_n` and
`lon_n`.

## Details

Coordinates are returned in LV95 (EPSG:2056) with `x` as easting and `y`
as northing, matching the maps of this package, and as WGS84 latitude
and longitude. Only the best match per address is kept; `n_treffer_n`
tells whether the address was ambiguous. Addresses without a match give
a row of `NA`.

## See also

[`locate()`](https://andrisignorell.github.io/helvetia/reference/locate.md).

Other swiss.spatial:
[`combineRegions()`](https://andrisignorell.github.io/helvetia/reference/combineRegions.md),
[`locate()`](https://andrisignorell.github.io/helvetia/reference/locate.md),
[`neighbours()`](https://andrisignorell.github.io/helvetia/reference/neighbours.md)

## Examples

``` r
if (FALSE) { # \dontrun{
adr <- geocode(c("Bundesplatz 3 Bern", "Quaderaweg 3 Trimmis"))
locate(adr$x, adr$y)
} # }
```
