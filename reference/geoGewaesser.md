# Get Lakes and Rivers

Returns the water bodies of the topographic layers of a ThemaKart set:
lakes in two size categories, rivers in five. They make thematic maps
readable as maps of Switzerland, and lakes explain gaps in maps drawn on
the vegetation area.

## Usage

``` r
geoGewaesser(
  type = c("see", "fluss"),
  category = NULL,
  setYear = NULL,
  crs = 2056,
  cache = TRUE,
  refresh = FALSE
)
```

## Arguments

- type:

  `"see"` for lakes or `"fluss"` for rivers.

- category:

  integer vector of size categories, 1 being the largest; `NULL` for
  all.

- setYear:

  year of the ThemaKart set; see Details.

- crs:

  target CRS. The default harmonises all layers to LV95 (EPSG:2056),
  since not all layers of a set share one CRS.

- cache:

  logical; if `TRUE`, results are read from and written to the local
  cache (see
  [`cacheList()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md)).

- refresh:

  logical; if `TRUE`, the source is queried again even if a cached copy
  exists.

## Value

An `sf` object with the columns `kategorie_n` and `geometry`.

## See also

[`addGewaesser()`](https://andrisignorell.github.io/helvetia/reference/addGewaesser.md)
to draw them.

Other swiss.geometry:
[`geo-boundaries`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md),
[`geoThemakart()`](https://andrisignorell.github.io/helvetia/reference/geoThemakart.md),
[`themakartSets()`](https://andrisignorell.github.io/helvetia/reference/themakartSets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
lakes <- geoGewaesser("see", category = 1)
} # }
```
