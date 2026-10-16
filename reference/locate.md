# Find the Spatial Units Containing Points

Determines the spatial unit (municipality, district, ...) containing
given points, or points clicked in the current map. Use it to assign
geocoded addresses to municipalities, or to identify a unit on a map.

## Usage

``` r
locate(
  x = NULL,
  y = NULL,
  map = NULL,
  nPoints = 512,
  multiple = c("first", "na", "error")
)
```

## Arguments

- x, y:

  coordinates in the CRS of `map`, in any form accepted by
  [`xy.coords()`](https://rdrr.io/r/grDevices/xy.coords.html). With
  `x = NULL` the points are clicked in the current plot with
  [`locator()`](https://rdrr.io/r/graphics/locator.html); finish with
  Esc or a right click.

- map:

  an `sf` object; `NULL` uses
  [`geoGemeinde()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md).

- nPoints:

  maximum number of points to click.

- multiple:

  rule for points in more than one unit, i.e. on a common boundary or in
  overlapping units: `"first"` takes the first unit in the row order of
  `map`, `"na"` assigns none, `"error"` stops.

## Value

A data.frame with one row per point, in input order: `x`, `y`,
`n_treffer_n` (number of units containing the point) and the attributes
of the assigned unit. Points outside all units and points with missing
coordinates (e.g. failed
[`geocode()`](https://andrisignorell.github.io/helvetia/reference/geocode.md)
matches) get `NA` attributes; the latter have `n_treffer_n = NA`.

## See also

[`geocode()`](https://andrisignorell.github.io/helvetia/reference/geocode.md).

Other swiss.spatial:
[`combineRegions()`](https://andrisignorell.github.io/helvetia/reference/combineRegions.md),
[`geocode()`](https://andrisignorell.github.io/helvetia/reference/geocode.md),
[`neighbours()`](https://andrisignorell.github.io/helvetia/reference/neighbours.md)

## Examples

``` r
if (FALSE) { # \dontrun{
locate(2683000, 1248000)
plotMap("kanton")
locate()
} # }
```
