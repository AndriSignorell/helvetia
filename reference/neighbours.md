# Find Neighbouring Spatial Units

Finds the units sharing a boundary or a point with the given units, e.g.
to define comparison regions or to check spatial autocorrelation.

## Usage

``` r
neighbours(map, id = NULL)
```

## Arguments

- map:

  an `sf` object, e.g. from
  [`geoGemeinde()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md).

- id:

  ids of the units whose neighbours are wanted, cantons also by
  abbreviation; `NULL` for all units.

## Value

A data.frame with one row per pair: the id column of the map (e.g.
`gem_id`) and the neighbour's id (e.g. `nb_gem_id`). A unit is not its
own neighbour. For the outer neighbours of a group of units, drop the
rows whose neighbour is in the group.

## See also

Other swiss.spatial:
[`combineRegions()`](https://andrisignorell.github.io/helvetia/reference/combineRegions.md),
[`geocode()`](https://andrisignorell.github.io/helvetia/reference/geocode.md),
[`locate()`](https://andrisignorell.github.io/helvetia/reference/locate.md)

## Examples

``` r
if (FALSE) { # \dontrun{
neighbours(geoKanton(), "ZH")
} # }
```
