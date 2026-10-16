# Merge Spatial Units into Larger Regions

Unions the geometries of a map by a grouping, e.g. municipalities to
premium regions or cantons to language areas. Use it to map any regional
division that is defined as a set of municipalities or cantons.

## Usage

``` r
combineRegions(map, groups, id = NULL)
```

## Arguments

- map:

  an `sf` object, e.g. from
  [`geoGemeinde()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md)
  or
  [`geoKanton()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md).

- groups:

  grouping, along the rows of `map` or, if `id` is given, along `id`;
  units with `NA` are left out.

- id:

  optional ids of the units `groups` refers to, cantons also by
  abbreviation. An id may occur repeatedly only with the same group
  (such duplicates are dropped); an id assigned to different groups is
  an error, as a unit cannot be part of two regions. Resolve such units
  explicitly before, e.g. the municipalities split over several premium
  regions (`split_l` in
  [`praemienregion()`](https://andrisignorell.github.io/helvetia/reference/praemienregion.md)).

## Value

An `sf` object with one row per group: the column `groups` (sorted) and
the unioned geometry.

## Note

Based on code by Juerg Guggenbuehl, adapted to conform to package
standards.

## See also

[`praemienregion()`](https://andrisignorell.github.io/helvetia/reference/praemienregion.md),
[`raumgliederung()`](https://andrisignorell.github.io/helvetia/reference/raumgliederung.md).

Other swiss.spatial:
[`geocode()`](https://andrisignorell.github.io/helvetia/reference/geocode.md),
[`locate()`](https://andrisignorell.github.io/helvetia/reference/locate.md),
[`neighbours()`](https://andrisignorell.github.io/helvetia/reference/neighbours.md)

## Examples

``` r
if (FALSE) { # \dontrun{
preg <- praemienregion(2023)
geo  <- geoGemeinde(as.Date("2023-01-01"))
# municipalities split over several regions: own group "mehrere"
preg$preg_x[preg$split_l] <- "mehrere"
reg  <- combineRegions(geo, preg$preg_x, id = preg$gem_id)
plotMap(reg)
} # }
```
