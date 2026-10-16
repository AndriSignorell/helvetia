# Map of Swiss Spatial Units

Draws spatial units (municipalities, districts, cantons, regions) and
colours a selection of them. This is the quick way to a choropleth map
of municipal or cantonal statistics, and to highlight single units.

## Usage

``` r
plotMap(
  map,
  id = NULL,
  main = NULL,
  col = NA,
  border = "grey",
  lwd = 1,
  bg = "white",
  labels = FALSE,
  vegetation = FALSE,
  add = FALSE,
  stamp = TRUE,
  ...
)
```

## Arguments

- map:

  an `sf` object from
  [`geoGemeinde()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md),
  [`geoKanton()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md),
  [`geoThemakart()`](https://andrisignorell.github.io/helvetia/reference/geoThemakart.md)
  etc., or a unit name passed to
  [`geoThemakart()`](https://andrisignorell.github.io/helvetia/reference/geoThemakart.md),
  e.g. `"kanton"`.

- id:

  ids of the units to colour, cantons also by abbreviation (e.g.
  `"ZH"`); `NULL` for all units.

- main:

  plot title; `NULL`, `""`, `NA` or `FALSE` draw none.

- col:

  fill colours, recycled along `id`.

- border:

  border colours, recycled along `id`.

- lwd:

  border line width.

- bg:

  fill colour of the units not in `id`.

- labels:

  `TRUE` to label the units in `id` with their ids, a character vector
  of labels along `id`, or a list holding such a value (unnamed or as
  `text`) and further arguments of
  [`text()`](https://rdrr.io/r/graphics/text.html) (`cex`, `col`,
  `font`, `adj`, `pos`, `offset`); `FALSE` draws none.

- vegetation:

  the vegetation area of ThemaKart maps: `TRUE` draws it with default
  colours, `FALSE`, `NULL` or `NA` draw none, a list
  `list(col = , border = , lwd = )` sets its style.

- add:

  logical; if `TRUE`, the map is added to the current plot.

- stamp:

  source note of ThemaKart maps: `TRUE` draws it, `FALSE`, `NULL` or
  `NA` suppress it, a list passes arguments to
  [`stampBfs()`](https://andrisignorell.github.io/helvetia/reference/stampBfs.md).
  Ignored with `add = TRUE` and for maps of other sources.

- ...:

  further graphical parameters passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for the
  geometry.

## Value

Invisibly, a data.frame with the id and the label coordinates `x`, `y`
of the units in `id`, for use with
[`text()`](https://rdrr.io/r/graphics/text.html).

## Details

Units not listed in `id` are drawn with the fill colour `bg` and a grey
border (unless `add = TRUE`). With a ThemaKart map the vegetation area
can be overlaid and labels are placed at the official label points;
otherwise at
[`sf::st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.html).
ThemaKart maps get the mandatory source note (see
[`stampBfs()`](https://andrisignorell.github.io/helvetia/reference/stampBfs.md)).

## See also

[`addGewaesser()`](https://andrisignorell.github.io/helvetia/reference/addGewaesser.md),
[`combineRegions()`](https://andrisignorell.github.io/helvetia/reference/combineRegions.md).

Other swiss.map:
[`addGewaesser()`](https://andrisignorell.github.io/helvetia/reference/addGewaesser.md),
[`stampBfs()`](https://andrisignorell.github.io/helvetia/reference/stampBfs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plotMap("kanton", id = c("ZH", "BE"), col = c("steelblue", "orange"),
        labels = TRUE)
addGewaesser()

# choropleth of the population by municipality
pop <- popGemeinde(2021)
geo <- geoGemeinde(unique(pop$gem_stand_d))   # stand of the population cube
pop <- aggregate(ch_n ~ gem_id, data = pop, FUN = sum)
br  <- cut(log10(pop$ch_n), 6)
plotMap(geo, id = pop$gem_id,
        col = hcl.colors(6, "Blues 3", rev = TRUE)[br], border = NA)
} # }
```
