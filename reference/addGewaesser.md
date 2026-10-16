# Add Lakes and Rivers to a Map

Overlays the water bodies of the ThemaKart set on the current plot.
Lakes and the larger rivers make a thematic map recognisable as
Switzerland at a glance.

## Usage

``` r
addGewaesser(
  lakes = 1,
  rivers = 1:5,
  col = NULL,
  border = "lightskyblue3",
  lwd = 1,
  setYear = NULL,
  crs = NULL,
  ...
)
```

## Arguments

- lakes:

  lake size categories to draw, 1 (largest) or 2; `NULL` for none.

- rivers:

  river size categories to draw, 1 (largest) to 5; `NULL` for none.

- col:

  fill colour of the lakes. The default `NULL` uses a lighter, opaque
  variant of `border`.

- border:

  colour of the rivers and the lake shores.

- lwd:

  line width.

- setYear:

  ThemaKart set; by default the set of the last map read.

- crs:

  coordinate reference system of the current plot. The default `NULL`
  uses the CRS of the last map drawn with
  [`plotMap()`](https://andrisignorell.github.io/helvetia/reference/plotMap.md)
  (LV95 if there is none); give it explicitly when the map was drawn
  otherwise.

- ...:

  further graphical parameters passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

`NULL`, invisibly.

## See also

[`geoGewaesser()`](https://andrisignorell.github.io/helvetia/reference/geoGewaesser.md)
for the geometries.

Other swiss.map:
[`plotMap()`](https://andrisignorell.github.io/helvetia/reference/plotMap.md),
[`stampBfs()`](https://andrisignorell.github.io/helvetia/reference/stampBfs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plotMap("kanton")
addGewaesser(lakes = 1, rivers = 1:2)
} # }
```
