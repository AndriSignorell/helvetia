# Add the ThemaKart Source Note to a Map

Writes the source note "Kartengrundlage: (c) BFS, ThemaKart, " into the
current plot. The note is mandatory for published maps based on the BFS
ThemaKart geometries;
[`plotMap()`](https://andrisignorell.github.io/helvetia/reference/plotMap.md)
adds it by default.

## Usage

``` r
stampBfs(
  x = "bottomright",
  y = NULL,
  year = NULL,
  text = NULL,
  cex = 0.6,
  adj = c(1, 0),
  ...
)
```

## Arguments

- x:

  position of the text: `"bottomright"` (default, also for `NULL`) for
  the lower right corner of the plot region, which works in any CRS, or
  an x coordinate in the CRS of the plot.

- y:

  y coordinate if `x` is numeric.

- year:

  year of the ThemaKart set. By default the set of the last map read,
  else the current year.

- text:

  text to use instead of the default note.

- cex:

  text size.

- adj:

  text adjustment, see [`text()`](https://rdrr.io/r/graphics/text.html).

- ...:

  further arguments passed to
  [`text()`](https://rdrr.io/r/graphics/text.html).

## Value

`NULL`, invisibly.

## See also

[`plotMap()`](https://andrisignorell.github.io/helvetia/reference/plotMap.md).

Other swiss.map:
[`addGewaesser()`](https://andrisignorell.github.io/helvetia/reference/addGewaesser.md),
[`plotMap()`](https://andrisignorell.github.io/helvetia/reference/plotMap.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plot(sf::st_geometry(geoKanton()))
stampBfs()
} # }
```
