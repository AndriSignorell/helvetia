# Get the Boundaries of Municipalities, Districts and Cantons

Returns the polygons of the Swiss political municipalities, districts or
cantons for a defined stand, ready to be joined with statistics of the
same stand and drawn with
[`plotMap()`](https://andrisignorell.github.io/helvetia/reference/plotMap.md).

## Usage

``` r
geoGemeinde(
  date = NULL,
  source = c("themakart", "swissboundaries3d"),
  crs = NULL,
  cache = TRUE,
  refresh = FALSE,
  ...
)

geoBezirk(date = NULL, crs = NULL, cache = TRUE, refresh = FALSE, ...)

geoKanton(date = NULL, crs = NULL, cache = TRUE, refresh = FALSE, ...)
```

## Arguments

- date:

  stand of the boundaries, a `Date` or a string `"YYYY-MM-DD"`. The
  default `NULL` uses the most recent stand.

- source:

  source of the municipal boundaries, `"themakart"` or
  `"swissboundaries3d"`.

- crs:

  optional target CRS, an EPSG code or anything accepted by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html);
  `NULL` keeps EPSG:2056.

- cache:

  logical; if `TRUE`, results are read from and written to the local
  cache (see
  [`cacheList()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md)).

- refresh:

  logical; if `TRUE`, the source is queried again even if a cached copy
  exists.

- ...:

  further arguments passed to
  [`geoThemakart()`](https://andrisignorell.github.io/helvetia/reference/geoThemakart.md),
  e.g. `area = "vegetation"` or `setYear`.

## Value

An `sf` object with `stand_d`, the unit's id and name (`gem_id`/`gem_x`,
`bezirk_id`/`bezirk_x`, or `kanton_id`/`kanton_x` and `kanton_c`) and
`geometry`. The key is `stand_d` + id.

## Details

Two sources are available for municipalities:

- `"themakart"`:

  the generalised BFS geometries for thematic maps (see
  [`geoThemakart()`](https://andrisignorell.github.io/helvetia/reference/geoThemakart.md)):
  small, fast and the official basis of statistical maps; total area
  including lakes.

- `"swissboundaries3d"`:

  swisstopo's full-resolution boundaries, obtained through the STAC API
  of data.geo.admin.ch, for precise spatial analysis. The release valid
  at `date` is used. Communal condominia and lake areas are excluded, Z
  coordinates dropped, parts combined to one MULTIPOLYGON per
  municipality, and invalid geometries repaired with
  [`sf::st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html)
  and reported (attributes `repairedGemId`, `emptyGemId`). The first
  download is large.

Districts and cantons always come from ThemaKart. Geometries are in LV95
(EPSG:2056); another CRS is only used if `crs` is given.

## See also

[`gemeinde()`](https://andrisignorell.github.io/helvetia/reference/gemeinde.md),
[`plotMap()`](https://andrisignorell.github.io/helvetia/reference/plotMap.md).

Other swiss.geometry:
[`geoGewaesser()`](https://andrisignorell.github.io/helvetia/reference/geoGewaesser.md),
[`geoThemakart()`](https://andrisignorell.github.io/helvetia/reference/geoThemakart.md),
[`themakartSets()`](https://andrisignorell.github.io/helvetia/reference/themakartSets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
geo <- geoGemeinde(as.Date("2021-01-01"))
plotMap(geo)
kt <- geoKanton()
} # }
```
