# Get Generalised Geometries for Thematic Maps (ThemaKart)

Reads a layer of the BFS map geometry sets for small-scale thematic maps
("Kartengeometrien ThemaKart"). These generalised geometries are the
official basis for statistical maps of Switzerland: small, fast to draw
and available for many spatial units besides the institutional ones.

## Usage

``` r
geoThemakart(
  unit = "gemeinde",
  date = NULL,
  area = c("total", "vegetation"),
  output = c("polygon", "point"),
  setYear = NULL,
  crs = 2056,
  cache = TRUE,
  refresh = FALSE
)
```

## Arguments

- unit:

  spatial unit: one of `"gemeinde"`, `"bezirk"`, `"kanton"`,
  `"schweiz"`, `"grossregion"`, `"ms-region"`, `"sprachgebiet"`,
  `"agglomeration"`, `"metropolitanraum"`, `"tourismusregion"`,
  `"arbeitsmarktregion"`, `"arbeitsmarktgrossregion"`, or a four-letter
  ThemaKart code such as `"deur"`.

- date:

  stand of the boundaries, a `Date` or a string `"YYYY-MM-DD"`. The
  default `NULL` uses the most recent stand in the set.

- area:

  `"total"` for the total area or `"vegetation"` for the vegetation
  area, i.e. the inhabitable area without lakes and high mountains.

- output:

  `"polygon"` for the areas or `"point"` for the official label points
  of the units.

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

An `sf` object with `stand_d`, an id and a name column
(`gem_id`/`gem_x`, `bezirk_id`/`bezirk_x`, `kanton_id`/`kanton_x` plus
`kanton_c`, otherwise `region_id`/`region_x`) and the geometry. The
attribute `"themakart"` records set, unit and stand, so that
[`plotMap()`](https://andrisignorell.github.io/helvetia/reference/plotMap.md)
can find the matching vegetation area and label points.

## Details

A set is downloaded once (about 12 MB) as zip archive into the cache and
read from the archive in place, without unpacking. `refresh = TRUE`
downloads it anew and replaces the cached archive only after the new one
has been verified; on failure the cached set is kept with a warning. A
set holds the geometries of its own stand (and possibly a few older
ones). Without `setYear`, the sets are searched for the most recent
stand on or before `date`: first the set of the year of `date`, then the
newer sets, then the older ones; each set searched is downloaded once.
Without `date` the most recent set is used. The stand found is returned
in `stand_d` and reported by a message if it differs from `date`. To use
an already unpacked set, e.g. from a former bfsMaps installation, set
`options(helvetia.themakartDir = <path>)` and optionally
`options(helvetia.themakartSet = <year>)`.

Maps published with these geometries must carry the source note
"Kartengrundlage: (c) BFS, ThemaKart"; see
[`stampBfs()`](https://andrisignorell.github.io/helvetia/reference/stampBfs.md).

## See also

[`geoGemeinde()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md),
[`plotMap()`](https://andrisignorell.github.io/helvetia/reference/plotMap.md),
[`geoGewaesser()`](https://andrisignorell.github.io/helvetia/reference/geoGewaesser.md).

Other swiss.geometry:
[`geo-boundaries`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md),
[`geoGewaesser()`](https://andrisignorell.github.io/helvetia/reference/geoGewaesser.md),
[`themakartSets()`](https://andrisignorell.github.io/helvetia/reference/themakartSets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
kt <- geoThemakart("kanton")
plot(sf::st_geometry(kt))
} # }
```
