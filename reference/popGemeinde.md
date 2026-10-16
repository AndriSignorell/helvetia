# Get the Population by Municipality, Sex and Age

Returns the permanent (or non-permanent) resident population on 31
December by municipality, sex and single year of age, from the BFS
population statistics STATPOP. It is the standard denominator for rates
per municipality and the basis for age-standardisation.

## Usage

``` r
popGemeinde(
  year = NULL,
  id = NULL,
  population = c("permanent", "non-permanent"),
  cache = TRUE,
  refresh = FALSE
)
```

## Arguments

- year:

  vector of statistical years (whole numbers). The default `NULL` uses
  the most recent year available.

- id:

  optional vector of BFS municipality numbers (whole numbers) to keep;
  `NULL` returns all municipalities.

- population:

  population type, `"permanent"` (ständige Wohnbevölkerung) or
  `"non-permanent"` (nichtständige Wohnbevölkerung).

- cache:

  logical; if `TRUE`, results are read from and written to the local
  cache (see
  [`cacheList()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md)).

- refresh:

  logical; if `TRUE`, the source is queried again even if a cached copy
  exists.

## Value

A data.frame with the columns `jahr_n`, `gem_stand_d`, `gem_id`, `sex_c`
(`"m"`, `"w"`), `age_n`, `age_open_l` and `ch_n` (number of persons).
The key is `jahr_n` + `gem_stand_d` + `gem_id` + `sex_c` + `age_n`.

## Details

The data come from the BFS PXWeb cube `px-x-0102010000_101`. All
selections are made through the cube metadata: variables are found by
name, municipalities by their level marker in the value labels (not by
position), the requested years are validated against the available ones
and sex is mapped explicitly (`1 = "m"`, `2 = "w"`). The open top age
class ("100 Jahre und mehr") is kept as `age_n = 100` and flagged with
`age_open_l = TRUE`; it must not be read as the exact age 100.

**Spatial reference.** The reference date of the counts (31 December,
`jahr_n`) and the municipality stand of the cube are two different
things. The BFS publishes the whole time series on *one* municipality
stand (e.g. "Raumbezug: Gemeinden / 06.04.2025"), not on the stand of
the respective year. That stand is read from the table page and returned
in `gem_stand_d`; join geometries and registers of that stand, e.g.
`geoGemeinde(unique(pop$gem_stand_d))`. If the page cannot be read,
`gem_stand_d` is `NA` and a warning is given. An unchanged BFS number
does not guarantee unchanged boundaries, so always join on the stand,
not on the year.

Large requests are split automatically to respect the cell limit of the
API. Each year is cached for the whole of Switzerland and per spatial
reference; `id` only filters the result. Cells the source marks as
unavailable are `NA`.
[`sourceInfo()`](https://andrisignorell.github.io/helvetia/reference/sourceInfo.md)
keeps the POST selections and the time of the original download of each
year.

## See also

[`gemeinde()`](https://andrisignorell.github.io/helvetia/reference/gemeinde.md),
[`geoGemeinde()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md).

Other swiss.statistics:
[`arealGemeinde()`](https://andrisignorell.github.io/helvetia/reference/arealGemeinde.md),
[`noas()`](https://andrisignorell.github.io/helvetia/reference/noas.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pop <- popGemeinde(year = 2021)
unique(pop$gem_stand_d)               # spatial reference of the cube
popTotal <- aggregate(ch_n ~ gem_id, data = pop, FUN = sum)
geo <- geoGemeinde(unique(pop$gem_stand_d))
} # }
```
