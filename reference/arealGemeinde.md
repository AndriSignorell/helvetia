# Get the Land Use Statistics by Municipality

Returns the area in hectares by municipality and NOAS04 class from the
Swiss land use statistics (Arealstatistik). Use it for settlement,
agricultural or forest shares per municipality, and for their change
between survey periods.

## Usage

``` r
arealGemeinde(
  period = NULL,
  nClasses = 17,
  checkSums = TRUE,
  threshold = 0.5,
  cache = TRUE,
  refresh = FALSE
)
```

## Arguments

- period:

  survey period, e.g. `"2013/18"`. The default `NULL` uses the most
  recent period; note that a period still in progress covers only part
  of Switzerland.

- nClasses:

  level of the nomenclature, given by its number of classes: `4`, `17`,
  `27` or `72`.

- checkSums:

  logical; if `TRUE`, the class sums are checked (see Details).

- threshold:

  difference in hectares above which `checkSums` warns.

- cache:

  logical; if `TRUE`, results are read from and written to the local
  cache (see
  [`cacheList()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md)).

- refresh:

  logical; if `TRUE`, the source is queried again even if a cached copy
  exists.

## Value

A data.frame in long format with the columns `period_x`, `gem_stand_d`,
`gem_id`, `noas_c`, `noas_n`, `noas_x`, `area_ha`, `value_status_c`,
`obs_status_c` and `flight_year_x`. The key is `period_x` +
`gem_stand_d` + `gem_id` + `noas_c`.

## Details

The data come from the dataflow `CH1.AREA/DF_AREA_NOAS` of the BFS SDMX
dissemination service (stats.swiss). Three time concepts are kept apart:
`period_x` is the survey period (e.g. `"2013/18"`), not a publication or
boundary year; `gem_stand_d` is the municipality boundary stand the
results are reported on; the year of the aerial photographs a value is
based on is kept in `flight_year_x` where the source delivers it.

Missing, zero and suppressed values are distinguished in
`value_status_c`: `"observed"` (including true zeros), `"suppressed"`
(confidentiality), `"not-applicable"` (the value cannot exist) and
`"missing"` (not delivered, including cells absent from the response).
The raw SDMX observation status is kept in `obs_status_c`.

With `checkSums = TRUE` the sum over the classes of the requested level
is compared, per municipality, with the sum over the four main domains;
differences above `threshold` hectares raise a warning. A sum is only
checkable if all its values are present: municipalities with a missing
or suppressed class or domain are reported as not checkable instead of
being compared on incomplete sums. The outcome per municipality is kept
in the attribute `"sumCheck"` (columns `gem_id`, `check_c` with `"ok"`,
`"difference"`, `"not-checkable"`, and `diff_ha`).

Results are cached per period, level and region codelist (i.e.
municipality stand) of the source, so a new municipality stand of the
dataflow is never served from an older cached copy.

## See also

[`noas()`](https://andrisignorell.github.io/helvetia/reference/noas.md)
for the nomenclature.

Other swiss.statistics:
[`noas()`](https://andrisignorell.github.io/helvetia/reference/noas.md),
[`popGemeinde()`](https://andrisignorell.github.io/helvetia/reference/popGemeinde.md)

## Examples

``` r
if (FALSE) { # \dontrun{
area <- arealGemeinde(period = "2013/18", nClasses = 17)
head(area)
} # }
```
