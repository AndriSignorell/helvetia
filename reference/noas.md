# Get the NOAS04 Land Use Nomenclature

Returns the hierarchical standard nomenclature NOAS04 of the Swiss land
use statistics (Arealstatistik), separate from the measured values: 4
main domains, 17 base categories, 27 aggregated categories and 72 basic
categories. Use it to label land use classes in four languages and to
aggregate fine classes to coarser ones reproducibly.

## Usage

``` r
noas(nClasses = 17, cache = TRUE, refresh = FALSE)
```

## Arguments

- nClasses:

  level(s) of the nomenclature, given by their number of classes: any of
  `4`, `17`, `27`, `72`.

- cache:

  logical; if `TRUE`, results are read from and written to the local
  cache (see
  [`cacheList()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md)).

- refresh:

  logical; if `TRUE`, the source is queried again even if a cached copy
  exists.

## Value

A data.frame with the columns `level_n` (number of classes of the
level), `noas_c` (hierarchical code), `noas_n` (class number within the
level, e.g. 1 to 17), `parent_c` (code of the parent class, `NA` for the
main domains), `noas_x_de`, `noas_x_fr`, `noas_x_it` and `noas_x_en`.

## Details

The nomenclature is read from the SDMX codelist `CL_AREA_NOAS` of the
BFS dissemination service. If the service is unreachable and no cached
copy exists, a German-only copy shipped with the package is used and a
message says so.

Codes are hierarchical: the main domain is one digit, each finer level
appends the two-digit class number of that level, e.g. `"1"` -\> `"102"`
(base category 2) -\> `"10202"` (aggregated category 2) -\> `"1020203"`
(basic category 3).

## See also

[`arealGemeinde()`](https://andrisignorell.github.io/helvetia/reference/arealGemeinde.md)
for the measured areas.

Other swiss.statistics:
[`arealGemeinde()`](https://andrisignorell.github.io/helvetia/reference/arealGemeinde.md),
[`popGemeinde()`](https://andrisignorell.github.io/helvetia/reference/popGemeinde.md)

## Examples

``` r
if (FALSE) { # \dontrun{
noas(17)

# base category (17 classes) of each of the 72 basic categories
n72 <- noas(72)
n27 <- noas(27)
n72$noas17_c <- n27$parent_c[match(n72$parent_c, n27$noas_c)]
} # }
```
