# Get the Municipalities Valid at a Reference Date

Returns the stock of Swiss political municipalities valid at a given
date, together with their district and canton, from the historicised
municipality register of the Swiss Federal Statistical Office (AGVCH).
This is the reference against which statistics and geometries of the
same stand are joined, and the starting point for every analysis that
crosses a municipality merger.

## Usage

``` r
gemeinde(date = NULL, cache = TRUE, refresh = FALSE)
```

## Arguments

- date:

  reference date, a `Date` or a string `"YYYY-MM-DD"`. The default
  `NULL` uses today.

- cache:

  logical; if `TRUE`, results are read from and written to the local
  cache (see
  [`cacheList()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md)).

- refresh:

  logical; if `TRUE`, the source is queried again even if a cached copy
  exists.

## Value

A data.frame with one row per municipality and the columns

- `stand_d`:

  date of the register stand.

- `gem_id`:

  BFS municipality number.

- `gem_x`:

  municipality name.

- `kanton_c`:

  canton abbreviation, e.g. `"ZH"`.

- `kanton_id`:

  BFS canton number, 1 to 26.

- `bezirk_id`:

  BFS number of the district or equivalent unit.

- `bezirk_x`:

  district name.

- `hist_id`:

  AGVCH historical number of the municipality version.

- `valid_from_d`, `valid_to_d`:

  validity of that version.

The key is `stand_d` + `gem_id`; source metadata via
[`sourceInfo()`](https://andrisignorell.github.io/helvetia/reference/sourceInfo.md).

## Details

The hierarchy canton -\> district -\> municipality is derived from the
`Parent` references of the AGVCH snapshot service, not from positional
or level codes. Each municipality occurs at most once per date; this is
checked and a violation is an error.

## See also

[`gemeindeMutationen()`](https://andrisignorell.github.io/helvetia/reference/gemeindeMutationen.md)
for the changes between two stands.

Other swiss.register:
[`gemeindeMutationen()`](https://andrisignorell.github.io/helvetia/reference/gemeindeMutationen.md),
[`kanton()`](https://andrisignorell.github.io/helvetia/reference/kanton.md),
[`praemienregion()`](https://andrisignorell.github.io/helvetia/reference/praemienregion.md),
[`raumgliederung()`](https://andrisignorell.github.io/helvetia/reference/raumgliederung.md)

## Examples

``` r
if (FALSE) { # \dontrun{
gem <- gemeinde(as.Date("2021-01-01"))
table(gem$kanton_c)
} # }
```
