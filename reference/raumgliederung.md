# Get the Spatial Classifications of Municipalities

Returns, for each municipality valid at a date, its code in the official
spatial classifications (Raumgliederungen) of the BFS: language regions
(`SPRGEB2020`), municipality typology (`GDETYP2020_9`, `GDETYP2020_25`),
agglomerations (`AGGL2020`), the urban-rural typology (`STALAN2020`) and
others. Use it to aggregate municipal data to regions or to stratify an
analysis by settlement type.

## Usage

``` r
raumgliederung(
  date = NULL,
  typology = NULL,
  lang = c("de", "fr", "it", "en"),
  cache = TRUE,
  refresh = FALSE
)
```

## Arguments

- date:

  reference date, a `Date` or a string `"YYYY-MM-DD"`. The default
  `NULL` uses today.

- typology:

  optional character vector of classification codes or code prefixes,
  matched case-insensitively, e.g. `"GDETYP2020"`. The default `NULL`
  returns all classifications.

- lang:

  language of the labels, one of `"de"`, `"fr"`, `"it"`, `"en"`.

- cache:

  logical; if `TRUE`, results are read from and written to the local
  cache (see
  [`cacheList()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md)).

- refresh:

  logical; if `TRUE`, the source is queried again even if a cached copy
  exists.

## Value

A data.frame in long format with the columns `stand_d`, `gem_id`,
`gem_x`, `typ_c` (classification) and `value_x` (code or label as
delivered). The key is `stand_d` + `gem_id` + `typ_c`.

## Details

The data come from the AGVCH levels service. The classifications
available depend on the date; they are selected by name, never by column
position.

## Note

Experimental. The levels service documents its output columns only
partially (they "vary with language, date and mapping configuration");
the result therefore keeps every classification column as delivered.

## See also

[`gemeinde()`](https://andrisignorell.github.io/helvetia/reference/gemeinde.md),
[`combineRegions()`](https://andrisignorell.github.io/helvetia/reference/combineRegions.md)
to draw the regions.

Other swiss.register:
[`gemeinde()`](https://andrisignorell.github.io/helvetia/reference/gemeinde.md),
[`gemeindeMutationen()`](https://andrisignorell.github.io/helvetia/reference/gemeindeMutationen.md),
[`kanton()`](https://andrisignorell.github.io/helvetia/reference/kanton.md),
[`praemienregion()`](https://andrisignorell.github.io/helvetia/reference/praemienregion.md)

## Examples

``` r
if (FALSE) { # \dontrun{
rg <- raumgliederung(as.Date("2021-01-01"), typology = "GDETYP2020_9")
} # }
```
