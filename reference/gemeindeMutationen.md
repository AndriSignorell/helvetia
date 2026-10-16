# Get the Mutations of Municipalities

Returns the change history of the Swiss political municipalities between
two dates: mergers, splits, creations, abolitions, renamings,
renumberings and reassignments to another district. It is needed to
bring data of different municipality stands onto a common stand, and to
explain why a `gem_id` appears in one year and not in another.

## Usage

``` r
gemeindeMutationen(
  from = NULL,
  to = NULL,
  territoryExchange = FALSE,
  cache = TRUE,
  refresh = FALSE
)
```

## Arguments

- from, to:

  start and end of the period, each a `Date` or a string `"YYYY-MM-DD"`.
  The defaults are 1960-01-01, the start of the register, and today.

- territoryExchange:

  logical; if `TRUE`, mutations that consist only of territory exchanges
  between municipalities are included.

- cache:

  logical; if `TRUE`, results are read from and written to the local
  cache (see
  [`cacheList()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md)).

- refresh:

  logical; if `TRUE`, the source is queried again even if a cached copy
  exists.

## Value

A data.frame with one row per predecessor-successor pair and the columns
`mutation_id`, `mutation_d`, `mutation_type_c`, `old_gem_id`,
`new_gem_id`, `old_gem_x`, `new_gem_x`, `old_hist_id`, `new_hist_id`,
`old_bezirk_x`, `new_bezirk_x`, `old_bezirk_hist_id`,
`new_bezirk_hist_id`, `old_step_c` and `new_step_c`. The last two are
the source fields `InitialStep` and `TerminalStep`, passed through
unchanged.

## Details

The history comes from the AGVCH mutation service and is kept separate
from the municipality stock returned by
[`gemeinde()`](https://andrisignorell.github.io/helvetia/reference/gemeinde.md).
Each row links one predecessor version (`old_*`) to one successor
version (`new_*`) of a mutation; a merger of three municipalities
therefore yields three rows sharing the same `mutation_id`. Several
predecessors or successors are never collapsed into one field.

The service delivers no type code, so `mutation_type_c` is derived from
the structure of each mutation:

- `"creation"`, `"abolition"`:

  no predecessor or no successor.

- `"merger"`:

  several predecessors, one successor.

- `"split"`:

  one predecessor, several successors.

- `"territory-exchange"`:

  several municipalities that all persist.

- `"reorganisation"`:

  any other many-to-many mutation.

- `"rename"`, `"renumber"`, `"reassign"`:

  one-to-one change of name, BFS number or district; combinations are
  joined with a hyphen in this order, e.g. `"rename-reassign"`.

- `"other"`:

  one-to-one without a detectable change, e.g. a pure territory change
  when `territoryExchange = TRUE`.

`"reassign"` refers to the district *version*: a district that is itself
renamed gives all its municipalities a new district version.

## See also

[`gemeinde()`](https://andrisignorell.github.io/helvetia/reference/gemeinde.md)
for the stock at one date.

Other swiss.register:
[`gemeinde()`](https://andrisignorell.github.io/helvetia/reference/gemeinde.md),
[`kanton()`](https://andrisignorell.github.io/helvetia/reference/kanton.md),
[`praemienregion()`](https://andrisignorell.github.io/helvetia/reference/praemienregion.md),
[`raumgliederung()`](https://andrisignorell.github.io/helvetia/reference/raumgliederung.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mut <- gemeindeMutationen(from = "2020-01-01", to = "2021-01-01")
subset(mut, mutation_type_c == "merger")
} # }
```
