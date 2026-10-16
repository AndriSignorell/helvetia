# List and Clear the Local Cache

Downloads are kept in a local cache so that repeated calls are fast and
work offline. `cacheList()` shows what is stored, e.g. to see which
stands are available without network access; `cacheClear()` removes
entries, e.g. to free disk space or to force a complete new download.

## Usage

``` r
cacheList(source = NULL)

cacheClear(source = NULL)
```

## Arguments

- source:

  optional source name to restrict the operation to, one of `"agvch"`,
  `"pxweb"`, `"sdmx"`, `"area"`, `"geometry"`, `"themakart"`,
  `"priminfo"`; `NULL` for all sources.

## Value

`cacheList()` returns a data.frame with the columns `source`, `version`,
`file`, `sizeMb` and `modified`. `cacheClear()` returns the number of
removed files invisibly.

## Details

The cache lives in
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)`("helvetia", "cache")`;
set `options(helvetia.cacheDir = <path>)` to use another directory. A
single result is refreshed with the argument `refresh = TRUE` of the
respective data function.

## See also

Other swiss.source:
[`sourceInfo()`](https://andrisignorell.github.io/helvetia/reference/sourceInfo.md)

## Examples

``` r
cacheList()
#> [1] source   version  file     sizeMb   modified
#> <0 rows> (or 0-length row.names)
```
