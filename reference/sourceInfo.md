# Get the Source Metadata of a Result

Returns the provenance record that every data object of helvetia
carries. Use it to cite a source correctly, to see which stand or
version a cached result is based on, or to reproduce a download exactly.

## Usage

``` r
sourceInfo(x)
```

## Arguments

- x:

  an object returned by one of the data functions, e.g.
  [`gemeinde()`](https://andrisignorell.github.io/helvetia/reference/gemeinde.md)
  or
  [`popGemeinde()`](https://andrisignorell.github.io/helvetia/reference/popGemeinde.md).

## Value

An object of class `SourceInfo`, a list with the elements

- `source`:

  description of the source.

- `url`:

  exact request URL(s).

- `retrievedAt`:

  time of the download.

- `checksum`:

  MD5 checksum of a downloaded file, else `NA`.

- `version`:

  source version, stand or period, else `NA`.

- `query`:

  POST selections sent to the source (PXWeb), else `NULL`; the URL alone
  does not describe such a request.

- `notes`:

  further remarks, or `NULL`.

`NULL` if `x` carries no metadata.

## Details

The record is stored in the attribute `"source"` and survives the cache:
a cached result reports the time of its original download, not of the
cache hit.

## See also

Other swiss.source:
[`cacheList()`](https://andrisignorell.github.io/helvetia/reference/cacheList.md)

## Examples

``` r
sourceInfo(kanton())
#> Source:     Official cantonal numbering and names (BFS)
#> URL:        /home/runner/work/_temp/Library/helvetia/extdata/kanton.csv
#> Retrieved:  2026-10-16 00:00:00 UTC
```
