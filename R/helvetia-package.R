#' helvetia: Swiss public statistics, regional classifications and spatial
#' data
#'
#' A small, stable API to official Swiss sources, mapped to one
#' historicisable data model:
#'
#' | Function | Source | Key |
#' |---|---|---|
#' | [gemeinde()] | AGVCH snapshot | `stand_d` + `gem_id` |
#' | [gemeinde_mutationen()] | AGVCH mutations | `mutation_id` + old/new version |
#' | [raumgliederung()] | AGVCH levels | `stand_d` + `gem_id` + `typ_c` |
#' | [pop_gemeinde()] | BFS PXWeb STATPOP | `jahr_n` + `gem_id` + `sex_c` + `age_n` |
#' | [geo_gemeinde()] | swissBOUNDARIES3D | `stand_d` + `gem_id` |
#' | [areal_gemeinde()] | BFS SDMX Arealstatistik | `period_x` + `gem_stand_d` + `gem_id` + `noas_c` |
#' | [noas()] | BFS SDMX codelist NOAS04 | `noas_c` |
#'
#' Column suffixes encode the type: `_id` identifiers, `_x` text, `_c`
#' codes, `_n` numbers, `_d` dates, `_l` logicals. Register stands
#' (`stand_d`), statistical years (`jahr_n`), survey periods (`period_x`)
#' and boundary stands (`gem_stand_d`) are never mixed: a bare `gem_id` is
#' not sufficient for historical analyses.
#'
#' Downloads are cached locally (see [cache_list()]); the source of every
#' result can be inspected with [source_info()].
#'
#' @keywords internal
"_PACKAGE"
