#' helvetia: Swiss Public Statistics, Regional Classifications and Spatial
#' Data
#'
#' @description
#' helvetia gives a small, stable API to official Swiss sources and maps them
#' to one historicisable data model. The functions fall into six groups:
#'
#' **Registers** -- which units exist at a given date:
#'
#' | Function | Purpose | Key |
#' |---|---|---|
#' | [gemeinde()] | municipalities valid at a date (AGVCH) | `stand_d` + `gem_id` |
#' | [gemeindeMutationen()] | mergers, splits, renamings (AGVCH) | `mutation_id` + old/new version |
#' | [raumgliederung()] | spatial classifications (AGVCH) | `stand_d` + `gem_id` + `typ_c` |
#' | [praemienregion()] | health insurance premium regions (BAG) | `jahr_n` + `gem_id` + `preg_c` |
#' | [kanton()] | cantons with numbers and names | `kanton_id` |
#'
#' **Statistics** -- values per municipality:
#'
#' | Function | Purpose | Key |
#' |---|---|---|
#' | [popGemeinde()] | population by sex and age (STATPOP) | `jahr_n` + `gem_stand_d` + `gem_id` + `sex_c` + `age_n` |
#' | [arealGemeinde()] | land use (Arealstatistik) | `period_x` + `gem_stand_d` + `gem_id` + `noas_c` |
#' | [noas()] | NOAS04 nomenclature | `noas_c` |
#'
#' **Geometries** -- polygons and points:
#'
#' | Function | Purpose |
#' |---|---|
#' | [geoGemeinde()], [geoBezirk()], [geoKanton()] | institutional boundaries |
#' | [geoThemakart()] | any ThemaKart layer, incl. vegetation area and label points |
#' | [geoGewaesser()] | lakes and rivers |
#' | [themakartSets()] | available ThemaKart sets |
#'
#' **Maps** (formerly bfsMaps): [plotMap()], [addGewaesser()],
#' [stampBfs()].
#'
#' **Spatial tools**: [combineRegions()], [neighbours()], [locate()],
#' [geocode()].
#'
#' **Sources**: [sourceInfo()], [cacheList()], [cacheClear()].
#'
#' Column names encode the type by suffix: `_id` identifiers, `_x` text,
#' `_c` codes, `_n` numbers, `_d` dates, `_l` logicals. Register stands
#' (`stand_d`), statistical years (`jahr_n`), survey periods (`period_x`)
#' and boundary stands (`gem_stand_d`) are never mixed: a bare `gem_id` is
#' not sufficient for historical analyses. The column names follow the
#' attribute conventions of the source databases and are therefore in
#' `snake_case`, unlike function arguments and list components, which are
#' lowerCamelCase.
#'
#' @importFrom graphics par plot strheight strwidth text title locator
#' @importFrom grDevices col2rgb rgb xy.coords
#' @importFrom stats na.omit setNames
#' @importFrom utils capture.output head modifyList packageVersion read.csv
#'   read.csv2 unzip
#' @keywords internal
"_PACKAGE"
