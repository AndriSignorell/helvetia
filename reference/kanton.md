# Get the Swiss Cantons

Returns the 26 Swiss cantons with their official BFS number,
abbreviation and name in German, French and Italian. Use it to label
cantonal results or to translate between the numeric code and the
abbreviation.

## Usage

``` r
kanton()
```

## Value

A data.frame with the columns `kanton_id`, `kanton_c`, `kanton_x_de`,
`kanton_x_fr` and `kanton_x_it`.

## Details

The table is shipped with the package; the cantonal numbering has been
stable since the founding of the canton of Jura in 1979.

## See also

[`geoKanton()`](https://andrisignorell.github.io/helvetia/reference/geo-boundaries.md).

Other swiss.register:
[`gemeinde()`](https://andrisignorell.github.io/helvetia/reference/gemeinde.md),
[`gemeindeMutationen()`](https://andrisignorell.github.io/helvetia/reference/gemeindeMutationen.md),
[`praemienregion()`](https://andrisignorell.github.io/helvetia/reference/praemienregion.md),
[`raumgliederung()`](https://andrisignorell.github.io/helvetia/reference/raumgliederung.md)

## Examples

``` r
kanton()
#>    kanton_id kanton_c            kanton_x_de                  kanton_x_fr
#> 1          1       ZH                 Zürich                       Zurich
#> 2          2       BE                   Bern                        Berne
#> 3          3       LU                 Luzern                      Lucerne
#> 4          4       UR                    Uri                          Uri
#> 5          5       SZ                 Schwyz                      Schwytz
#> 6          6       OW               Obwalden                       Obwald
#> 7          7       NW              Nidwalden                      Nidwald
#> 8          8       GL                 Glarus                       Glaris
#> 9          9       ZG                    Zug                         Zoug
#> 10        10       FR               Freiburg                     Fribourg
#> 11        11       SO              Solothurn                      Soleure
#> 12        12       BS            Basel-Stadt                   Bâle-Ville
#> 13        13       BL       Basel-Landschaft                Bâle-Campagne
#> 14        14       SH           Schaffhausen                  Schaffhouse
#> 15        15       AR Appenzell Ausserrhoden Appenzell Rhodes-Extérieures
#> 16        16       AI  Appenzell Innerrhoden Appenzell Rhodes-Intérieures
#> 17        17       SG             St. Gallen                   Saint-Gall
#> 18        18       GR             Graubünden                      Grisons
#> 19        19       AG                 Aargau                      Argovie
#> 20        20       TG                Thurgau                    Thurgovie
#> 21        21       TI                 Tessin                       Tessin
#> 22        22       VD                  Waadt                         Vaud
#> 23        23       VS                 Wallis                       Valais
#> 24        24       NE              Neuenburg                    Neuchâtel
#> 25        25       GE                   Genf                       Genève
#> 26        26       JU                   Jura                         Jura
#>           kanton_x_it
#> 1              Zurigo
#> 2               Berna
#> 3             Lucerna
#> 4                 Uri
#> 5              Svitto
#> 6             Obvaldo
#> 7            Nidvaldo
#> 8             Glarona
#> 9                Zugo
#> 10           Friburgo
#> 11            Soletta
#> 12      Basilea Città
#> 13   Basilea Campagna
#> 14          Sciaffusa
#> 15 Appenzello Esterno
#> 16 Appenzello Interno
#> 17          San Gallo
#> 18           Grigioni
#> 19            Argovia
#> 20           Turgovia
#> 21             Ticino
#> 22               Vaud
#> 23            Vallese
#> 24          Neuchâtel
#> 25            Ginevra
#> 26              Giura
```
