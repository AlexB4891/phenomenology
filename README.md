
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phenomenology

La historia de `phenomenology` se remonta a 2020, un año que cambio la
vida de todos definitivamente. Durante dos años, con mi experiencia y la
cantidad de código que he escrito, he desarrollado funciones de R para
hacer análisis económico teorico y empirico. Teorico en cuanto la
familia de funciones `Micro` permiten hacer análisis microeconómicos al
estilo de los ejercicios presentados en el libro *Principios de
microeconomía* de Mankiw. Y por otro lado las funciones de la familia
`taxform` son funciones que permiten generar visualizaciones listas para
presentación empleando tablas de resultados o de resumen.

La dualidad de estas dos familias de funciones tiene tambien que ver con
los temas que a mi me llaman la atención los datos y la economía. El
nombre de la libreria es un tributo a Georg Wilhelm Friederich Hegel, de
quien aprendí que dualidades y antonimías son apenas el cascaron en el
camino hacía la comprensión de la realidad.

<!-- badges: start -->
<!-- badges: end -->

The goal of phenomenology is to …

## Installation

You can install the development version of phenomenology like so:

``` r
devtools::inst
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(phenomenology)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
