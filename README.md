
[![Travis build
status](https://travis-ci.org/kevinykuo/forge.svg?branch=master)](https://travis-ci.org/kevinykuo/forge)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# forge

Helper functions to mold values into shape for interoperating with other
language runtimes.

## Installation

You can install the development version from GitHub with

``` r
devtools::install_github("kevinykuo/forge")
```

## Examples

``` r
library(sparklyr)
sc <- spark_connect(master = "local")

spark_vector <- function(sc, x) {
  v <- as.list(forge::cast_double(x))
  invoke_new(sc, "org.apache.spark.ml.linalg.DenseVector", v)
}

spark_vector(sc, 1:3)
#> <jobj[9]>
#>   org.apache.spark.ml.linalg.DenseVector
#>   [1.0,2.0,3.0]
```

-----

Please note that the ‘forge’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
