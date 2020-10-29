
[![Travis build
status](https://travis-ci.org/rstudio/forge.svg?branch=master)](https://travis-ci.org/rstudio/forge)[![Coverage
status](https://codecov.io/gh/rstudio/forge/branch/master/graph/badge.svg)](https://codecov.io/github/rstudio/forge?branch=master)[![AppVeyor
build
status](https://ci.appveyor.com/api/projects/status/github/kevinykuo/forge?branch=master&svg=true)](https://ci.appveyor.com/project/kevinykuo/forge)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# forge

**forge** provides functions for input checking and casting. It is
intended to be used by package developers, especially for interoperating
with other runtimes, such as Python and JVM languages. It contains two
families of functions, `cast_*` and `certify()`. The former casts a
value to a specific type, while the latter ensures certain conditions
are met.

## Installation

You can install **forge** from CRAN with

``` r
install.packages("forge")
```

You can install the development version from GitHub with

``` r
devtools::install_github("rstudio/forge")
```

## Examples

Here we demonstrate **forge** with a trivial function:

``` r
#' @import forge
fib <- function(n) {
  n <- cast_scalar_integer(n, return_id = TRUE) %>%
    certify(gte(0))
  
  if (n <= 2) {
    if( n >= 0) 1 else 0 
  } else {
    Recall(n - 1) + Recall(n - 2)
  }
}
fib(10)
#> [1] 55
```

``` r
fib(1.5)
#> Error: `n` cannot be cast to an integer vector.
```

``` r
fib(-2)
#> Error: Condition `gte(0)` not satisfied for `n`.
```

We can also provide arbitrary conditions to `certify()`:

``` r
some_vec <- 1:5
certify(some_vec,  ~ any(.x < 2))
#> [1] 1 2 3 4 5
```

``` r
certify(some_vec, ~ mean(.x) > 2)
#> [1] 1 2 3 4 5
```

``` r
certify(some_vec, ~ all(.x <= 5), ~ mean(.x) > 3)
#> Error: Condition `~mean(.x) > 3` not satisfied for `some_vec`.
```

-----

Please note that the ‘forge’ project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
