
# tidycausal

<!-- badges: start -->
<!-- badges: end -->

<!--

The goal of tidycausal is to ...


## Installation

You can install the development version of tidycausal from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("shafayetShafee/tidycausal")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidycausal)
## basic example code
```

-->

## Done so far

- created function for calculationg RR, OR (for working on both data and inside tidyverse functions like `mutate` or `summarise`). Function name started with `d` means its meant for dataframe. But actually we need to do unit test on input data quality.

- created function for generating pseudo popn



## TODOS:

- MOST IMPORTANTLY, we need to decide on how to recode yes or no so that in function defintion use of `n[2]`, `sum(n)` remains same.
- need to solve the R CMD check note `no visible binding for global variable`
- writing tests (both development-time and runtime or assertions)
- how to handle the case for null value or NA
- Chekcing for data other than wcgs
- writing bare minimum docs (using roxygen)
