# onelogin

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/onelogin)](https://cran.r-project.org/package=onelogin)
<!-- badges: end -->

The goal of `onelogin` is to wrap the API for the identity provider 'OneLogin'.

## Installation

You can install the current version of `onelogin` on CRAN at <https://CRAN.R-project.org/package=onelogin> or the development version from 'github' using

```
devtools::install_github("akgold/onelogin")
```

## Get All Users

This is a basic example which shows you how to get all users:

``` r
library(onelogin)
con <- onelogin()

ol_users_get(con)
```

