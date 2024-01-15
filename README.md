
<!-- README.md is generated from README.Rmd. Please edit that file -->

# roboplotr

<!-- badges: start -->

<!-- badges: end -->

The goal of roboplotr is to help experts build outstanding interactive
plots and setup plots according to organizations’ visual brand.

## Installation

You can install the development version of roboplotr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("robonomist/roboplotr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(roboplotr)
roboplotr::energiantuonti |>
  dplyr::filter(Alue == "EU-maat") |>
  roboplot(
    color = Suunta,
    title = "Energian ulkomaankauppa",
    subtitle = "Milj. euroa",
    caption = "Tilastokeskus",
    xaxis_ceiling = "year"
  )
```

![roboplotr
example](https://github.com/robonomist/roboplotr/blob/main/man/energian_ulkomaankauppa_levea.png?raw=true)
