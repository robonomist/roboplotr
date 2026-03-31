# Set `robotable()` global styling defaults.

Control stripe appearance only at the moment.

## Usage

``` r
set_table_options(
  stripecolor = getOption("roboplot.table_defaults")$stripecolor,
  stripeopacity = getOption("roboplot.table_defaults")$stripeopacity,
  ...
)
```

## Arguments

- stripecolor:

  Character. A valid css color name or 6-digit hex color.

- stripeopacity:

  Numeric. A number between 0 and 1.

- ...:

  Placeholder for future table_options
