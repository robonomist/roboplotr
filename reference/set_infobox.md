# Info box configuration for [`roboplot()`](https://robonomist.github.io/roboplotr/reference/roboplot.md) or [`robotable()`](https://robonomist.github.io/roboplotr/reference/robotable.md)

Global parameters to add and customize info boxes in
[roboplots](https://robonomist.github.io/roboplotr/reference/roboplot.md)
and
[robomaps](https://robonomist.github.io/roboplotr/reference/robomap.md).

## Usage

``` r
set_infobox(
  background = first(unlist(unique(getOption("roboplot.grid")[c("xcolor", "ycolor")]))),
  border = first(unlist(unique(getOption("roboplot.border")[c("xcolor", "ycolor")]))),
  border_width = 1
)
```

## Arguments

- background, border:

  Characters. Colors used for the corresponding element of the infopopup
  of
  [`robotable()`](https://robonomist.github.io/roboplotr/reference/robotable.md)s
  and
  [`roboplot()`](https://robonomist.github.io/roboplotr/reference/roboplot.md)s.
  Must be a hexadecimal color or a valid css color.

- border_width:

  Integer. The border width of infomodal.

## Value

A list of class roboplot.set_infobox.
