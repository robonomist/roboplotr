# Modebar configuration

Parameters to add and customize modebars in
[roboplots](https://robonomist.github.io/roboplotr/reference/roboplot.md).

## Usage

``` r
set_modebar(
  buttons = getOption("roboplot.modebar")$buttons,
  display = getOption("roboplot.modebar")$display,
  title = getOption("roboplot.modebar")$title,
  zoom = getOption("roboplot.modebar")$zoom,
  ...
)
```

## Arguments

- buttons:

  Character vector. Buttons contained in modebar in the given order.
  Must contain any of "home", "closest", "compare", "zoomin", "zoomout",
  "img_w", "img_n", "img_s", "data_dl" and "robonomist" in any order.

- display:

  Character. One of "constant", "hover" or "none". Controls whether
  modebar is visible always, only on hover, or never. Whatever the
  choice, static exports will not display a modebar.

- title:

  This will be the title used for static files downloaded through
  modebar. Control image download title extension specifications with
  [`set_imgdl_layout()`](https://robonomist.github.io/roboplotr/reference/set_imgdl_layout.md).
  [roboplot](https://robonomist.github.io/roboplotr/reference/roboplot.md)
  `title` will be used if no title is given here.

- zoom:

  Integer. Static plot downloaded through modebar will be magnified by
  this multiplier. Note that this will affect final file dimensions if
  you have specified some with
  [set_roboplot_options](https://robonomist.github.io/roboplotr/reference/set_roboplot_options.md).
  Default is 1.

- ...:

  Placeholder for other parameters.

## Value

A list.
