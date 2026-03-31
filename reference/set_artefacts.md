# Visualization export configuration

Parameters to configure exports from
[roboplots](https://robonomist.github.io/roboplotr/reference/roboplot.md)
or
[robotables](https://robonomist.github.io/roboplotr/reference/robotable.md).

## Usage

``` r
set_artefacts(
  artefacts = getOption("roboplot.artefacts")$artefacts,
  title = NULL,
  filepath = getOption("roboplot.artefacts")$filepath,
  render = getOption("roboplot.artefacts")$render,
  self_contained = getOption("roboplot.artefacts")$self_contained,
  zoom = getOption("roboplot.artefacts")$zoom,
  auto = getOption("roboplot.artefacts")$auto,
  width = getOption("roboplot.artefacts")$width,
  height = getOption("roboplot.artefacts")$height,
  delay = getOption("roboplot.artefacts")$delay,
  quiet = getOption("roboplot.artefacts")$quiet
)
```

## Arguments

- artefacts:

  Character vector. Controls what artefacts are saved. One or more of
  "html", "png", "jpg", "jpge", "webp", or "pdf".

- title:

  Character. The filename of the artefact(s) created (without file
  format). Will be formatted with underscores, and the `title` text of
  `p` will be used if no title is provided.

- filepath:

  Character. The filepath to the created artefacts.

- render:

  Logical. Controls if the artefact will be displayed in viewer. Will be
  returned silently in either case.

- self_contained:

  Logical. Controls whether artefact's dependencies' are saved in an
  adjacent directory or contained within the file, increasing size.

- zoom:

  Numeric. Controls the zoom level of static plots if any are defined
  with 'artefacts'. Default 1.

- auto:

  Logical. Whether
  [roboplot](https://robonomist.github.io/roboplotr/reference/roboplot.md)
  or
  [robotable](https://robonomist.github.io/roboplotr/reference/robotable.md)
  will create artefacts automatically.

- width, height:

  Numeric. Sets the size of any static plots created. Any artefacts
  created with
  [`roboplot()`](https://robonomist.github.io/roboplotr/reference/roboplot.md)'s
  `artefacts` parameter will use the given dimensions, if any, for that
  plot.

- delay:

  Numeric. Delay in milliseconds before taking a screenshot. Used with
  static file creation. Default 0.2

- quiet:

  Logical. Controls whether artefact creation messages are printed.
  Default FALSE.

## Value

A list of class roboplot.set_artefacts.

## Examples

``` r
# Used to set global defaults for widget or other artefact creation. Any of
# these can be overridden by `roboplot()`. Use `filepath` to control which
# directory the artefacts are created to, `render` to control if the object will
# be rendered on artefact creation, `self_contained` to control if html plot
# dependencies are placed in an adjacent directory or contained within the html file,
# `artefacts` (one of "html", "png","jpg", "jpge", "jpge", "webp" or "pdf) to
# control what artefacts are created, and 'zoom' to set static artefact zoom level.

# create_widget() shows how the parameters are used.
```
