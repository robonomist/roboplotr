# Table label configuration

Parameters to add and customize labeling in
[robotables](https://robonomist.github.io/roboplotr/reference/robotable.md).

## Usage

``` r
set_robotable_labels(
  search = getOption("roboplot.locale")$robotable_labels$search,
  info = getOption("roboplot.locale")$robotable_labels$info,
  lengthMenu = getOption("roboplot.locale")$robotable_labels$lengthMenu,
  emptyTable = getOption("roboplot.locale")$robotable_labels$emptyTable,
  first = getOption("roboplot.locale")$robotable_labels$first,
  last = getOption("roboplot.locale")$robotable_labels$last,
  `next` = ">>",
  previous = "<<"
)
```

## Arguments

- search:

  Character. Label for search box.

- info, lengthMenu, first, last, next, previous:

  Character. Table navigation items.

- emptyTable:

  Character. The text shown when the table is empty.
