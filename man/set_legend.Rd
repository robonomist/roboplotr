% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labels.R
\name{set_legend}
\alias{set_legend}
\alias{set_plot_legend}
\alias{set_map_legend}
\title{Legend configuration}
\usage{
set_legend(...)

set_plot_legend(
  position = NULL,
  orientation = NULL,
  maxwidth = NULL,
  title = FALSE,
  tidy = getOption("roboplot.legend")$tidy,
  xref = getOption("roboplot.legend")$xref,
  ...
)

set_map_legend(
  breaks = 5,
  position = "bottomleft",
  orientation = "vertical",
  title = NULL,
  opacity = 1,
  labformat = NULL,
  gradient = NULL,
  ...
)
}
\arguments{
\item{...}{
  Arguments passed on to \code{\link[=set_plot_legend]{set_plot_legend}}, \code{\link[=set_map_legend]{set_map_legend}}
  \describe{
    \item{\code{}}{}
  }}

\item{position}{Character. Either "bottom" or NA for no legend for \link[=roboplot]{roboplot},
or one of "bottomleft", "bottomright", or "none" for \link[=robomap]{robomap}. On a
\link[=roboplot]{roboplot}, the legend is removed on default if column in data for
param \code{d} of the \link[=roboplot]{roboplot} that is defined by param \code{color} of that
plot has only one observation.}

\item{orientation}{Character. Currently unused.}

\item{maxwidth}{Numeric. All \link[=roboplot]{roboplot} legend items (and y-axis
values for horizontal barplots) longer than this will be trunctated with an ellipsis.}

\item{title}{Logical or character. TRUE if you want the parameter \code{color} from
the \link[=roboplot]{roboplot} to be the legend title. Character if you want to provide
your own legend title.}

\item{tidy}{Logical. Controls whether the \link[=roboplot]{roboplot} legend items
will have matching widths
across columns. Default is FALSE.}

\item{breaks}{Numeric vector. A length 1 vector attempts to make that many
\link[=robomap]{robomap} legend entries. Length 2+ vector should be the breaks in
values of param \code{d} of \link[=robomap]{robomap} where the legend should be split.}

\item{opacity}{Numeric. The opacity of the \link[=robomap]{robomap} legend, ranging
from  0 to 1.}

\item{labformat}{Function. Specify custom label formatting function. The function
must take a numeric vector of labels the map legend might have. Mostly useful for
small number of specific labels.}

\item{gradient}{Logical. If TRUE, the legend will be a gradient. Default TRUE
for numeric data, FALSE for factor data.}
}
\value{
A list of class roboplotr.set_legend
}
\description{
Parameters to customize legend in \link[=roboplot]{roboplots} and \link[=robomap]{robomaps}.
}
\examples{
# You can use `set_legend` to control how legends are displayed on a `roboplot()`.

d <- energiantuonti |>
  dplyr::filter(Alue == "Venäjä")

# Use `position` NA to remove the legend.
d |>
  roboplot(Suunta, legend = set_legend(position = NA))

# `roboplot()` omits the legend to give more space for the plot area, when there
# is only a single trace for `color`. Use `position` to force the legend to show.
# You can also use `title` to set a title.

d |>
  dplyr::filter(Suunta == "Tuonti") |>
  roboplot(Suunta, legend = set_legend(title = "Example", position = "bottom"))

# Legend title is distinct from axis-specific legend titles which are controlled
# by `set_axes()` parameters `ylegend` and `y2legend`, when `y2` is used to move
# items from `color` to a secondary y-axis.

d |>
  roboplot(
    color = Suunta,
    plot_axes = set_axes(
      y2 = "Tuonti",
      ylegend = "Left yaxis",
      y2legend = "Right yaxis"
    ),
    legend = set_legend(title = "Example")
  )

# Use `tidy` to force legend items to have equal horizontal space across columns
# for slightly neater looking plots. Avoid if space is at premium.

energiantuonti |>
  dplyr::filter(Suunta == "Tuonti") |>
  roboplot(
    Alue,
    legend = set_legend(tidy = TRUE)
  )

# `set_legend()` works with `robomap()` too, but with a bit different parameters.
if (requireNamespace("sf", quietly = TRUE)) {

d <- vaesto_postinumeroittain |>
  dplyr::filter(stringr::str_detect(Postinumero, "^00(8|9)"))

# Control number of legend items with `breaks`. `robomap()` attempts to use that
# many items, but might settle for a near value.

d |> robomap(Postinumero, "Väkiluku", legend = set_legend(breaks = 5))

# If your legend won't be gradient, you can set specific breakpoints
d |>
  robomap(Postinumero,
          "V\u00e4kiluku",
          legend = set_legend(breaks = c(9000, 12000, 18000), gradient = FALSE))

# Adjust position and opacity.
d |>
  robomap(Postinumero,
          "V\u00e4kiluku",
          legend = set_legend(position = "bottomright", opacity = 0.3))

# Use factor column as value to have labeled legend.
d |>
  dplyr::mutate(
    value = dplyr::case_when(
      value >= stats::quantile(d$value)["75\%"] ~ "Suuri",
      value <= stats::quantile(d$value)["25\%"] ~ "Pieni",
      TRUE ~ "Normaali"
  ),
  value = forcats::fct_relevel(value, "Pieni","Normaali","Suuri")
  ) |>
  robomap(Postinumero, "V\u00e4kiluku")

}
}
