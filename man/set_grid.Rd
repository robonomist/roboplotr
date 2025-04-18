% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frame.R
\name{set_grid}
\alias{set_grid}
\title{Grid configuration.}
\usage{
set_grid(
  xcolor = getOption("roboplot.grid")$xcolor,
  ycolor = getOption("roboplot.grid")$ycolor,
  xcolor_minor = getOption("roboplot.grid")$xcolor_minor,
  ycolor_minor = getOption("roboplot.grid")$ycolor_minor,
  xwidth = getOption("roboplot.grid")$xwidth,
  ywidth = getOption("roboplot.grid")$ywidth,
  xdash = getOption("roboplot.grid")$xdash,
  ydash = getOption("roboplot.grid")$ydash,
  xtick = NULL,
  ytick = NULL
)
}
\arguments{
\item{xcolor, ycolor, xtick, ytick, xcolor_minor, ycolor_minor}{Characters. Plot gridline colors. Must be hexadecimal
colors or a valid css colors.}

\item{xwidth, ywidth}{Numerics. Plot gridline widths.}

\item{xdash, ydash}{Characters. Plot gridline linetypes. Must contain one of
"solid", "dash", "dot", "longdash", "dashdot", and "longdashdot" in any order.}
}
\value{
A list of class roboplot.set_grid
}
\description{
Parameters to customize grids in \link[=roboplot]{roboplots}.
}
\examples{
# You can control x- and y-grid color, width and linetype by using
# `set_grid()` inside `set_roboplot_options()`.

set_roboplot_options(
  grid = set_grid(ycolor = "green", ydash = "longdash", xwidth = 3),
  )

energiantuonti |> dplyr::filter(Alue == "Venäjä") |> roboplot(Suunta)

# Reset to defaults
set_roboplot_options(reset = TRUE)

}
