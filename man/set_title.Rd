% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labels.R
\name{set_title}
\alias{set_title}
\title{Title configuration.}
\usage{
set_title(
  title = NULL,
  include = getOption("roboplot.title")$include,
  xref = getOption("roboplot.title")$xref,
  font = NULL,
  ...
)
}
\arguments{
\item{title}{Character. Optional. Will try to determine a title based on
attributes of \code{d} in a \link[=roboplot]{roboplots}. Defaults to "PLACEHOLDER". Omit
by providing "", but for \link[=roboplot]{roboplot}, you need to provide a \code{color}
if you don't have a title.}

\item{include}{Logical. Determines whether the given title will be used in
the displayed plot. Will always inlcude it for modebar-exported static images.
You should still provide a \code{title} even if you don't want it displayed in the
plot (see \code{title}).}

\item{xref}{Character. Either "container" or "plot". Determines if the title
is anchored to the left plot or the container edge.}

\item{font}{Call. Use \code{set_font()} to set font parameters for the title.
Currently only supports for global settings through \code{set_roboplot_options(title)}.}

\item{...}{Placeholder for other parameters.}
}
\value{
A list of class roboplotr.set_title
}
\description{
Parameters to customize titles of \link[=roboplot]{roboplots}. Can be used with
\link[=robomap]{robotables} and \link[=robomap]{robomaps}, but don't really do anything.
}
\examples{
# Used to set titles for plots created with `roboplot()`. You can
# simply give a charater string for plot titles.

d <- energiantuonti |> dplyr::filter(Alue == "USA",Suunta == "Tuonti")

d |> roboplot(Alue,"Energian tuonti Yhdysvalloista")

# However, if you want to render the plot without the title included, you
# can use `set_title(include = F)` to omit it from the plot. This
# is for cases where you will insert the plot into environment where the
# title is outside the plot element. When exporting, you would still want to
# have the title included, and `roboplot()` takes care of this. If you include
# a subtitle, it will be displayed regardless.


d |>
roboplot(Alue,
title = set_title("Energian tuonti Yhdysvalloista", include = FALSE),
subtitle = "Milj. €")

# Anchor the title to the left edge of the container instead of the plot with
# `xref`. You probably want to do the same for caption, but you don't have to.

 d |>
  roboplot(Alue,
           title = set_title("Energian tuonti Yhdysvalloista", xref = "container"),
           caption = set_caption("Tilastokeskus", xref = "container")
           )

}
