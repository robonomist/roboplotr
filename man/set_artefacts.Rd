% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/widgets.R
\name{set_artefacts}
\alias{set_artefacts}
\title{Artefact control for \code{\link[=roboplot]{roboplot()}}}
\usage{
set_artefacts(
  artefacts = getOption("roboplot.artefacts")$artefacts,
  title = NULL,
  filepath = getOption("roboplot.artefacts")$filepath,
  render = getOption("roboplot.artefacts")$render,
  self_contained = getOption("roboplot.artefacts")$self_contained,
  zoom = getOption("roboplot.artefacts")$zoom,
  auto = getOption("roboplot.artefacts")$auto,
  width = getOption("roboplot.artefacts")$width,
  height = getOption("roboplot.artefacts")$height
)
}
\arguments{
\item{artefacts}{Character vector. Controls what artefacts are saved. One or
more of "html", "png", "jpg", "jpge", "webp", or "pdf".}

\item{title}{Character. The filename of the artefact(s) created
(without file format). Will be formatted with underscores, and the title
of argument 'p' will be used if no title is provided.}

\item{filepath}{Character. The filepath to the created artefacts.}

\item{render}{Logical. Controls if the plot saved will be displayed in
viewer. Plot will be returned silently in either case.}

\item{self_contained}{Logical. Controls if the plot dependencies will be
saved in an adjacent directory "plot_dependencies" or contained within the
file, increasing size.}

\item{zoom}{Numeric. Controls the zoom level of static images if any are
defined with 'artefacts'. Default 1.}

\item{auto}{Logical. Whether \code{\link[=roboplot]{roboplot()}} will create artefacts automatically.}

\item{width, height}{Numeric. Sets the size of any static images created. Any
artefacts created with \code{\link[=roboplot]{roboplot()}}'s 'artefacts' parameter will use the given
dimensions, if any, for that plot.}
}
\value{
A list.
}
\description{
Set global parameters in \code{\link[=set_roboplot_options]{set_roboplot_options()}} for artefact creation
of \code{\link[=roboplot]{roboplot()}} plots.
}
\examples{
# Used to set global defaults for widget or other artefact creation. Any of
# these can be overridden by roboplotr::roboplot(). Only supposed to be
# called inside roboplotr::set_roboplot_options() or roboplotr::roboplot().
# Use 'filepath' to control which directory the artefacts are created to,
# 'render' to control if the roboplot() plot will be rendered on artefact
# creation, 'self_contained' to control if html plot dependencies are placed
# in an adjacent directory or contained within the html file, 'artefacts'
# (one of "html", "png","jpg", "jpge", "jpge", "webp" or "pdf) to control
# what artefacts are created, and 'zoom' to set static artefact zoom level.

# create_widget() shows how the parameters are used.
}
