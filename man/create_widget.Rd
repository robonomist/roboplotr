% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/widgets.R
\name{create_widget}
\alias{create_widget}
\title{Export visualizations to various formats}
\usage{
create_widget(
  p,
  title = NULL,
  filepath = getOption("roboplot.artefacts")$filepath,
  render = getOption("roboplot.artefacts")$render,
  self_contained = getOption("roboplot.artefacts")$self_contained,
  zoom = getOption("roboplot.artefacts")$zoom,
  artefacts = getOption("roboplot.artefacts")$artefacts,
  width = getOption("roboplot.artefacts")$width,
  height = getOption("roboplot.artefacts")$height,
  delay = getOption("roboplot.artefacts")$delay,
  ...
)
}
\arguments{
\item{p}{A plotly object.}

\item{title}{Character. The filename of the artefact(s) created
(without file format). Will be formatted with underscores, and the \code{title} text
of \code{p} will be used if no title is provided.}

\item{filepath}{Character. The filepath to the created artefacts.}

\item{render}{Logical. Controls if the artefact will be displayed in
viewer. Will be returned silently in either case.}

\item{self_contained}{Logical. Controls whether artefact's dependencies' are
saved in an adjacent directory or contained within the
file, increasing size.}

\item{zoom}{Numeric. Controls the zoom level of static plots if any are
defined with 'artefacts'. Default 1.}

\item{artefacts}{Character vector. Controls what artefacts are saved. One or
more of "html", "png", "jpg", "jpge", "webp", or "pdf".}

\item{width, height}{Numeric. Sets the size of any static plots created. Any
artefacts created with \code{\link[=roboplot]{roboplot()}}'s \code{artefacts} parameter will use the given
dimensions, if any, for that plot.}

\item{delay}{Numeric. Delay in seconds before taking a screenshot. Used with
static file creation. Default 0.2.}

\item{...}{Additional parameters in future use.}
}
\value{
What was passed as \code{p}.
}
\description{
Write html and and other files from \link[=roboplot]{roboplots}, \link[=robotable]{robotables}
or \link[=robomap]{robomaps}.
}
\examples{
set_roboplot_options(verbose = "Warning", .default = TRUE)
# Saving `roboplot()` plots as files can be controlled by setting global options
# with `set_roboplot_options()`, and using `set_artefacts()` in `roboplot()`
# `artefacts`, but you can use this function as well. Control location of the
# files with `filepath` (default is current working directory).
\dontrun{
d <- energiantuonti |> dplyr::filter(Alue == "USA",Suunta == "Tuonti")

d |>
  roboplot(
    Alue, "Energian tuonti Yhdysvalloista", "Milj €", "Tilastokeskus"
    ) |>
  create_widget(filepath = tempdir())

file.exists(paste0(tempdir(),"/energian_tuonti_yhdysvalloista.html"))

# You can provide the filename as string and `create_widget()` will parse the
# filename from that. The plot will always be silently returned, but `render`
# controls whether it will be displayed in viewer on widget creation. Normally
# `roboplotr` html widgets will have dependencies contained in an
# external folder, but they can be bundled within with 'self_contained'.

d |>
  roboplot(
    Alue, "Yhdysvaltojen energiantuonti", "Milj €", "Tilastokeskus"
    ) |>
  create_widget(
    title = "Energian tuonti - Yhdysvallat",
    filepath = tempdir(),
    render = FALSE,
    self_contained = FALSE
  )

file.exists(paste0(tempdir(),"/energian_tuonti_yhdysvallat.html"))

# If you want to create non-interactive files, use a character vector of file
# types in 'artefacts'. Possible filetypes are "html","png","jpg","jpge","webp",
# "pdf". The static files created this way will respect the plot layout specifications
# of the `roboplot()` plot, unlike the ones exported with modebar. Note that
# modebar gives access to svg file format, while automating it the file creation with
# `create_widget()` or `roboplot()` allows for pdf files.

  d |>
    roboplot(
      color = Alue,
      title = "Yhdysvaltojen energiantuonti",
      subtitle = "Milj €",
      caption = "Tilastokeskus",
      width = 400,
      height = 800
    ) |>
    create_widget(filepath = tempdir(), artefacts = "pdf")

  utils::browseURL(paste0(tempdir(), "/yhdysvaltojen_energiantuonti.pdf"))
}
}
