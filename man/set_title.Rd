% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labels.R
\name{set_title}
\alias{set_title}
\title{Title configuration.}
\usage{
set_title(title = NULL, include = T, ...)
}
\arguments{
\item{title}{Character. Optional. Will try to determine a title based on
attributes of \code{d} in a \link[=roboplot]{roboplots}. Defaults to "PLACEHOLDER".}

\item{include}{Logical. Determines whether the given title will be used in
the plot. Will always inlcude it for exported static images.}

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

d <- energiantuonti |> dplyr::filter(Alue == "Kanada",Suunta == "Tuonti")

d |> roboplot(Alue,"Energian tuonti Kanadasta")

# However, if you want to render the plot without the title included, you
# can use `set_title(include = F)` to omit it from the plot. This
# is for cases where you will insert the plot into environment where the
# title is outside the plot element. When exporting, you would till want to have
# the title included, and `roboplot()` takes care of this. If you include a subtitle,
# it will be displayed regardless.


d |>
roboplot(Alue,
title = set_title("Energian tuonti Kanadasta", include = FALSE),
subtitle = "Milj. €")


}
