% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colors_patterns.R
\name{set_markers}
\alias{set_markers}
\title{Set roboplot() marker appearance}
\usage{
set_markers(
  symbol = getOption("roboplot.markers")$symbol,
  size = getOption("roboplot.markers")$size
)
}
\arguments{
\item{symbol}{Character. The marker symbol when drawn by \code{\link[=roboplot]{roboplot()}}. Any
one of "circle","square","diamond","cross","x", "line" or "star".}

\item{size}{Numeric. Marker size for markers.}
}
\description{
Set roboplot() marker appearance
}
\examples{
# You can change the markers roboplot() uses by using set_markers().
energiantuonti |>
  dplyr::filter(Suunta == "Tuonti", Alue == "Venäjä") |>
  dplyr::group_by(Alue) |>
  dplyr::mutate(sd = sd(value)) |>
  dplyr::ungroup() |>
  roboplot(
    Alue,
    plot_type = "scatter",
    plot_mode = "scatter",
    markers = set_markers(symbol = "diamond", size = 12)
  )
# You cannot control the markers by trace, but you can use pattern along with
# markers if you have set 'plot_mode' to "scatter+line".
energiantuonti |>
  dplyr::filter(Suunta == "Tuonti", Alue == "Venäjä") |>
  dplyr::group_by(Alue) |>
  dplyr::mutate(sd = sd(value)) |>
  dplyr::ungroup() |>
  roboplot(
    Alue,
    plot_type = "scatter",
    plot_mode = "scatter+line",
    pattern = set_pattern(Alue, pattern_types = c("Venäjä" = "dot")),
    markers = set_markers(symbol = "square", size = 8)
  )

}
