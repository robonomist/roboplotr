% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hovertemplate.R
\name{set_hovertext}
\alias{set_hovertext}
\title{Hovertemplate specifications for \code{\link[=roboplot]{roboplot()}}}
\usage{
set_hovertext(
  frequency = NULL,
  rounding = 1,
  unit = "",
  text_col = NULL,
  extra = NULL
)
}
\arguments{
\item{frequency}{Character. Determines how hovertemplate dates are formatted.
One of "Annual", "Quarterly", "Monthly", "Weekly", "Daily" or NULL. Default
NULL. With NULL roboplot will try to determine the format based on argument
'd' of \code{\link[=roboplot]{roboplot()}}.}

\item{rounding}{Double. Determines the number of small digits of \code{\link[=roboplot]{roboplot()}}
hovertemplate values. Defaults to 1.}

\item{unit}{Character. Unit displayed for \code{\link[=roboplot]{roboplot()}} hovertemplate values.}

\item{text_col}{Symbol, string, or function resulting in symbol or string.
Column from param 'd' in roboplot() to use for labeling. If NULL, the column
used for 'color' (and if given, 'pattern') is used for labels.}

\item{extra}{Character vector. Extra text displayed under \code{\link[=roboplot]{roboplot()}}
hovertemplate.}
}
\value{
A list
}
\description{
Use in \code{\link[=roboplot]{roboplot()}} parameter 'hovertext' to get a list used for
hovertemplate text formatting
}
\examples{
# Use to give hovertemplate specifications for roboplotr::roboplot() plots,
# assumed normally to be called within roboplotr::roboplot().

# Without specifying hovertext, roboplotr::roboplot() will construct it based
# on data from argument 'd' of roboplotr::roboplot(), guessing frequency and using
# the argument subtitle' as the unit of value. Control displayed time with
# 'frequency', and unit rounding and labeling with 'rounding' and 'unit'.

d <- energiantuonti |>
  dplyr::filter(Alue \%in\% c("Kanada","Norja","Yhdistynyt kuningaskunta"),
                Suunta == "Tuonti")

d |> roboplot(
  Alue, "Energian tuonti", "Miljoonaa euroa", "Lähde: Tilastokeskus.",
  hovertext = set_hovertext(
    frequency = "Monthly", rounding = 2, unit = "Milj. €"
    )
)


# Parameter 'extra' will be added to last line(s) of the hovertemplate, which
# can be plain text or reference to the plotly object data, with d3 syntax.

d |> roboplot(
  Alue, "Energian tuonti", "Milj. €", "Lähde: Tilastokeskus.",
  hovertext = set_hovertext("Annual", extra = "(\%{x:|Q})")
)

}
