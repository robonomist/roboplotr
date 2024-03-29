% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labels.R
\name{set_caption}
\alias{set_caption}
\title{Captions for \code{\link[=roboplot]{roboplot()}}}
\usage{
set_caption(text, ..., template = getOption("roboplot.caption.template"))
}
\arguments{
\item{text}{Character. The text used in the template (make sure the template has the parameter 'text').}

\item{...}{Other parameters to be passed to template.}

\item{template}{Character. Template for \code{\link[=str_glue]{str_glue()}} used to parse the caption with the given parameters.}
}
\value{
A string of classes 'glue' and 'character'.
}
\description{
Get a string for \code{\link[=roboplot]{roboplot()}} captions.
}
\examples{
# Used to define how captions are constructed inside roboplotr::roboplot()
# The used parameters are used inside stringr::str_glue() to parse a caption
# string. You can skip using set_caption() simply by giving a character string.
# You can provide the template here, or use the one defined globally with
# roboplotr::set_roboplot_options().


d <- energiantuonti |> dplyr::filter(Alue == "Kanada",Suunta == "Tuonti")

d |> roboplot(Alue,"Energian tuonti Kanadasta","Milj. €",
                   caption = "Tilastokeskus")


# Override the global template with function parameters and provide
# parameters for it. The example is unnecessarily complicated, but gives the
# idea how this could work.

d |>
  roboplot(Alue, "Energy import","Million euros",
           caption =
             set_caption(
               prepend = "(Canada)",
               append = paste0("(Customs Finland, International trade ",
                               "statistics;<br>Radiation and Nuclear Safety ",
                               "Authority; Gasum LLC)"),
               caption = "Statistics Finland",
               template = "{prepend}.<br>Source: {caption}.<br>{append}."
               )
  )

# If you need to make manual changes repeatedly, you are probably better off
# using roboplotr::set_roboplot_options() (documented therein) to change the
# defaults to something more sensible.

set_roboplot_options(
  caption_template = "Source: {text}.",
  )

d |> roboplot(Alue,"Energy import from Canada","M€", "Statistic Finland")

# Revert to defaults:
set_roboplot_options(reset = TRUE)

}
