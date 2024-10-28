roboplotr_hoverlabel <- function(p) {
  .font <- getOption("roboplot.font.main")
  .bg <- .font$color
  .font$color <- roboplotr_text_color_picker(.bg, .font$size)
  p |> layout(hoverlabel = list(
    bgcolor = .bg,
    font = .font,
    bordercolor =  first(unlist(getOption(
      "roboplot.border"
    )[c("xcolor", "ycolor")]))
  ))
}

roboplotr_hovertemplate_freq <- function(f, default = "%Y-%m-%d") {
  if (is.null(f)) {
    default
  } else {
    switch(
      f,
      "Annual" = "%Y",
      "Quarterly" = "%YQ%q",
      "Monthly" = "%m/%Y",
      "Weekly" = "%YW%V",
      "Daily" = getOption("roboplot.locale")$date,
      default
    )
  }
}

#' Hovertext configuration
#'
#' Parameters to control the formatting and content of hovertext in [roboplots][roboplot()].
#'
#' @param frequency Character. Determines the format of hovertemplate dates. One
#' of "Annual", "Quarterly", "Monthly", "Weekly", "Daily", or NULL. Defaults to
#' NULL, where [roboplot()] infers the format from `d`.
#' @param rounding Double. Number of decimal places for hovertemplate values. Default is set with [set_roboplot_options()].
#' @param unit Character. Unit displayed.
#' @param format Function. A function for exact control of unit formatting.
#' @param text_col Symbol or character. Column used from `d` in a [roboplot][roboplot()]
#' for labeling. If NULL, the column used for `color` (and, if given, `pattern`)
#' is used for labels.
#' @param extra Character vector. Extra text displayed under any hovertext.
#' @importFrom stringr str_c
#' @importFrom rlang enquo
#' @examples
#' # Use to give hovertemplate specifications for `roboplot()` plots.
#' # Without specifying hovertext, `roboplot()` will construct it based
#' # on data from argument `d` of `roboplot()`, guessing frequency and using
#' # the argument `subtitle` as the unit of value. Control formatting with
#' # `frequency`, `rounding`, and `unit`.
#'
#' d <- energiantuonti |>
#'   dplyr::filter(Alue %in% c("Kanada","Norja","Yhdistynyt kuningaskunta"),
#'                 Suunta == "Tuonti")
#'
#' d |> roboplot(
#'   Alue, "Energian tuonti", "Miljoonaa euroa", "Lähde: Tilastokeskus.",
#'   hovertext = set_hovertext(
#'     frequency = "Monthly", rounding = 2, unit = "Milj. €"
#'     )
#' )
#'
#' # Parameter 'extra' will be added to last line(s) of the hovertemplate, which
#' # can be plain text or reference to `d` of the `roboplot()`, with d3 syntax.
#'
#' d |> roboplot(
#'   Alue, "Energian tuonti", "Milj. €", "Lähde: Tilastokeskus.",
#'   hovertext = set_hovertext("Annual", extra = "(%{x:|Q})")
#' )
#'
#' @returns A list of class roboplotr.set_hovertext
#' @export
set_hovertext <- function(frequency = NULL,
                          rounding = getOption("roboplot.rounding"),
                          unit = "",
                          text_col = NULL,
                          format = NULL,
                          extra = NULL) {
  text_col <- enquo(text_col)
  roboplotr_typecheck(frequency, "character")
  if (!is.null(frequency)) {
    roboplotr_valid_strings(frequency,
                            c("Annual", "Quarterly", "Monthly", "Weekly", "Daily"),
                            .fun = any)
  }
  roboplotr_typecheck(rounding, "numeric", allow_null = F)
  roboplotr_typecheck(unit, "character", allow_null = F)
  roboplotr_typecheck(extra, "character")

  rounding <- round(rounding)
  if (length(extra) > 1) {
    extra <- str_c("<br>", str_c(extra, collapse = " "))
  }

  if (!is.null(frequency)) {
    frequency <- roboplotr_hovertemplate_freq(frequency, NULL)
  }
  if (unit != "") {
    unit <- str_c(" ", unit)
  }

  if (!is.null(extra)) {
    extra <- str_c("\n", extra)
  }

  .res <- list(
    dateformat = frequency,
    rounding = rounding,
    unit = unit,
    extra = extra,
    col = text_col,
    format = format
  )

  .res <- structure(.res,
                    class = c("roboplotr", "roboplotr.set_hovertext", class(.res)))

  .res

}

roboplotr_hovertemplate <- function(params, lab = "text", ticktypes) {
  labstring <- str_c("%{", lab, "}")
  ystring <- case_when(
    all(
      ticktypes$pie == TRUE,
      ticktypes$yticktype %in% c("numeric", "log")
    ) ~ str_c("%{value:,.", params$rounding, "f}", params$unit),
    ticktypes$yticktype == "character" ~ "",
    ticktypes$yticktype %in% c("numeric", "log") ~ str_c("%{y:,.", params$rounding, "f}", params$unit),
    ticktypes$yticktype == "date" ~ str_c("%{y|", params$dateformat, "}"),
    TRUE ~ ""
  )
  if (str_length(ystring) == 0) {
    ystring <- NULL
  }
  xstring <- case_when(
    ticktypes$pie == TRUE ~ "",
    ticktypes$xticktype == "character" ~ "%{x}",
    ticktypes$xticktype %in% c("numeric", "log") ~ str_c("%{x:,.", params$rounding, "f}", params$unit),
    ticktypes$xticktype == "date" ~ str_c("%{x|", params$dateformat, "}"),
    TRUE ~ ""
  )
  if (str_length(xstring) == 0) {
    xstring <- NULL
  }
  str_c(c(labstring, ystring, xstring, params$extra, "<extra></extra>"),
        collapse = "<br>")
}
