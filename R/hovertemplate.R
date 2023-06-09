roboplotr_hovertemplate_freq <- function(f, default = "%Y-%m-%d") {
  if (is.null(f)) { default } else {
    switch(f,
           "Annual" = "%Y",
           "Quarterly" = "%YQ%q",
           "Monthly" = "%m/%Y",
           "Weekly" = "%YW%V",
           "Daily" = getOption("roboplot.locale")$date,
           default
    )
  }
}


#' Use in [roboplot()] parameter 'hovertext' to get a list used for
#' hovertemplate text formatting
#'
#' @param frequency Character. Determines how hovertemplate dates are formatted.
#' One of "Annual", "Quarterly", "Monthly", "Weekly", "Daily" or NULL. Default
#' NULL. With NULL roboplot will try to determine the format based on argument
#' 'd' of [roboplot()].
#' @param rounding Double. Determines the number of small digits of [roboplot()]
#' hovertemplate values. Defaults to 1.
#' @param unit Character. Unit displayed for [roboplot()] hovertemplate values.
#' @param extra Character vector. Extra text displayed under [roboplot()]
#' hovertemplate.
#' @importFrom stringr str_c
#' @examples
#' # Use to give hovertemplate specifications for roboplotr::roboplot() plots,
#' # assumed normally to be called within roboplotr::roboplot().
#'
#' # Without specifying hovertext, roboplotr::roboplot() will construct it based
#' # on data from argument 'd' of roboplotr::roboplot(), guessing frequency and using
#' # the argument subtitle' as the unit of value. Control displayed time with
#' # 'frequency', and unit rounding and labeling with 'rounding' and 'unit'.
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
#'
#' # Parameter 'extra' will be added to last line(s) of the hovertemplate, which
#' # can be plain text or reference to the plotly object data, with d3 syntax.
#'
#' d |> roboplot(
#'   Alue, "Energian tuonti", "Milj. €", "Lähde: Tilastokeskus.",
#'   hovertext = set_hovertext("Annual", extra = "(%{x:|Q})")
#' )
#'
#' @returns A list
#' @export
set_hovertext <- function(frequency = NULL, rounding = 1, unit = "", extra = NULL) {

  roboplotr_check_param(frequency, "character", allow_null = T)
  if(!is.null(frequency)) { roboplotr_valid_strings(frequency,c("Annual","Quarterly","Monthly","Weekly","Daily"), .fun = any) }
  roboplotr_check_param(rounding, "numeric", allow_null = F)
  roboplotr_check_param(unit, "character", allow_null = F)
  roboplotr_check_param(extra, "character", allow_null = T)

  rounding <- round(rounding)
  if (length(extra) > 1) { extra <- str_c("<br>",str_c(extra, collapse = " ")) }

  if(!is.null(frequency)) {
    frequency <- roboplotr_hovertemplate_freq(frequency, NULL)
  }
  if(unit != "") { unit <- str_c(" ", unit)}

  if(!is.null(extra)) {
    extra <- str_c("\n",extra)
  }

  list(dateformat = frequency, rounding = rounding, unit = unit, extra = extra)
}

roboplotr_hovertemplate <- function(params, lab = "text", ticktypes) {
  labstring <- str_c("%{",lab,"}")
  # browser()
  ystring <- case_when(all(ticktypes$pie == TRUE,ticktypes$yticktype %in% c("numeric","log")) ~ str_c("%{value:,.",params$rounding,"f}",params$unit),
                       ticktypes$yticktype == "character" ~ "",
                       ticktypes$yticktype %in% c("numeric","log") ~ str_c("%{y:,.",params$rounding,"f}",params$unit),
                       ticktypes$yticktype == "date" ~ str_c("%{y|",params$dateformat,"}"),
                       TRUE ~ "")
  if(str_length(ystring) == 0) { ystring <- NULL }
  xstring <- case_when(ticktypes$pie == TRUE ~ "",
                       ticktypes$xticktype == "character" ~ "%{x}",
                       ticktypes$xticktype %in% c("numeric","log") ~ str_c("%{x:,.",params$rounding,"f}",params$unit),
                       ticktypes$xticktype == "date" ~ str_c("%{x|",params$dateformat,"}"),
                       TRUE ~ "")
  if(str_length(xstring) == 0) { xstring <- NULL }
  str_c(c(labstring,ystring,xstring,params$extra,"<extra></extra>"), collapse = "<br>")
}
