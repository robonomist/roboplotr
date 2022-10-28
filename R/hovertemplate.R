roboplotr_hovertemplate_freq <- function(f, default = "%Y-%m-%d") {
  if (is.null(f)) { default } else {
    switch(f,
           "Annual" = "%Y",
           "Quarterly" = "%YQ%q",
           "Monthly" = "%m/%Y",
           "Weekly" = "%YW%V",
           "Daily" = "%d.%m.%Y",
           default
    )
  }
}


#' Creates a list used to construct roboplot hovertemplate
#'
#' @param frequency Character. Determines how hovertemplate dates are formatted.
#' One of "Annual", "Quarterly", "Monthly", "Weekly", "Daily" or NULL. Default NULL.
#' With NULL roboplot will try to determine the format based on arg 'd' or roboplot.
#' @param rounding Double. Determines the number of small digits of hovertemplate values. Default 1.
#' @param unit Character. Unit displayed for hovertemplate values.
#' @param extra Character vector. Extra text displayed on hovertemplate.
#' @importFrom stringr str_c
#' @returns A list
#' @export
roboplot_set_hovertext <- function(frequency = NULL, rounding = 1, unit = "", extra = "") {

  roboplotr_check_param(frequency, "character", allow_null = T)
  roboplotr_check_param(rounding, "numeric", allow_null = F)
  roboplotr_check_param(unit, "character", allow_null = F)
  roboplotr_check_param(extra, "character", allow_null = T)

  rounding <- round(rounding)
  if (length(extra) > 1) { extra <- str_c("<br>",str_c(extra, collapse = " ")) }

  if(!is.null(frequency)) {
    frequency <- roboplotr_hovertemplate_freq(frequency, NULL)
  }

  if(unit != "") { unit <- str_c(" ", unit)}

  list(dateformat = frequency, rounding = rounding, unit = unit, extra = extra)
}

roboplotr_hovertemplate <- function(params, lab = "text", val = "y", time = "x") {
  if(is.null(time)) { time <- "" } else { time <- str_c("%{",time,"|",params$dateformat,"}") }
  str_c("%{",lab,"}<br>%{",val,":,.",params$rounding,"f}",params$unit,"<br>",time,params$extra,"<extra></extra>")

}
