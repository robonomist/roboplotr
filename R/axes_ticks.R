#' Define axis and axis tick formats for [roboplot()] plots.
#'
#' @param x,y Character. The name of the column from parameter 'd' of [roboplot()] used for the axis. Defaults to "time" and "value".
#' @param xticktype,yticktype Character. Determines axis formatting within [roboplot()]. One of "character", "date" or "numeric". Defaults to "date" and "numeric".
#' @return A list.
#' @examples
#' # Used to define how axis ticks are formatted inside roboplotr::roboplot()
#' # if the default "time" with tick formatting as "date" for x-axis and /
#' # or"value" with tick formatting as "numeric" for y-axis are not suitable.
#'
#' @export
#' @importFrom dplyr case_when
roboplot_set_axes <- function(x = "time", y = "value", xticktype = "date", yticktype = "numeric", xtitle = "", ytitle = "") {
  roboplotr_check_param(x, "character",allow_null = F, allow_na = F)
  roboplotr_check_param(y, "character",allow_null = F, allow_na = F)
  roboplotr_check_param(xticktype, "character",allow_null = F, allow_na = F)
  roboplotr_check_param(yticktype, "character",allow_null = F, allow_na = F)
  axis_types <- c("character","date","numeric")
  roboplotr_valid_strings(xticktype,axis_types,any)
  roboplotr_valid_strings(yticktype,axis_types,any)

  setclass <- function(type) {
    case_when(type == "date" ~ list(c("POSIXct","POSIXt","Date")),
              type == "character" ~ list(c("factor","character")),
              TRUE ~ list(type)
    ) |> unlist()}

  list(x = x, y = y, xticktype = xticktype, yticktype = yticktype, xclass = setclass(xticktype), yclass = setclass(yticktype), xtitle = xtitle, ytitle = ytitle)
}


#' @importFrom plotly layout config
roboplotr_set_axis_ranges <- function(p, range = list(x = c(NA,NA), y = c(NA,NA))) {
  fixed_range <- if (any(c("zoom","zoomin2d","pan") %in% getOption("roboplot.modebar.buttons"))) { F } else { T }
  if(!"x" %in% names(range)) { range$x <- c(NA,NA) }
  if(!"y" %in% names(range)) { range$y <- c(NA,NA) }
  if(!all(is.na(range$y)) & any(is.na(range$y)) || !all(is.na(range$x)) & any(is.na(range$x))) {
    roboplotr_alert("Provide both ends for any axis limits!")
  }
  config(p, locale = "fi") |>
    layout(separators = ", ") |>
    layout(xaxis = if(all(is.na(range$x))) { list(fixedrange = fixed_range) } else { list(fixedrange = fixed_range, range = range$x) },
           yaxis = list(fixedrange = fixed_range, range = range$y)) |>
    layout(hovermode = "compare") |>
    roboplotr_axis_labels()

}

roboplotr_get_tick_layout <- function(ticktype,
                                     axis,
                                     tickformat,
                                     dtick,
                                     reverse,
                                     title,
                                     background_color = getOption("roboplot.colors.background"),
                                     border_color = getOption("roboplot.colors.border"),
                                     tick_color = getOption("roboplot.colors.ticks")) {
  font <- getOption("roboplot.font.main")
  if (ticktype == "date") {
    # print(ticktype)
    dlist <- list(tickfont = font,
                  mirror = TRUE,
                  ticks = 'outside',
                  type = "date",
                  title = list(text = title, font = font),
                  tickformatstops =
                  list(
                    list(
                      dtickrange = list(NULL, 604800000),
                      value = tickformat[1]
                    ),
                    list(
                      dtickrange = list(604800000, "M1"),
                      value = tickformat[2]
                    ),
                    list(
                      dtickrange = list("M1", "M12"),
                      value = tickformat[3]
                    ),
                    list(
                      dtickrange = list("M12", NULL),
                      value = tickformat[4]
                    )
                  )
                  ,
                  # tickformat = roboplotr_hovertemplate_freq(ticktypes$dateformat),
                  tickcolor = tick_color[[axis]],
                  showline = background_color != border_color[[axis]])
    if(!is.null(dtick)) { append(dlist, list(dtick = dtick)) } else { dlist }
  } else if (ticktype == "numeric") {
    list(tickfont = font,
         tickformat = ",.3~f",
         ticksuffix = " ",
         mirror = TRUE,
         ticks = 'outside',
         title = list(text = title, font = font),
         tickcolor = tick_color[[axis]],
         showline = background_color != border_color[[axis]])
  } else if (ticktype == "character") {
    list(tickfont = font,
         ticksuffix = " ",
         autorange = ifelse(reverse, "reversed", TRUE),
         categoryorder = "trace",#ifelse(reverse, "trace", "trace"),
         tickmode = ifelse(axis == "y","linear","auto"),
         tickangle = "auto",
         mirror = TRUE,
         type = "category",
         ticks = 'outside',
         title = list(text = title, font = font),
         tickcolor = tick_color[[axis]],
         showline = background_color != border_color[[axis]])
  }

}
#' @importFrom plotly layout
#' @importFrom rlang %||%
roboplotr_set_ticks <- function(p, ticktypes) {
  dateformats <- c("Annual" = "%Y",
                   "Quarterly" = "%YQ%q",
                   "Monthly" = "%m/%Y",
                   # "Weekly" = "%YW%V",
                   "Weekly" = "%m/%Y",
                   "Daily" = "%d.%m.%Y")
  tickformat <- ticktypes$dateformat %||%  "%Y"
  tickformat <- which(dateformats == tickformat) %||% 1
  tickformat <- dateformats[c(max(tickformat-2,1),max(tickformat-1,1), tickformat, min(tickformat+1,5))]
  dtick <- if (length(unique(p$data$time)) < 6) {
    switch(ticktypes$dateformat %||%  "%Y","%Y" = "M12","%YQ%q" = "M3","%m/%Y" = "M1",#"%YW%V" = 604800000,
           "%d.%m.%Y" = 86400000, "M12")
  } else { NULL }

  p <- p |>
    layout(xaxis= roboplotr_get_tick_layout(ticktypes$x, "x", tickformat, dtick, ticktypes$reverse, ticktypes$xtitle),
           yaxis= roboplotr_get_tick_layout(ticktypes$y, "y", tickformat, dtick, ticktypes$reverse, ticktypes$ytitle))

  #placeholder, relayout.js pitää kirjoittaa tätä varten vähän uusiksi jotta legend, lähde ja logo asettuvat oikein jos tickit oikeasti poistaa
  if(length(unique(p$data$time))==1 & ticktypes$x == "date") {
    p <- p |> layout(xaxis = list(tickfont = list(color = getOption("roboplot.colors.background"))))
  }
  p
}


#' @importFrom dplyr case_when
#' @importFrom lubridate month quarter wday week yday
roboplotr_guess_xaxis_ceiling <- function(d, hovertext) {
  attr_freq <- roboplotr_get_dateformat(d,msg = F)
  hovertext_freq <- hovertext$dateformat

  freq <- if(any(!is.null(attr_freq), !is.null(hovertext_freq))) {
    c(attr_freq, hovertext_freq) |> first()
  } else { NULL }

  if(!is.null(freq)) {
    freq <- switch(freq, "Annual" = "years",
                   "Quarterly" = "quarters",
                   "Monthly" = "months",
                   "Weekly" = "weeks",
                   "Daily" = "days",
                   NULL)
    maxd <- max(d$time)
    yd <- yday(maxd)
    yq <- quarter(maxd)
    ym <- month(maxd)
    yw <- week(maxd)
    wd <- wday(maxd,week_start = 1)
    freq <- case_when(
      freq == "years" ~ "quarters",
      freq == "quarters" & yq == 4 ~ "years",
      freq == "months" & ym >= 10 ~ "years",
      freq == "weeks" & yw >= 40 ~"quarters",
      freq == "weeks" & yw >= 26 ~ "months",
      freq == "days" & yd >= 240 ~ "years",
      freq == "days" & yd >= 120 ~ "months",
      freq == "days" ~ "weeks",
      TRUE ~ freq)
  }

  if(is.null(freq)) { roboplotr_alert("Failed to guess the 'xaxis_ceiling', ne frequency information available.") } else {
    roboplotr_message("Roboplotr guesses the xaxis_ceiling is rounded to \"",freq,"\".")
  }
  freq
}
