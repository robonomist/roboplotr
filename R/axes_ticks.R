#' Define axis and axis tick formats for [roboplot()] plots.
#'
#' @param x,y Character. The name of the column from parameter 'd' of
#' [roboplot()] used for the axis. Defaults to "time" and "value".
#' @param xticktype,yticktype Characters. Determines axis formatting within
#' [roboplot()]. One of "character", "date" or "numeric". Defaults to "date" and
#' "numeric".
#' @param xtitle,ytitle Character. Used as the axis title.
#' @param xformat,yformat Character. Formatting for axis tick text. Use
#' \href{https://github.com/d3/d3-3.x-api-reference/blob/master/Time-Formatting.md}{d3 time format}
#' for dates and \href{https://github.com/d3/d3-3.x-api-reference/blob/master/Formatting.md#d3_forma}{d3 number format}
#' for numbers. Ignored for charaters.
#' @param xlim,ylim Vector of length 2. Used as the axis limits. These are not
#' type-checked by [roboplot()], but should match the values used in the
#' respecting plot axis.
#' @return A list.
#' @examples
#' # The primary usage is for creating horizontal bar plots when combining the
#' # roboplotr::roboplot plot_axes control with plot_mode "horizontal".
#' d <- energiantuonti |>
#'   dplyr::filter(Alue %in% c("Kanada","Norja","Yhdistynyt kuningaskunta"))
#' attr(d, "frequency") <- "Quarterly"
#'
#' # Control the plot axes with roboplotr::roboplot_set_axes, setting the columns
#' # from roboplotr::roboplot variable 'd' as the axis data sources with 'x' and
#' # 'y', and ticktypes with 'xticktype' and 'yticktype'.
#' d |> dplyr::filter(time == max(time)) |>
#'   roboplot(Suunta,
#'            glue::glue("Energian tuonti {lubridate::year(max(d$time))}"),
#'            "Milj. €","Tilastokeskus",
#'            plot_type = "bar",
#'            plot_mode = "horizontal",
#'            plot_axes = roboplot_set_axes(
#'              y = "Alue",
#'              yticktype = "character",
#'              x = "value",
#'              xticktype = "numeric")
#'   )
#'
#' # You can use 'xtitle' and 'ytitle' to provide labeling for axis titles,
#' # and control the axis format with d3 time
#' # (https://github.com/d3/d3-3.x-api-reference/blob/master/Time-Formatting.md)
#' # and number
#' # (https://github.com/d3/d3-3.x-api-reference/blob/master/Formatting.md#d3_forma)
#' # formatting. You can also control the axis limits by providing a 2-length
#' # vector for 'xlim' and 'ylim'. These are not typed by roboplotr::roboplot(),
#' # but must match the corresponding axis values.
#'
#' d |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue,"Energian tuonti","Milj. €","Tilastokeskus",
#'            plot_axes = roboplot_set_axes(
#'              ytitle = "Arvo",
#'              xformat = "Y%y",
#'              yformat = "+$05,",
#'              ylim = c(-200,1500),
#'              xlim = c("2015-01-01","2023-01-01"))
#'   )
#'
#' # roboplotr::roboplot_set_axes also gives the user fine-grained control for
#' # plots where there might not be data in a format that is directly transerable
#' # to date or numeric format.
#'
#' d2 <- dplyr::tribble(
#'   ~time, ~value,
#'   "2021 Jan-Oct", 7.2,
#'   "Jan-Nov", 6.0,
#'   "Jan-Dec", 4.4,
#'   "2022 Jan-Feb", 3.7,
#'   "Jan-Mar", 0.7,
#'   "Jan-Apr", -2.7,
#'   "Jan-May", -4.0,
#'   "Jan-Jun", -5.4,
#'   "Jan-Jul", -6.4,
#'   "Jan-Aug", -7.4,
#'   "Jan-Sep", -8.0,
#'   "Jan-Oct", -8.8
#' )
#'
#' d2 |>
#'   roboplotr::roboplot(title = "Growth Rate of Investment",
#'                       caption = "National bureau of statistics, China",
#'                       plot_axes = roboplot_set_axes(xticktype = "character"))
#'
#' # Or you might have numeric data on both axes. roboplotr::roboplot() will draw
#' # any traces in the order they appear in the data, so it is up to the user to
#' # order the traces properly.
#'
#' d3 <- dplyr::tibble(time =
#'                seq.Date(from = lubridate::floor_date(
#'                  lubridate::today(), "years"
#'                )-lubridate::years(10),
#'                to = lubridate::floor_date(lubridate::today(),"years"),
#'                by = "year"),
#'              obs1 = round(runif(11, min = 0, max = 120),1),
#'              value = round(runif(11, min = -120, max = 120),1))
#'
#' d3 |> dplyr::arrange(obs1, value) |>
#'   roboplotr::roboplot(title = "Random values",
#'                       caption = "Robonomist",
#'                       plot_axes = roboplot_set_axes(
#'                         x = "obs1",
#'                         xticktype = "numeric"))
#'
#' # You might just want to switch the axes for time and value
#' d |> dplyr::filter(Suunta == "Tuonti") |>
#'   roboplotr::roboplot(Alue, "Energian tuonti","Milj. €", "Tilastokeskus",
#'                       plot_axes = roboplot_set_axes(
#'                         y = "time",
#'                         yticktype = "date",
#'                         x = "value",
#'                         xticktype = "numeric"
#'                       ))
#'
#' # Or you might want to draw character strings on some axis.
#' d |> dplyr::filter(Suunta == "Tuonti") |>
#'   roboplotr::roboplot(Alue, "Energian tuonti","Milj. €", "Tilastokeskus",
#'                       plot_axes = roboplot_set_axes(
#'                         y = "Alue",
#'                         yticktype = "character",
#'                         x = "value",
#'                         xticktype = "numeric"
#'                       ))
#'
#' # Making a reasonable line plot like this is a challenge, though, and you are
#' # better off with a horizontal bar plot.
#' d |> dplyr::filter(Suunta == "Tuonti") |>
#'   roboplotr::roboplot(Alue, "Energian tuonti","Milj. €", "Tilastokeskus",
#'                       plot_type = "bar",
#'                       plot_mode = "horizontal",
#'                       plot_axes = roboplot_set_axes(
#'                         y = "Alue",
#'                         yticktype = "character",
#'                         x = "value",
#'                         xticktype = "numeric"
#'                       ))
#'
#' @export
#' @importFrom dplyr case_when
roboplot_set_axes <- function(x = "time", y = "value", xticktype = "date", yticktype = "numeric", xtitle = "", ytitle = "", xformat = NULL, yformat = NULL, xlim = c(NA,NA), ylim = c(NA, NA)) {

  roboplotr_check_param(x, "character",allow_null = F, allow_na = F)
  roboplotr_check_param(y, "character",allow_null = F, allow_na = F)
  roboplotr_check_param(xticktype, "character",allow_null = F, allow_na = F)
  roboplotr_check_param(yticktype, "character",allow_null = F, allow_na = F)
  axis_types <- c("character","date","numeric")
  roboplotr_valid_strings(xticktype,axis_types,any)
  roboplotr_valid_strings(yticktype,axis_types,any)
  roboplotr_check_param(xformat, "character")
  roboplotr_check_param(yformat, "character")
  roboplotr_check_param(xlim, "vector", size = 2, allow_null = F, allow_na = F)
  roboplotr_check_param(ylim, "vector", size = 2, allow_null = F, allow_na = F)


  setclass <- function(type) {
    case_when(type == "date" ~ list(c("POSIXct","POSIXt","Date")),
              type == "character" ~ list(c("factor","character")),
              TRUE ~ list(type)
    ) |> unlist()}

  list(
    x = x,
    y = y,
    xticktype = xticktype,
    yticktype = yticktype,
    xclass = setclass(xticktype),
    yclass = setclass(yticktype),
    xtitle = xtitle,
    ytitle = ytitle,
    xformat = xformat,
    yformat = yformat,
    xlim = xlim,
    ylim = ylim
    )
}


#' @importFrom plotly layout config
roboplotr_set_axis_ranges <- function(p, range) {
  fixed_range <- if (any(c("zoom","zoomin2d","pan") %in% getOption("roboplot.modebar.buttons"))) { F } else { T }
  if(!"xlim" %in% names(range)) { range$xlim <- c(NA,NA) }
  if(!"ylim" %in% names(range)) { range$ylim <- c(NA,NA) }
  if(!all(is.na(range$ylim)) & any(is.na(range$ylim)) || !all(is.na(range$xlim)) & any(is.na(range$xlim))) {
    roboplotr_alert("Provide both ends for any axis limits!")
  }
  config(p, locale = "fi") |>
    layout(separators = ", ") |>
    layout(xaxis = if(all(is.na(range$xlim))) { list(fixedrange = fixed_range) } else { list(fixedrange = fixed_range, range = range$xlim) },
           yaxis = list(fixedrange = fixed_range, range = range$ylim)) |>
    layout(hovermode = "compare")

}

roboplotr_get_tick_layout <- function(ticktype,
                                     axis,
                                     tickformat,
                                     tickformatstops,
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
                  tickformatstops = if(is.null(tickformat)) {
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
                  } else {
                    NULL
                  },
                  tickformat = tickformat,
                  tickcolor = tick_color[[axis]],
                  showline = background_color != border_color[[axis]])
    if(!is.null(dtick)) { append(dlist, list(dtick = dtick)) } else { dlist }
  } else if (ticktype == "numeric") {
    list(tickfont = font,
         tickformat = if(is.null(tickformat)) { ",.3~f" } else { tickformat },
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
  tickformatstops <- ticktypes$dateformat %||%  "%Y"
  tickformatstops <- which(dateformats == tickformatstops) %||% 1
  tickformatstops <- dateformats[c(max(tickformatstops-2,1),max(tickformatstops-1,1), tickformatstops, min(tickformatstops+1,5))]
  dtick <- if (length(unique(p$data$time)) < 6) {
    switch(ticktypes$dateformat %||%  "%Y","%Y" = "M12","%YQ%q" = "M3","%m/%Y" = "M1",#"%YW%V" = 604800000,
           "%d.%m.%Y" = 86400000, "M12")
  } else { NULL }
  p <- p |>
    layout(xaxis= roboplotr_get_tick_layout(ticktypes$xticktype, "x", ticktypes$xformat, tickformatstops, dtick, ticktypes$reverse, ticktypes$xtitle),
           yaxis= roboplotr_get_tick_layout(ticktypes$yticktype, "y", ticktypes$yformat, tickformatstops, dtick, ticktypes$reverse, ticktypes$ytitle))

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
