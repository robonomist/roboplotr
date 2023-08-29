#' Axis and tick control for [roboplot()]
#'
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
#' for numbers. Ignored for characters.
#' @param xlim,ylim Vector of length 2. Used as the axis limits. These are not
#' type-checked by [roboplot()], but should match the values used in the
#' respecting plot axis.
#' @return A list.
#' @examples
#' # The primary usage is for creating horizontal bar plots when combining the
#' # roboplotr::roboplot plot_axes control with plot_mode "horizontal".
#'
#' set_roboplot_options(
#'   caption_template = "Lähde: {text}."
#' )
#'
#' d <- energiantuonti |>
#'   dplyr::filter(Alue %in% c("Kanada","Norja","Yhdistynyt kuningaskunta"))
#' attr(d, "frequency") <- "Quarterly"
#'
#' # Control the plot axes with roboplotr::set_axes, setting the columns
#' # from roboplotr::roboplot variable 'd' as the axis data sources with 'x' and
#' # 'y', and ticktypes with 'xticktype' and 'yticktype'.
#' d |> dplyr::filter(time == max(time)) |>
#'   roboplot(Suunta,
#'            glue::glue("Energian tuonti {lubridate::year(max(d$time))}"),
#'            "Milj. €","Tilastokeskus",
#'            plot_type = "bar",
#'            plot_mode = "horizontal",
#'            plot_axes = set_axes(
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
#' # vector for 'xlim' and 'ylim'. These are not typed by roboplotr::roboplot(),
#' # but must match the corresponding axis values.
#'
#' d |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue,"Energian tuonti","Milj. €","Tilastokeskus",
#'            plot_axes = set_axes(
#'              ytitle = "Arvo",
#'              xformat = "Y%y",
#'              yformat = "+$05,",
#'              ylim = c(-200,1500),
#'              xlim = c("2015-01-01","2023-01-01"))
#'   )
#'
#' # Additionally, you may use logartihmic axis for any numeric variable used in
#' # plot axes.
#' d |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue,"Energian tuonti","Milj. €","Tilastokeskus",
#'            plot_axes = set_axes(yticktype = "log")
#'   )
#'
#' # roboplotr::set_axes also gives the user fine-grained control for
#' # plots where there might not be data in a format that is directly transferable
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
#' set_roboplot_options(
#'   caption = list(prefix = "Source: ", lineend = ".", updated = FALSE)
#'  )
#'
#' d2 |>
#'   roboplotr::roboplot(title = "Growth Rate of Investment",
#'                       caption = "National bureau of statistics, China",
#'                       plot_axes = set_axes(xticktype = "character"))
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
#'                       plot_axes = set_axes(
#'                         x = "obs1",
#'                         xticktype = "numeric"))
#'
#' set_roboplot_options(
#' caption = list(prefix = "Lähde: ", lineend = ".", updated = FALSE)
#' )
#'
#' # You might just want to switch the axes for time and value
#' d |> dplyr::filter(Suunta == "Tuonti") |>
#'   roboplotr::roboplot(Alue, "Energian tuonti","Milj. €", "Tilastokeskus",
#'                       plot_axes = set_axes(
#'                         y = "time",
#'                         yticktype = "date",
#'                         x = "value",
#'                         xticktype = "numeric"
#'                       ))
#'
#' # Or you might want to draw character strings on some axis.
#' d |> dplyr::filter(Suunta == "Tuonti") |>
#'   roboplotr::roboplot(Alue, "Energian tuonti","Milj. €", "Tilastokeskus",
#'                       plot_axes = set_axes(
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
#'                       plot_axes = set_axes(
#'                         y = "Alue",
#'                         yticktype = "character",
#'                         x = "value",
#'                         xticktype = "numeric"
#'                       ))
#' # Revert to defaults:
#' set_roboplot_options(reset = TRUE)
#'
#' @export
#' @importFrom dplyr case_when
set_axes <-
  function(y = NULL,
           x = NULL,
           yticktype = NULL,
           xticktype = NULL,
           ytitle = "",
           xtitle = "",
           yformat = NULL,
           xformat = NULL,
           ylim = c(NA, NA),
           xlim = c(NA, NA)) {
    if (is.null(y)) {
      y <- "value"
    } else if (y != "value" & is.null(x)) {
      x <- "value"
    }

    if (is.null(x)) {
      x <- "time"
    }

    if (is.null(yticktype)) {
      if (y == "value") {
        yticktype <-
          "numeric"
      } else if (x == "time") {
        yticktype <- "date"
      } else {
        yticktype <- "character"
      }
    }

    if (is.null(xticktype)) {
      if (x == "time") {
        xticktype <-
          "date"
      } else if (x == "value") {
        xticktype <- "numeric"
      }
    }

    if (is.null(yticktype)) {
      if (y == "value") {
        yticktype <-
          "numeric"
      } else if (x == "time") {
        yticktype <- "date"
      }
    }

    roboplotr_check_param(x, "character", allow_null = F, allow_na = F)
    roboplotr_check_param(y, "character", allow_null = F, allow_na = F)
    roboplotr_check_param(xticktype,
                          "character",
                          allow_null = F,
                          allow_na = F)
    roboplotr_check_param(yticktype,
                          "character",
                          allow_null = F,
                          allow_na = F)
    axis_types <- c("character", "date", "numeric", "log")
    roboplotr_valid_strings(xticktype, axis_types, any)
    roboplotr_valid_strings(yticktype, axis_types, any)
    roboplotr_check_param(xformat, "character")
    roboplotr_check_param(yformat, "character")
    roboplotr_check_param(
      xlim,
      "any type",
      size = 2,
      allow_null = F,
      allow_na = F
    )
    roboplotr_check_param(
      ylim,
      "any type",
      size = 2,
      allow_null = F,
      allow_na = F
    )


    setclass <- function(type) {
      case_when(type == "date" ~ list(c("POSIXct", "POSIXt", "Date")),
                type == "character" ~ list(c("factor", "character")),
                TRUE ~ list(type)) |> unlist()
    }

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
roboplotr_set_axis_ranges <- function(p, range, rangeslider) {
  fixed_range <-
    F#if (any(c("zoom","zoomin2d","pan") %in% getOption("roboplot.modebar.buttons"))) { F } else { T }
  if (!"xlim" %in% names(range)) {
    range$xlim <- c(NA, NA)
  }
  if (!"ylim" %in% names(range)) {
    range$ylim <- c(NA, NA)
  }
  if (!all(is.na(range$ylim)) &
      any(is.na(range$ylim)) ||
      !all(is.na(range$xlim)) & any(is.na(range$xlim))) {
    roboplotr_alert("Provide both ends for any axis limits!")
  }
  p <- config(p, locale = getOption("roboplot.locale")$locale) |>
    layout(separators = getOption("roboplot.locale")$separators) |>
    layout(hovermode = "compare")

  if (rangeslider == F) {
    if (!all(is.na(range$xlim))) {
      p <- p |> layout(xaxis = list(range = range$xlim))
    }
    if (!all(is.na(range$ylim))) {
      p <- p |> layout(yaxis = list(range = range$ylim))
    }
  } else {
    p <- p |> layout(yaxis = list(range = range$ylim))
  }

  p |> layout(xaxis = list(fixedrange = fixed_range),
              yaxis = list(fixedrange = fixed_range))

}

roboplotr_get_tick_layout <- function(ticktype,
                                      axis,
                                      tickformat,
                                      dateformat,
                                      dtick,
                                      reverse,
                                      title,
                                      background_color = getOption("roboplot.colors.background"),
                                      tick_color = getOption("roboplot.colors.ticks")) {
  font <- getOption("roboplot.font.main")[c("color", "family", "size")]

  if (ticktype == "date") {
    dateformats <- c(
      "Annual" = "%Y",
      "Quarterly" = "%YQ%q",
      "Monthly" = "%m/%Y",
      "Weekly" = "%YW%V",
      # "Weekly" = "%m/%Y",
      "Daily" = getOption("roboplot.locale")$date
    )

    tickformatstops <- list(
      Annual = list(
        list(dtickrange = list(NULL, "M1"),
             value = "%m/%Y"),
        list(dtickrange = list("M1", "M6"),
             value = "%YQ%q"),
        list(dtickrange = list("M6", NULL),
             value = "%Y")
      ),
      Quarterly = list(
        list(dtickrange = list(NULL, "M3"),
             value = "%m/%Y"),
        list(dtickrange = list("M3", "M12"),
             value = "%YQ%q"),
        list(dtickrange = list("M12", NULL),
             value = "%Y")
      ),
      Monthly = list(
        list(dtickrange = list(NULL, "M3"),
             value = "%m/%Y"),
        list(dtickrange = list("M3", "M6"),
             value = "%YQ%q"),
        list(dtickrange = list("M6", NULL),
             value = "%Y")
      ),
      Weekly = list(
        list(dtickrange = list(NULL, "M1"),
             value = "%YW%V"),
        list(dtickrange = list("M1", "M3"),
             value = "%m/%Y"),
        list(dtickrange = list("M3", "M12"),
             value = "%YQ%q"),
        list(dtickrange = list("M12", NULL),
             value = "%Y")
      ),
      Daily = list(
        list(dtickrange = list(NULL, "M1"),
             value = getOption("roboplot.locale")$date),
        list(dtickrange = list("M1", "M3"),
             value = "%m/%Y"),
        list(dtickrange = list("M3", "M12"),
             value = "%YQ%q"),
        list(dtickrange = list("M12", NULL),
             value = "%Y")
      )
    )

    tfopt <- names(which(dateformat == dateformats))

    if(length(tfopt) == 0) {
      tfstops <- tickformatstops[["Annual"]]
    } else if (!tfopt %in% names(tickformatstops)) {
      roboplotr_messages(str_glue("The given ticktype \"{tfopt}\" couldn't be used by roboplotr xaxis handler."), "alert")
      tfstops <- tickformatstops[["Annual"]]
    } else {
      tfstops <- tickformatstops[[tfopt]]
    }
    dlist <- list(
      tickfont = font,
      ticks = 'outside',
      type = 'date',
      title = list(text = title, font = font),
      tickformatstops = if (is.null(tickformat)) {
        tfstops
      } else {
        NULL
      },
      tickformat = tickformat,
      tickcolor = tick_color[[axis]]
    )
    if (!is.null(dtick)) {
      append(dlist, list(dtick = dtick))
    } else {
      dlist
    }
  } else if (ticktype %in% c("numeric","log")) {
    ticklayout <- list(
      tickfont = font,
      tickformat = if (is.null(tickformat)) {
        ",.3~f"
      } else {
        tickformat
      },
      ticksuffix = " ",
      ticks = 'outside',
      title = list(text = title, font = font),
      tickcolor = tick_color[[axis]],
      type = ifelse(ticktype == "log","log","-")
    )
    if(ticktype == "log") {
      append(ticklayout, list(dtick = NULL))
    } else {
      ticklayout
    }
  } else if (ticktype == "character") {
    list(
      tickfont = font,
      ticksuffix = " ",
      autorange = ifelse(reverse, "reversed", TRUE),
      categoryorder = "trace",
      #ifelse(reverse, "trace", "trace"),
      tickmode = ifelse(axis == "y", "linear", "auto"),
      tickangle = "auto",
      type = "category",
      ticks = 'outside',
      title = list(text = title, font = font),
      tickcolor = tick_color[[axis]]
    )
  }

}
#' @importFrom dplyr case_when
#' @importFrom plotly layout
#' @importFrom rlang %||%
roboplotr_set_ticks <- function(p, ticktypes) {
  dtick <- if (!"time" %in% names(p$data)) {
    NULL
  } else if (length(unique(p$data$time)) < 6) {
    tdf <- ticktypes$dateformat %||% "%Y"
    case_when(
      tdf == "%Y" ~ list("M12"),
      tdf == "%YQ%q" ~ list("M3"),
      tdf == "%m/%Y" ~ list("M1"),
      tdf == getOption("roboplot.locale")$date ~ list(86400000),
      TRUE ~ list("M12")
    )[[1]]
    # switch(ticktypes$dateformat %||% "%Y","%Y" = "M12","%YQ%q" = "M3","%m/%Y" = "M1",#"%YW%V" = 604800000,
    #        "%d.%m.%Y" = 86400000, "M12")
  } else {
    NULL
  }
  p <- p |>
    layout(
      xaxis = roboplotr_get_tick_layout(
        ticktypes$xticktype,
        "x",
        ticktypes$xformat,
        ticktypes$dateformat,
        dtick,
        ticktypes$reverse,
        ticktypes$xtitle
      ),
      yaxis = roboplotr_get_tick_layout(
        ticktypes$yticktype,
        "y",
        ticktypes$yformat,
        ticktypes$dateformat,
        dtick,
        ticktypes$reverse,
        ticktypes$ytitle
      )
    )

  #placeholder, relayout.js pitää kirjoittaa tätä varten vähän uusiksi jotta legend, lähde ja logo asettuvat oikein jos tickit oikeasti poistaa
  # if(length(unique(p$data[[ticktypes$x]]))==1 & ticktypes$x == "date") {
  #   p <- p |> layout(xaxis = list(tickfont = list(color = getOption("roboplot.colors.background"))))
  # }
  p
}


#' @importFrom dplyr case_when
#' @importFrom lubridate month quarter wday week yday
roboplotr_guess_xaxis_ceiling <-
  function(d, hovertext, what = "xaxis_ceiling") {
    attr_freq <- roboplotr_get_dateformat(d, msg = F)
    hovertext_freq <- hovertext$dateformat

    freq <- if (any(!is.null(attr_freq),!is.null(hovertext_freq))) {
      c(attr_freq, hovertext_freq) |> first()
    } else {
      NULL
    }

    if (!is.null(freq)) {
      freq <- switch(
        freq,
        "Annual" = "years",
        "Quarterly" = "quarters",
        "Monthly" = "months",
        "Weekly" = "weeks",
        "Daily" = "days",
        NULL
      )
      maxd <- max(d$time)
      yd <- yday(maxd)
      yq <- quarter(maxd)
      ym <- month(maxd)
      yw <- week(maxd)
      wd <- wday(maxd, week_start = 1)
      freq <- case_when(
        freq == "years" ~ "quarters",
        freq == "quarters" & yq == 4 ~ "years",
        freq == "months" & ym >= 10 ~ "years",
        freq == "weeks" & yw >= 40 ~ "quarters",
        freq == "weeks" & yw >= 26 ~ "months",
        freq == "days" & yd >= 240 ~ "years",
        freq == "days" & yd >= 120 ~ "months",
        freq == "days" ~ "weeks",
        TRUE ~ freq
      )
    }

    if (is.null(freq)) {
      roboplotr_alert("Failed to guess the '",
                      what,
                      "', ne frequency information available.")
    } else {
      roboplotr_message("Roboplotr guesses the '",
                        what,
                        "' is rounded to \"",
                        freq,
                        "\".")
    }
    freq
  }
