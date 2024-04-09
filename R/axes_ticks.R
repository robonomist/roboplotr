#' Axis and tick control for [roboplot()]
#'
#' Define axis and axis tick formats for [roboplot()] plots.
#'
#' @param x,y Character. The name of the column from parameter 'd' of
#' [roboplot()] used for the axis. Defaults to "time" and "value".
#' @param xticktype,yticktype Characters. Determines axis formatting within
#' [roboplot()]. One of "character", "date" or "numeric". Defaults to "date" and
#' "numeric".
#' @param xtitle,ytitle,y2title Character. Used as the axis title.
#' @param xformat,yformat Character. Formatting for axis tick text. Use
#' \href{https://github.com/d3/d3-3.x-api-reference/blob/master/Time-Formatting.md}{d3 time format}
#' for dates and \href{https://github.com/d3/d3-3.x-api-reference/blob/master/Formatting.md#d3_forma}{d3 number format}
#' for numbers. Ignored for characters.
#' @param xlim,ylim Vector of length 2. Used as the axis limits. These are not
#' type-checked by [roboplot()], but should match the values used in the
#' respecting plot axis.
#' @param xfont,yfont,y2font Functions. Use [set_font()]. Note that secondary
#' y-axis will always use the font size of the main y-axis, but you may set the
#' family and color separately.
#' @param y2 Character vector. Observations from param 'color' of [roboplot]
#' which will use a secondary y-axis. Parameter 'zeroline' will be ignored.
#' @param ylegend,y2legend Characters. Only used when y2 is given. Use to label
#' observations described by y2 separately in plot legend from other observations.
#' @return A list.
#' @examples
#' # The primary usage is for creating horizontal bar plots when combining the
#' # roboplotr::roboplot plot_axes control with plot_mode "horizontal".
#'
#' set_roboplot_options(caption_template = "Lähde: {text}.")
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
#'            stringr::str_glue("Energian tuonti {lubridate::year(max(d$time))}"),
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
#' # Providing a vector of strings matching observations from roboplot() param 'color'
#' # as 'y2' will add a secondary y-axis. Any provided zeroline is currently ignored
#' # when two y-axes are present.
#' d |>
#'   dplyr::filter(Suunta == "Tuonti", Alue %in% c("Yhdistynyt kuningaskunta","Norja")) |>
#'   roboplot(Alue,
#'            "Energian tuonti",
#'            "Milj. \u20AC",
#'            "Tilastokeskus",
#'            plot_axes = set_axes(y2 = "Yhdistynyt kuningaskunta"))
#'
#' # Giving no observations that match the param 'color' in roboplot() causes roboplot()
#' # to stop.
#' d2 <- d |>
#'   dplyr::filter(Suunta == "Tuonti",
#'                 Alue %in% c("Yhdistynyt kuningaskunta", "Norja"))
#' if(interactive()) {
#'   d2 |>
#'     roboplot(Alue,
#'              "Energian tuonti",
#'              "Milj. \u20AC",
#'              "Tilastokeskus",
#'              plot_axes = set_axes(y2 = "Kanada"))
#' }
#'
#' # You might want to override the default labels that are added to legend. Using
#' # them without giving any items in y2 does nothing. You probably want to explicitly
#' # reorder the observations.
#' d2 |>
#'   dplyr::mutate(Alue = forcats::fct_relevel(Alue, "Yhdistynyt kuningaskunta","Norja")) |>
#'   roboplot(
#'     Alue,
#'     "Energian tuonti",
#'     "Milj. \u20AC",
#'     "Tilastokeskus",
#'     plot_axes = set_axes(
#'       y2 = "Yhdistynyt kuningaskunta",
#'       ylegend = "1.",
#'       y2legend = "2."
#'     )
#'   )
#'
#' # Or maybe you only have two observations and you want to match them by color
#' # instead of labeling.
#' d2 |>
#'   dplyr::mutate(Alue = forcats::fct_relevel(Alue, "Yhdistynyt kuningaskunta","Norja")) |>
#'   roboplot(
#'     Alue,
#'     "Energian tuonti",
#'     "Milj. \u20AC",
#'     "Tilastokeskus",
#'     plot_axes = set_axes(
#'       y2 = "Yhdistynyt kuningaskunta",
#'       yfont = set_font(color = getOption("roboplot.colors.traces")[2]),
#'       y2font = set_font(color = getOption("roboplot.colors.traces")[1]),
#'       ylegend = "",
#'       y2legend = ""
#'     )
#'   )
#'
#'
#' # Or you could use the axis titles to differentiate between the observations. This
#' # might be especially appropriate if the values are on a completely different scale.
#' d2 |>
#'   dplyr::mutate(Alue = forcats::fct_relevel(Alue, "Yhdistynyt kuningaskunta","Norja")) |>
#'   dplyr::mutate(value = ifelse(Alue == "Norja", value / 1000, value)) |>
#'   roboplot(
#'     Alue,
#'     "Energian tuonti",
#'     caption = "Tilastokeskus",
#'     plot_axes = set_axes(
#'       y2 = "Yhdistynyt kuningaskunta",
#'       ytitle = "Norja, Mrd. €",
#'       y2title = "Yhdistynyt kuningaskunta, Milj €",
#'       ylegend = "",
#'       y2legend = ""
#'     )
#'   )
#'
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
#'   caption_template = "Source: {text}."
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
#' caption_template = "Lähde {text}."
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
#' d |>
#' dplyr::filter(Suunta == "Tuonti", time == max(time)) |>
#'   roboplotr::roboplot(Alue, "Energian tuonti","Milj. €", "Tilastokeskus",
#'                       plot_type = "bar",
#'                       plot_mode = "horizontalfill",
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
           xlim = c(NA, NA),
           yfont = NULL,
           xfont = NULL,
           y2 = NULL,
           y2font = NULL,
           ylegend = NULL,
           y2title = "",
           y2legend = NULL
           ) {

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

    yfont <- substitute(yfont)
    xfont <- substitute(xfont)
    y2font <- substitute(y2font)
    axis_fonts <- list("yfont" = yfont, "xfont" = xfont, "y2font" = y2font)
    for(i in seq_along(axis_fonts)) {
      .fontname <- names(axis_fonts[i])
      .font <- unname(axis_fonts[i])[[1]]
      if(!is.null(.font)) {
        if(.font[1] != "set_font()" & .font[1] != "roboplotr::set_font()") { stop("Use 'roboplotr::set_font()' for any plot_axes fonts!", call. = F)}
        .font$type <- "main"
        assign(.fontname, eval(.font))
      } else {
        assign(.fontname, getOption("roboplot.font.main"))
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
    roboplotr_check_param(ylegend, "character")
    roboplotr_check_param(y2legend, "character")
    if(!is.null(y2)) {
      if(is.null(ylegend)) {
        ylegend <- getOption("roboplot.locale")$ylegendlabs$left
      }
      if(is.null(y2legend)) {
        y2legend <- getOption("roboplot.locale")$ylegendlabs$right
      }
    }


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
      ylim = ylim,
      xfont = xfont,
      yfont = yfont,
      y2 = y2,
      y2font = y2font,
      ylegend = ylegend,
      y2legend = y2legend,
      y2title = y2title
    )
  }

#' @importFrom plotly layout config
roboplotr_set_axis_ranges <- function(p, range, rangeslider, hovermode) {
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
    layout(hovermode = hovermode)

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
                                      tick_color = getOption("roboplot.colors.ticks"),
                                      font = getOption("roboplot.font.main")
                                      ) {

  font <- font[c("color", "family", "size")]

  .title = if(str_length(title) > 0) {  list(text = title, font = font) } else { list(text = NULL, font = font) }

  font_list <- list(
    tickfont = font,
    ticks = 'outside',
    title = .title,
    tickcolor = tick_color[[axis]]
  )

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
      type = 'date',
      tickformatstops = if (is.null(tickformat)) {
        tfstops
      } else {
        NULL
      },
      tickformat = tickformat
    )
    dlist <- append(font_list, dlist)
    if (!is.null(dtick)) {
      append(dlist, list(dtick = dtick))
    } else {
      dlist
    }
  } else if (ticktype %in% c("numeric","log")) {
    ticklayout <- list(
      tickformat = if (is.null(tickformat)) {
        ",.3~f"
      } else {
        tickformat
      },
      ticksuffix = " ",
      type = ifelse(ticktype == "log","log","-")
    )
    if(ticktype == "log") {
      append(font_list,append(ticklayout, list(dtick = NULL)))
    } else {
      append(font_list,ticklayout)
    }
  } else if (ticktype == "character") {
    append(font_list,
           list(
             ticksuffix = " ",
             autorange = ifelse(reverse, "reversed", TRUE),
             categoryorder = "array",
             tickmode = ifelse(axis == "y", "linear", "auto"),
             tickangle = "auto",
             type = "category"
           ))

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
        ticktypes$xtitle,
        font = ticktypes$xfont
      ),
      yaxis = roboplotr_get_tick_layout(
        ticktypes$yticktype,
        "y",
        ticktypes$yformat,
        ticktypes$dateformat,
        dtick,
        ticktypes$reverse,
        ticktypes$ytitle,
        font = ticktypes$yfont
      )
    )

  #placeholder, relayout.js pitää kirjoittaa tätä varten vähän uusiksi jotta legend, lähde ja logo asettuvat oikein jos tickit oikeasti poistaa
  # if(length(unique(p$data[[ticktypes$x]]))==1 & ticktypes$x == "date") {
  #   p <- p |> layout(xaxis = list(tickfont = list(color = getOption("roboplot.colors.background"))))
  # }
  p
}


#' @importFrom dplyr case_when
#' @importFrom lubridate as_date month quarter wday week yday
roboplotr_guess_xaxis_ceiling <-
  function(d, hovertext, ceiling = "guess", what = "xaxis_ceiling") {

    this_time <- unique(d$time)
    this_time <- subset(this_time, !is.na(this_time))

    if(suppressWarnings(!is.na(as_date(ceiling)))) {
      if(ceiling < max(this_time)) {
        roboplotr_message(str_glue("Provided {what} is less than max time of parameter 'd' of roboplotr::roboplot()."))
      }
      this_ceiling <- ceiling
    } else if(ceiling != "guess") {
      this_ceiling <- ceiling_date(max(this_time), ceiling, week_start = 1)
    } else {
      attr_freq <- roboplotr_get_dateformat(d, msg = F)
      hovertext_freq <- hovertext$dateformat

      if (any(!is.null(attr_freq),!is.null(hovertext_freq))) {
        freq <- c(hovertext_freq, attr_freq) |> first()
      } else {
        freq <- NULL
      }

      if (!is.null(freq)) {
        freq <- case_when(
          freq %in% c("Annual", "%Y") ~ "years",
          freq %in% c("Quarterly", "%YQ%q") ~ "quarters",
          freq %in% c("Monthly", "%m/%Y") ~ "monthly",
          freq %in% c("Weekly", "%YW%V") ~ "days",
          freq %in% c("Daily", getOption("roboplot.locale")$date) ~ "days"
        )

        this_time <- unique(d$time)
        this_time <- subset(this_time, !is.na(this_time))
        this_ceiling <- ceiling(length(this_time) * 1.05) - length(this_time)
        freqs <- c("years","quarters","months","weeks","days")
        if (this_ceiling >= length(this_time) * 0.5) {
          freq <- freqs[min(which(freqs == freq)+2,length(freqs))]
        } else if (this_ceiling >= length(this_time) * 0.05) {
          freq <- freqs[min(which(freqs == freq)+1,length(freqs))]
        }
        this_ceiling <-
          seq.Date(as_date(max(this_time)), length.out = this_ceiling + 1, by = freq) |>
          max()
      }

      if(what != "rangeslider") {
        if (is.null(freq)) {
          roboplotr_alert(str_glue("Roboplotr failed to guess the {what}. Please provide frequency in parameter 'hovertext' with roboplotr::set_hovertext()."))
        } else {
          roboplotr_message(str_glue("Roboplotr guesses the {what} is \"{this_ceiling}\"."))
        }
      }
    }

    this_ceiling
  }
