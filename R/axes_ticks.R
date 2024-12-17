#' Axis configuration
#'
#' Parameters to configure axis settings in [roboplots][roboplot()]. Includes options for specifying
#' axis columns, formatting, titles, limits, and font settings, as well as handling secondary y-axes.
#'
#' @param x,y Character. Names of columns from `d` in [roboplot][roboplot()] for
#' the axes. Defaults to "time" and "value".
#' @param xticktype,yticktype Characters. Axis formatting: "character", "date",
#' or "numeric". Defaults to "date" and "numeric".
#' @param xtitle,ytitle,y2title Character. Titles for the axes.
#' @param xformat,yformat Character. Formatting for axis tick text. Use
#' [d3 time format](https://d3js.org/api#d3-time) for dates and
#' [d3 number format](https://d3js.org/api#d3-format) for numbers.
#' @param xlim,ylim Vector of length 2. Axis limits. Should match the axis values
#' in the plot by type.
#' @param xfont,yfont,y2font Functions. Use [set_font()]. Secondary y-axis uses
#' the main y-axis font size but allows separate family and color.
#' @param y2 Character vector. Observations from `color` in plots using a secondary y-axis.
#' @param ylegend,y2legend Characters. Labels for legend title when `y2` is given.
#' @param xangle,yangle Numeric. Angle for axis tick text.
#' @param xanchor Date. Only usable for date x-axes. Sets the point at which tick marks are drawn from. Must provide xstep.
#' @param xstep Numeric. The interval of tick marks for date x-axes in months. Must provide xstart.
#' @returns List of class roboplotr.set_axes
#' @examples
#' # The primary usage is for creating horizontal bar plots when combining the
#' # roboplotr::roboplot plot_axes control with plot_mode "horizontal".
#'
#' set_roboplot_options(caption_template = "Lähde: {text}.")
#'
#' d <- energiantuonti |>
#'   dplyr::filter(Alue %in% c("USA","Norja","Iso-Britannia"))
#' attr(d, "frequency") <- "Quarterly"
#'
#' # Control the plot axes with roboplotr::set_axes, setting the columns
#' # from roboplotr::roboplot variable 'd' as the axis data sources with 'x' and
#' # 'y', and possible define ticktypes with 'xticktype' and 'yticktype'. The latter
#' # is not mandatory, though.
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
#' # You can use `xtitle` and `ytitle` to provide labeling for axis titles,
#' # and control the axis format with d3 time (https://d3js.org/api#d3-time)
#' # and number (https://d3js.org/api#d3-format) -formatted vectors for `xlim`
#' # and 'ylim'. These are not typechecked by `roboplot()`, but must match the
#' # corresponding axis values. Alter axis tick labels' angle with `xangle` and `yangle`.
#'
#' d |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue,"Energian tuonti","Milj. €","Tilastokeskus",
#'            plot_axes = set_axes(
#'              ytitle = "Arvo",
#'              xformat = "Y%y",
#'              yformat = "+$05,",
#'              ylim = c(-200,1500),
#'              xlim = c("2015-01-01","2023-01-01"),
#'              yangle = 45,
#'              xangle = 66
#'              )
#'   )
#'
#' # You can use `set_axes(xanchor, xstep)` to control xaxis ticks for date axes. Use
#' # `xanchor` to set the point at which tick marks are drawn from, and `xstep` to set the
#' # interval in months. Use NA for numeric axis limits to derive one end from data.
#'
#' d |> dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue,"Energian tuonti","Milj. €","Tilastokeskus",
#'            plot_axes = set_axes(
#'              xanchor = as.Date("2020-01-01"),
#'              xstep = 12,
#'              ylim = c(-120, NA)
#'            )
#'   )
#'
#' # Additionally, you may use logartihmic axis for any numeric variable used in
#' # `plot_axes```.
#' d |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue,"Energian tuonti","Milj. €","Tilastokeskus",
#'            plot_axes = set_axes(yticktype = "log")
#'   )
#'
#' # Providing a vector of strings matching observations from `roboplot()` param
#' # `color` as `y2` will add a secondary y-axis. Any provided `zeroline`does not
#' # work for both axes at the time.
#' d |>
#'   dplyr::filter(Suunta == "Tuonti", Alue %in% c("Iso-Britannia","Norja")) |>
#'   roboplot(Alue,
#'            "Energian tuonti",
#'            "Milj. \u20AC",
#'            "Tilastokeskus",
#'            plot_axes = set_axes(y2 = "Iso-Britannia"))
#'
#' # Giving no observations that match param `color` in `roboplot()` fails.
#' d2 <- d |>
#'   dplyr::filter(Suunta == "Tuonti",
#'                 Alue %in% c("Iso-Britannia", "Norja"))
#' \dontrun{
#'   d2 |>
#'     roboplot(Alue,
#'              "Energian tuonti",
#'              "Milj. \u20AC",
#'              "Tilastokeskus",
#'              plot_axes = set_axes(y2 = "USA"))
#' }
#'
#' # You might want to override the default axis reference labels that are added
#' # to legend. Using them without giving any items in `y2` does nothing. You probably
#' # want to explicitly reorder the observations.
#' d2 |>
#'   dplyr::mutate(Alue = forcats::fct_relevel(Alue, "Iso-Britannia","Norja")) |>
#'   roboplot(
#'     Alue,
#'     "Energian tuonti",
#'     "Milj. \u20AC",
#'     "Tilastokeskus",
#'     plot_axes = set_axes(
#'       y2 = "Iso-Britannia",
#'       ylegend = "1.",
#'       y2legend = "2."
#'     )
#'   )
#'
#' # Or maybe you only have two observations and you want to match them by color
#' # instead of labeling.
#' d2 |>
#'   dplyr::mutate(Alue = forcats::fct_relevel(Alue, "Iso-Britannia","Norja")) |>
#'   roboplot(
#'     Alue,
#'     "Energian tuonti",
#'     "Milj. \u20AC",
#'     "Tilastokeskus",
#'     plot_axes = set_axes(
#'       y2 = "Iso-Britannia",
#'       yfont = set_font(color = getOption("roboplot.colors.traces")[2]),
#'       y2font = set_font(color = getOption("roboplot.colors.traces")[1]),
#'       ylegend = "",
#'       y2legend = ""
#'     )
#'   )
#'
#'
#' # Or you could use the axis titles to differentiate between the observations.
#' # This might be especially appropriate if the values are on a completely different
#' # scale.
#' d2 |>
#'   dplyr::mutate(Alue = forcats::fct_relevel(Alue, "Iso-Britannia","Norja")) |>
#'   dplyr::mutate(value = ifelse(Alue == "Norja", value / 1000, value)) |>
#'   roboplot(
#'     Alue,
#'     "Energian tuonti",
#'     caption = "Tilastokeskus",
#'     plot_axes = set_axes(
#'       y2 = "Iso-Britannia",
#'       ytitle = "Norja, Mrd. €",
#'       y2title = "Iso-Britannia, Milj €",
#'       ylegend = "",
#'       y2legend = ""
#'     )
#'   )
#'
#'
#' # `set_axes()` also gives the user fine-grained control for plots where there
#' # might not be data in a format that is directly transferable to date or numeric
#' # format.
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
#' # Or you might have numeric data on both axes. `roboplot()` will draw any
#' # traces in the order they appear in the data, so it is up to the user to
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
           xangle = NULL,
           yangle = NULL,
           y2 = NULL,
           y2font = NULL,
           ylegend = NULL,
           y2title = "",
           y2legend = NULL,
           xanchor = NULL,
           xstep = NULL
           ) {

    if (is.null(y)) {
      y <- "value"
    } else if (y != "value" & is.null(x)) {
      x <- "value"
    }

    if (is.null(x)) {
      x <- "time"
    }

    # if (is.null(yticktype)) {
    #   if (y == "value") {
    #     yticktype <-
    #       "numeric"
    #   } else if (x == "time") {
    #     yticktype <- "date"
    #   } else {
    #     yticktype <- "character"
    #   }
    # }
    #
    # if (is.null(xticktype)) {
    #   if (x == "time") {
    #     xticktype <-
    #       "date"
    #   } else if (x == "value") {
    #     xticktype <- "numeric"
    #   }
    # }
    #
    # if (is.null(yticktype)) {
    #   if (y == "value") {
    #     yticktype <-
    #       "numeric"
    #   } else if (x == "time") {
    #     yticktype <- "date"
    #   }
    # }

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
    arggs <- as.list(environment())


    for (.name in names(arggs[c("xticktype","yticktype")])) {
      Parameter <- arggs[[.name]]
      roboplotr_typecheck(Parameter, "character", allow_null = T, extra = str_glue("`{.name}` in set_axes()"))
      if(length(arggs[[.name]]) > 0) {
        assign(.name, as(arggs[[.name]], "character"))
      }
    }

    for (.name in names(arggs[c("x","y")])) {
      Parameter <- arggs[[.name]]
      roboplotr_typecheck(Parameter, "character", allow_null = F, extra = str_glue("`{.name}` in set_axes()"))
      if(length(arggs[[.name]]) > 0) {
        assign(.name, as(arggs[[.name]], "character"))
      }
    }

    axis_types <- c("character", "date", "numeric", "log")
    purrr::walk(c(xticktype,yticktype)[!is.null(c(xticktype,yticktype))],
         ~    roboplotr_valid_strings(.x, axis_types, any, "Tick types in set_axes()")
         )

    for (.name in names(arggs[c("xformat","yformat","ylegend","y2legend")])) {
      Parameter <- arggs[[.name]]
      roboplotr_typecheck(Parameter, "character", extra = str_glue("`{.name}` in set_axes()"))
      if(length(arggs[[.name]]) > 0) {
        assign(.name, as(arggs[[.name]], "character"))
      }
    }

    roboplotr_typecheck(xlim, NULL, size = 2, allow_null = F, allow_na = T)
    roboplotr_typecheck(ylim, NULL, size = 2, allow_null = F, allow_na = T)
    if(!is.null(y2)) {
      if(is.null(ylegend)) {
        ylegend <- getOption("roboplot.locale")$ylegendlabs$left
      }
      if(is.null(y2legend)) {
        y2legend <- getOption("roboplot.locale")$ylegendlabs$right
      }
    }

    roboplotr_typecheck(xangle, "numeric")
    roboplotr_typecheck(yangle, "numeric")
    if(!is.null(xangle)) {
      roboplotr_is_between(xangle, "set_axes", c(-360, 360))
    }
    if(!is.null(yangle)) {
      roboplotr_is_between(yangle, "set_axes", c(-360, 360))
    }

    setclass <- function(type) {
      case_when(type == "date" ~ list(c("POSIXct", "POSIXt", "Date")),
                type == "character" ~ list(c("factor", "character")),
                TRUE ~ list(type)) |> unlist()
    }

    roboplotr_typecheck(xstep, "numeric",allow_null = T)

    .res <- list(
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
      xangle = xangle,
      yangle = yangle,
      y2 = y2,
      y2font = y2font,
      ylegend = ylegend,
      y2legend = y2legend,
      y2title = y2title,
      xstart = xanchor,
      xstep = xstep
    )

    .res <- structure(.res, class = c("roboplotr", "roboplotr.set_axes", class(.res)))

    .res
  }

roboplotr_set_ticktypes <- function(d, axis_specs) {

  setclass <- function(type) {
    case_when(type == "date" ~ list(c("POSIXct", "POSIXt", "Date")),
              type == "character" ~ list(c("factor", "character")),
              TRUE ~ list(type)) |> unlist()
  }

    for(.xis in c("x","y")) {
      if(is.null(axis_specs[[str_c(.xis,"ticktype")]])) {
        col <- d[[axis_specs[[.xis]]]]
        roboplot_class <- case_when(inherits(col, c("Date","POSIXt")) ~ "date",
                                    inherits(col, c("numeric","integer")) ~ "numeric",
                                    inherits(col, c("factor","character")) ~ "character",
                                    TRUE ~ NA
        )
        if(is.na(roboplot_class)) {
          stop("Couldn't determine the ticktype for axis ",.xis,". Please provide it with set_axes()!")
        }
        axis_specs[str_c(.xis,"ticktype")] <- roboplot_class
        axis_specs[[str_c(.xis,"class")]] <- setclass(roboplot_class)
      }
    }

    axis_specs

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
                                      tick_color = setNames(getOption("roboplot.grid")[c("xtick","ytick")], c("x","y")),
                                      font = getOption("roboplot.font.main"),
                                      angle = NULL,
                                      start = NULL
) {

  font <- font[c("color", "family", "size")]

  .title = if(str_length(title) > 0) {  list(text = title, font = font) } else { list(text = NULL, font = font) }

  font_list <- list(
    tickfont = font,
    ticks = 'outside',
    title = .title,
    tickcolor = tick_color[[axis]],
    tickangle = angle %||% "auto"
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

    if(!is.null(tickformat)) {
      tfstops <- NULL
    } else if (length(tfopt) == 0) {
      tfstops <- tickformatstops[["Annual"]]
    } else if (!tfopt %in% names(tickformatstops)) {
      roboplotr_alert(str_glue("The given ticktype \"{tfopt}\" couldn't be used by roboplotr xaxis handler."))
      tfstops <- tickformatstops[["Annual"]]
    } else {
      tfstops <- tickformatstops[[tfopt]]
    }
    dlist <- list(
      type = 'date',
      tickformatstops = tfstops,
      tickformat = tickformat
    )
    dlist <- append(font_list, dlist)
    if (!is.null(dtick)) {
      if(!"roboplotr.dtick" %in% class(dtick)) {
        dtick <- str_glue("M{dtick}")
      }
      dlist <- append(dlist, list(dtick = dtick))
    }
    if (!is.null(start)) {
      dlist <- append(dlist, list(tick0 = start))
    }
    dlist
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
             categoryorder = ifelse(axis == "y", "array", "trace"),
             tickmode = ifelse(axis == "y", "linear", "auto"),
             type = "category"
           ))

  }

}
#' @importFrom dplyr case_when
#' @importFrom plotly layout
#' @importFrom rlang %||%
roboplotr_set_ticks <- function(p, ticktypes) {

  if (!"time" %in% names(p$data)) {
    dtick <- NULL
  } else if (length(unique(p$data$time)) < 6) {

    tdf <- ticktypes$dateformat %||% "%Y"

    dtick <- case_when(
      tdf == "%Y" ~ list("M12"),
      tdf == "%YQ%q" ~ list("M3"),
      tdf == "%m/%Y" ~ list("M1"),
      tdf == getOption("roboplot.locale")$date ~ list(86400000),
      TRUE ~ list("M12")
    )[[1]]
    dtick <- structure(dtick, class = c("roboplotr.dtick", class(dtick)))

  } else {
    dtick <- NULL
  }

  p <- p |>
    layout(
      xaxis = roboplotr_get_tick_layout(
        ticktypes$xticktype,
        "x",
        ticktypes$xformat,
        ticktypes$dateformat,
        ticktypes$xstep %||% dtick,
        ticktypes$reverse,
        ticktypes$xtitle,
        font = ticktypes$xfont,
        angle = ticktypes$xangle,
        start = ticktypes$xstart
      ),
      yaxis = roboplotr_get_tick_layout(
        ticktypes$yticktype,
        "y",
        ticktypes$yformat,
        ticktypes$dateformat,
        dtick,
        ticktypes$reverse,
        ticktypes$ytitle,
        font = ticktypes$yfont,
        angle = ticktypes$yangle,
        start = NULL
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

#' @importFrom tidyr replace_na
roboplotr_expand_axis_limits <- function(plot_axes, d, zeroline) {

  if(any(!is.na(plot_axes$ylim)) & is.numeric(d[[plot_axes$y]])) {
    yMax <- max(d[[plot_axes$y]], na.rm = T)
    yMin <- min(d[[plot_axes$y]], na.rm = T)
    yMaxMod <- abs(yMax)*0.05
    yMinMod <- abs(yMin)*0.05
    plot_axes$ylim[1] <- replace_na(plot_axes$ylim[1], yMin) - yMinMod
    plot_axes$ylim[2] <- replace_na(plot_axes$ylim[2], yMax) + yMaxMod
  }

  if(any(!is.na(plot_axes$xlim)) & is.numeric(d[[plot_axes$x]])) {
    xMax <- max(d[[plot_axes$x]], na.rm = T)
    xMin <- min(d[[plot_axes$x]], na.rm = T)
    xMaxMod <- abs(xMax)*0.05
    xMinMod <- abs(xMin)*0.05
    plot_axes$xlim[1] <- replace_na(plot_axes$xlim[1], xMin) - xMinMod
    plot_axes$xlim[2] <- replace_na(plot_axes$xlim[2], xMax) + xMaxMod
  }

  alter_x <- F
  if (is.logical(zeroline$position)) {
    if (zeroline$position) {
      alter_x <- T
    }
  } else {
    alter_x <- T
  }

  if(alter_x & plot_axes$xticktype == "date") {
    xMax <- max(d[[plot_axes$x]], na.rm = T)
    xMin <- min(d[[plot_axes$x]], na.rm = T)
    plot_axes$xlim <- as_date(plot_axes$xlim)
    plot_axes$xlim[1] <- replace_na(as.Date(plot_axes$xlim[1]), xMin)
    plot_axes$xlim[2] <- replace_na(as.Date(plot_axes$xlim[2]), xMax)
  }

  plot_axes
}
