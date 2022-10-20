#' @importFrom plotly layout config
roboplot_set_axes <- function(p, range = list(x = c(NA,NA), y = c(NA,NA))) {
  fixed_range <- if (any(c("zoom","zoomin2d","pan") %in% getOption("roboplot.modebar.buttons"))) { F } else { T }
  if(!"x" %in% names(range)) { range$x <- c(NA,NA) }
  if(!"y" %in% names(range)) { range$y <- c(NA,NA) }
  if(!all(is.na(range$y)) & any(is.na(range$y)) || !all(is.na(range$x)) & any(is.na(range$x))) {
    message("Provide both ends for any axis limits!")
  }
  config(p, locale = "fi") |>
    layout(separators = ", ") |>
    layout(xaxis = if(all(is.na(range$x))) { list(fixedrange = fixed_range) } else { list(fixedrange = fixed_range, range = range$x) },
           yaxis = list(fixedrange = fixed_range, range = range$y)) |>
    layout(hovermode = "compare") |>
    roboplot_set_axis_labels()

}

roboplot_get_tick_layout <- function(ticktype,
                                     axis,
                                     tickformat,
                                     dtick,
                                     background_color = getOption("roboplot.colors.background"),
                                     border_color = getOption("roboplot.colors.border"),
                                     tick_color = getOption("roboplot.colors.ticks")) {
  if (ticktype == "date") {
    dlist <- list(tickfont = getOption("roboplot.font.main"),
                  mirror = TRUE,
                  ticks = 'outside',
                  type = "date",
                  tickformatstops = list(
                    list(dtickrange = list(NULL, 604800000),value = "%Y"#tickformat[3]#"%d.%m.%Y"
                    ),
                    # end is year in milliseconds
                    list(dtickrange = list(604800000, 3.15e10),value = "%Y"#tickformat[2]#"%m/%Y"
                    ),
                    list(dtickrange = list(3.15e10, NULL),value = "%Y"#tickformat[1]#"%Y"
                    )
                  ),
                  # tickformat = roboplot_hovertemplate_freq(ticktypes$dateformat),
                  tickcolor = tick_color[[axis]],
                  showline = background_color != border_color[[axis]])
    if(!is.null(dtick)) { append(dlist, list(dtick = dtick)) } else { dlist }
  } else if (ticktype == "double") {
    list(tickfont = getOption("roboplot.font.main"),
         tickformat = ",.3~f",
         ticksuffix = " ",
         mirror = TRUE,
         ticks = 'outside',
         tickcolor = tick_color[[axis]],
         showline = background_color != border_color[[axis]])
  } else if (ticktype == "character") {
    font <- getOption("roboplot.font.main")
    list(tickfont = font,
         ticksuffix = " ",
         autorange = "reversed",
         tickmode = "linear",
         tickangle = "auto",
         mirror = TRUE,
         type = "category",
         ticks = 'outside',
         tickcolor = tick_color[[axis]],
         showline = background_color != border_color[[axis]])
  }

}
#' @importFrom plotly layout
#' @importFrom rlang %||%
roboplot_set_ticks <- function(p, ticktypes) {
  dateformats <- c("Annual" = "%Y",
                   "Quarterly" = "%YQ%q",
                   "Monthly" = "%m/%Y",
                   "Weekly" = "%YW%V",
                   "Daily" = "%d.%m.%Y")
  tickformat <- ticktypes$dateformat %||% "Monthly"
  tickformat <- which(names(dateformats) == tickformat) %||% "Monthly"
  tickformat <- dateformats[c(max(tickformat-1,1), tickformat, min(tickformat+1,5))]
  dtick <- if (length(unique(p$data$time)) < 6) {
    format <- ticktypes$dateformat %||% "None"
    switch(format,
           "Annual" = "M12","Quarterly" = "M3","Monthly" = "M1",NULL)
  } else { NULL }

  p <- p |>
    layout(xaxis= roboplot_get_tick_layout(ticktypes$x, "x", tickformat, dtick),
           yaxis= roboplot_get_tick_layout(ticktypes$y, "y", tickformat, dtick))

  #placeholder, relayout.js pitää kirjoittaa tätä varten vähän uusiksi jotta legend, lähde ja logo asettuvat oikein jos tickit oikeasti poistaa
  if(length(unique(p$data$time))==1 & ticktypes$x == "date") {
    p <- p |> layout(xaxis = list(tickfont = list(color = getOption("roboplot.colors.background"))))
  }
  p
}


#' @importFrom dplyr case_when
#' @importFrom lubridate month quarter wday week yday
roboplot_guess_xaxis_ceiling <- function(d, hovertext) {
  attr_freq <- roboplot_get_dateformat(d,msg = F)
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

  if(is.null(freq)) { message("Failed to guess the 'xaxis_ceiling', ne frequency information available.") } else {
    message("Roboplotr guesses the xaxis_ceiling is rounded to \"",freq,"\".")
  }
  freq
}
