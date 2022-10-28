#' @importFrom plotly layout config
roboplotr_set_axes <- function(p, range = list(x = c(NA,NA), y = c(NA,NA))) {
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
                                     background_color = getOption("roboplot.colors.background"),
                                     border_color = getOption("roboplot.colors.border"),
                                     tick_color = getOption("roboplot.colors.ticks")) {
  if (ticktype == "date") {
    dlist <- list(tickfont = getOption("roboplot.font.main"),
                  mirror = TRUE,
                  ticks = 'outside',
                  type = "date",
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
roboplotr_set_ticks <- function(p, ticktypes) {
  dateformats <- c("Annual" = "%Y",
                   "Quarterly" = "%YQ%q",
                   "Monthly" = "%m/%Y",
                   "Weekly" = "%YW%V",
                   "Daily" = "%d.%m.%Y")
  tickformat <- ticktypes$dateformat %||%  "%Y"
  tickformat <- which(dateformats == tickformat) %||% 1
  tickformat <- dateformats[c(max(tickformat-2,1),max(tickformat-1,1), tickformat, min(tickformat+1,5))]
  dtick <- if (length(unique(p$data$time)) < 6) {
    switch(ticktypes$dateformat %||%  "%Y","%Y" = "M12","%YQ%q" = "M3","%m/%Y" = "M1", "%YW%V" = 604800000, "%d.%m.%Y" = 86400000, "M12")
  } else { NULL }

  p <- p |>
    layout(xaxis= roboplotr_get_tick_layout(ticktypes$x, "x", tickformat, dtick),
           yaxis= roboplotr_get_tick_layout(ticktypes$y, "y", tickformat, dtick))

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
