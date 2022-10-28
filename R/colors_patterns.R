#' Gives a vector of colors of the desired length.
#'
#' Outputs a vector of colors.
#'
#' @param n_unique How many colors are needed.
#' @param robocolors Optional. A vector or colors.
#' @param accessibility_params Placeholder for accessibility features.
#' @return plotly object
#' @return Vector of colors.
#' @importFrom grDevices colorRampPalette
#' @importFrom plotly plot_ly

roboplotr_get_colors <- function(n_unique, robocolors = getOption("roboplot.colors.traces"), accessibility_params) {
  if(n_unique <= 4) {
    cols <- robocolors[1:n_unique]
  } else {
    cols <- colorRampPalette(robocolors, interpolate = "spline")(n_unique)
  }
  cols# |> alter_color_for_accessibility() ## t"\u00e4"h"\u00e4"n tulee saavutettavuuskoodi
}

#' @importFrom dplyr filter pull summarize
#' @importFrom forcats fct_relevel
#' @importFrom purrr map2
#' @importFrom rlang as_name
#' @importFrom stats setNames

roboplotr_set_colors <- function(trace_color, unique_groups, highlight, d, color) {
  if(!is.null(trace_color)) {

    if(!all(roboplotr_are_colors(trace_color))) {
      stop("Trace colors must be 6-character hexadecimal colors or among strings provided by grDevices::colors!", call. = F)
    } else if (length(trace_color) == 1 & is.null(names(trace_color))){
      color_vector <- rep(trace_color, length(unique_groups)) |> setNames(unique_groups)
    } else if (!all(unique_groups %in% names(trace_color)) & !(".other" %in% names(trace_color)) ) {
      stop(str_c("Either trace color must be a single color string, or all variables in column \"",as_name(color),"\" must have a corresponding trace color, or key \".other\" must be included, or trace_color must be NULL!"), call. = F)
    } else {
      ug <- as.character(unique_groups)
      missing_groups <- ug |> subset(!ug %in% names(trace_color))
      if(length(missing_groups) > 0) {
        detected_traces <- map2(unname(trace_color), names(trace_color), function(tc,nm) {
          miss <- missing_groups |> subset(str_detect(missing_groups, str_c(nm, collapse = "|")))
          rep(tc, length(miss)) |> setNames(miss)
        }) |> roboplotr_compact() |> unlist()
        trace_color <- c(trace_color, detected_traces)
      }
      color_vector <- c(trace_color[ug[ug %in% names(trace_color)]],
                        ug[!ug %in% names(trace_color)] |> length() |> rep(x = trace_color[".other"]) |>
                          setNames(ug[!ug %in% names(trace_color)]))
    }
    if(!is.null(highlight)) { roboplotr_alert("The argument 'highlight' is ignored when providing trace colors.")}

  } else if(!is.null(highlight)) {
    if (is.double(highlight)) {
      un_groups <- d |> group_by(!!color) |> summarize(value = max(.data$value, na.rm = T), .groups = "drop") |> filter(.data$value >= highlight) |> pull(!!color)
    } else if (is.list(highlight)) {
      if (!all(c("value",".fun") %in% names(highlight))) {
        stop("Highlight must be NA, double, or a list with \"value\" and \".fun\"!\n Value limits what is shown in legend and given a color, .fun is the function used (default is max).")
      } else {
        un_groups <- d |> group_by(!!color) |> summarize(value = highlight$.fun(.data$value, na.rm = T), .groups = "drop") |> filter(.data$value >= highlight$value) |> pull(!!color)
      }
    } else {
      stop("Highlight must be NA, double, or a list with \"value\" and \".fun\"!\n Value limits what is shown in legend and given a color, .fun is the function used (default is max).")
    }
    un_groups <- unique_groups |> subset(unique_groups %in% un_groups) |> droplevels()
    if(length(un_groups) == 0) { stop(str_c("No trace in \"",as_name(color),"\" fulfill the highlight requirements."), call. = F) }
    color_vector <- roboplotr_get_colors(length(un_groups)) |> setNames(un_groups)
    greyed_out <- subset(unique_groups, !unique_groups %in% un_groups)
    if(length(greyed_out) > 0) {
      greyed_out <- rep(first(unique(unlist(getOption("roboplot.colors.grid")))), length(greyed_out)) |> setNames(greyed_out)
      color_vector <- c(color_vector, greyed_out)
    }
    d[[as_name(color)]] <- fct_relevel(d[[as_name(color)]], levels(un_groups))
  } else {
    color_vector <- roboplotr_get_colors(length(unique_groups)) |> setNames(unique_groups)
  }

  color_vector
}


#' @importFrom grDevices colors
#' @importFrom stringr str_detect
#' @importFrom tidyr replace_na
roboplotr_are_colors <- function(x) {
  (str_detect(toupper(x), "#[0-9A-F]{6}") | x %in% colors()) |> replace_na(F)
}


#' @importFrom dplyr case_when
#' @importFrom farver add_to_channel convert_colour decode_colour
roboplotr_alter_color <- function(color,modifier) {
  b <- (color |> decode_colour() |> convert_colour("rgb","hsb"))[1,"b"] |> unname()
  s <- (color |> decode_colour() |> convert_colour("rgb","hsb"))[1,"s"] |> unname()
  m <- case_when(
    modifier == "lighter" ~ ifelse(b > 0.5, 0.15, 0.25),
    modifier == "light" ~ ifelse(b > 0.5, 0.10, 0.20),
    modifier == "less_light" ~ ifelse(b > 0.5, 0.05, 0.15),
    modifier == "none" ~ 0,
    modifier == "less_dark" ~ ifelse(b > 0.5, -0.02, -0.05),
    modifier == "dark" ~ ifelse(b < 0.5, -0.05, -0.15),
    modifier == "darker" ~ ifelse(b < 0.5, -0.15, -0.25),
    TRUE ~ 0
  )
  bm <- b+m
  d_sat <- case_when(
    bm > 1 ~ s-((1-(bm-1)*3)*s),
    bm < 0 ~ bm,
    TRUE ~ 0
  )
  if (modifier == "desaturated_dark") {
    add_to_channel(color,"s", -0.1, space = "hsb") |> roboplotr_alter_color("less_dark")
  } else if (modifier == "desaturated") {
    add_to_channel(color,"s", -0.2, space = "hsb")
  } else if (modifier == "saturated") {
    add_to_channel(color,"s", 0.1, space = "hsb")
  } else {
    add_to_channel(color, "b", m, space = "hsb") |>
      add_to_channel("s", -d_sat, space = "hsb")
  }
}

#' @importFrom dplyr case_when filter mutate pull rowwise slice_head slice_tail ungroup
#' @importFrom purrr map_dbl
roboplotr_text_color_picker <- function(picked_colors, fontsize = 12, fontweight = 500, draw_kunta = F, grey_shades = roboplotr_grey_shades()) {

  map(picked_colors, function(picked_color) {
    #font_color <- ifelse(rev == F, "#FFFFFF", "#000000")
    clr_lmn <- roboplotr_get_luminance(picked_color)
    ratio_lim <- case_when(
      #fontsize < 11 ~ 7,
      fontsize >= 18 || (fontsize >= 14 && fontweight >= 700) ~ 3,
      TRUE ~ 4.5)
    gs <- grey_shades |>
      mutate(
        max_l = map_dbl(.data$luminance, ~ max(clr_lmn,.x) + 0.05),
        min_l = map_dbl(.data$luminance, ~ min(clr_lmn,.x) + 0.05),
        ratio = .data$max_l/.data$min_l) |>
      filter(.data$ratio >= ratio_lim)
    if(nrow(gs) == 0) {ifelse(draw_kunta == F, "#000000", "#FFFFFF")} else {
      {if(draw_kunta == F) {slice_head(gs, n = 1)} else {slice_tail(gs, prop = 0.85) |> slice_head()}} |>
        pull(.data$color)
    }
  }) |> unlist()
}

#' @importFrom farver decode_colour
roboplotr_get_luminance <- function(color) {
  color <- decode_colour(color, "rgb", "rgb")
  RsRGB <-color[,"r"]/255
  GsRGB <-color[,"g"]/255
  BsRGB <-color[,"b"]/255
  R <- ifelse(RsRGB <= 0.03928, RsRGB/12.92, ((RsRGB+0.055)/1.055) ^ 2.4)
  G <- ifelse(GsRGB <= 0.03928, GsRGB/12.92, ((GsRGB+0.055)/1.055) ^ 2.4)
  B <- ifelse(BsRGB <= 0.03928, BsRGB/12.92, ((BsRGB+0.055)/1.055) ^ 2.4)
  L <- 0.2126 * R + 0.7152 * G + 0.0722 * B
  L
}

#' @importFrom farver add_to_channel
roboplotr_darken_white <- function(darken, white_hue = "#FFFFFF") {
  add_to_channel(white_hue, "r", -darken, space = "rgb") |>
    add_to_channel("g", -darken, space = "rgb") |>
    add_to_channel("b", -darken, space = "rgb")
}

#' @importFrom dplyr tibble
#' @importFrom purrr map reduce
roboplotr_grey_shades <- function() {
  grey_shades <- reduce(map(c(seq(0,255,5)), function(x) {roboplotr_darken_white(x)} ),c)
  tibble("color" = grey_shades,
         "luminance" = roboplotr_get_luminance(grey_shades))
}

#' @importFrom forcats fct_inorder fct_relevel fct_rev
roboplotr_get_pattern <- function(d, pattern) {
  dashtypes <- getOption("roboplot.dashtypes")
  patterntypes <- getOption("roboplot.patterntypes")
  bothtypes <- c(dashtypes, patterntypes)
  if(!is.null(pattern)) {
    if(!is.factor(d[[as_name(pattern)]])) {
      d[[as_name(pattern)]] <- fct_inorder(d[[as_name(pattern)]])
    }
    d <- d |> mutate(roboplot.dash = ifelse(.data$roboplot.plot.type == "scatter",dashtypes[!!pattern],dashtypes[1]),
                     roboplot.pattern = ifelse(.data$roboplot.plot.type != "scatter",patterntypes[!!pattern],patterntypes[1])
    )
  } else { d <- mutate(d, roboplot.dash = dashtypes[1], roboplot.pattern = patterntypes[1]) }
  d <- mutate(d,
              roboplot.dash = fct_relevel(.data$roboplot.dash, dashtypes[dashtypes %in% .data$roboplot.dash]),
              roboplot.pattern = fct_relevel(.data$roboplot.pattern, patterntypes[patterntypes %in% .data$roboplot.pattern]))
  d
}

#' @importFrom dplyr add_count group_by last mutate row_number ungroup
#' @importFrom rlang .data sym

roboplotr_get_bar_widths <- function(df, width_col) {
  get_offset <- function(the_count) {
    seq(-0.45, length.out = the_count, by = 1/the_count)
  }
  df |>
    add_count((!!sym(width_col)) , name = "roboplot.bar.width") |>
    group_by((!!sym(width_col)) ) |>
    mutate(roboplot.bar.offset = get_offset(max(.data$roboplot.bar.width)),
           roboplot.bar.width = ifelse(row_number() == last(row_number()), 1/.data$roboplot.bar.width*0.9, 1/.data$roboplot.bar.width)) |>
    ungroup()

}