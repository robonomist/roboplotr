#' @importFrom grDevices colorRampPalette
#' @importFrom plotly plot_ly
#' @importFrom purrr map

roboplotr_get_colors <- function(n_unique, robocolors = getOption("roboplot.colors.traces"), accessibility_params) {
  if(n_unique <= length(robocolors)) {
    cols <- robocolors[1:n_unique]
  } else if (n_unique <= length(robocolors)*1.5) {
    cols <- colorRampPalette(robocolors, interpolate = "linear")(n_unique)
  } else {
    cols <- colorRampPalette(robocolors, interpolate = "linear")(n_unique)
    cols <- split(cols, cut(seq_along(cols), ceiling(length(cols)/length(robocolors)), right = F, labels = F))
    theseq <- cols |> map(~ length(.x)) |> unlist() |> max()
    cols <- seq(theseq) |> map(function(x) {
      (map(cols, ~ .x[x]) |> roboplotr_compact())
    }) |> unlist()
    cols <-  cols |> subset(!is.na(cols)) |> unname()
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

    roboplotr_valid_colors(trace_color)
    if (length(trace_color) == 1 & is.null(names(trace_color))){
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
      greyed_out <- rep(first(unique(unlist(getOption("roboplot.grid")[c("ycolor","xcolor")]))), length(greyed_out)) |> setNames(greyed_out)
      color_vector <- c(color_vector, greyed_out)
    }
    d[[as_name(color)]] <- fct_relevel(d[[as_name(color)]], levels(un_groups))
  } else {
    color_vector <- roboplotr_get_colors(length(unique_groups)) |> setNames(unique_groups)
  }

  color_vector
}


#' @importFrom htmltools parseCssColors
#' @importFrom stringr str_detect
#' @importFrom tidyr replace_na
roboplotr_are_colors <- function(x) {
  !any(is.na(parseCssColors(x, mustWork = F)))
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
roboplotr_text_color_picker <- function(picked_colors, fontsize = 12, fontweight = 500, grey_shades = roboplotr_grey_shades()) {

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
    if(nrow(gs) == 0) { "#000000" } else {
      { slice_head(gs, n = 1)} |>
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
  grey_shades <- reduce(map(c(seq(0,255,5)), function(x) { roboplotr_darken_white(x) } ),c)
  tibble("color" = grey_shades,
         "luminance" = roboplotr_get_luminance(grey_shades))
}

#' @importFrom forcats fct_inorder fct_relevel fct_rev
roboplotr_get_pattern <- function(d, pattern, pattern_type = NULL) {
  dashtypes <- getOption("roboplot.dashtypes")
  patterntypes <- getOption("roboplot.patterntypes")
  if(!is.null(pattern)) {
    if(!is.factor(d[[as_name(pattern)]])) {
      d[[as_name(pattern)]] <- fct_inorder(d[[as_name(pattern)]])
    }
    defined_patterns <- roboplotr_set_pattern(d, pattern, pattern_type)
    if (!is.null(defined_patterns)) {
      d <- d |> mutate(roboplot.dash = ifelse(.data$roboplot.plot.type == "scatter",defined_patterns[!!pattern],dashtypes[1]),
                  roboplot.pattern = ifelse(.data$roboplot.plot.type != "scatter",defined_patterns[!!pattern],patterntypes[1]))
    } else {
      d <- d |> mutate(roboplot.dash = ifelse(.data$roboplot.plot.type == "scatter",dashtypes[!!pattern],dashtypes[1]),
                       roboplot.pattern = ifelse(.data$roboplot.plot.type != "scatter",patterntypes[!!pattern],patterntypes[1])
      )
    }
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

#' @importFrom farver add_to_channel
roboplotr_adjust_brightness <- function(val, color = "#FFFFFF") {
  add_to_channel(color, "r", -val, space = "rgb") |>
    add_to_channel("g", -val, space = "rgb") |>
    add_to_channel("b", -val, space = "rgb")
}

roboplotr_get_shades <- function(color = "#FFFFFF") {
  shades <- reduce(map(c(seq(-255,255,5)), function(x) {
    roboplotr_adjust_brightness(x, color)
  } ),c)
  tibble("color" = shades,
         "luminance" = roboplotr_get_luminance(shades)) |>
    distinct()
}

#' @importFrom dplyr ends_with full_join if_all starts_with
#' @importFrom tidyr pivot_longer
roboplotr_accessible_colors <- function(colors2alt, compared_colors = c(), background = "white",
                                        chart = T, fontsize = 12, fontweight = 500, chart.lim = 3) {

  colors2alt <- parseCssColors(colors2alt)

  get_priority <- function(o,n) {
    if(o == 1) {
      c(seq(0, length.out = o))
    } else if (o == nrow(n)) {
      c(seq(0,length.out = o) |> rev())
    } else {
      c(seq(1,length.out = o-1) |> rev(), 0, seq(1, nrow(n)-o))
    }
  }

  compared_colors <- c(background, compared_colors)

  get_altered_color <- function(color2alt, compared.colors = compared_colors, chart_lim = chart.lim, background) {

    clr_shades <- roboplotr_get_shades(color2alt)
    color_ops <- map(compared.colors, function(compared_color) {
      clr_lmn <- roboplotr_get_luminance(compared_color)
      ratio_lim <- case_when(
        chart == T ~ chart_lim,
        fontsize < 11 ~ 7,
        fontsize >= 18 || (fontsize >= 14 && fontweight >= 700) ~ 3,
        TRUE ~ 4.5)
      comp_shades <- clr_shades |>
        mutate(
          max_l = map_dbl(.data$luminance, ~ max(clr_lmn,.x) + 0.05),
          min_l = map_dbl(.data$luminance, ~ min(clr_lmn,.x) + 0.05),
          ratio = .data$max_l/.data$min_l) |>
        ungroup() |>
        mutate("{{compared_color}}_ratio" := ifelse(.data$ratio >= ratio_lim, T, F))
      {
        if(compared_color == background & length(compared.colors) > 1) {
          filter(comp_shades, .data$ratio > ratio_lim)
        } else { comp_shades }
        } |>
        select(.data$color,
               starts_with("ratio"), ends_with("ratio"))
    }) |>
      reduce(full_join, by = "color")

    these_colors <- color_ops$color
    these_shades <- clr_shades$color |> subset(clr_shades$color %in% these_colors)
    color_ops <- color_ops |> mutate(color = toupper(.data$color), color = fct_relevel(.data$color, these_shades)) |> arrange(.data$color)
    color_ops <- color_ops |> mutate(color = as.character(.data$color), priority = get_priority(which(tolower(color_ops$color) == tolower(color2alt)), color_ops)) |>
      mutate(count = rowSums(across(ends_with(c("_ratio")), ~ as.numeric(.x)), na.rm = T)) |>
      filter(if_all(starts_with("ratio"), ~ !is.na(.x))) |>
      filter(.data$count == max(.data$count, na.rm = T)) |>
      slice_min(order_by = .data$priority, n = 1, with_ties = F)
    pulled <- color_ops$color
    failed <- select(color_ops, ends_with("_ratio")) |> pivot_longer(everything()) |> filter(.data$value == F)
    if(nrow(failed) > 0) {
      msg <- failed$name |> str_extract("(?<=\\\").{1,}(?=\\\")") |> knitr::combine_words() |>
      str_c("Color contrast fails with ",msg," for ",pulled) |> roboplotr_warning(severity = "warning")

    }
    color_ops |> pull(.data$color)
  }

  altered <- map(colors2alt, function(color2alt) {

    d <- get_altered_color(color2alt, compared.colors = compared_colors, background = background)

    d
  }) |> unlist()

  wrn <- map2(colors2alt, altered, ~ if(.x != .y) { str_c(.x, " was changed to ", .y) }) |>
    roboplotr_compact() |>
    unlist() |>
    roboplotr_combine_words()

  if(length(wrn) > 0) {
    roboplotr_alert(str_c("Roboplot trace colors ",wrn," for accessibility."))
  }

  altered

}

#' @importFrom grDevices colorRamp rgb
#' @importFrom purrr map_chr
roboplotr_heatmap_colorfun <- function(d, cols = NULL,
                              hmin = first(getOption("roboplot.colors.traces")),
                              hmid = getOption("roboplot.colors.background"),
                              hmax = last(getOption("roboplot.colors.traces"))) {

  if(is.null(cols)) {
    numeric_columns <- d |> select(where(is.numeric))
  } else {
    numeric_columns <- d |> select(any_of(cols)) |> select(where(is.numeric))
  }
  vals <- numeric_columns |> unlist()

  if(is.numeric(hmin)) {
    anchor_min <- hmin
  } else {
    anchor_min <- min(vals, na.rm = T) |> setNames(hmin)
  }

  if(is.numeric(hmax)) {
    anchor_max <- hmax
  } else {
    anchor_max <- max(vals, na.rm = T) |> setNames(hmax)
  }

  if(is.numeric(hmid)) {
    anchor_mid <- hmid
  } else {
    anchor_mid <- mean(c(anchor_min, anchor_max), na.rm = T) |> setNames(hmid)
  }

  if(any(anchor_mid >= anchor_max, anchor_min >= anchor_mid)) {
    stop("Heatmap ranges are not usable! Ensure 'minvalue' is smaller than 'midvalue', and 'maxvalue' is larger than 'midvalue'!", call. = F)
  }

  .color_mapping <- function(values) {
    map_chr(values, function(value) {
      if(value < anchor_min) {
        names(anchor_min)
      } else if (value > anchor_max) {
        names(anchor_max)
      } else if (value <= anchor_mid) {
        col <- colorRamp(names(c(anchor_min, anchor_mid)))( (value - anchor_min) / (anchor_mid - anchor_min) )
        rgb(col[, 1L], col[, 2L], col[, 3L], maxColorValue = 255)
      } else {
        col <- colorRamp(names(c(anchor_mid, anchor_max)))( (value - anchor_mid) / (anchor_max - anchor_mid) )
        rgb(col[, 1L], col[, 2L], col[, 3L], maxColorValue = 255)
      }

    })
  }
  .color_mapping
}

#' interal function for adding heatmap styling [roboplotr::robotable]
#' @importFrom DT formatStyle styleEqual
roboplotr_heatmap <- function(d, dt, heatmap) {

  if(is.null(heatmap)) {
    dt
  } else {

    heatmap_fun <- roboplotr_heatmap_colorfun(d,hmin = heatmap$min, hmid = heatmap$mid, hmax = heatmap$max)

    .orders <- attributes(d)$dt_orders

    for(col in seq_len(length(.orders))) {
      order_col <- (.orders[col] |> names() |> as.numeric()) + 1
      col <- as.numeric(.orders[col])+1
      color_bg <- heatmap_fun(d[[order_col]])
      color_tx <- roboplotr_text_color_picker(color_bg)
      dt <- dt |> formatStyle(
        col,
        backgroundColor = styleEqual(d[[col]], color_bg),
        color = styleEqual(d[[col]], color_tx)
      )
    }

    dt
  }

}

#' Heatmap specifications for [robotable()]
#'
#' Use in [robotable()] parameter 'heatmap' to get a list used for setting the title.
#'
#' @param maxcolor,midcolor,mincolor Characters. Colors used hor heatmap color range. Must be a hexadecimal color strings or a valid css color strings.
#' @param maxvalue,midvalue,minvalue Numerics. Optional. Numeric breakpoints where the 'maxcolor', 'midcolor' and 'mincolor' colors are set at.
#' Any values falling outside of this range will have the nearest corresponding color. If not provided, [robotable()] calculates the values from the data.
#' Currently only support heatmaps across all numeric columns in the [robotable()].
#' @examples
#' # Use [set_heatmap()] to specify any the colors are value breaks used in heatmaps.
#' d <- energiantuonti |>
#'   dplyr::filter(Alue %in% c("Ruotsi","Kanada")) |>
#'   tidyr::unite(Tiedot, Alue, Suunta, sep = ", ") |>
#'   dplyr::arrange(Tiedot, time) |>
#'   tidyr::pivot_wider(names_from = Tiedot) |>
#'   dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) |>
#'   dplyr::arrange(time)
#' # No specifications uses the ends of the default trace colors set with
#' [set_roboplot_options()] and bases the numeric breakpoints on the data.
#'
#' d |> robotable(heatmap = set_heatmap())
#' #You can specify any of the parameters separately, and [robotable()] fills in the rest.
#' d |>
#'   robotable(heatmap = set_heatmap(
#'     midcolor = "white",
#'     mincolor = "lightblue",
#'     midvalue = 75
#'   ))

#' @returns A list
#' @importFrom stats setNames
#' @export
set_heatmap <-
  function(maxcolor = last(getOption("roboplot.colors.traces")),
           midcolor = getOption("roboplot.colors.background"),
           mincolor = first(getOption("roboplot.colors.traces")),
           maxvalue = NULL,
           midvalue = NULL,
           minvalue = NULL) {

    roboplotr_check_param(maxcolor, "character", allow_null = F)
    roboplotr_check_param(midcolor, "character", allow_null = F)
    roboplotr_check_param(mincolor, "character", allow_null = F)
    roboplotr:::roboplotr_valid_colors(c(maxcolor, midcolor, mincolor),"Any colors set with set_heatmap()")
    roboplotr_check_param(maxvalue, "numeric", allow_null = T)
    roboplotr_check_param(midvalue, "numeric", allow_null = T)
    roboplotr_check_param(minvalue, "numeric", allow_null = T)

    min <- if(is.null(minvalue)) {mincolor} else {setNames(minvalue, mincolor)}
    mid <- if(is.null(midvalue)) {midcolor} else {setNames(midvalue, midcolor)}
    max <- if(is.null(maxvalue)) {maxcolor} else {setNames(maxvalue, maxcolor)}

    list(min = min, mid = mid, max = max)

  }

#' Internal function for handling named patterns for traces
#' @importFrom dplyr distinct select
#' @importFrom purrr pmap reduce
#' @importFrom rlang quo_name
#' @importFrom stats setNames
roboplotr_set_pattern <- function(d, pattern, pattern_type) {
  if(!is.null(pattern_type)) {
    roboplotr_check_param(pattern_type, "character", NULL, allow_null = F)
    if (!all(unique(pull(select(d, quo_name(pattern)))) %in% names(pattern_type)) & !(".other" %in% names(pattern_type))) {
      stop(str_c("Either the parameter 'pattern_type' must be a named character vector with a name matching every variable in column \"",quo_name(pattern),"\", or name \".other\" must be included, or 'pattern_type' must be NULL!"), call. = F)
    }
    d |>
      select({{pattern}}, roboplot.plot.type) |>
      distinct() |>
      pmap(function(...) {
        this <- list(...)
        this_name <- as.character(this[[quo_name(pattern)]])
        if(!this_name %in% names(pattern_type)) {
          ptype <- pattern_type[".other"]
        } else {
          ptype <- pattern_type[this_name]
        }
        if (this$roboplot.plot.type == "scatter") {
          roboplotr_valid_strings(ptype, c("solid", "dash", "dot", "longdash", "dashdot", "longdashdot"), .fun = any, msg = str_glue("Pattern for \"{this_name}\""))
        } else {
          roboplotr_valid_strings(ptype, c("","/","\\","x","-","|","+","."), .fun = any, msg = str_glue("Pattern for \"{this_name}\""))
        }
        ptype |> setNames(this_name)
      }) |> reduce(c)
  } else {
    NULL
  }
}
