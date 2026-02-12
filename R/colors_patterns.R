#' @importFrom grDevices colorRampPalette
#' @importFrom purrr map
roboplotr_get_colors <- function(n_unique,
                                 robocolors = getOption("roboplot.colors.traces"),
                                 accessibility_params) {
  if (n_unique <= length(robocolors)) {
    cols <- robocolors[1:n_unique]
  } else if (n_unique <= length(robocolors) * 1.5) {
    cols <- colorRampPalette(robocolors, interpolate = "linear")(n_unique)
  } else {
    cols <- colorRampPalette(robocolors, interpolate = "linear")(n_unique)
    cols <- split(cols, cut(
      seq_along(cols),
      ceiling(length(cols) / length(robocolors)),
      right = F,
      labels = F
    ))
    theseq <- cols |> map( ~ length(.x)) |> unlist() |> max()
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
roboplotr_set_colors <- function(trace_color,
                                 unique_groups,
                                 highlight,
                                 d,
                                 color) {
  if (!is.null(trace_color)) {
    roboplotr_valid_colors(trace_color)
    if (length(trace_color) == 1 & is.null(names(trace_color))) {
      color_vector <- rep(trace_color, length(unique_groups)) |> setNames(unique_groups)
    } else if (!all(unique_groups %in% names(trace_color)) &
               !(".other" %in% names(trace_color))) {
      ug <- as.character(unique_groups)

      missing_groups <- ug |> subset(!ug %in% names(trace_color))
      if (length(missing_groups) > 0) {
        detected_traces <- map2(unname(trace_color), names(trace_color), function(tc, nm) {
          miss <- missing_groups |> subset(str_detect(missing_groups, str_c(nm, collapse = "|")))
          rep(tc, length(miss)) |> setNames(miss)
        }) |> roboplotr_compact() |> unlist()
        trace_color <- c(trace_color, detected_traces)
      }
      color_vector <- c(trace_color[ug[ug %in% names(trace_color)]],
                        ug[!ug %in% names(trace_color)] |> length() |> rep(x = trace_color[".other"]) |>
                          setNames(ug[!ug %in% names(trace_color)]))

      na_groups <- color_vector[is.na(color_vector)]
      color_vector <- c(color_vector[!is.na(color_vector)],
                        roboplotr_get_colors(length(na_groups)) |> setNames(names(na_groups)))
      # stop(str_c("Either trace color must be a single color string, or all variables in column \"",as_name(color),"\" must have a corresponding trace color, or key \".other\" must be included, or trace_color must be NULL!"), call. = F)

    } else {
      ug <- as.character(unique_groups)
      missing_groups <- ug |> subset(!ug %in% names(trace_color))
      if (length(missing_groups) > 0) {
        detected_traces <- map2(unname(trace_color), names(trace_color), function(tc, nm) {
          miss <- missing_groups |> subset(str_detect(missing_groups, str_c(nm, collapse = "|")))
          rep(tc, length(miss)) |> setNames(miss)
        }) |> roboplotr_compact() |> unlist()
        trace_color <- c(trace_color, detected_traces)
      }
      color_vector <- c(trace_color[ug[ug %in% names(trace_color)]],
                        ug[!ug %in% names(trace_color)] |> length() |> rep(x = trace_color[".other"]) |>
                          setNames(ug[!ug %in% names(trace_color)]))
    }

    if (!is.null(highlight)) {
      roboplotr_alert("The argument 'highlight' is ignored when providing trace colors.")
    }

  } else if (!is.null(highlight)) {
    if (is.numeric(highlight)) {
      un_groups <- d |> group_by(!!color) |> summarize(value = max(.data$value, na.rm = T),
                                                       .groups = "drop") |> filter(.data$value >= highlight) |> pull(!!color)
    } else if (is.list(highlight)) {
      if (!all(c("value", ".fun") %in% names(highlight))) {
        stop(
          "Highlight must be NA, double, or a list with \"value\" and \".fun\"!\n Value limits what is shown in legend and given a color, .fun is the function used (default is max)."
        )
      } else {
        un_groups <- d |> group_by(!!color) |> summarize(value = highlight$.fun(.data$value, na.rm = T),
                                                         .groups = "drop") |> filter(.data$value >= highlight$value) |> pull(!!color)
      }
    } else {
      stop(
        "Highlight must be NA, numeric, or a list with \"value\" and \".fun\"!\n Value limits what is shown in legend and given a color, .fun is the function used (default is max)."
      )
    }
    un_groups <- unique_groups |> subset(unique_groups %in% un_groups) |> droplevels()
    if (length(un_groups) == 0) {
      stop(
        str_c(
          "No trace in \"",
          as_name(color),
          "\" fulfill the highlight requirements."
        ),
        call. = F
      )
    }
    color_vector <- roboplotr_get_colors(length(un_groups)) |> setNames(un_groups)
    greyed_out <- subset(unique_groups, !unique_groups %in% un_groups)
    if (length(greyed_out) > 0) {
      greyed_out <- rep(first(unique(unlist(
        getOption("roboplot.grid")[c("ycolor", "xcolor")]
      ))), length(greyed_out)) |> setNames(greyed_out)
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
roboplotr_alter_color <- function(color, modifier) {
  b <- (color |> decode_colour() |> convert_colour("rgb", "hsb"))[1, "b"] |> unname()
  s <- (color |> decode_colour() |> convert_colour("rgb", "hsb"))[1, "s"] |> unname()
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
  bm <- b + m
  d_sat <- case_when(bm > 1 ~ s - ((1 - (bm - 1) * 3) * s), bm < 0 ~ bm, TRUE ~ 0)
  if (modifier == "desaturated_dark") {
    add_to_channel(color, "s", -0.1, space = "hsb") |> roboplotr_alter_color("less_dark")
  } else if (modifier == "desaturated") {
    add_to_channel(color, "s", -0.2, space = "hsb")
  } else if (modifier == "saturated") {
    add_to_channel(color, "s", 0.1, space = "hsb")
  } else {
    add_to_channel(color, "b", m, space = "hsb") |>
      add_to_channel("s", -d_sat, space = "hsb")
  }
}

#' @importFrom dplyr case_when filter mutate pull rowwise slice_head slice_tail ungroup
#' @importFrom purrr map_dbl
roboplotr_text_color_picker <- function(picked_colors,
                                        fontsize = 12,
                                        fontweight = 500,
                                        grey_shades = roboplotr_grey_shades()) {
  .picked_colors <- unique(picked_colors)
  replacements <- map(.picked_colors, function(picked_color) {
    #font_color <- ifelse(rev == F, "#FFFFFF", "#000000")
    clr_lmn <- roboplotr_get_luminance(picked_color)
    ratio_lim <- case_when(#fontsize < 11 ~ 7,
      fontsize >= 18 || (fontsize >= 14 && fontweight >= 700) ~ 3,
      TRUE ~ 4.5)
    gs <- grey_shades |>
      mutate(
        max_l = map_dbl(.data$luminance, ~ max(clr_lmn, .x) + 0.05),
        min_l = map_dbl(.data$luminance, ~ min(clr_lmn, .x) + 0.05),
        ratio = .data$max_l / .data$min_l
      ) |>
      filter(.data$ratio >= ratio_lim)
    if (nrow(gs) == 0) {
      "#000000"
    } else {
      {
        slice_head(gs, n = 1)
      } |>
        pull(.data$color)
    }
  }) |> unlist() |>
    setNames(.picked_colors)
  v_replaced <- picked_colors
  v_replaced[v_replaced %in% names(replacements)] <- replacements[match(v_replaced, names(replacements))]
  v_replaced
}

#' @importFrom farver decode_colour
roboplotr_get_luminance <- function(color) {
  color <- decode_colour(color, "rgb", "rgb")
  RsRGB <- color[, "r"] / 255
  GsRGB <- color[, "g"] / 255
  BsRGB <- color[, "b"] / 255
  R <- ifelse(RsRGB <= 0.03928, RsRGB / 12.92, ((RsRGB + 0.055) / 1.055) ^ 2.4)
  G <- ifelse(GsRGB <= 0.03928, GsRGB / 12.92, ((GsRGB + 0.055) / 1.055) ^ 2.4)
  B <- ifelse(BsRGB <= 0.03928, BsRGB / 12.92, ((BsRGB + 0.055) / 1.055) ^ 2.4)
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
  grey_shades <- reduce(map(c(seq(0, 255, 5)), function(x) {
    roboplotr_darken_white(x)
  }), c)
  tibble("color" = grey_shades, "luminance" = roboplotr_get_luminance(grey_shades))
}

#' Pattern configuration.
#'
#' Parameters to customize patterns in [roboplots][roboplot()].
#'
#' @param pattern Symbol or string. Column from param `d` of a [roboplot][roboplot()]
#' to use for the linetype of `plot_type` "scatter" or pattern of `plot_type` "bar".
#' Not supported for pie charts.
#' @param pattern_types Named character vector. Pattern types for all traces.
#' Names must correspond to values in the column referenced by `pattern. See
#' [set_roboplot_options()] parameter `dashtypes` and `patterns` for options, or
#' use ".other" if multiple traces are of the same `plot_type` and share the pattern.
#' @param show_legend Logical, named if vector. If any pattern(s) will have
#' their own legend entries. If FALSE, only the first item in param 'pattern'
#' will be in a [roboplot's][roboplot()] legend. If named, the must correspond to
#' values in the column referenced by `pattern`.
#' @param pattern_along Symbol or string. Column from param `d` of a [roboplot][roboplot()]
#' along which the series should be continuous along. If set, and the `plot_mode`
#' corresponding to all items in `pattern` is "line", [roboplot][roboplot()] will
#' try to fill the series in such a way as to create a continuous line.
#' @param sep Character. The separator of `color` and `pattern` in legend labels.
#' Default is ", ". Use NA if you want to omit the pattern label from legend,
#' resulting only the [roboplot()] param `color` being the label.
#' @examples
#' # You can use [set_pattern()] to just give the column name, but in that case
#' # you could just as well provide the column name by itself.
#' # Compare this:
#' energiantuonti |>
#'   dplyr::filter(Alue %in% c("Belgia","USA")) |>
#'   roboplot(
#'     Alue,
#'     pattern = Suunta
#'   )
#' #' # To this:
#' energiantuonti |>
#'   dplyr::filter(Alue %in% c("Belgia","USA")) |>
#'   roboplot(
#'     Alue,
#'     pattern = set_pattern(Suunta)
#'   )
#' # Suppose you have a series where some values are predictions and some are
#' # observations, you would have a gap in a line plot.
#' d <- energiantuonti |>
#'   dplyr::filter(Alue == "Belgia", Suunta == "Tuonti") |>
#'   dplyr::mutate(Sarjatyyppi = ifelse(
#'     lubridate::year(time) == max(lubridate::year(time)),
#'     "Ennuste",
#'     "Toteuma"
#'   ))
#'
#' roboplot(d, Alue, pattern = Sarjatyyppi)
#'
#' # Use the parameter `pattern_along` to provide the column along which the plot
#' # should show a continous line.
#' roboplot(d, Alue, pattern = set_pattern(Sarjatyyppi, pattern_along = time))
#'
#' # Use the parameter `show_legend` to omit all patterns expect the first one
#' # from the legend, and use the parameter `sep` to control the separator
#' # between the `roboplot()` param `color` and `pattern` in the legend.
#' roboplot(d,
#'          Alue,
#'          pattern = set_pattern(
#'            Sarjatyyppi,
#'            pattern_along = time,
#'            show_legend = FALSE,
#'            sep = " - "
#'          ))
#'
#' # Or just use `sep` = NA to omit the pattern from the legend labels.
#' roboplot(d, Alue,
#'            pattern =
#'              set_pattern(
#'                Sarjatyyppi,
#'                pattern_along = time,
#'                show_legend = FALSE,
#'                sep = NA
#'              ))
#'
#' # Finally, control the patterns for linetypes and bars with the parameter
#' # `pattern_types`, with a named vector containing either all the observations
#' # in the column `pattern`, or ".other" as a catch-all category.
#' energiantuonti |>
#'   dplyr::filter(Alue %in% c("USA", "Belgia", "Ruotsi"), Suunta == "Tuonti") |>
#'   roboplot(Alue, pattern = set_pattern(Alue, pattern_types = c(
#'     "USA" = "dash", ".other" = "dot"
#'   )))
#' # Bar plots use the pattern_types too, but they are different from the ones
#' # used by line plots. If you get them wrong, `roboplot()` informs you which you
#' # should be using.
#' energiantuonti |>
#'   dplyr::filter(Alue %in% c("USA", "Belgia", "Ruotsi"), Suunta == "Tuonti") |>
#'   roboplot(Alue,
#'            plot_type = "bar",
#'            pattern = set_pattern(Alue, pattern_types = c(
#'              "Ruotsi" = "", ".other" = "x"
#'            )))
#' @export
#' @returns A list of class roboplotr.set_pattern
set_pattern <- function(pattern = NULL,
                        pattern_types = NULL,
                        pattern_along = NULL,
                        show_legend = TRUE,
                        sep = ", ") {
  pattern <- enquo(pattern)
  roboplotr_typecheck(pattern_types, "character", size = NULL, extra = "in set_pattern()")
  pattern_along <- enquo(pattern_along)
  roboplotr_typecheck(show_legend, "logical", NULL, F, extra = "in set_pattern()")
  roboplotr_typecheck(sep, "character", 1, F, allow_na = T, extra = "set_pattern()")

  .res <- list(
    pattern = pattern,
    pattern_types = pattern_types,
    pattern_along = pattern_along,
    show_legend = show_legend,
    pattern_sep = sep
  )

  .res <- structure(.res, class = c("roboplotr", "roboplotr.set_pattern", class(.res)))

  .res
}

#' @importFrom dplyr mutate
#' @importFrom forcats fct_inorder fct_relevel fct_rev
#' @importFrom purrr map2_chr
#' @importFrom rlang as_name
roboplotr_get_pattern <- function(d, pattern, pattern_type = NULL) {
  dashtypes <- getOption("roboplot.dashtypes")
  patterntypes <- getOption("roboplot.patterntypes")
  if (!is.null(pattern)) {

    if (any((d |> distinct(!!pattern, roboplot.plot.type, roboplot.plot.mode) |> count(!!pattern) |> pull(n)) > 1)) {
      stop(str_glue("Too many combinations of `roboplot(plot_type, pattern)`."), call. = F)
    }
    if (!is.factor(d[[as_name(pattern)]])) {
      d[[as_name(pattern)]] <- fct_inorder(d[[as_name(pattern)]])
    }
    if (!".other" %in% names(pattern_type)) {
      .default <- c("dash" = dashtypes[1], "bar" = patterntypes[1])
    } else if (pattern_type[".other"] %in% dashtypes) {
      .default <- c("dash" = unname(pattern_type[".other"]), "bar" = patterntypes[1])
    } else if (pattern_type[".other"] %in% patterntypes) {
      .default <- c("dash" = dashtypes[1], "bar" = unname(pattern_type[".other"]))
    } else {
      .default <- c("dash" = dashtypes[1], "bar" = patterntypes[1])
    }
    
    defined_patterns <- roboplotr_set_pattern(d, pattern, pattern_type)

    if (!is.null(defined_patterns)) {
      d <- d |>
        mutate(
          roboplot.dash = map2_chr(.data$roboplot.plot.type, !!pattern, ~ {
            ifelse(.x == "scatter", defined_patterns[as.character(.y)], .default["dash"])
          }),
          roboplot.pattern = map2_chr(.data$roboplot.plot.type, !!pattern, ~ {
            ifelse(.x != "scatter", defined_patterns[as.character(.y)], .default["bar"])
          })
        )
    } else {
      scatter_labs_length <- length(unique(d[[as_label(pattern)]][d$roboplot.plot.type == "scatter"]))
      dashtypes <- roboplotr_generate_dashtypes(scatter_labs_length,dashtypes)
      other_labs_length <- length(unique(d[[as_label(pattern)]][d$roboplot.plot.type != "scatter"]))
      roboplotr_is_between(other_labs_length, "set_pattern()", lims = c(0,length(patterntypes)), what = str_glue("must either specify \".other\" in `pattern_type`, or the number of unique variables in column {as_label(pattern)} of param `roboplot(d)` not using pattern type \"scatter\""))
      d <- d |> mutate(
        roboplot.dash = ifelse(
          .data$roboplot.plot.type == "scatter",
          dashtypes[!!pattern],
          .default["dash"]
        ),
        roboplot.pattern = ifelse(
          .data$roboplot.plot.type != "scatter",
          patterntypes[!!pattern],
          .default["bar"]
        )
      )
    }
  } else {
    d <- mutate(d,
                roboplot.dash = dashtypes[1],
                roboplot.pattern = patterntypes[1])
  }
  d <- mutate(
    d,
    roboplot.dash = fct_relevel(.data$roboplot.dash, dashtypes[dashtypes %in% .data$roboplot.dash]),
    roboplot.pattern = fct_relevel(.data$roboplot.pattern, patterntypes[patterntypes %in% .data$roboplot.pattern])
  )
  d
}

#' @importFrom dplyr add_count group_by last mutate row_number ungroup
#' @importFrom rlang .data sym
roboplotr_get_bar_widths <- function(df, width_col, color) {
  get_offset <- function(the_count) {
    seq(-0.45, length.out = the_count, by = 1 / the_count)
  }
  df |>
    arrange(!!color) |>
    add_count((!!sym(width_col)) , name = "roboplot.bar.width") |>
    group_by((!!sym(width_col))) |>
    mutate(
      roboplot.bar.offset = get_offset(max(.data$roboplot.bar.width)),
      roboplot.bar.width = ifelse(
        row_number() == last(row_number()),
        1 / .data$roboplot.bar.width * 0.9,
        1 / .data$roboplot.bar.width
      )
    ) |>
    ungroup()

}

#' @importFrom farver add_to_channel
roboplotr_adjust_brightness <- function(val, color = "#FFFFFF") {
  add_to_channel(color, "r", -val, space = "rgb") |>
    add_to_channel("g", -val, space = "rgb") |>
    add_to_channel("b", -val, space = "rgb")
}

roboplotr_get_shades <- function(color = "#FFFFFF") {
  shades <- reduce(map(c(seq(-255, 255, 5)), function(x) {
    roboplotr_adjust_brightness(x, color)
  }), c)
  tibble("color" = shades, "luminance" = roboplotr_get_luminance(shades)) |>
    distinct()
}

#' @importFrom dplyr ends_with full_join if_all starts_with
#' @importFrom tidyr pivot_longer
roboplotr_accessible_colors <- function(colors2alt,
                                        compared_colors = c(),
                                        background = "white",
                                        chart = T,
                                        fontsize = 12,
                                        fontweight = 500,
                                        chart.lim = 3) {
  colors2alt <- parseCssColors(colors2alt)

  get_priority <- function(o, n) {
    if (o == 1) {
      c(seq(0, length.out = o))
    } else if (o == nrow(n)) {
      c(seq(0, length.out = o) |> rev())
    } else {
      c(seq(1, length.out = o - 1) |> rev(), 0, seq(1, nrow(n) - o))
    }
  }

  compared_colors <- c(background, compared_colors)

  get_altered_color <- function(color2alt,
                                compared.colors = compared_colors,
                                chart_lim = chart.lim,
                                background) {
    clr_shades <- roboplotr_get_shades(color2alt)
    color_ops <- map(compared.colors, function(compared_color) {
      clr_lmn <- roboplotr_get_luminance(compared_color)
      ratio_lim <- case_when(
        chart == T ~ chart_lim,
        fontsize < 11 ~ 7,
        fontsize >= 18 || (fontsize >= 14 && fontweight >= 700) ~ 3,
        TRUE ~ 4.5
      )
      comp_shades <- clr_shades |>
        mutate(
          max_l = map_dbl(.data$luminance, ~ max(clr_lmn, .x) + 0.05),
          min_l = map_dbl(.data$luminance, ~ min(clr_lmn, .x) + 0.05),
          ratio = .data$max_l / .data$min_l
        ) |>
        ungroup() |>
        mutate("{{compared_color}}_ratio" := ifelse(.data$ratio >= ratio_lim, T, F))
      {
        if (compared_color == background & length(compared.colors) > 1) {
          filter(comp_shades, .data$ratio > ratio_lim)
        } else {
          comp_shades
        }
        } |>
        select(.data$color, starts_with("ratio"), ends_with("ratio"))
    }) |>
      reduce(full_join, by = "color")

    these_colors <- color_ops$color
    these_shades <- clr_shades$color |> subset(clr_shades$color %in% these_colors)
    color_ops <- color_ops |> mutate(color = toupper(.data$color),
                                     color = fct_relevel(.data$color, these_shades)) |> arrange(.data$color)
    color_ops <- color_ops |> mutate(color = as.character(.data$color),
                                     priority = get_priority(which(
                                       tolower(color_ops$color) == tolower(color2alt)
                                     ), color_ops)) |>
      mutate(count = rowSums(across(ends_with(c(
        "_ratio"
      )), ~ as.numeric(.x)), na.rm = T)) |>
      filter(if_all(starts_with("ratio"), ~ !is.na(.x))) |>
      filter(.data$count == max(.data$count, na.rm = T)) |>
      slice_min(order_by = .data$priority,
                n = 1,
                with_ties = F)
    pulled <- color_ops$color
    failed <- select(color_ops, ends_with("_ratio")) |> pivot_longer(everything()) |> filter(.data$value == F)
    if (nrow(failed) > 0) {
      msg <- failed$name |> str_extract("(?<=\\\").{1,}(?=\\\")") |> roboplotr_combine_words() |>
        str_c("Color contrast fails with ", msg, " for ", pulled) |> roboplotr_alert(severity = "warning")

    }
    color_ops |> pull(.data$color)
  }

  altered <- map(colors2alt, function(color2alt) {
    d <- get_altered_color(color2alt,
                           compared.colors = compared_colors,
                           background = background)

    d
  }) |> unlist()

  wrn <- map2(colors2alt, altered, ~ if (.x != .y) {
    str_c(.x, " was changed to ", .y)
  }) |>
    roboplotr_compact() |>
    unlist() |>
    roboplotr_combine_words()

  if (length(wrn) > 0) {
    roboplotr_alert(str_c("Roboplot trace colors ", wrn, " for accessibility."))
  }

  altered

}

#' @importFrom dplyr any_of
#' @importFrom grDevices colorRamp rgb
#' @importFrom purrr map_chr
#' @importFrom stats setNames
roboplotr_tbl_heatmap_colorfun <- function(d,
                                           cols = NULL,
                                           hmin = first(getOption("roboplot.colors.traces")),
                                           hmid = getOption("roboplot.colors.background"),
                                           hmax = last(getOption("roboplot.colors.traces")),
                                           na_color = getOption("roboplot.colors.background")) {
  if (is.null(cols)) {
    numeric_columns <- d |> select(where(is.numeric))
  } else {
    numeric_columns <- d |> select(any_of(cols)) |> select(where(is.numeric))
  }
  vals <- numeric_columns |> unlist()

  if (is.numeric(hmin)) {
    anchor_min <- hmin
  } else {
    anchor_min <- min(vals, na.rm = T) |> setNames(hmin)
  }

  if (is.numeric(hmax)) {
    anchor_max <- hmax
  } else {
    anchor_max <- max(vals, na.rm = T) |> setNames(hmax)
  }

  if (is.numeric(hmid)) {
    anchor_mid <- hmid
  } else {
    anchor_mid <- mean(c(anchor_min, anchor_max), na.rm = T) |> setNames(hmid)
  }

  if (any(anchor_mid >= anchor_max, anchor_min >= anchor_mid)) {
    stop(
      "Heatmap ranges are not usable! Ensure 'minvalue' is smaller than 'midvalue', and 'maxvalue' is larger than 'midvalue'!",
      call. = F
    )
  }

  .color_mapping <- function(values) {
    # map_chr(values, function(value) {
    #   if (is.na(value)) {
    #     na_color
    #   } else if (value < anchor_min) {
    #     names(anchor_min)
    #   } else if (value > anchor_max) {
    #     names(anchor_max)
    #   } else if (value <= anchor_mid) {
    #     col <- colorRamp(names(c(anchor_min, anchor_mid)))((value - anchor_min) / (anchor_mid - anchor_min))
    #     rgb(col[, 1L], col[, 2L], col[, 3L], maxColorValue = 255)
    #   } else {
    #     col <- colorRamp(names(c(anchor_mid, anchor_max)))((value - anchor_mid) / (anchor_max - anchor_mid))
    #     rgb(col[, 1L], col[, 2L], col[, 3L], maxColorValue = 255)
    #   }
    #
    # })
    map_chr(values, function(value) {
      if (is.na(value)) {
        na_color
      } else if (value < anchor_min) {
        names(anchor_min)
      } else if (value > anchor_max) {
        names(anchor_max)
      } else if (value <= anchor_mid) {
        col <- colorRamp(c(names(anchor_min), names(anchor_mid)))((value - anchor_min) / (anchor_mid - anchor_min))
        rgb(col[1L], col[2L], col[3L], maxColorValue = 255)
      } else {
        col <- colorRamp(c(names(anchor_mid), names(anchor_max)))((value - anchor_mid) / (anchor_max - anchor_mid))
        rgb(col[1L], col[2L], col[3L], maxColorValue = 255)
      }
    })
  }
  attr(.color_mapping, "colorType") <- "numeric"
  attr(.color_mapping, "colorArgs") <- list(na.color = na_color)
  .color_mapping
}

#' interal function for adding heatmap styling [roboplotr::robotable]
#' @importFrom DT formatStyle styleEqual
#' @noRd
roboplotr_tbl_heatmap <- function(d, dt, heatmap) {
  if (is.null(heatmap)) {
    dt
  } else {
    heatmap_fun <- roboplotr_tbl_heatmap_colorfun(
      d,
      hmin = heatmap$min,
      hmid = heatmap$mid,
      hmax = heatmap$max,
      na_color = heatmap$na
    )
    .orders <- attributes(d)$dt_orders

    for (col in seq_len(length(.orders))) {
      order_col <- (.orders[col] |> names() |> as.numeric()) + 1
      col <- as.numeric(.orders[col]) + 1
      color_bg <- heatmap_fun(d[[order_col]])
      color_tx <- roboplotr_text_color_picker(color_bg, getOption("roboplot.font.main")$size)
      dt <- dt |> formatStyle(
        col,
        backgroundColor = styleEqual(d[[col]], color_bg),
        color = styleEqual(d[[col]], color_tx)
      )
    }

    dt
  }
}

#' Heatmap configuration.
#'
#' Parameters to customize heatmaps in [robotables][robotable()] and [robomaps][robomap()].
#'
#' @param maxcolor,midcolor,mincolor,na_color Characters. Colors used for heatmap color
#' range. Must be a hexadecimal colors or a valid css colors.
#' @param maxvalue,midvalue,minvalue Numerics. Optional. Numeric breakpoints where
#' `maxcolor`, `midcolor` and `mincolor` are set at. Any values falling outside of
#' this range will have the nearest corresponding color. If not provided, [robotable()]
#' calculates the values from the data.
#' Currently only support heatmaps across all numeric columns in the given [robotable()].
#' @examples
#' # Use `set_heatmap()` to specify any the colors are value breaks used in heatmaps.
#'
#' d <- roboplotr::energiantuonti |>
#'   dplyr::filter(Alue %in% c("Ruotsi","USA")) |>
#'   tidyr::unite(Tiedot, Alue, Suunta, sep = ", ") |>
#'   dplyr::arrange(Tiedot, time) |>
#'   tidyr::pivot_wider(names_from = Tiedot) |>
#'   dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) |>
#'   dplyr::arrange(time)
#' # No specifications uses the ends of the default trace colors set with
#' # `set_roboplot_options()` and bases the numeric breakpoints on the data.
#'
#' d |> robotable(heatmap = set_heatmap())
#'
#' # You can specify any of the parameters separately, and `robotable()` fills in the rest.
#'
#' d |>
#'   robotable(heatmap = set_heatmap(
#'     midcolor = "white",
#'     mincolor = "lightblue",
#'     midvalue = 75
#'   ))
#' @returns A list of class roboplot.set_heatmap
#' @importFrom stats setNames
#' @export
set_heatmap <-
  function(maxcolor = last(getOption("roboplot.colors.traces")),
           midcolor = getOption("roboplot.colors.background"),
           mincolor = first(getOption("roboplot.colors.traces")),
           maxvalue = NULL,
           midvalue = NULL,
           minvalue = NULL,
           na_color = getOption("roboplot.colors.background")) {
    roboplotr_typecheck(maxcolor, "character", allow_null = F)
    roboplotr_typecheck(midcolor, "character", allow_null = F)
    roboplotr_typecheck(mincolor, "character", allow_null = F)
    roboplotr_valid_colors(c(maxcolor, midcolor, mincolor, na_color),
                           "Any colors set with set_heatmap()")
    roboplotr_typecheck(maxvalue, "numeric")
    roboplotr_typecheck(midvalue, "numeric")
    roboplotr_typecheck(minvalue, "numeric")

    min <- if (is.null(minvalue)) {
      mincolor
    } else {
      setNames(minvalue, mincolor)
    }
    mid <- if (is.null(midvalue)) {
      midcolor
    } else {
      setNames(midvalue, midcolor)
    }
    max <- if (is.null(maxvalue)) {
      maxcolor
    } else {
      setNames(maxvalue, maxcolor)
    }

    .res <- list(
      min = min,
      mid = mid,
      max = max,
      na = na_color
    )

    .res <- structure(.res, class = c("roboplotr", "roboplotr.set_heatmap", class(.res)))

    .res

  }


#' Internal function for handling named patterns for traces
#' @importFrom dplyr distinct mutate select
#' @importFrom purrr pmap reduce
#' @importFrom rlang quo_name
#' @importFrom stats setNames
#' @noRd
roboplotr_set_pattern <- function(d, pattern, pattern_type) {
  if (!is.null(pattern_type)) {
    
    roboplotr_typecheck(pattern_type, "character", NULL, allow_null = F)

    roboplotr_valid_strings(c(names(pattern_type)), unique(d[[as_label(pattern)]]), all, "Names in `set_pattern(pattern_type)`", placeholder = c(".bar",".scatter")) 

    dashtypes <- getOption("roboplot.dashtypes")
    patterntypes <- getOption("roboplot.patterntypes")
    res <- d |>
      select(!!pattern, .data$roboplot.plot.type) |>
      distinct() |>
      mutate({{pattern}} := as.character({{pattern}})) |>
      pmap(function(...) {
        this <- list(...)
        this_name <- as.character(this[[quo_name(pattern)]])
        this_type <- ifelse(this$roboplot.plot.type == "scatter", ".scatter",".bar")
        if (!this_name %in% names(pattern_type)) {
          ptype <- pattern_type[this_type]
        } else {
          ptype <- pattern_type[this_name]
        }
        if (this_type == ".scatter") {
          roboplotr_valid_strings(
            ptype,
            dashtypes,
            .fun = any,
            msg = str_glue("`set_pattern(pattern_type)` for \"{this_name}\""),
            placeholder = this_type)
        } else {
          roboplotr_valid_strings(
            ptype,
            patterntypes,
            .fun = any,
            msg = str_glue("`set_pattern(pattern_type)` for \"{this_name}\""),
            placeholder = this_type)
        }
        ptype |> setNames(this_name)
      }) |> reduce(c)
    res |> make.unique() |> setNames(names(res))
  } else {
    NULL
  }
}

roboplotr_get_map_palette <- function(domain, map_colors, gradient = F, rev = T) {
  if(gradient) {
    leaflet::colorNumeric(
      map_colors,
      domain = domain,
      reverse = rev,
      na.color = "#00000000"
    )
  } else {
    leaflet::colorFactor(
      map_colors,
      domain = domain,
      reverse = !rev,
      na.color = "#00000000"
    )
  }
}

#' Determine if legend item for a given pattern is shown in [roboplot()]
#' @importFrom rlang is_quosure
#' @importFrom stats setNames
#' @noRd
roboplotr_get_pattern_showlegend <- function(d,
                                             pattern,
                                             pattern_showlegend,
                                             legend_position) {
  if (is.null(pattern_showlegend)) {
    return(NULL)
  }

  if (!is_quosure(pattern)) {
    pattern <- enquo(pattern)
  }
  if ((!quo_is_null(pattern) & !is.null(pattern_showlegend))) {
    pattern_levels <- levels(d[[as_name(pattern)]]) |> as.character()
    if (is.null(names(pattern_showlegend))) {
      if (all(pattern_showlegend == T)) {
        pattern_showlegend <- rep(T, length(pattern_levels)) |> setNames(pattern_levels)
      } else {
        pattern_showlegend <- c(T, rep(F, length(pattern_levels) - 1)) |> setNames(pattern_levels)
      }
    } else {
      roboplotr_valid_strings(names(pattern_showlegend), pattern_levels, any)
      other_patterns <- pattern_levels[!pattern_levels %in% names(pattern_showlegend)]
      other_patterns <- rep(F, length(other_patterns)) |> setNames(other_patterns)
      pattern_showlegend <- c(pattern_showlegend, other_patterns)
    }
  } else {
    pattern_showlegend <- NULL
  }
  
  pattern_showlegend
}

#' Marker configuration
#'
#' Parameters to customize markers in [roboplots][roboplot()]. Currently not available
#' by trace.
#'
#' @param symbol Character. The marker symbol when drawn by [roboplot()]. Any
#' one of "circle","square","diamond","cross","x", "line" or "star".
#' @param size Numeric. Marker size for markers.
#' @examples
#' # You can change the markers `roboplot()` uses by using `set_markers()`.
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Tuonti", Alue == "Venäjä") |>
#'   dplyr::group_by(Alue) |>
#'   dplyr::mutate(sd = sd(value)) |>
#'   dplyr::ungroup() |>
#'   roboplot(
#'     Alue,
#'     plot_type = "scatter",
#'     plot_mode = "scatter",
#'     markers = set_markers(symbol = "diamond", size = 12)
#'   )
#' # You cannot control the markers by trace, but you can use `pattern` in `roboplot()`
#' # with markers if you have set 'plot_mode' to "scatter+line".
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Tuonti", Alue == "Venäjä") |>
#'   dplyr::group_by(Alue) |>
#'   dplyr::mutate(sd = sd(value)) |>
#'   dplyr::ungroup() |>
#'   roboplot(
#'     Alue,
#'     plot_type = "scatter",
#'     plot_mode = "scatter+line",
#'     pattern = set_pattern(Alue, pattern_types = c("Venäjä" = "dot")),
#'     markers = set_markers(symbol = "square", size = 8)
#'   )
#'
#' @importFrom stringr str_replace
#' @export
#' @returns A list of class roboplotr.set_markers
set_markers <-
  function(symbol = getOption("roboplot.markers")$symbol,
           size = getOption("roboplot.markers")$size) {
    roboplotr_typecheck(size, "numeric", allow_null = F)
    roboplotr_typecheck(symbol, "character", allow_null = F)
    symbol <- str_remove(symbol, "-ew-open$")
    roboplotr_valid_strings(
      symbol,
      c("circle", "square", "diamond", "cross", "x", "line", "star"),
      any,
      "set_markers(symbol)"
    )

    .res <- list(symbol = str_replace(symbol, "line", "line-ew-open"),
                 size = size)

    .res <- structure(.res, class = c("roboplotr", "roboplotr.set_markers", class(.res)))

    .res

  }

#' [roboplot()] attempts to bind patterns for continuous line for certain plots
#' @importFrom dplyr arrange bind_rows filter group_by group_split left_join mutate pull select
#' @importFrom purrr imap_dfr
#' @noRd
#'
roboplotr_continuous_pattern <- function(d, along, pattern, group) {
  split <- d |>
    arrange({{pattern}}) |>
    group_by({{pattern}}) |>
    group_split()
  imap_dfr(split, function(.group, i) {
    if(i == 1) {
      .group
    } else {
      prev <- split[[i-1]] |>
        group_by({{group}}) |>
        filter({{along}} == max({{along}})) |>
        ungroup() |>
        select(-starts_with("roboplot."), -{{pattern}})
      .first <- .group |> group_by({{group}}) |> filter({{along}} == min({{along}})) |> select(-{{along}}, -.data$value) |> ungroup()
      prev |> left_join(.first, by = as_label(group)) |>
        drop_na() |>
        bind_rows(.group)
    }
  }) |>
    arrange({{along}})
}

roboplotr_dashtypes <- function(n,
                                px_on  = c(2,3,4,6,7,8,10,12,14),
                                px_off = c(1,2,3,4,6,7,9,11),
                                allow_lengths = c(4, 6),
                                similarity_threshold = 0.22,
                                seed = 1) {
  
  stopifnot(n >= 1)
  set.seed(seed)
  
  # Plotly default-ish dasharrays (common mapping)
  # (We treat these as "banned" and also ban anything too close.)
  defaults <- list(
    dash        = c(5,5),
    dot         = c(1,5),
    longdash    = c(10,5),
    dashdot     = c(5,5,1,5),
    longdashdot = c(10,5,1,5)
    # solid is "no dasharray" => handled by ensuring we always return a pattern
  )
  
  # Normalize a pattern so scaling doesn't fool similarity checks
  norm_pat <- function(x) {
    x <- as.numeric(x)
    x / sum(x)
  }
  
  # Compare patterns with different lengths by repeating the shorter to match LCM length
  expand_to <- function(x, L) rep(x, length.out = L)
  
  pat_dist <- function(a, b) {
    a <- norm_pat(a); b <- norm_pat(b)
    L <- (length(a) * length(b)) / gcd(length(a), length(b))
    aa <- expand_to(a, L); bb <- expand_to(b, L)
    sqrt(mean((aa - bb)^2))
  }
  
  gcd <- function(a,b) if (b == 0) a else Recall(b, a %% b)
  
  too_close_to_defaults <- function(p) {
    # also reject very "regular" patterns that visually resemble simple dash/dot
    # (e.g., on==off repeating)
    if (length(p) %in% c(2,4,6)) {
      if (sd(p) < 0.6) return(TRUE)  # too uniform
    }
    
    # reject closeness to known defaults
    for (d in defaults) {
      if (pat_dist(p, d) < similarity_threshold) return(TRUE)
    }
    FALSE
  }
  
  # Build candidate patterns (4 or 6 segments: on,off,on,off,...)
  make_candidates <- function(L) {
    # L is number of segments; half "on", half "off"
    half <- L / 2
    ons  <- replicate(half, sample(px_on,  1))
    offs <- replicate(half, sample(px_off, 1))
    as.vector(rbind(ons, offs))
  }
  
  candidates <- character(0)
  seen <- new.env(parent = emptyenv())
  
  # Generate until we have enough (cap attempts to avoid infinite loops)
  attempts <- 0
  max_attempts <- 50000
  
  while (length(candidates) < n && attempts < max_attempts) {
    attempts <- attempts + 1
    L <- sample(allow_lengths, 1)
    p <- make_candidates(L)
    
    # Avoid patterns that are basically "solid": very long on, tiny off everywhere
    if (sum(p[seq(2, length(p), by = 2)]) <= 2) next
    
    if (too_close_to_defaults(p)) next
    
    key <- paste(p, collapse = ",")
    if (exists(key, envir = seen, inherits = FALSE)) next
    assign(key, TRUE, envir = seen)
    
    candidates <- c(candidates, paste0(p, "px", collapse = ","))
  }
  
  if (length(candidates) < n) {
    warning("Could not generate enough distinct dash patterns under current constraints; returning what I found.")
  }
  
  candidates[seq_len(min(n, length(candidates)))]
}


roboplotr_generate_dashtypes <- function(n, dashtypes) {
  if(n <= length(dashtypes)) return(dashtypes)
  
  c(dashtypes, roboplotr_dashtypes(n-length(dashtypes)))
}
