#' Error bar configuration
#'
#' Parameters to add and customize error bars in [roboplots][roboplot()].
#'
#' @param ... Parameters passed to `set_confidence_interval()`.
#' @export
#' @returns A list of class roboplot.set_confidence_interval
set_errorbars <- function (...) {
  .Deprecated("set_confidence_interval", package = "roboplotr")
  
  do.call(set_confidence_interval, list2(...))
  
}

#' Configure Confidence Display
#'
#' Adds and customizes error bars or shaded areas in [roboplot()]s.
#' Use this function to visually represent uncertainty in the data by adding error bars or shaded areas, based on the specified type.
#'
#' @param type Character. Specifies the type of confidence display: "bars" for error bars or "area" for shaded areas.
#' @param confidence Symbol or character. Columns from `d` in `roboplot()` that provide the labels (if character or factor) or values (if numeric) for error areas and bars.
#' @param bar_color Characters. Colors for error bars. Must be hexadecimal colors or valid CSS colors. Only applicable when `type = "bars"`.
#' @param opacity Numeric. Opacity of the shaded area, ranging from 0 (completely transparent) to 1 (completely opaque). Only applicable when `type = "area"`.
#' @param smoothing Numeric. Smoothing factor for the shaded area, typically between 0 and 1. Only applicable when `type = "area"`.
#' @param start_type Currently inactive. Character. Defines how the shaded area starts when `type = "area"`. Use "flat" or "point". Default is "flat".
#' @param stop_type Currently inactive. Character. Defines how the shaded area ends when `type = "area"`. Use "flat" or "point". Default is "flat".
#' @param show_legend Logical. Whether to show the legend item for the shaded area. Default is TRUE.
#' @export
#' @details 
#' `set_confidence_interval()` replaces the deprecated `set_errorbars()` function. Use this function to add either error bars or shaded areas to your plots by setting the `type` parameter.
#' When `type = "bars"`, you can specify `error_y`, and `ycolor` to customize the appearance of error bars.
#' When `type = "area"`, you can adjust the `opacity` and `smoothing` of the shaded area to represent the confidence range visually.
#'
#' @examples
#' # When the error is symmetrical for each time point, use `set_confidence_interval()`
#' # use `set_confidence_interval()` to add error bars to a plot where the confidence
#' # interval is a numeric column. The given value will be added to or subtracted from 
#' # the trace the confidence interval value relates to by `roboplot()` param `color`.
#' 
#' d <- energiantuonti |>
#'   dplyr::filter(Alue == "Venäjä", Suunta == "Tuonti") |>
#'   dplyr::mutate(confidence_interval = sd(value))
#' 
#' d |>
#'   roboplot(
#'     Suunta,
#'     pattern = Alue,
#'     title = "Energian tuonti",
#'     subtitle = "Virhepalkeilla",
#'     caption = "Tilastokeskus",
#'     confidence_interval = set_confidence_interval(type = "bars")
#'   )
#' # Or use `type = "area"` to add confidence area to a plot.
#' d |>
#'   roboplot(Suunta,
#'            title = "Energian tuonti",
#'            subtitle = "Luottamusalueella",
#'            caption = "Tilastokeskus",
#'            confidence_interval = set_confidence_interval("area"))
#' 
#' # Change the color of the error bars
#' d |>
#'   roboplot(
#'     Suunta,
#'     title = "Energian tuonti",
#'     subtitle = "Luottamusalueella",
#'     caption = "Tilastokeskus",
#'     confidence_interval = set_confidence_interval("bars", bar_color = "green")
#'   )
#' 
#' # Change opacity and smoothing of the confidence area, hide the area from legend, 
#' # and pass the column name explicitly if it not called "confidence_interval".
#' d |>
#'   dplyr::rename(conf = confidence_interval) |>
#'   roboplot(
#'     Suunta,
#'     title = "Energian tuonti",
#'     subtitle = "Luottamusalueella",
#'     caption = "Tilastokeskus",
#'     confidence_interval = set_confidence_interval(
#'       "area",
#'       confidence = conf,
#'       opacity = 0.1,
#'       smoothing = 0.9,
#'       show_legend = FALSE
#'     )
#'   )
#' 
#' 
#' # Create custom confidence intervals using `set_confidence_interval()`.
#' # The column used for the confidence interval should be a character or factor, 
#' # describing the labels of the confidence areas. The color of the areas correspond
#' # to the `color` parameter of `roboplot`. The main trace should be refererred 
#' # either as `NA` or the name of your y-axis column. For this example, arbitrary 
#' # confidence areas are created.
#' d <- energiantuonti  |>
#'   dplyr::filter(Alue == "Venäjä") |>
#'   dplyr::mutate(`95%hi` = purrr::map_dbl(value,~ .x + .x * runif(1) * 0.2),
#'          `95%lo` = purrr::map_dbl(value,~ .x - .x * runif(1) * 0.2),
#'          `95%` = purrr::map2_dbl(`95%hi`,`95%lo`,mean),
#'          `70%hi` = purrr::map_dbl(`95%hi`, ~ .x + .x * runif(1) * 0.3),
#'          `70%lo` = purrr::map_dbl(`95%lo`, ~ .x - .x * runif(1) * 0.3),
#'          `70%` = purrr::map2_dbl(`70%hi`,`70%lo`,mean)
#'   ) |>
#'   tidyr::pivot_longer(dplyr::where(is.numeric), names_to = "confidence_interval") |>
#'   dplyr::mutate(confidence_interval = confidence_interval |>
#'                   stringr::str_remove("(hi|lo)$") |>
#'                   forcats::fct_relevel(c("95%"))
#'   ) |>
#'   dplyr::filter(confidence_interval == "value" | time >= "2020-01-01")
#' 
#' d |>  
#'   roboplot(
#'     Suunta,
#'     "Confidence interval",
#'     caption = "Tilastokeskus",
#'     confidence_interval = set_confidence_interval("area", start_type = "point")
#'   )
#' 
#' # Start the area from the last point of the trace where you have no confidence interval values.
#' d |> 
#'   roboplot(
#'   Suunta,
#'   "Confidence interval",
#'   caption = "Tilastokeskus",
#'   confidence_interval = set_confidence_interval("area", start_type = "point")
#' )
#' # Works when you want to skip the line of the trace, only draw the area.
#' d |>
#'   dplyr::filter(Suunta == "Tuonti", confidence_interval != "value") |>
#'   roboplot(
#'   Suunta,
#'   "Confidence interval",
#'   caption = "Tilastokeskus",
#'   confidence_interval = set_confidence_interval("area")
#' )
#' 
#' # For error_bars, you can also use a character or factor column if the values
#' # won't be symmetrical for each time point. For more than one level of confidence,
#' # use `type = "area"`.
#' d |>
#'   dplyr::filter(confidence_interval %in% c("value","95%")) |>
#'   roboplot(
#'     Suunta,
#'     "Confidence interval",
#'     caption = "Tilastokeskus",
#'     pattern = Alue,
#'     confidence_interval = set_confidence_interval("bars")
#'   )
set_confidence_interval <- function(type = NULL,
                                    confidence = NULL,
                                    bar_color = NULL,
                                    opacity = NULL,
                                    smoothing = NULL,
                                    start_type = "flat",
                                    stop_type = "flat",
                                    show_legend = TRUE
) {
  
  roboplotr_typecheck(type, "character", size = 1, allow_null = T)
  roboplotr_typecheck(show_legend, "logical", size = 1, allow_null = F)
  
  if(stop_type != "flat") {
    roboplotr_alert("The `start_type` and `stop_type` parameters are currently inactive in `set_confidence_interval()`. Ignoring these parameters.")
  }
  stop_type <- "flat"
  error_x <- NULL
  xcolor <- NULL
  if (!is.null(type)) {
    roboplotr_valid_strings(type, c("area", "bars"), any, "`type` in `set_confidence_interval()`")
    error_x <- enquo(error_x)
    error_y <- enquo(confidence)
    if(quo_is_null(error_y)) {
      error_y <- sym("confidence_interval")
      error_y <- enquo(error_y)
    }
    roboplotr_typecheck(xcolor, "character", allow_null = T)
    roboplotr_typecheck(bar_color, "character", allow_null = T)
    
    if (type == "area") {
      roboplotr_typecheck(start_type, "character", allow_null = T)
      roboplotr_typecheck(smoothing, "character", allow_null = T)
      roboplotr_valid_strings(start_type, c("flat", "point"), any, "`start_type` in `set_confidence_interval()`")
      roboplotr_valid_strings(stop_type, c("flat", "point"), any, "`stop_type` in `set_confidence_interval()`")
      roboplotr_typecheck(opacity, "numeric", allow_null = T)
      roboplotr_typecheck(smoothing, "numeric", allow_null = T)
      smoothing <- smoothing %||% 0.3
      opacity <- opacity %||% 0.4
      roboplotr_is_between(smoothing, "set_confidence_interval()")
      roboplotr_is_between(opacity, "set_confidence_interval()")
    } else {
      if (any(!is.null(smoothing), !is.null(opacity))) {
        roboplotr_alert('Only `type = "area"` can have `smoothing` and `opacity` parameters in `set_confidence_interval()`. Ignoring these parameters.')
      }
      xcolor <- xcolor %||% getOption("roboplot.errorbars")$xcolor
      bar_color <- bar_color %||% getOption("roboplot.errorbars")$ycolor
      roboplotr_valid_colors(c(xcolor, bar_color), "colors in set_confidence_interval()")
    }
    .res <- list(
      error_x = error_x,
      error_y = error_y,
      xcolor = xcolor,
      ycolor = bar_color,
      type = type,
      opacity = opacity,
      smoothing = smoothing,
      start_type = start_type,
      stop_type = stop_type,
      show_legend = show_legend
    )
  } else {
    .res <- list(
      error_x = NULL,
      error_y = NULL,
      xcolor = xcolor,
      ycolor = bar_color %||% getOption("roboplot.errorbars")$ycolor,
      type = NULL,
      opacity = opacity,
      smoothing = smoothing
    )
  }
  
  .res <- structure(.res,
                    class = c("roboplotr", "roboplotr.set_confidence_interval", class(.res)))
  
  .res
}


#' @importFrom rlang as_name quo_is_null
#' @importFrom stringr str_glue
roboplotr_validate_confidence <- function(confidence, d, plot_axes) {
  
  if(!is.null(confidence$type)) {
    
    d_names <- names(d)
    if (!is.null(confidence)) {
      error_x <- confidence$error_x
      error_y <- confidence$error_y
      roboplotr_check_valid_var(error_x, d_names, "set_confidence_interval")
      roboplotr_check_valid_var(error_y, d_names, "set_confidence_interval")
      if(confidence$type == "area") {
        if(plot_axes$x != "time") {
          stop(
            str_glue(
              'To display a confidence area, the date column "time" from param `d` of `roboplot()` must be used as the x-axis of the plot! Use `set_axes(x = "time")`.'
            ),
            call. = FALSE
          )
        }
      }

    }  
  }
  
}

#' @importFrom dplyr if_else
roboplotr_cumulative_alphas <- function(alphas) {
  if (any(alphas < 0 | alphas > 1)) {
    stop("Alpha values should be between 0 and 1", call.= F)
  }
  
  cumulative_alpha <- numeric(length(alphas))
  
  for (i in seq_along(alphas)) {
    cumulative_alpha[i] <- 1 - prod(1 - alphas[i:length(alphas)])
  }
  return(cumulative_alpha)
}

#' @importFrom lubridate days
#' @importFrom purrr list_rbind map2_dbl
#' @importFrom rlang eval_tidy quo_is_symbolic
roboplotr_get_confidence_areas <- function(split_d, confidence, ticktypes) {
  
  if (confidence$type %||% "nothing" == "area") {
    
    err_areas <- imap(split_d, function(d, y) {
      
      if(unique(d$roboplot.plot.mode %in% c("line","marker","smooth"))) {
        xcol <- sym(ticktypes$x)
        ycol <- sym(ticktypes$y)
        if (!quo_is_symbolic(confidence$error_y)) {
          confidence$error_y <- sym(eval_tidy(confidence$error_y))
        }
        
        if(is.numeric(d[[as_label(confidence$error_y)]])) { #handle numeric confidence intervals
          .extender <- tibble()
          .tail <- tibble()
          d <- list(
            .tail,
            d |> mutate({{ycol}} := {{ycol}} + .data[[confidence$error_y]]),
            .extender,
            d |> mutate({{ycol}} := {{ycol}} - .data[[confidence$error_y]]) |>
              arrange(desc({{xcol}})),
            .tail
          ) |> 
            list_rbind() |>
            mutate(roboplot.plot.type = "scatter",
                   roboplot.plot.mode = "line"
                   )
          d[[as_label(confidence$error_y)]] <- as_label(confidence$error_y)
          
          attr(d, "roboplot.confidence.area") <- confidence[c("opacity","smoothing","show_legend")]
          attr(d, "roboplot.confidence.area")$col <- as_label(confidence$error_y)
          
          d
          
        } else { # handle other types of confidence intervals

          d <- d  |> mutate(!!confidence$error_y := if_else(!!confidence$error_y == as_label(ycol), NA, !!confidence$error_y))
          
          if (length(d[[xcol]][!is.na(d[[as_label(confidence$error_y)]])]) > 0) {
            first_nona <- d |> 
              filter(.data[[xcol]] < min(.data[[xcol]][!is.na(.data[[as_label(confidence$error_y)]])])) 
          } else {
            first_nona <- tibble()
          }
          if (nrow(first_nona) > 0) {
            first_nona <- first_nona |> filter({{xcol}} == max(!!xcol))
          }
          
          d <- d |> filter(!is.na(!!confidence$error_y))
          
          if(!is.factor(d[[as_label(confidence$error_y)]])) {
            d[[as_label(confidence$error_y)]] <- fct_inorder(d[[as_label(confidence$error_y)]])
          }
          
          get_opacity <- function(.this_level) {
            opacity <- confidence$opacity
            .levels <- nlevels(d[[as_label(confidence$error_y)]])
            opacity <- (.levels - (.this_level-1)) * opacity / .levels
            opacity
          }
          confidence_levels <- levels(d[[as_label(confidence$error_y)]])
          confidence_levels <- confidence_levels |> subset(confidence_levels != as_label(ycol))
          confidence_opacity <- get_opacity(as.numeric(sort(unique(d[[as_label(confidence$error_y)]])))) |> 
            setNames(confidence_levels)
          legend_opacity <- confidence_opacity |> roboplotr_cumulative_alphas() |>
            setNames(confidence_levels)
          
          conf_groups <- d |> group_by(!!confidence$error_y) |> 
            arrange(!!confidence$error_y) |>
            group_split() |> rev()
          
          d <- map(conf_groups, function(cg) {
            .extender <- tibble()
            
            if(confidence$stop_type == "point") {
              
              .lastobs <- filter(cg, {{xcol}} == max(!!xcol))
              .tailtime <- tail(cg[[xcol]],2)
              .lerr <- pull(.lastobs, !!confidence$error_y)
              .lval <- pull(.lastobs, ycol)
              .ltim <- .lastobs[[xcol]]
              .tailtime <- as.numeric(abs(difftime(.tailtime[1],.tailtime[2])))
              .fwd <- .tailtime * abs(.lerr / .lval) / 3
              .dateseq <- seq.Date(.ltim,to = .ltim + days(round(.fwd)),length.out = 6)
              .extenddays <- c(.dateseq, rev(.dateseq)[-1])
              xtend <- .lval + (cg |> filter({{xcol}} %in% tail(d[[xcol]],2)) |> pull({{ycol}}) |> diff()) * .fwd / .tailtime
              start_vals <- seq(.lval + .lerr, xtend, length.out = round(length(.extenddays) / 2))
              end_vals <- seq(xtend, .lval - .lerr, length.out = round(length(.extenddays) / 2))
              .extendvals <- c(start_vals, end_vals[-1])
              .extender <- tibble({{xcol}} := .extenddays, {{ycol}} := .extendvals)
              .extender <- .extender |> pmap(function(...) {
                .this <- list(...)
                .lastobs |> mutate({{xcol}} := .this[[xcol]], {{ycol}} := .this[[ycol]])
              }
              ) |> list_rbind() |>
                filter({{xcol}} >= .ltim) 
            }
            
            .tail <- tibble()
            if(confidence$start_type == "point" & nrow(first_nona) > 0) {
              .tail <- first_nona |> mutate(confidence_interval = unique(cg[[as_label(confidence$error_y)]]))
            }
            
            cg <- list(
              .tail,
              cg |> group_by({{xcol}}) |> filter({{ycol}} == max({{ycol}})) |> ungroup(),
              .extender,
              cg |> group_by({{xcol}}) |> filter({{ycol}} == min({{ycol}})) |>
                arrange(desc({{xcol}})) |> ungroup(),
              .tail
            ) |> 
              list_rbind() |>
              mutate(roboplot.plot.type = "scatter",
                     roboplot.plot.mode = "line")  
            
            
            attr(cg, "roboplot.confidence.area") <- confidence[c("smoothing","show_legend")]
            attr(cg, "roboplot.confidence.area")$opacity <- confidence_opacity[[as.character(unique(cg[[as_label(confidence$error_y)]]))]]
            attr(cg, "roboplot.confidence.area")$legend_opacity <- legend_opacity[[as.character(unique(cg[[as_label(confidence$error_y)]]))]]
            attr(cg, "roboplot.confidence.area")$col <- as_label(confidence$error_y)
            
            cg
            
          })
          
          d 
        }
      }
    }) |> roboplotr_compact()
    
    
    get_final_areas <- function(err_areas) {
      final_areas <- list()
      
      for (area in err_areas) {
        if (is.data.frame(area)) {
            final_areas <- append(final_areas, list(area), after = 0)} 
        else {
            final_areas <- append(final_areas, area, after = 0)
          }
      }
      
      final_areas
    }

    err_areas <- get_final_areas(err_areas)
    split_d <- split_d |> 
      map(~ {
        if(is.numeric(.x[[as_label(confidence$error_y)]])) {
          .x
        } else {
          .res <- .x |> 
            mutate(!!confidence$error_y := if_else(!!confidence$error_y == ticktypes$y, NA, !!confidence$error_y)) |> 
            filter(is.na(!!confidence$error_y))
          if(nrow(.res) > 0) {
            .res
          } 
        }
      }) |>
      roboplotr_compact() |>
      append(err_areas,after = 0)
    
  } else if (confidence$type %||% "nothing" == "bars") {
    if (!quo_is_symbolic(confidence$error_y)) {
      confidence$error_y <- sym(eval_tidy(confidence$error_y))
    }
    pivot_bar_data <- function(split_d) {
      bar_data <- list()
      for(d in split_d) {
        if(is.numeric(d[[as_label(confidence$error_y)]])) {
          d <- d |> 
            mutate(roboplot.errorbar.min = d[[as_label(confidence$error_y)]],
                   roboplot.errorbar.max = d[[as_label(confidence$error_y)]]) |> 
            select(-as_label(confidence$error_y))
          bar_data <- append(bar_data, list(d))
        } else {

          d <- d  |> mutate(!!confidence$error_y := if_else(!!confidence$error_y == ticktypes$y, NA, !!confidence$error_y))
          if(!is.factor(d[[as_label(confidence$error_y)]])) {
            d[[as_label(confidence$error_y)]] <- fct_inorder(d[[as_label(confidence$error_y)]])
          }
          .levels <- levels(d[[as_label(confidence$error_y)]])
          if(length(subset(.levels, .levels != ticktypes$y)) > 1) {
            stop(str_glue("Error bars can only be used with a single level of confidence intervals not including \"{ticktypes$y}\", or you can pass a numeric column."), call. = FALSE)
          }
          .errorbar_data <- 
            d |> 
            filter(!is.na(.data[[as_label(confidence$error_y)]])) |>
            group_by(.data[[ticktypes$x]]) |>
            summarize(roboplot.errorbar.min = min(.data[[ticktypes$y]]), 
                      roboplot.errorbar.max = max(.data[[ticktypes$y]]))
          d <- d |> 
            filter(is.na(.data[[as_label(confidence$error_y)]])) |>
            select(-as_label(confidence$error_y)) |>
            left_join(.errorbar_data, by = ticktypes$x)
          
          d <- d |> mutate(roboplot.errorbar.min = abs(map2_dbl(.data$roboplot.errorbar.min, .data[[ticktypes$y]], ~ diff(c(.x, .y)))),
                           roboplot.errorbar.max = abs(map2_dbl(.data$roboplot.errorbar.max, .data[[ticktypes$y]], ~ diff(c(.x, .y)))))
          bar_data <- append(bar_data, list(d))
        }
      } 
      bar_data
    }
    
    split_d <- pivot_bar_data(split_d)

  }
  
  
  split_d
}

