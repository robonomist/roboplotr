#' @importFrom plotly layout
#' @importFrom dplyr case_when
#' @importFrom plotly rangeslider
roboplotr_rangeslider <- function(p, enable_rangeslider, height = 0.1) {

  `enable_rangeslider$rangeslider` <- enable_rangeslider$rangeslider
  roboplotr_typecheck(`enable_rangeslider$rangeslider`, c("date","Date","logical","character"))
  if(is.logical(enable_rangeslider$rangeslider)) {
    enable <- enable_rangeslider$rangeslider
    slider_range <- NULL
  } else {
    `enable_rangeslider$max` <- enable_rangeslider$max
    roboplotr_typecheck(`enable_rangeslider$max`, c("date","Date","character","numeric"))
    slider_range <- map(enable_rangeslider, ~ as_date(.x) |> as.character()) |> unname()
    enable <- T
  }

  if(enable == T) {
    height <- case_when(height > 0.5 ~ 0.5, height < 0.1 ~ 0.1, TRUE ~ height)
    p |> rangeslider(start = slider_range[[1]], end = slider_range[[2]], thickness = height) |>
      onRender(
        jsCode = "function(gd, params, data) {
          yrangeRelayout({'xaxis.range': [{}]}, gd, 0, false)

          }")
  } else { p }
}


roboplotr_add_shapes <- function(p, zeroline, shadearea) {
  zeroline <- roboplotr_zeroline(zeroline)
  if(is.null(zeroline)) {
    p <- p |> layout(yaxis = list(zeroline = F))
  }
  the_shapes <- list(shadearea, zeroline) |> roboplotr_compact()
  range_relayout <- "shadearea" %in% map(the_shapes, ~ .x$shapeId) |> unlist()
  # remove default zeroline in all cases
  if(length(the_shapes) > 0) {
    p |>
      layout(
        shapes = unname(the_shapes)
      ) |>
      onRender(
        jsCode = "function(gd, params, data) {
          editShapes(gd, data.zeroline)
          }", data =
          list(zeroline = zeroline$y0))

  } else {
    p
  }
}

#' @importFrom plotly layout
roboplotr_zeroline <- function(z) {
  zeroline <- z$zeroline

  if (z$zeroline$position == F & !is.numeric(z$zeroline$position)) {
    NULL
  } else {
    zero_line <- ifelse(is.logical(z$zeroline$position) & z$zeroline$position == T, 0, z$zeroline$position)
    list(
      type = "line",
      x0 = z$xrange$min,
      x1 = z$xrange$max,
      xref = "x",
      y0 = zero_line,
      y1 = zero_line,
      yref = "y",
      layer = "between",
      line = list(
        color = z$zeroline$color,
        width = z$zeroline$width
      ),
      shapeId = "zeroline"
    )
  }
}

#' @importFrom purrr map_lgl
roboplotr_shadearea <- function(d, shadearea = set_shadearea()) {
  if (all(map_lgl(shadearea[c("ymin","ymax","xmin","xmax")], is.null))) {
    NULL
  } else {
    check_shadearea <- function(param, param2, .fun = min) {
      if (is.null(param)) {
        .fun(param2, na.rm = T)
        # } else if (class(param) != class(param2)) {
        #   stop("Shadearea y or x values don't match the plot axes.")
      }
      else {
        param
      }
    }

    list(
      type = "rect",
      y0 = check_shadearea(shadearea$ymin, d$value),
      y1 = check_shadearea(shadearea$ymax, d$value, max),
      x0 = check_shadearea(shadearea$xmin, d$time),
      xnull = ifelse(is.null(shadearea$xmax), TRUE, FALSE),
      x1 = check_shadearea(shadearea$xmax, d$time, max),
      line = list(color = shadearea$border),
      fillcolor = shadearea$color,
      opacity = shadearea$opacity,
      shapeId = "shadearea",
      xref = "x",
      yref = "y",
      layer = shadearea$layer
    )
  }

}

#' Shade configuration
#'
#' Parameters to add and customize shadeareas in [roboplots][roboplot()].
#'
#' @param xmin,xmax Currently not validated. These need to be of the same type as
#' the respective axis for the shade area to work.
#' @param border,color Characters. The color for the shade area. Must be a
#' hexadecimal color or a valid css color.
#' @param opacity Numeric. Controls the opacity of the shade area. Use a value
#' between 0 and 1.
#' @param layer Character. Whether the shade area is above or below the plot
#' traces. Use "above" or "below".
#' @examples
#' # You can use` shadearea` when the x-axis is numeric or date to highlight areas
#' # of the plot.
#' d <- energiantuonti |>
#'   dplyr::filter(Alue == "Venäjä")
#' d |> roboplot(Suunta, shadearea = set_shadearea(xmin = "2018-01-01"))
#'
#' # Use the other parameters to fine-tune the appearance of the `shadearea`.
#' d |> roboplot(
#'   Suunta,
#'   shadearea = set_shadearea(
#'     xmin = "2018-01-01",
#'     color = "green",
#'     border = "black",
#'     opacity = 0.8,
#'     layer = "below"
#'   )
#' )
#'
#' @export
#' @returns A list of class roboplotr.set_shadearea
set_shadearea <-
  function(xmin = NULL,
           xmax = NULL,
           border = getOption("roboplot.colors.traces")[1],
           color = getOption("roboplot.colors.traces")[1],
           opacity = 0.2,
           layer = "between") {
    roboplotr_typecheck(opacity, "numeric", allow_null = F)
    if (!all(opacity > 0, opacity <= 1)) {
      stop("set_shadearea() `opacity` must be between 0 and 1!", call. = F)
    }
    roboplotr_valid_colors(c(color, border), "Shade area color and border")
    roboplotr_typecheck(layer, "character", allow_null = F)
    roboplotr_valid_strings(layer, c("below","above","between"), any, msg = "set_shadearea() `layer`")

    .res <- list(
      xmin = xmin,
      xmax = xmax,
      ymin = NULL,
      ymax = NULL,
      border = border,
      color = color,
      opacity = opacity,
      layer = layer
    )

    .res <- structure(.res, class = c("roboplotr", "roboplotr.set_shadearea", class(.res)))

    .res

  }


#' Zeroline configuration
#'
#' Parameters to customize zeroline in [roboplots][roboplot()].
#'
#' @param position Numeric or logical. The position of the zeroline. If TRUE, the line is drawn at 0.
#' @param color Character. Must be a hexadecimal color or a valid css color.
#' @param width Numeric.
#' @examples
#' set_roboplot_options(zeroline = set_zeroline(color = "aliceblue", width = 4))
#' energiantuonti |> dplyr::filter(Alue == "Venäjä") |> roboplot(Suunta, zeroline = 200)
#' @export
#' @returns A list of class roboplotr.set_zeroline
set_zeroline <-
  function(position = NULL,
           color = getOption("roboplot.zeroline")$color,
           width  = getOption("roboplot.zeroline")$width) {
    roboplotr_typecheck(width, "numeric", allow_null = F)
    roboplotr_typecheck(color, "character", allow_null = F)
    roboplotr_valid_colors(color, "set_zeroline() color")

    .res <- list(position = position, color = color, width = width)

    .res <- structure(.res, class = c("roboplotr","roboplotr.set_zeroline", class(.res)))

    .res

  }
