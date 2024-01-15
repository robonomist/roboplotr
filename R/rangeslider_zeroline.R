#' @importFrom plotly layout
#' @importFrom dplyr case_when
#' @importFrom plotly rangeslider
roboplotr_rangeslider <- function(p, enable_rangeslider, height = 0.1) {

  `enable_rangeslider$rangeslider` <- enable_rangeslider$rangeslider
  roboplotr_check_param(`enable_rangeslider$rangeslider`, c("date","Date","logical","character"))

  if(is.logical(enable_rangeslider$rangeslider)) {
    enable <- enable_rangeslider$rangeslider
    slider_range <- NULL
  } else {
    `enable_rangeslider$max` <- enable_rangeslider$max
    roboplotr_check_param(`enable_rangeslider$max`, c("date","Date","character","numeric"))
    slider_range <- map(enable_rangeslider, ~ as_date(.x) |> as.character()) |> unname()
    enable <- T
  }

  if(enable == T) {
    height <- case_when(height > 0.5 ~ 0.5, height < 0.1 ~ 0.1, TRUE ~ height)
    p |> rangeslider(start = slider_range[[1]], end = slider_range[[2]], thickness = height)
  } else { p }
}


roboplotr_add_shapes <- function(p, zeroline, shadearea) {
  zeroline <- roboplotr_zeroline(zeroline)
  the_shapes <- list(shadearea, zeroline) |> roboplotr_compact()
  if(length(the_shapes) > 0) {

  }
  p |>
    layout(
      shapes = unname(the_shapes)
    ) |>
    onRender(
      jsCode = "function(gd, params, data) {
          editShapes(gd, data.zeroline)
          }", data =
        list(zeroline = zeroline$y0))
}

#' @importFrom plotly layout
roboplotr_zeroline <- function(z) {
  zeroline <- z$zeroline
  roboplotr_check_param(zeroline, c("logical", "numeric"))

  if (z$zeroline == F & !is.numeric(z$zeroline)) {
    NULL
  } else {
    zero_line <- ifelse(z$zeroline == T, 0, z$zeroline)
    list(
      type = "line",
      x0 = z$xrange$min,
      x1 = z$xrange$max,
      xref = "x",
      y0 = zero_line,
      y1 = zero_line,
      yref = "y",
      layer = "below",
      line = list(
        color = getOption("roboplot.zeroline")$color,
        width = getOption("roboplot.zeroline")$width
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

#' Set up a shade area to highlight a part of [roboplot()] background
#' @param xmin,xmax Currently not validated. These need
#' to be of the same type as the respective axis for the shade area to work. If
#' none are provided, no shade area will be displayed.
#' @param border,color Characters. The color for the shade area. Must be a
#' hexadecimal color strings or a valid css color strings.
#' @param opacity Numeric. Controls the opacity of the shade area. Use a value
#' that is between > 0 and 1.
#' @param layer Character. Whether the shade area is above or below the plot
#' traces. Use "above" or "below".
#' @examples
#' # You can use shadearea when the x-axis is numeric or date, to highlight areas of
#' # the plot.
#' d <- energiantuonti |>
#'   dplyr::filter(Alue == "Venäjä")
#' d |> roboplot(Suunta, shadearea = set_shadearea(xmin = "2018-01-01"))
#' # Use the other parameters to fine-tune the appearance of the shade area
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
set_shadearea <-
  function(xmin = NULL,
           xmax = NULL,
           # ymin = NULL,
           # ymax = NULL,
           border = getOption("roboplot.colors.traces")[1],
           color = getOption("roboplot.colors.traces")[1],
           opacity = 0.2,
           layer = "above") {
    roboplotr_check_param(opacity, "numeric")
    if (!all(opacity > 0, opacity <= 1)) {
      stop("Shade area opacity must be between 0 and 1!", call. = F)
    }
    roboplotr_valid_colors(c(color, border), "Shade area color and border")
    roboplotr_check_param(layer, "character")
    roboplotr_valid_strings(layer, c("below","above"), any, msg = "shadearea 'layer'")
    list(
      xmin = xmin,
      xmax = xmax,
      ymin = NULL,#ymin,
      ymax = NULL,#ymax,
      border = border,
      color = color,
      opacity = opacity,
      layer = layer
    )
  }


#' Set global defaults for [roboplot()] zerolines
#' Only relevant when a zeroline is set with [roboplot()]
#' @param color Character. Zeroline color. Must be a hexadecimal color strings or a valid css color strings.
#' @param width Numeric. Zeroline width.
#' @examples
#' set_roboplot_options(zeroline = set_zeroline(color = "aliceblue", width = 4))
#' energiantuonti |> dplyr::filter(Alue == "Venäjä") |> roboplot(Suunta, zeroline = 200)
#' @export
set_zeroline <-
  function(position,
           color = getOption("roboplot.zeroline")$color,
           width  = getOption("roboplot.zeroline")$width) {
    roboplotr_check_param(width, "numeric")
    roboplotr_valid_colors(color, "Zeroline color")
    list(color = color, width = width)
  }
