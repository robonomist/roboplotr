#' @importFrom plotly layout
roboplotr_grid <- function(p, grid = getOption("roboplot.grid"), border = getOption("roboplot.border")) {
  bg <- getOption("roboplot.colors.background")
  p |> layout(
    xaxis = list(
      automargin = FALSE,
      showgrid = TRUE,
      gridcolor = grid$xcolor,
      gridwidth = grid$xwidth,
      griddash = grid$xdash,
      linecolor = border$xcolor,
      linewidth = border$xwidth,
      mirror = border$xmirror,
      showline = bg != border$xcolor),
    yaxis = list(
      automargin = TRUE,
      showgrid = TRUE,
      gridcolor = grid$ycolor,
      gridwidth = grid$ywidth,
      griddash = grid$ydash,
      linecolor = border$ycolor,
      linewidth = border$ywidth,
      mirror = border$ymirror,
      showline = bg != border$ycolor)
  )
}

#' Plot border configuration
#'
#' Global parameters to add and customize plot borders of [roboplots][roboplot()].
#'
#' @param xcolor,ycolor Characters. Plot border colors. Must be hexadecimal colors
#' or valid css color.
#' @param xmirror,ymirror Logicals. Will the given axis be mirrored to left to right
#' or bottom to top.
#' @param xwidth,ywidth Numerics. Border width.
#' @examples
#' # You can global control x- and y-axis border mirroring, width and color by using
#' # `set_border()` inside `set_roboplot_options()`.
#'
#' set_roboplot_options(
#'   border = set_border(ycolor = "green", xmirror = FALSE, xwidth = 5)
#'   )
#'
#' energiantuonti |> dplyr::filter(Alue == "Venäjä") |> roboplot(Suunta)
#'
#' # Reset to defaults
#' set_roboplot_options(reset = TRUE)
#'
#' @returns A list of class roboplotr.set_border.
#' @export
set_border <- function(

  xcolor = getOption("roboplot.border")$xcolor,
  ycolor = getOption("roboplot.border")$ycolor,
  xmirror = getOption("roboplot.border")$xmirror,
  ymirror = getOption("roboplot.border")$ymirror,
  xwidth = getOption("roboplot.border")$xwidth,
  ywidth = getOption("roboplot.border")$ywidth) {

  roboplotr_typecheck(xcolor, "character", allow_null = F)
  roboplotr_typecheck(ycolor, "character", allow_null = F)
  roboplotr_valid_colors(c(xcolor,ycolor), message = "Colors in set_border()")
  roboplotr_typecheck(xmirror, "logical", allow_null = F)
  roboplotr_typecheck(ymirror, "logical", allow_null = F)
  roboplotr_typecheck(xwidth, "numeric", allow_null = F)
  roboplotr_typecheck(ywidth, "numeric", allow_null = F)

  .res <- list(xcolor = xcolor, ycolor = ycolor, xmirror = xmirror, ymirror = ymirror, xwidth = xwidth, ywidth = ywidth)

  .res <- structure(.res, class = c("roboplotr","roboplotr.set_border", class(.res)))

  .res

}

#' Grid configuration.
#'
#' Parameters to customize grids in [roboplots][roboplot()].
#'
#' @param xcolor,ycolor,xtick,ytick Characters. Plot gridline colors. Must be hexadecimal
#' colors or a valid css colors.
#' @param xwidth,ywidth Numerics. Plot gridline widths.
#' @param xdash,ydash Characters. Plot gridline linetypes. Must contain one of
#' "solid", "dash", "dot", "longdash", "dashdot", and "longdashdot" in any order.
#' @examples
#' # You can control x- and y-grid color, width and linetype by using
#' # `set_grid()` inside `set_roboplot_options()`.
#'
#' set_roboplot_options(
#'   grid = set_grid(ycolor = "green", ydash = "longdash", xwidth = 3),
#'   )
#'
#' energiantuonti |> dplyr::filter(Alue == "Venäjä") |> roboplot(Suunta)
#'
#' # Reset to defaults
#' set_roboplot_options(reset = TRUE)
#'
#' @returns A list of class roboplot.set_grid
#' @export
set_grid <- function(

  xcolor = getOption("roboplot.grid")$xcolor,
  ycolor = getOption("roboplot.grid")$ycolor,
  xwidth = getOption("roboplot.grid")$xwidth,
  ywidth = getOption("roboplot.grid")$ywidth,
  xdash = getOption("roboplot.grid")$xdash,
  ydash = getOption("roboplot.grid")$ydash,
  xtick = NULL,
  ytick = NULL
  ) {
  roboplotr_typecheck(xcolor, "character", allow_null = F)
  roboplotr_typecheck(ycolor, "character", allow_null = F)
  roboplotr_typecheck(xcolor, "character", allow_null = F)
  roboplotr_typecheck(ycolor, "character", allow_null = F)
  roboplotr_typecheck(xtick, "character")
  if(is.null(xtick)) { xtick <- xcolor}
  roboplotr_typecheck(ytick, "character")
  if(is.null(ytick)) { ytick <- ycolor}
  roboplotr_valid_colors(c(xtick,ytick,xcolor,ycolor), message = "Colors in set_grid()")
  roboplotr_typecheck(xwidth, "numeric", allow_null = F)
  roboplotr_typecheck(ywidth, "numeric", allow_null = F)
  roboplotr_typecheck(xdash, "character", allow_null = F)
  roboplotr_typecheck(ydash, "character", allow_null = F)
  roboplotr_valid_strings(c(xdash,ydash),c("solid", "dash", "dot", "longdash", "dashdot", "longdashdot"),any, msg = "Dash types in set_grid()")

  .res <- list(xcolor = xcolor, ycolor = ycolor, xwidth = xwidth, ywidth = ywidth, xdash = xdash, ydash = ydash, xtick = xtick, ytick = ytick)

  .res <- structure(.res, class = c("roboplotr","roboplotr.set_grid", class(.res)))

  .res
}

#' @importFrom plotly layout
roboplotr_set_background <- function(p, color = getOption("roboplot.colors.background")) {
  p |> layout(paper_bgcolor = color, plot_bgcolor = color)
}

#' Zooming configuration
#'
#' Customize zooming behavior of [roboplots][roboplot()].
#'
#' @param zoom Characters. Controls how [roboplots][roboplot()] are zoomed in.
#' Valid strings are "none", "scroll" (scrollwheel and two-finger drag) and "drag"
#' (mouse or finger drag). Currently not used in [robomaps][robomap()]. Default is "scroll".
#' @param ... Placeholder for future arguments.
#' @export
#' @examples
#'
#'
#' # Zoom by dragging with mouse.
#' energiantuonti |>
#'   dplyr::filter(Alue == "Venäjä") |>
#'   roboplot(Suunta, zoom = "drag")
#'
#' # Better for mobile to scroll with two fingers, or mobile on desktop.
#' energiantuonti |>
#'   dplyr::filter(Alue == "Venäjä") |>
#'   roboplot(Suunta, zoom = "scroll")
#'
#' # Note that modebar zooming is not affected by this setting.
#' energiantuonti |>
#'   dplyr::filter(Alue == "Venäjä") |>
#'   roboplot(Suunta, zoom = "none",
#'            modebar = set_modebar(buttons = c("zoomin","zoomout","pan")))
#'
set_zoom <- function(zoom = getOption("roboplot.scroll"), ...) {
  roboplotr_typecheck(zoom, "character", allow_null = FALSE)
  roboplotr_valid_strings(zoom, c("none","drag","scroll"), any)

  .res <- structure(zoom, class = c("roboplotr","roboplotr.set_zoom", class(zoom)))

  .res
}

#' @importFrom plotly layout
#' @importFrom stringr str_remove
roboplotr_set_margin <-function(p, margin, zoom = "scroll") {

  if (!is.list(margin)) {
    stop("Insert plot margins as list (default is list(t,r,b,l,pad))\nNote that top and bottom margins will be currently ignored but programmatically set instead.", call. = F)
  } else if (any(!names(margin) %in% c("t","r","b","l","pad")) | any(!is.double(unlist(margin)))) {
    stop("All plot margins must be of double type, and named one or more of t, r, b, l or pad.\nNote that top and bottom margins will be currently ignored but programmatically set instead.", call. = F)
  } else {
    p |>
      layout(
        margin = margin,
        autosize = T,
        dragmode = ifelse(zoom == "drag", T, F)
      )
    # p
  }
}
