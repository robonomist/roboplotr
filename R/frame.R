#' @importFrom plotly layout
roboplotr_set_grid <- function(p, grid_color = getOption("roboplot.colors.grid"), border_color = getOption("roboplot.colors.border")) {
  p |> layout(
    xaxis = list(showgrid = TRUE, gridcolor = grid_color$x, linecolor = border_color$x, size = 1.5),
    yaxis = list(showgrid = TRUE, gridcolor = grid_color$y, linecolor = border_color$y, size = 1.5)
  )
}

#' @importFrom plotly layout
roboplotr_set_background <- function(p, color = getOption("roboplot.colors.background")) {
  p |> layout(paper_bgcolor = color, plot_bgcolor = color)
}

#' @importFrom plotly layout
#' @importFrom stringr str_remove
roboplotr_set_margin <-function(p, margin) {
  
  if (!is.list(margin)) {
    stop("Insert plot margins as list (default is list(t,r,b,l,pad))\nNote that top and bottom margins will be currently ignored but programmatically set instead.", call. = F)
  } else if (any(!names(margin) %in% c("t","r","b","l","pad")) | any(!is.double(unlist(margin)))) {
    stop("All plot margins must be of double type, and named one or more of t, r, b, l or pad.\nNote that top and bottom margins will be currently ignored but programmatically set instead.", call. = F)
  } else {
    drag_mode <- getOption("roboplot.modebar.buttons")
    drag_mode <- if (any(c("zoom","zoomin2d","pan") %in% drag_mode)) { str_remove(drag_mode[drag_mode %in% c("zoom","zoomin2d","pan")][1],"in2d") } else { F }
    p |>
      layout(
        margin = margin,
        autosize = T,
        dragmode = drag_mode
      )
    # p
  }
}
