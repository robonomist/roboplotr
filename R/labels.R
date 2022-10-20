#' @importFrom farver decode_colour encode_colour
#' @importFrom plotly layout
#' @importFrom stringr str_extract
roboplot_set_legend <- function(p, legend_position, orientation, legend_order) {

  if(!is.null(orientation)) { message("legend_orientation is currently ignored.") }

  roboplot_check_param(legend_position, "character", allow_na = T)
  if(is.null(legend_position)) { legend_position <- "bottom" }

  if (is.na(legend_position)) {
    p |> layout(showlegend = F)
  } else if (!legend_position %in% c("bottom")) {
    stop("legend_position must be \"bottom\", or NA for no legend!", call. = F)
  } else {
    x.pos <- ifelse(legend_position == "right", 100, 0)
    y.pos <- ifelse(legend_position == "right", 1, 0) #-0.05
    # orientation <- "h"#case_when(orientation == "auto" ~ ifelse(legend_position == "right", "v","h"), TRUE ~ str_extract(orientation, "^(v|h)"))
    p |> layout(
      showlegend = T,
      legend = list(font = getOption("roboplot.font.main"),
                    bgcolor = getOption("roboplot.colors.background") |> decode_colour() |> encode_colour(alpha = 0.7),
                    x = x.pos, y = y.pos,
                    orientation = "h",
                    xanchor = "left",
                    yanchor = "bottom",
                    traceorder = legend_order
                    ))
  }
}


#' @importFrom plotly layout
roboplot_caption <- function(p, caption) {

  roboplot_check_param(caption, type = "character", allow_null = T)

  if(is.null(caption)) {
    p
  } else {
    p |>
      layout(
        annotations = list(
          x = 0, text = caption, align = "left",
          showarrow = F, xref = 'paper', yref = "paper",
          xanchor='left', yanchor = 'bottom', xshift=0, yshift=0,
          font = getOption("roboplot.font.caption")
        )
      )
  }
}

#' @importFrom plotly layout
#' @importFrom htmltools tag HTML
#' @importFrom stringr str_c
roboplot_set_title <- function(p, title, subtitle) {

  roboplot_check_param(title, type = "character", allow_null = F)
  roboplot_check_param(subtitle, type = "character", allow_null = F)

  p |>
      layout(
        title = list(
          text = str_c(
            "<span>",title,
            as.character(tags$span(HTML(str_c("<br>",subtitle)), style = "font-size: 75%")),"</span>"
          ),
          font = getOption("roboplot.font.title"),
          xanchor = "left",
          yachor = "bottom",
          y = 0.925,
          x = 0,
          xref = "paper")
      )
}

#' Outputs a string used for roboplot captions.
#'
#' @inheritParams roboplot
#' @param text Character. The text for plot caption, probably data source name for argument 'd'.
#' @param updated Date or logical. If Date, will be inserted as the last update time in the caption. If TRUE, will try to get this information from argument 'd'. Default from roboplot.options.
#' @param line.end Character. Character inserted to end of line. Default from roboplot.options.
#' @param append,prepend Character vectors. Characters inserted after or before other caption text. Every item in vector will get a new line.
#' @return A string.
#' @importFrom lubridate as_date is.Date
#' @importFrom stringr str_c
#' @export
roboplot_set_caption <- function(text = NULL, prefix = getOption("roboplot.caption")$prefix, updated = getOption("roboplot.caption")$updated, .data = NULL, line.end = getOption("roboplot.caption")$lineend, prepend = NULL,append = NULL) {

  roboplot_check_param(prefix, type = "character")
  roboplot_check_param(.data, type = "data.frame", var.length = NULL, allow_null = T)
  roboplot_check_param(text, type = "character", allow_null = F)
  roboplot_check_param(updated, type = c("logical","Date"))
  roboplot_check_param(line.end, type = c("character"))
  roboplot_check_param(append, type = "character", var.length = NULL)
  roboplot_check_param(prepend, type = "character", var.length = NULL)

  if(!is.null(updated)) {
    if(updated == T & !is.null(.data)) {
      upd <- attributes(d)$`last-updated`
      if(!is.null(upd)) {
        updated <- upd %>% reduce(c) %>% reduce(c) %>% as_date() %>% format("%-d.%-m.%Y")
      } else {
        updated <- NULL
      }
    } else if (updated == F) {
        updated <- NULL
        }
  }

  text <- str_c(text,line.end)
  if (is.character(updated)) {
    updated <- str_c("\nPäivitetty: ",updated,line.end)
  }
  if (is.character(prefix)) {
    text <- str_c(prefix,": ",text)
  }

  if (is.character(append)) {
    append <- str_c("\n",str_c(append, collapse = str_c(line.end,"\n")),line.end)
  }
  if (is.character(prepend)) {
    prepend <- str_c(str_c(prepend, collapse = str_c(line.end,"\n")),line.end,"\n")
  }
  str_c(prepend, text, updated, append)
}

#' @importFrom plotly layout
#' @importFrom stats na.omit
roboplot_set_axis_labels <- function(p, label_x = NA, label_y = NA) {
  if (any(!is.na(c(label_x, label_y)))||any(is.null(c(label_x, label_y)))) {
    if(!any(is.character(na.omit(c(label_x, label_y))))) {
      stop("Axis labels must be strings or NA.", call. = F)
    }
  }
  p |>
    layout(
      xaxis = list(title = label_x),
      yaxis = list(title = label_y)
    )
}


roboplot_highlight_legend <- function(highlight, df) {

  if(is.null(highlight)) {
    T
  } else if (is.double(highlight)) {
    max(df$value, na.rm = T) >= highlight
  } else if (is.list(highlight)) {
    if (!all(c("value",".fun") %in% names(highlight))) {
      stop("Highlight must be NA, double, or a list with \"value\" and \".fun\"!\n Value limits what is shown in legend and given a color, .fun is the function used (default is max).")
    } else {
      highlight$.fun(df$value, na.rm = T) >= highlight$value
    }
  } else {
    stop("Highlight must be NA, double, or a list with \"value\" and \".fun\"!\n Value limits what is shown in legend and given a color, .fun is the function used (default is max).")
  }
}
