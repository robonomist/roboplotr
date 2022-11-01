#' @importFrom farver decode_colour encode_colour
#' @importFrom plotly layout
#' @importFrom stringr str_extract
roboplotr_legend <- function(p, legend_position, orientation, legend_order) {

  if(!is.null(orientation)) { roboplotr_message("The argument 'legend_orientation' is currently ignored.") }

  roboplotr_check_param(legend_position, "character", allow_na = T)
  if(is.null(legend_position)) { legend_position <- "bottom" }

  if (is.na(legend_position)) {
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
                    yanchor = "top",
                    traceorder = legend_order
      ))
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
                    yanchor = "top",
                    traceorder = legend_order
                    ))
  }
}


#' @importFrom plotly layout
roboplotr_caption <- function(p, caption) {

  roboplotr_check_param(caption, type = "character", allow_null = T)

  if(is.null(caption)) {
    p
  } else {
    p |>
      layout(
        annotations = list(
          x = 0, text = caption, align = "left",
          showarrow = F, xref = 'paper', yref = "paper",
          xanchor='left', yanchor = 'bottom', xshift=0, yshift=0#,
          # font = getOption("roboplot.font.caption")
        )
      )
  }
}

#' @importFrom plotly layout
#' @importFrom htmltools tag HTML
#' @importFrom stringr str_c
roboplotr_title <- function(p, title, subtitle) {

  roboplotr_check_param(title, type = "character", allow_null = F)
  roboplotr_check_param(subtitle, type = "character", allow_null = F)

  p |>
      layout(
        title = list(
          text = str_c(
            "<span>",title,
            as.character(tags$span(HTML(str_c("<br>",subtitle)), style = "font-size: 75%")),"</span>"
          ),
          font = getOption("roboplot.font.title"),
          xanchor = "left",
          yanchor = "bottom",
          # y = 0.9325,
          y = 1,
          x = 0,
          # yref = "paper",
          xref = "paper"
          )
      )
}

#' Outputs a string used for roboplot captions.
#'
#' @param text Character. The text for plot caption, probably data source name for argument 'd'.
#' @param prefix Character. Prefix inserted before the argument 'text', automatically separated by ": ".
#' @param .data Data frame. The data frame the caption attempts to read the time of the last update from.
#' @param updated Date or logical. If Date, will be inserted as the last update time in the caption. If TRUE, will try to get this information from argument '.data'.
#' @param line.end Character. Character inserted to end of line. Default from roboplot.options.
#' @param append,prepend Character vectors. Characters inserted after or before other caption text. Every item in vector will get a new line.
#' @return A string.
#' @importFrom lubridate as_date is.Date
#' @importFrom stringr str_c
#' @export
roboplot_set_caption <- function(text = NULL, prefix = getOption("roboplot.caption")$prefix, updated = getOption("roboplot.caption")$updated, .data = NULL, line.end = getOption("roboplot.caption")$lineend, prepend = NULL,append = NULL) {

  roboplotr_check_param(prefix, type = "character")
  roboplotr_check_param(.data, type = "data.frame", NULL, allow_null = T)
  roboplotr_check_param(text, type = "character", allow_null = F)
  roboplotr_check_param(updated, type = c("logical","Date"))
  roboplotr_check_param(line.end, type = c("character"))
  roboplotr_check_param(append, type = "character", NULL)
  roboplotr_check_param(prepend, type = "character", NULL)

  if(!is.null(updated)) {
    if(updated == T & !is.null(.data)) {
      upd <- attributes(.data)$`last-updated`
      if(!is.null(upd)) {
        updated <- upd |> reduce(c) |> reduce(c) |> as_date() |> format("%-d.%-m.%Y")
      } else {
        updated <- NULL
      }
    } else if (updated == F) {
        updated <- NULL
        }
  }

  text <- str_c(text,line.end)
  if (is.character(updated)) {
    updated <- str_c("\nP\uE4ivitetty: ",updated,line.end)
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
roboplotr_axis_labels <- function(p, label_x = NA, label_y = NA) {
  p |>
    layout(
      xaxis = list(title = label_x),
      yaxis = list(title = label_y)
    )
}


roboplotr_highlight_legend <- function(highlight, df) {

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


#' Creates a list used to construct plot fonts
#'
#' @param path Character. Either "serif", "sans-serif" or a path to .otf or .ttf file.
#' @param size Double. Determines font size for the font.
#' @param color Character. Must be a hexadecimal color or among strings provided by grDevices::colors.
#' @param bold Logical. Only used for title font. Determines if the title is bolded.
#' @param type Character. One of "main", "title" or "caption".
#' @importFrom stringr str_c str_extract
#' @returns A list
#' @export
roboplot_set_font <- function(path = "sans-serif", size = 12, color = "black", bold = T, type = NULL) {

  roboplotr_check_param(path, "character", allow_null = F)
  if(!path %in% c("serif","sans-serif")) {
    if (!file.exists(path) | !str_extract(path, "(?<=\\.).*$") %in% c("otf","ttf")) {
      stop(str_c("Font path does not seem to exist or the font file is not in file format .otf or .ttf. Is the file path correct?\nTry using a call of system.file() (eg. system.file(\"www\",\"fonts\",\"Roboto-Regular.ttf\", package = \"roboplotr\"))\nYou can also use \"serif\" or \"sans-serif\" in place of a font path."), call. = F)
    }
  }
  roboplotr_check_param(size, "numeric", allow_null = F)
  roboplotr_check_param(color, "character", allow_null = F)
  if(!roboplotr_are_colors(color)) {
    stop (str_c("Any font 'color' must be hexadecimal colors or among strings provided by grDevices::colors!"), call. = F)
  }
  roboplotr_check_param(bold, "logical", allow_null = F)
  roboplotr_check_param(type, "character", allow_null = F)
  type <- tolower(type)
  if(!type %in% c("main","title","caption")) {
    stop("Any font 'type' must be one of \"main\", \"title\" or \"caption\".", call. = F)
  }

  if(path %in% c("serif","sans-serif")) {
    family <- path
    path <- NULL
  } else {
    family <- str_c("roboplot-",type)
  }

  list(path = path, family = family, size = size, color = color, bold = bold)
}
