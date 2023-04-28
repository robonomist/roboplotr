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
      legend = list(font = getOption("roboplot.font.main")[c("color","family","size")],
                    bgcolor = getOption("roboplot.colors.background") |> decode_colour() |> encode_colour(alpha = 0.1),
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
      legend = list(font = getOption("roboplot.font.main")[c("color","family","size")],
                    bgcolor = getOption("roboplot.colors.background") |> decode_colour() |> encode_colour(alpha = 0.1),
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

  roboplotr_check_param(caption, type = "character", allow_null = F)

  if(!is.null(caption)) {
    p <- p |>
      layout(
        annotations = list(
          x = 0, text = caption, align = "left",
          showarrow = F, xref = 'paper', yref = "paper",
          xanchor='left', yanchor = 'bottom', xshift=0, yshift=0,
          font = getOption("roboplot.font.caption")[c("color","family","size")]
        )
      )
  }


  p

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
          font = getOption("roboplot.font.title")[c("color","family","size")],
          xanchor = "left",
          yanchor = "bottom",
          # y = 0.9325,
          y = 1,
          x = 0,
          yref = "container",
          xref = "paper"
          )
      )
}

#' Get a string for [roboplot()] captions.
#'
#' @param caption Character. Character. The text used in the template (make sure the template has the parameter).
#' @param ... Other parameters to be passed to template.
#' @param template Character. Character. Template for [str_glue()] used to parse the caption wit the given parameters.
#' @return A string of classes 'glue' and 'character'.
#' @examples
#' # Used to define how captions are constructed inside roboplotr::roboplot()
#' # The used parameters are used inside stringr::str_glue() to parse a caption
#' # string. You can skip using set_caption() simply by giving a character string.
#' # You can provide the template here, or use the one defined globally with
#' # roboplotr::set_roboplot_options().
#'
#'
#' d <- energiantuonti |> dplyr::filter(Alue == "Kanada",Suunta == "Tuonti")
#'
#' d |> roboplot(Alue,"Energian tuonti Kanadasta","Milj. €",
#'                    caption = "Tilastokeskus")
#'
#'
#' # Override the global template with function parameters and provide
#' # parameters for it. The example is unnecessarily complicated, but gives the
#' # idea how this could work.
#'
#' d |>
#'   roboplot(Alue, "Energy import","Million euros",
#'            caption =
#'              set_caption(
#'                prepend = "(Canada)",
#'                append = paste0("(Customs Finland, International trade ",
#'                                "statistics;<br>Radiation and Nuclear Safety ",
#'                                "Authority; Gasum LLC)"),
#'                caption = "Statistics Finland",
#'                template = "{prepend}.<br>Source: {caption}.<br>{append}."
#'                )
#'   )
#'
#' # If you need to make manual changes repeatedly, you are probably better off
#' # using roboplotr::set_roboplot_options() (documented therein) to change the
#' # defaults to something more sensible.
#'
#' set_roboplot_options(
#'   caption_template = "Source: {text}.",
#'   )
#'
#' d |> roboplot(Alue,"Energy import from Canada","M€", "Statistic Finland")
#'
#' # Revert to defaults:
#' set_roboplot_options(reset = TRUE)
#'
#' @importFrom stringr str_glue
#' @export

set_caption <- function(text, ..., template = getOption("roboplot.caption.template")) {

  args <- list(...)
  for (i in seq_along(args)) {
    assign(names(args)[[i]], args[[i]])
  }
  str_glue(template)


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


#' Get a list for [set_roboplot_options()] used as font specifications in [roboplot()]
#'
#' @param font Character. One of "Arial", "Verdana", "Tahoma", "Trebuchet", "Times New Roman", "Georgia", "Garamond", "Courier New", "Brush Script MT" or a path to an .otf or .ttf file.
#' @param fallback Character. Only used when a file path is used with 'font'. Fallback font when the defined font is unavailable. Especially concerns image downloads.
#' @param size Double. Determines font size for the font.
#' @param color Character. Must be a hexadecimal color or a valid css color.
#' @param bold_title Logical. Only used for font of type 'title'. Determines if the title is bolded.
#' @param type Character. One of "main", "title" or "caption".
#' @importFrom stringr str_c str_extract
#' @returns A list
#' @examples
#' # Used to set fonts used by roboplotr::roboplot(). Only supposed to be called
#' # inside roboplotr::set_roboplot_options(). 'path' can be a file path or
#' # string "serif" or "sans-serif", if no specific font path will be provided.
#' # This will change fonts according to the list output of
#' # roboplotr::set_font(). You can designate font color, size, and
#' # path, and (for title font only) whether it is bolded. You must also provide
#' # one of "title", "main" or "caption" as 'type' and a web-safe fallback font
#' # if an actual filepath is
#' # provided for 'path'.
#'
#'
#' set_roboplot_options(
#'   caption = list(prefix = "Lähde: ", lineend = ".", updated = FALSE),
#'   font_title = set_font(size = 17, font = "Tahoma", bold = TRUE),
#'   font_main =  set_font(
#'     size = 11,
#'     font = system.file("www","fonts","bladerunner.TTF", package = "roboplotr"),
#'     fallback = "Verdana",
#'     color = "darkred",
#'     type = "main"),
#'   font_caption = set_font(color = "green")
#' )
#'
#' d <- energiantuonti |> dplyr::filter(Alue == "Kanada",Suunta == "Tuonti")
#'
#' d |> roboplot(Alue,"Energian tuonti Kanadasta","Milj. €","Tilastokeskus")
#'
#' # Note that plotly's own downloadImage does not support external fonts.
#'
#' if(interactive()) {
#'
#'   d |>
#'   roboplot(Alue,"Energian tuonti Kanadasta","Milj. €","Tilastokeskus") |>
#'   roboplot_create_widget(filepath = tempdir(), artefacts = "img_w")
#'
#'   utils::browseURL(paste0(tempdir(),"/energian_tuonti_kanadasta_levea.png"))
#' }
#'
#' # revert to defaults:
#' set_roboplot_options(reset = TRUE)
#' @export
set_font <- function(font = "Arial", fallback = NULL, size = 12, color = "black", bold_title = T, type = NULL) {

  websafe <- c("Arial","Verdana","Tahoma","Trebuchet","Times New Roman","Georgia","Garamond","Courier New","Brush Script MT")

  roboplotr_check_param(font, "character", allow_null = F)

  if(!font %in% websafe) {
    if (!file.exists(font) | !str_extract(font, "[^\\.]*$") %in% c("otf","ttf","OTF","TTF")) {
      stop(str_c("The give 'font' does not seem to exist or the font file is not in file format .otf or .ttf. Is the file path correct?\n",
                 "Try using a call of system.file() (eg. system.file(\"www\",\"fonts\",\"Roboto-Regular.ttf\", package = \"roboplotr\"))\n",
                 "You can use any of ",roboplotr_combine_words(str_c("\"",websafe,"\""), and = " or ")," instead."), call. = F)
    }
  }

  roboplotr_check_param(size, "numeric", allow_null = F)
  roboplotr_check_param(color, "character", allow_null = F)
  roboplotr_valid_colors(color)
  roboplotr_check_param(bold_title, "logical", allow_null = F)

  if(!font %in% websafe) {
    roboplotr_check_param(type, "character", allow_null = F)
    type <- tolower(type)
    if(!type %in% c("main","title","caption")) {
      stop("Any font 'type' must be one of \"main\", \"title\" or \"caption\".", call. = F)
    }
  }

  fallbacks <- c("Arial|Verdana|Tahoma|Trebuchet" = "sans-serif",
                 "Times New Roman|Georgia|Garamond" = "serif",
                 "Courier New" = "monospace",
                 "Brush Script MT" = "cursive")

  if(font %in% websafe) {
    fallback <- str_replace_all(font, fallbacks)
    family <- str_c(font,", ",fallback)
    font <- NULL
    fallback <- NULL
  } else {
    roboplotr_check_param(fallback, "character", allow_null = F)
    roboplotr_valid_strings(fallback, websafe, any)
    fallback <- str_c(fallback," , ",str_replace_all(fallback, fallbacks))
    family <- str_c("roboplot-",type)
    # roboplotr_check_param(fallback, "character", allow_null = F)
    # roboplotr_valid_strings(fallback, websafe, any)
    # fallback <- str_c(fallback,", ",str_replace_all(fallback, fallbacks))
    # family <- str_c("roboplot-",type,", ",fallback)
  }

  list(path = font, family = family, fallback = fallback, size = size, color = color, bold = bold_title)
}
