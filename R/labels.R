#' @importFrom farver decode_colour encode_colour
#' @importFrom plotly layout
#' @importFrom stringr str_extract
roboplotr_legend <- function(p, legend) {
# print(legend_order)
  if(!is.null(legend$orientation)) { roboplotr_message("The set_legend() argument `legend_orientation` is currently ignored.") }

  if(is.null(legend$position)) { legend$position <- "bottom" }

    if (is.null(legend$title)) {
      .legend_title <- NULL
    } else if (is.character(legend$title)) {
      .legend_title <- str_c("<b>", legend$title, "</b>")
    }


  if (is.na(legend$position)) {
    x.pos <- ifelse(legend$position == "right", 100, 0)
    y.pos <- ifelse(legend$position == "right", 1, 0) #-0.05
    # orientation <- "h"#case_when(orientation == "auto" ~ ifelse(legend_position == "right", "v","h"), TRUE ~ str_extract(orientation, "^(v|h)"))
    p |> layout(
      showlegend = T,
      legend = list(font = getOption("roboplot.font.main")[c("color","family","size")],
                    bgcolor = getOption("roboplot.colors.background") |> decode_colour() |> encode_colour(alpha = 0.1),
                    x = x.pos, y = y.pos,
                    orientation = "h",
                    xanchor = "left",
                    yanchor = "top",
                    traceorder = legend$order
      ))
  } else if (!legend$position %in% c("bottom")) {
    stop("set_legend() `position` must be \"bottom\", or NA for no legend!", call. = F)
  } else {
    x.pos <- ifelse(legend$position == "right", 100, 0)
    y.pos <- ifelse(legend$position == "right", 1, 0) #-0.05
    # orientation <- "h"#case_when(orientation == "auto" ~ ifelse(legend_position == "right", "v","h"), TRUE ~ str_extract(orientation, "^(v|h)"))
    p |> layout(
      showlegend = T,
      legend = list(font = getOption("roboplot.font.main")[c("color","family","size")],
                    bgcolor = getOption("roboplot.colors.background") |> decode_colour() |> encode_colour(alpha = 0.1),
                    x = x.pos, y = y.pos,
                    orientation = "h",
                    xanchor = "left",
                    yanchor = "top",
                    title = list(text = .legend_title, font = getOption("roboplot.font.main")),
                    traceorder = legend$order
                    ))
  }
}


#' @importFrom plotly layout
roboplotr_caption <- function(p, caption) {

  roboplotr_typecheck(caption, "character", allow_null = F)

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

#' Title configuration.
#'
#' Parameters to customize titles of [roboplots][roboplot()]. Can be used with
#' [robotables][robomap()] and [robomaps][robomap()], but don't really do anything.
#'
#' @param title Character. Optional. Will try to determine a title based on
#' attributes of `d` in a [roboplots][roboplot()]. Defaults to "PLACEHOLDER".
#' @param include Logical. Determines whether the given title will be used in
#' the plot. Will always inlcude it for exported static images.
#' @param ... Placeholder for other parameters.
#' @examples
#' # Used to set titles for plots created with `roboplot()`. You can
#' # simply give a charater string for plot titles.
#'
#' d <- energiantuonti |> dplyr::filter(Alue == "Kanada",Suunta == "Tuonti")
#'
#' d |> roboplot(Alue,"Energian tuonti Kanadasta")
#'
#' # However, if you want to render the plot without the title included, you
#' # can use `set_title(include = F)` to omit it from the plot. This
#' # is for cases where you will insert the plot into environment where the
#' # title is outside the plot element. When exporting, you would till want to have
#' # the title included, and `roboplot()` takes care of this. If you include a subtitle,
#' # it will be displayed regardless.
#'
#'
#' d |>
#' roboplot(Alue,
#' title = set_title("Energian tuonti Kanadasta", include = FALSE),
#' subtitle = "Milj. €")
#'
#'
#' @returns A list of class roboplotr.set_title
#' @export
set_title <- function(title = NULL, include = T, ...) {

  args <- list(...)

  if (!is.null(args$.extra)) {
    .extra <- args$.extra
  } else {
    .extra <- "in set_title()"
  }

  roboplotr_typecheck(title, "character", extra = .extra)

  .res <- list(title = title, include = include)

  .res <- structure(.res, class = c("roboplotr","roboplotr.set_title", class(.res)))

  .res
}


#' @importFrom plotly layout
#' @importFrom htmltools tag HTML
#' @importFrom stringr str_c
roboplotr_title <- function(p, title, subtitle) {

  if(!is.null(title)) {
    `title$title` <- title$title
    `title$include` <- title$include
    roboplotr_typecheck(`title$title`, "character")
    roboplotr_typecheck(`title$include`, "logical")
  }

  roboplotr_typecheck(subtitle, "character", allow_null = F)

  if(title$include == T) {
    txt <- str_c(
      "<span>",title$title,
      as.character(tags$span(HTML(str_c("<br>",subtitle)), style = "font-size: 75%")),"</span>"
    )
  } else {
    txt <- as.character(tags$span("",tags$span(HTML(str_c(subtitle)), style = "font-size: 75%")))
  }
  p |>
      layout(
        title = list(
          text = txt,
          font = getOption("roboplot.font.title")[c("color","family","size")],
          xanchor = "left",
          yanchor = "bottom",
          y = 1,
          x = 0,
          yref = "container",
          xref = "paper"
          )
      )
}

#' Caption configuration
#'
#' Parameters to customize caption defaults.
#'
#' @param text Character. The text used in the template (make sure the template
#' has the parameter `text`).
#' @param ... Other parameters to be passed to template.
#' @param template Character. Template for [stringr::str_glue()] used to parse
#' the caption with the given parameters.
#' @returns A character of class glue and roboplot.set_caption
#' @examples
#' # Used to define how captions are constructed inside `roboplot()`. The used
#' # parameters are used inside stringr::str_glue() to parse a caption
#' # string. You can skip using `set_caption()` simply by giving a character string.
#' # You can provide the template here, or use the one defined globally with
#' # `set_roboplot_options()`.
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
#' # using `set_roboplot_options()` to change the defaults to something more sensible.
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

  list2env(list(...), envir = environment())
  .res <- str_glue(template)

  .res <- structure(.res, class = c("roboplotr","roboplotr.set_caption", class(.res)))

  .res

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


#' Font configuration.
#'
#' Parameters to customize fonts in various `roboplotr` contexts.
#'
#' @param font Character. One of your system fonts if `systemfonts` package is available.
#' "Arial", "Verdana", "Tahoma", "Trebuchet", "Times New Roman", "Georgia", "Garamond",
#' "Courier New", "Brush Script MT", a Google font, or a path to an .otf or .ttf file.
#' @param fallback Character. Only used when a file path is used with `font`. Fallback
#' font when the defined font is unavailable. Especially concerns image downloads.
#' @param size Numeric. Font size.
#' @param color Character. Must be a hexadecimal color or a valid css color.
#' @param bold_title Logical. Only used for font of type `title`. Determines if
#' the title is bolded.
#' @param type Character. One of "main", "title" or "caption".
#' @importFrom httr content GET status_code
#' @importFrom stringr str_c str_extract
#' @returns A list of class roboplot.set_font
#' @examples
#' # Used to set fonts used by `roboplotr` functions. You can designate various
#' # specifications. You must provide a web-safe fallback (as accepted by `roboplotr`,
#' # see `font`) font if an actual filepath is provided for 'path'.
#'
#' set_roboplot_options(
#'   caption_template = "Lähde: {text}!!",
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

#' # Valid Google Fonts work as well.
#' set_roboplot_options(font_title = set_font(font = "Exo"))
#'
#' # Note that `plotly`'s own downloadImage method does not support external fonts.
#' # It can only use the fonts the browser has available. Use `set_artefacts()`
#' # `roboplot()` to automate their creation if you need static images with external fonts.
#'
#' \dontrun{
#'   # Create a html file with `roboplot()`.
#'   d |>
#'     roboplot(
#'       color = Alue,
#'       title = "Energian tuonti Kanadasta",
#'       subtitle = "Milj. €",
#'       caption = "Tilastokeskus",
#'       artefacts = set_artefacts(artefacts = c("html", "png"), filepath = tempdir())
#'     )
#'
#'   # The html has proper fonts, but files exported from modebar will not.
#'   utils::browseURL(paste0(tempdir(),"/energian_tuonti_kanadasta.html"))
#'   # The images automated with `set_artefacts()` will have the proper font.
#'   utils::browseURL(paste0(tempdir(),"/energian_tuonti_kanadasta.png"))
#' }
#'
#' # Revert to defaults:
#' set_roboplot_options(reset = TRUE)
#' @export
set_font <- function(font = "Arial", fallback = NULL, size = NULL, color = NULL, bold_title = T, type = NULL) {

  if(is.null(font)) {
    font <- getOption(str_glue("roboplot.font.{type}"))$.font
  }
  if(is.null(fallback)) {
    fallback <- getOption(str_glue("roboplot.font.{type}"))$.fallback
  }
  if(is.null(size)) {
    size <- getOption(str_glue("roboplot.font.{type}"))$size
  }
  if(is.null(color)) {
    color <- getOption(str_glue("roboplot.font.{type}"))$color
  }

  websafe <- c("Arial","Verdana","Tahoma","Trebuchet","Times New Roman","Georgia","Garamond","Courier New","Brush Script MT")

  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    .fonts <- websafe
  } else {
    .fonts <- c(websafe, systemfonts::system_fonts()$family) |> unique()
  }

  roboplotr_typecheck(font, "character", allow_null = F)

  google_font <- NULL
  if (!font %in% .fonts & !str_detect(tolower(font), "(ttf|otf)$")) {
    google_font <- (function(font_name = font) {

      font_name <- str_replace_all(font_name, ' ','+')
      font_url <- str_glue("https://fonts.googleapis.com/css2?family={font_name}")
      resp <- GET(font_url)

      if(status_code(resp) == 200) {
       google_font <- content(resp)
       google_font <- list(url = str_extract(google_font, "(?<=url\\()[^\\)]*(?=\\))"), format = str_extract(google_font, "(?<=format\\()[^\\)]*(?=\\))"))
      } else {
          NULL
        }
    })()
  }

  if(!font %in% .fonts & is.null(google_font)) {
    if (!file.exists(font) | !str_extract(font, "[^\\.]*$") %in% c("otf","ttf","OTF","TTF")) {
      stop(str_c("The give 'font' does not seem to exist or the font file is not in file format .otf or .ttf. Is the file path correct?\n",
                 "Try using a call of system.file() (eg. system.file(\"www\",\"fonts\",\"Roboto-Regular.ttf\", package = \"roboplotr\"))\n",
                 "You can use any of ",roboplotr_combine_words(str_c("\"",websafe,"\""), and = " or ")," instead, or try using a valid Google Font."), call. = F)
    }
  }

  if(!font %in% websafe) {
    .safes <- roboplotr_combine_words(str_c("\"",websafe,"\""), and = " or ")
    roboplotr_message(str_glue("The font '{font}' might not work in all contexts, consider using one of {.safes}."))
  }


  .font <- font
  roboplotr_typecheck(size, "numeric", allow_null = F)
  roboplotr_typecheck(color, "character", allow_null = F)
  roboplotr_valid_colors(color)
  roboplotr_typecheck(bold_title, "logical", allow_null = F)

  if(!font %in% websafe & is.null(google_font)) {
    roboplotr_typecheck(type, "character", allow_null = F)
    type <- tolower(type)
    if(!type %in% c("main","title","caption")) {
      stop("When the font is a file, the parameter 'type' must be one of \"main\", \"title\" or \"caption\".", call. = F)
    }
  }

  fallbacks <- c("Arial|Verdana|Tahoma|Trebuchet" = "sans-serif",
                 "Times New Roman|Georgia|Garamond" = "serif",
                 "Courier New" = "monospace",
                 "Brush Script MT" = "cursive")

  if(font %in% .fonts) {
    .fallback <- fallback
    fallback <- str_replace_all(font, fallbacks)
    family <- str_c(font,", ",fallback)
    font <- NULL
    font_face <- NULL
  } else if (!is.null(google_font)) {
    roboplotr_typecheck(fallback, "character", allow_null = F)
    roboplotr_valid_strings(fallback, websafe, any)
    .fallback <- fallback
    fallback <- str_c(fallback," , ",str_replace_all(fallback, fallbacks))
    font_face <- font
    family <- str_glue("{font}, {fallback}")
    font <- NULL
  } else {
    roboplotr_typecheck(fallback, "character", allow_null = F)
    roboplotr_valid_strings(fallback, websafe, any)
    .fallback <- fallback
    fallback <- str_c(fallback," , ",str_replace_all(fallback, fallbacks))
    font_face <- str_glue("roboplot-{type}")
    family <- str_glue("roboplot-{type}, {fallback}")
  }

  .res <- list(path = font, family = family, font_face = font_face, size = size, color = color, bold = bold_title, google_font = google_font, .fallback = .fallback, .font = .font)
  .res <- structure(.res, class = c("roboplotr.set_font", class(.res)))
  .res
}

#' Legend configuration
#'
#' Parameters to customize legend in [roboplots][roboplot()] and [robomaps][robomap()].
#' @inheritDotParams set_plot_legend
#' @inheritDotParams set_map_legend
#' @param ... Placeholder for other parameters.
#' @returns A list of class roboplotr.set_legend
#' @export
#' @examples
#' # You can use `set_legend` to control how legends are displayed on a `roboplot()`.
#'
#' d <- energiantuonti |>
#'   dplyr::filter(Alue == "Venäjä")
#'
#' # Use `position` NA to remove the legend.
#' d |>
#'   roboplot(Suunta,
#'            legend = set_legend(
#'              position = NA
#'            )
#'            )
#'
#' # `roboplot()` omits the legend to give more space for the plot area, when there
#' # is only a single trace for `color`. Use `position` to force the legend to show.
#' # You can also use `title` to set a title.
#'
#' d |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Suunta, legend = set_legend(title = "Example", position = "bottom"))
#'
#' # Legend title is distinct from axis-specific legend titles which are controlled
#' # by `set_axes()` parameters `ylegend` and `y2legend`, when `y2` is used to move
#' # items from `color` to a secondary y-axis.
#'
#' d |>
#'   roboplot(
#'     color = Suunta,
#'     plot_axes = set_axes(
#'       y2 = "Tuonti",
#'       ylegend = "Left yaxis",
#'       y2legend = "Right yaxis"
#'     ),
#'     legend = set_legend(title = "Example")
#'   )
#'
#' # Use `tidy` to force legend items to have equal horizontal space across columns
#' # for slightly neater looking plots. Avoid if space is at premium.
#'
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(
#'     Alue,
#'     legend = set_legend(tidy = TRUE)
#'   )
#'
#' # `set_legend()` works with `robomap()` too, but with a bit different parameters.
#' # Control number of legend items with `breaks`.
#'
#' d <- vaesto_postinumeroittain |>
#'   dplyr::filter(stringr::str_detect(Postinumero, "^00(8|9)"))
#'
#' d |>
#'   robomap(Postinumero, "Väkiluku", legend = set_legend(breaks = 3))
#'
#' # Or give speficic breakpoints (minimum of two breakpoints). Omit ranges from
#' # labeling by using `range`.
#'
#' d |>
#'   robomap(Postinumero, "Väkiluku", legend = set_legend(breaks = c(9000, 18000), range = FALSE))
#'
#' # Adjust position and opacity.
#' d |>
#'   robomap(Postinumero,
#'           "Väkiluku",
#'           legend = set_legend(position = "bottomleft", opacity = 0.3))
#'
#' # Give a character vector for labels when specific breaks are given. The length
#' # of labels must be one more than the length of breaks.
#' d |>
#'   robomap(Postinumero, "Väkiluku",
#'           legend = set_legend(
#'     breaks = c(9000, 18000),
#'     labels = c("Vähän", "Keskiverto", "Paljon")
#'   ))
#'
#' # Or when breaks is a single value, giving the number of legend items, you
#' # can give a logical vector of that length, controlling legend item visibility.
#' # This way you can approximate a gradient legend.
#' d |> robomap(
#'   area = Postinumero,
#'   title = "Väkiluku",
#'   legend = set_legend(
#'     breaks = 13,
#'     labels = c(
#'       TRUE,
#'       FALSE,
#'       TRUE,
#'       FALSE,
#'       TRUE,
#'       FALSE,
#'       TRUE,
#'       FALSE,
#'       TRUE,
#'       FALSE,
#'       TRUE,
#'       FALSE,
#'       TRUE
#'     ),
#'     range = FALSE
#'   )
#' )
set_legend <- function(...) {
  arggs <- list(...)
  purpose <- arggs$purpose
  calls <- sys.calls()
  purpose <- calls |> unlist() |> as.character() |> str_extract("(roboplot|robomap)(?=\\()")
  if ("roboplot" %in% purpose) {
    set_plot_legend(...)
  } else if ("robomap" %in% purpose) {
    set_map_legend(...)
  } else {
    set_plot_legend(...)
  }
}

#' @param position Character. Either "bottom" or NA for no legend for [roboplot][roboplot()],
#' or one of "bottomleft", "bottomright", or "none" for [robomap][robomap()]. On a
#' [roboplot][roboplot()], the legend is removed on default if column in data for
#' param `d` of the [roboplot][roboplot()] that is defined by param `color` of that
#' plot has only one observation.
#' @param orientation Character. Currently unused.
#' @param maxwidth Numeric. All [roboplot][roboplot()] legend items (and y-axis
#' values for horizontal barplots) longer than this will be trunctated with an ellipsis.
#' @param title Logical or character. TRUE if you want the parameter `color` from
#' the [roboplot][roboplot()] to be the legend title. Character if you want to provide
#' your own legend title.
#' @param tidy Logical. Controls whether the [roboplot][roboplot()] legend items
#' will have matching widths
#' across columns. Default is FALSE.
#' @rdname set_legend
set_plot_legend <- function(position = NULL,
                                      orientation = NULL,
                                      maxwidth = NULL,
                                      title = FALSE,
                                      tidy = getOption("roboplot.legend.tidy"),
                                      ...) {


  roboplotr_typecheck(position, "character", allow_na = T)
  roboplotr_typecheck(orientation, "character")
  roboplotr_typecheck(maxwidth, "numeric")
  roboplotr_typecheck(title, c("logical","character"), allow_null = F)
  roboplotr_typecheck(tidy, "logical", allow_null = F)

  .res <- list(position = position, orientation = orientation, maxwidth = maxwidth, title = title, tidy = tidy)

  .res <- structure(.res, class = c("roboplotr","roboplotr.set_legend", class(.res)))

  .res
}



#' @param breaks Numeric vector. A length 1 vector attempts to make that many
#' [robomap][robomap()] legend entries. Length 2+ vector should be the breaks in
#' values of param `d` of [robomap][robomap()] where the legend should be split.
#' @param labels Character or logical vector. If [robomap][robomap()] `breaks` is
#' of length 1, a logical vector of length equal to value of `breaks`, controlling
#' which legend items are shown. If `breaks` is of length 2+, provide a character
#' vector of length 1 + length of `breaks`.
#' @param range Logical. Whether [robomap][robomap()] shows single values or ranges.
#' Ignored if `labels` are provided as characters.
#' @param opacity Numeric. The opacity of the [robomap][robomap()] legend, ranging
#' from  0 to 1.
#' @importFrom dplyr between
#' @rdname set_legend
set_map_legend <- function(
    breaks = 5,
    labels = NULL,
    range = T,
    position = "bottomright",
    orientation = "vertical",
    title = NULL,
    opacity = 1,
    ...
) {

  roboplotr_typecheck(position, "character",allow_null = F)
  roboplotr_valid_strings(position, c("bottomleft","bottomright","none"), any, "set_legend() param 'position'")
  roboplotr_typecheck(orientation, "character",allow_null = F)
  roboplotr_valid_strings(orientation, c("vertical"), any, "set_legend() param 'orientation'")
  # robomap-info css: display: flex; flex-flow: row wrap; gap: 5px; margin-left: 10px; margin-right: 10px;
  # muuta myös legend titleä.. pitäisikö tulla ylös
  roboplotr_typecheck(title, "character")
  roboplotr_typecheck(breaks, "numeric", size = NULL, allow_null = F)
  roboplotr_typecheck(range, "logical", allow_null = F)
  roboplotr_typecheck(opacity, "numeric", allow_null = F)
  if(length(breaks) == 1) {
    labtype <- "logical"
    labsize <- as.numeric(breaks)
  } else {
    labtype <- "character"
    labsize <- length(breaks) + 1
  }
  roboplotr_typecheck(labels, labtype, size = labsize)

  if(!between(opacity, 0, 1)) {
    stop("set_legend() param 'opacity' must be between 0 and 1!", call. = F)
  }
  .res <- list(position = position, breaks = breaks, position = position, labels = labels, opacity = opacity, title = title, range = range)

  .res <- structure(.res, class = c("roboplotr","roboplotr.set_legend", class(.res)))

  .res
}


roboplotr_set_caption <- function(caption, d, where) {

  roboplotr_typecheck(caption, c("character","set_caption"), extra = where)

  if (!is.null(caption)) {
    if (!inherits(caption,"roboplotr.set_caption")) {
      caption <- set_caption(text = caption)
    }
  } else {
    cpt <- attributes(d)$source
    if (length(cpt) == 1) {
      roboplotr_message("Using the attribute \"source\" for plot caption.")
      caption <- set_caption(text = unlist(cpt)[1])
    } else if (!is.null(cpt[[getOption("roboplot.locale")$locale]])) {
      roboplotr_message("Using the attribute \"source\" as plot caption.")
      caption <-
        set_caption(text = cpt[[getOption("roboplot.locale")$locale]][1])
    } else {
      roboplotr_alert("Missing the caption, using placeholder.")
      caption <- set_caption(text = "PLACEHOLDER")
    }
  }

  caption
}


roboplotr_set_title <- function(title, d, where) {

  roboplotr_typecheck(title, c("set_title","character"), extra = where)

  if (is.null(title)) {
    title <- attributes(d)[c("title", "robonomist_title")]
    if (!is.null(title$robonomist_title)) {
      roboplotr_message("Using the attribute \"robonomist_title\" for plot title.")
      title <- set_title(title$robonomist_title, .extra = where)
    } else if (!is.null(title$title) & length(title$title != 1)) {
      roboplotr_message("Using the attribute \"title\" as plot title.")
      title <- set_title(title$title, .extra = where)
    } else {
      roboplotr_message("Missing the title, using placeholder.")
      title <- set_title("PLACEHOLDER")
    }
  } else if (!"roboplotr.set_title" %in% class(title)) {
    title <- set_title(title = as.character(title), include = TRUE, .extra = where)
  }
  title
}
