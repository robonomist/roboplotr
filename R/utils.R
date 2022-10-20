#' Override the default options for colors, fonts etc. for any plot created with roboplot()
#'
#' @param roboplot_options List. The roboplot options to be set.
#' @param notify Logical. Controls the message for set options.
#' @param shinyapp Logical. Makes fonts, css and javascript available for shiny apps.
#' @export
#' @importFrom htmltools singleton tagList tags
#' @importFrom knitr combine_words
#' @importFrom purrr iwalk
#' @importFrom shiny addResourcePath
#' @importFrom stringr str_c str_detect str_extract str_subset
#' @importFrom R.utils setOption
roboplot_set_options <- function(roboplot_options, notify = T, shinyapp = F) {
  opts_names <- names(options())

  # reset session specific options

  widget_deps_names <- subset(opts_names, str_detect(opts_names, "^roboplot.widget.deps"))
  for(opt in widget_deps_names) {
    setOption(opt, NULL)
    }

  opts_names <- subset(opts_names, str_detect(opts_names, "^roboplot"))
  opts_error <- function() stop(str_c("Roboplot options must be a named list with names among\n",combine_words(opts_names, and = " and/or "),"!\n"), call. = F)
  if(!is.list(roboplot_options)) { opts_error() }
  if(is.null(names(roboplot_options))) {
    opts_error()
  } else if (!all(names(roboplot_options) %in% opts_names)) {
    opts_error()
  }

  check_colors <- function() {
    color_options <- roboplot_options[str_subset(names(roboplot_options), "roboplot.colors")]
    list_options <- color_options[str_subset(names(color_options), "ticks|grid|border")]
    if(length(list_options) > 0) {
      for(op in list_options) {
        if(!all(c("x","y") == names(op)) | !is.list(op)) { stop("Any color roboplot options for border, grid and ticks must be a named list with names x and y!", call. = F) }
        }
    }
    if(length(color_options) > 0) {
      if(any(roboplot_are_colors(unlist(color_options))) == F) { stop ("Any color roboplot_options must be hexadecimal values or among strings provided by grDevices::colors!", call. = F) }
    }
  }

  check_dashtypes <- function() {
    dashtype_options <- roboplot_options[str_subset(names(roboplot_options), "roboplot.dashtypes")]
    if(length(dashtype_options) > 0) {
      valid_dashtypes <- c("solid", "dash", "dot", "longdash", "dashdot", "longdashdot")
      if(!all(dashtype_options %in% valid_dashtypes) | !all(valid_dashtypes %in% dashtype_options)) { stop (str_c("Dashtype roboplot options must contain all of ",combine_words(valid_dashtypes)," in any order!"), call. = F) }
    }
  }

  check_modebar_buttons <- function() {
    button_options <- roboplot_options[str_detect(names(roboplot_options),"roboplot.modebar.buttons")]
    valid_buttons <- c("closest","compare","zoomin","zoomout","img_w","img_n","img_s","data_dl","robonomist")
    if(!any(unlist(button_options) %in% valid_buttons) & length(button_options) > 0) {
      stop(str_c("Roboplot modebar button options must be one or more of ",combine_words(valid_buttons),"!"), call. = F)
    }
  }

  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

  check_png_fonts <- function() {
    png_fonts <- roboplot_options[str_detect(names(roboplot_options),"roboplot.png.font.size")]
    valid_png_fonts <- c("title","main","caption")
    for(png_font in png_fonts) {
      if(!is.list(png_font) | !all(names(png_font) == valid_png_fonts) | any(!is.wholenumber(unlist(png_font)))) {
        stop(str_c("Roboplot png fontsize options must be one or more of ",combine_words(valid_png_fonts)," and be whole numbers!"), call. = F)
        }
    }
  }

  check_fonts <- function(roboplot_options) {

    font_options <- roboplot_options[str_detect(names(roboplot_options),"roboplot.font")]

    for(i in seq_len(length(font_options)) ) {

      opt_name <- names(font_options[i])
      opt <- font_options[[i]]

      if(is.null(opt) | !any(c("size","path","color") %in% names(opt))) {
        stop(str_c(opt_name, " must be a list with \"size\", \"path\" and \"color\", and optionally \"bold\" used with titles."), call. = F)
      }

      for(name in names(opt)) {
        if(is.null(opt[[name]])) { stop(str_c(opt_name," options can't be NULL!"), call. = F) }
      }

      if(!opt$path %in% c("serif","sans-serif")) {
        if (!file.exists(opt$path) | !tools::file_ext(tolower(opt$path)) %in% c("otf","ttf")) {
          stop (str_c(opt_name, " path must be \"roboplot-main\", \"serif\", \"sans-serif\", or a filepath to a file with .otf or .ttf extension."), call. = F)
        } else {
          opt$family <- str_c("roboplot-",str_extract(opt_name,"[^\\.]*$"))
        }
      } else if (opt$path %in% c("serif","sans-serif")) {
        opt$family <- opt$path
        opt$path <- NULL
      }

      if(!is.wholenumber(opt$size)) {
        stop(str_c(opt_name, "  size must be a whole number."), call. = F)
      }

      if(roboplot_are_colors(opt$color) == F & length(opt$color) != 1) {
        stop(str_c(opt_name, " colors must be 6-character hexadecimal colors or among strings provided by grDevices::colors!."), call. = F)
      }

      if(str_detect(opt_name,"title") & is.logical(opt$bold)) {
        if(opt$bold == F) {
         opt$bold <- function(x) { x }
        } else {
          opt$bold <- function(x) { str_c("<b>",x,"</b>") }
        }
      } else if(str_detect(opt_name,"title") &!is.logical(opt$bold)) { stop("roboplot.font.main$bold must be TRUE or FALSE.") }

      roboplot_options[[opt_name]] <- opt

    }

    roboplot_options
  }
  roboplot_options <- check_fonts(roboplot_options)
  check_modebar_buttons()
  check_colors()

  iwalk(roboplot_options, ~ setOption(names(roboplot_options[.y]), .x))

  if(notify == T) { message(str_c("Roboplot option",if(length(roboplot_options) == 1) {""} else {"s"}," ",combine_words(names(roboplot_options))," set.")) }

  if(!str_detect(getOption("roboplot.logo"),"robonomist.png")) {
    setOption("roboplot.modebar.buttons", c(getOption("roboplot.modebar.buttons"),"robonomist"))
  }

  if(shinyapp) {
    roboplot_make_widget_deps(tempdir())
    addResourcePath("js", system.file("www","js", package = "roboplotr"))
    addResourcePath("fonts", file.path(tempdir(),"fonts"))
    addResourcePath("css", file.path(tempdir(),"css"))

    tagList(
        singleton(
          tags$head(
            tagList(
              tags$script(type = "text/javascript", src = "js/relayout.js"),
              tags$link(rel = "stylesheet", type = "text/css", src = "css/style.css")
            ),
          )
        )
      )

    # message('Remember to add tags$head(tags$script(type = "text/javascript", src = "js/relayout.js")) to your app ui!')
  }

}

#' Strips a string to filename format for roboplots
#'
#' @param string The string that will be trunctated to filename
#' @importFrom stringr str_extract_all str_replace_all str_c str_squish
roboplot_string2filename <- function(string) {
  str_extract_all(string, "[a-z\uE5\uE4\uF6,A-Z\uC5\uC4\uD6,\\s,_,\\.,0-9]", simplify = T) |>
    str_c(collapse = "") |>
    str_squish() |>
    tolower() |>
    str_replace_all(c("\uE4" = "a", "\uE5" = "o", "\uF6" = "o", " |\\." = "_", "," = ""))
}


#' @importFrom dplyr across everything mutate rename select
#' @importFrom rlang sym quo_name
#' @importFrom stringr str_replace
#' @importFrom tidyr unite
roboplot_transform_data_for_download <- function(d, color, pattern, facet_split, plot_mode, plot_yaxis) {
  d <- d |> rename(csv.data.tiedot = !! sym(quo_name(color)))
  if(!is.null(facet_split)) { d <- unite(d, "csv.data.tiedot", .data$csv.data.tiedot, !!facet_split, sep = ", ")}
  if(!is.null(pattern)) { if(quo_name(color) != quo_name(pattern)) { d <- unite(d, "csv.data.tiedot", .data$csv.data.tiedot, !!pattern, sep = ", ") }}
  if(str_detect(plot_mode,"horizontal")) {
    if (quo_name(color) != quo_name(plot_yaxis)) {
      d <- unite(d, "csv.data.tiedot", .data[[plot_yaxis]], .data$csv.data.tiedot, sep = ", ")
    }
    }
  d |>
    select(.data$csv.data.tiedot, .data$time, .data$value) |>
    mutate(
      across(everything(), ~ as.character(.x)),
      value = str_replace(.data$value, "\\.",",")
    )
}

#' @importFrom dplyr arrange desc group_by mutate pull summarize
#' @importFrom forcats fct_inorder
#' @importFrom stringr str_c str_length str_split
roboplot_str_split_rows <- function(str, rows = 2, .forJSON = F) {
  breaker <- if(.forJSON) { "\\<b>" } else { "<br>" }
  str <- str |> str_split(" ") |> unlist()
  if(length(str) == 1) {
    str
  } else {
    tibble("word" = str) |>
      mutate(word = fct_inorder(.data$word)) |>
      arrange(desc(.data$word)) |>
      mutate(bins = cut(cumsum(str_length(.data$word)), breaks = rows, include.lowest = F, ordered_result = F, labels = F)) |>
      group_by(.data$bins) |>
      arrange(.data$word) |>
      summarize(word = str_c(.data$word, collapse = " ")) |>
      pull(.data$word) |>
      rev() |>
      str_c(collapse = breaker)
  }
}


#' @importFrom dplyr case_when
#' @importFrom padr get_interval
#' @importFrom stringr str_c
roboplot_get_dateformat <- function(d, msg = T) {

  dateformats <- c("Annual" = "%Y",
                   "Quarterly" = "%YQ%q",
                   "Monthly" = "%m/%Y",
                   "Weekly" = "%YW%V",
                   "Daily" = "%d.%m.%Y")

  get_padr_frequency <- function(ts) {
    ts <- tryCatch(get_interval(ts), error = function(e) return( NA ))
    ts <- case_when(str_detect(ts, "^day") ~ "Daily",
                    str_detect(ts, "^week") ~ "Weekly",
                    str_detect(ts, "^month") ~ "Monthly",
                    str_detect(ts, "^quarter") ~ "Quarterly",
                    str_detect(ts, "^year") ~ "Annual",
                    TRUE ~ as.character(NA))
    if(!is.na(ts)) { ts } else { NULL }

  }
  d_attrs <- attributes(d)
  wrn <- F
  tf <- if(is.null(d_attrs$frequency)) {
    wrn <- T
    get_padr_frequency(d$time)
  } else if (is.list(d_attrs$frequency)) {
    if(is.null(d_attrs$frequency$en)) {
      wrn <- T
      get_padr_frequency(d$time)
    } else{
      as.character(d_attrs$frequency$en)
    }
  } else {
    as.character(d_attrs$frequency)
  }
  if(wrn == T & msg == T) {
    wrn <- if(is.null(tf)) {"Resorting to default %Y-%m-%d" } else { str_c("Guessing frequency \"",names(dateformats[tf]),"\" for date format ",dateformats[[tf]]) }
    message(str_c("No frequency attribute was detected for hoverlabel from data 'd', and has not been provided as \"frequency\" in the argument 'hovertext'.\n", wrn,"."))
  }
  tf
}


#' Create a css file or string
#' @param css_defs css style definitions. Each object you provide must be a list of three elements.
#'   The first element will be a vector of the selectors to be styled (e.g. table, th, an id or html
#'   class). If the first element is a vector of length greater than one then the selectors will be
#'   comma separated in the css. The second element will be a vector of the css definitions and the
#'   third element will a vector of the values of those definitions.
#'
#' @param file Character sting. If a file name is provided then the css code will be printed into
#'   that file. If the argument is NULL (default) then a string will be returned.
#'
#' @importFrom htmltools HTML
#' @return css definitions.
#'
#' @examples
#' \dontrun{
#' roboplot_make_css(list(list('table', c('text-align', 'font-size'), c('center', '20px')),
#'          list('th', c('background-color', 'height'), c('lightgreen', '30px'))))
#'
#' roboplot_make_css(list(list(c('table', 'td'), c('text-align', 'font-size'), c('center', '20px')),
#'          list('th', c('background-color', 'height'), c('lightgreen', '30px'))))
#'
#' roboplot_make_css(list(list('tr:hover', c('text-align', 'font-size'), c('center', '20px')),
#'          list('th', c('background-color', 'height'), c('lightgreen', '30px'))))
#' }
#'
roboplot_make_css <- function(css_defs, file = NULL) {

  # css_defs <- list(...)

  #make sure all arguments are lists
  for (x in css_defs) {
    if ((!is.list(x)) | (length(x) != 3L)) {
      stop('Each element in css_defs needs to be a list of three elements', call. = F)
    }
    if (length(x[[2]]) != length(x[[3]])) {
      stop('The second and third elements of each list need to have the same length', call. = F)
    }
  }

  #create the css string
  all_css <-
    vapply(css_defs, function(x) {

      #make the styles
      css_comp <- paste0(x[[2]], ': ', x[[3]], ';')
      style <- paste(css_comp, collapse = '\n  ')

      #comma separate selectors if > 1
      to_be_styled <- paste(x[[1]], collapse = ',\n')

      #create a css string for the above selectors
      paste0(to_be_styled,
             ' {\n  ',
             style,
             '\n}\n')

    }, FUN.VALUE = character(1))

  #return a big string with all the selectors and style definitions
  css_string <- HTML(paste(all_css, collapse = '\n'))

  #check if file is provided and return either a file or a string
  if (is.null(file)) {
    css_string
  } else {
    cat(css_string, file = file)
    invisible(NULL)
  }

}
