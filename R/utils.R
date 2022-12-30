#' Override the default options for colors, fonts etc. for any plot created with [roboplot()]
#'
#' @param artefacts Function. Control html and other file creation. Use [set_artefacts()].
#' @param border_colors,grid_colors,tick_colors List. Plot frame element colors. Values need to be hexadecimal colors or valid css colors, named "x" and "y".
#' @param background_color Character. Plot background color. Must be a hexadecimal color or a valid css color.
#' @param caption_defaults List. Used to parse caption. Values must be named "prefix", "lineend" and "updated". "prefix" is character, and added to caption text with ": ". "lineend" is character added to caption line ends. "updated" is logical that determines whether caption tries to guess latest update date from plot data.
#' @param dashtypes Character vector. Line trace linetypes in order of usage. Must contain all of "solid", "dash", "dot", "longdash", "dashdot", and "longdashdot" in any order.
#' @param font_main,font_title,font_caption Functions. Use [set_font()].
#' @param height,width Numerics. Height and width of roboplotr plots in pixels.
#' @param linewidth Numeric. The default roboplotr line trace width.
#' @param locale Function. Defines locale parameters as [roboplot()] needs them. Use [set_artefacts()].
#' @param logo_file Character. The filepath to the logo used in every plot.
#' @param modebar Character vector. Buttons contained in modebar in the given order. Must contain any of "home", "closest", "compare", "zoomin", "zoomout", "img_w", "img_n", "img_s", "data_dl" and "robonomist" in any order.
#' @param patterns Character vector. Line trace linetypes in order of usage. Must contain all of "", "/", "\\", "x", "-", "|", "+" and "." in any order.
#' @param trace_colors Character vector. Trace colors in order of usage. Needs to be a hexadecimal color or a valid css color. You should provide enough colors for most use cases, while roboplotr adds colors as needed.
#' @param xaxis_ceiling Character. Default rounding for yaxis limit. One of "default", "days", "months", "weeks", "quarters", "years" or "guess".
#' @param imgdl_wide,imgdl_narrow,imgdl_small Functions. Use [set_imgdl_layout]. Controls the dimensions and fonts of image files downloaded through modebar buttons.
#' @param verbose Character. Will roboplot display all messages, alerts and warnings, or warnings only? Must be one of "All", "Alert", or "Warning".
#' @param shinyapp Logical. Makes fonts, css and javascript available for shiny apps.
#' @param reset Logical. Ignores other options, resets options to defaults.
#' @export
#' @examples
#' # Control global options for roboplotr::roboplot(). Some of these you can set
#' # also in roboplotr::roboplot(), some are available only globally.
#'
#'
#' # Basic plot frame colors for ticks, grid and border must be defined by axis
#' # with lists of color hex codes or valid css colors. Same with plot backround.
#' # Height can be controlled by-plot or globally. You can also preconstruct
#' # some defaults for captions.
#'
#' d <- energiantuonti |>
#'   dplyr::filter(Alue %in% c("Kanada","Norja"), Suunta == "Tuonti")
#'
#' set_roboplot_options(
#'   border_colors = list(x = "#eed5d2", y = "#8b7d7b"),
#'   caption = list(prefix = "Lähde: ", lineend = ".", updated = FALSE),
#'   grid_colors = list(x = "#9aff9a", y = "cornsilk"),
#'   tick_colors = list(x = "darkgray", y = "dimgrey"),
#'   background_color = "ghostwhite",
#'   height = 700,
#' )
#'
#' p <- d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus")
#'
#' p
#'
#' # When testing different options, you can use 'reset' to reset default values.
#'
#' set_roboplot_options(reset = TRUE)
#'
#' p <- d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus")
#'
#' p
#'
#' # You can set the displayed lower right logo by 'logo_file'. Logo file needs
#' # the filepath to the logo used. The logo will be automatically scaled for
#' # roboplotr::roboplot() usage. If the logo is not Robonomist logo, one will
#' # be automatically added to modebar (or you can add it manually as
#' # "robonomist").  Use 'verbose' to control if roboplotr writes all messages,
#' # alerts and warnings only, or warnings only.
#'
#' # Of modebar options, "closest" and "compare" control on hover comparison
#' # points, and "zoomin" and "zoomout" are simple zoom buttons. You will
#' # probably want "home" along with the previous options for resetting the
#' # zoom. For downloading the plot data in .csv file format use "data_dl".
#' # Image sizes for downloaded image files are differentiated with
#' # "img_w"(ide), "img_n"(arrow) or "img_s"(mall).
#'
#' set_roboplot_options(
#'   logo_file = system.file("images", "Rlogo.png", package = "roboplotr"),
#'   modebar = c("home","closest","compare","zoomin","zoomout",
#'               "img_w","img_n","img_s","data_dl"),
#'   verbose = "All")
#'
#' p <- d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus")
#'
#' p
#'
#' # Images downloaded through plotly modebar require separate layout
#' # specifications which roboplotr::roboplot automatically takes care of. Image
#' # sizes and font sizes might require extra specifications. Set these globally
#' # with 'img_wide', 'img_narrow', and / or 'img_small' using
#' # roboplotr::set_imgdl_layout(), documented in detail in that
#' # function.
#'
#' set_roboplot_options(imgdl_wide = set_imgdl_layout(width = 1600))
#'
#'
#' # Captions are partly controlled by 'caption defaults', while you must
#' # provide the basic text by-plot in roboplotr::roboplot().
#' # roboplotr::set_caption() is used on by-plot basis for more
#' # control, but you can provide global settings for some features. Provide a
#' # named list with all of "prefix", "lineend" and "updated", and captions will
#' # be changed accordingly. Use 'updated' = TRUE for roboplotr::roboplot() to
#' # try and extract the latest update from the data used.
#'
#' set_roboplot_options(
#'   caption_defaults = list(prefix = "Lähteenä", lineend = "", updated = TRUE)
#' )
#'
#' p <- d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus")
#'
#' p
#'
#' # Trace appearance is controlled globally by 'dashtypes', 'linewidth',
#' # 'patterns' and 'trace_colors'. Trace color you can determine by plot, the
#' # other options are available only globally. Linewidth simply controls the
#' # line width, but the other options are used specifically in the given order
#' # for roboplotr::roboplot() variables as factors. With only a few trace
#' # colors given roboplotr::roboplot() might struggle with a lot of traces, so
#' # it alerts the user for this. When provided, 'yaxis_ceiling' will leave
#' # either a predefined gap between x-axis end and the plot edge, or try to
#' # guess an appropriate gap. This can also be controlled by-plot and will not
#' # work with bar plots.
#'
#' set_roboplot_options(
#'   dashtypes = c("longdash", "dashdot", "longdashdot", "solid", "dash", "dot"),
#'   linewidth = 4,
#'   patterns = c("x","-","|","+",".","","/","\\"),
#'   trace_colors = c("#027f93", "#153a42", "#f78b04"),
#'   xaxis_ceiling = "guess"
#' )
#'
#' d <- energiantuonti |> dplyr::filter(Alue %in% c("Kanada","Norja"))
#'
#' p <- d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus", pattern = Suunta)
#'
#' p
#'
#' p <- d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus",
#'                    plot_type = "bar",
#'                    pattern = Suunta)
#'
#' p
#'
#' set_roboplot_options(reset = TRUE)
#'
#'
#' # When used inside shiny apps (assumed to use bs4Dash::dashboardPage()), run
#' # roboplotr::set_roboplot_options() in app ui header with 'shinyapp' = TRUE.
#'
#' if(interactive()) {
#'
#' ui <- bs4Dash::dashboardPage(
#'   bs4Dash::dashboardHeader(title = "Basic dashboard",
#'                            set_roboplot_options(shinyapp = TRUE)
#'   ),
#'   bs4Dash::dashboardSidebar(),
#'   bs4Dash::dashboardBody(
#'     # Boxes need to be put in a row (or column)
#'     shiny::fluidRow(
#'       bs4Dash::box(plotly::plotlyOutput("plot1", height = 550)),
#'       bs4Dash::box(plotly::plotlyOutput("plot2", height = 500))
#'     )
#'   )
#' )
#'
#' server <- function(input, output) {
#'
#' output$plot1 <- plotly::renderPlotly({
#'   roboplot(
#'     dplyr::filter(energiantuonti, Suunta == "Tuonti", Alue == "Kanada"),
#'     Alue, "Energian tuonti Kanadasta",
#'     "Milj. €",
#'     "Tilastokeskus")
#' })
#' output$plot2 <- plotly::renderPlotly({
#'   roboplot(
#'     dplyr::filter(energiantuonti, Suunta == "Vienti", Alue == "Kanada"),
#'     Alue, "Energian vienti Kanadaan",
#'     "Milj. €",
#'     "Tilastokeskus",
#'     height = 500)
#' })
#' }
#'
#' shiny::shinyApp(ui, server)
#'
#' }
#'
#' @importFrom htmltools singleton tagList tags
#' @importFrom purrr iwalk
#' @importFrom shiny addResourcePath
#' @importFrom stringr str_c str_detect str_extract str_subset
#' @importFrom R.utils setOption
set_roboplot_options <- function(
    artefacts = NULL,
    border_colors = NULL,
    background_color = NULL,
    caption_defaults = NULL,
    dashtypes = NULL,
    font_main = NULL,
    font_title = NULL,
    font_caption = NULL,
    grid_colors = NULL,
    height = NULL,
    imgdl_wide = NULL,
    imgdl_narrow = NULL,
    imgdl_small = NULL,
    linewidth = NULL,
    locale = NULL,
    logo_file = NULL,
    modebar = NULL,
    patterns = NULL,
    tick_colors = NULL,
    trace_colors = NULL,
    xaxis_ceiling = NULL,
    verbose = NULL,
    width = NULL,
    shinyapp = F,
    reset = F
    ) {

  set_roboplot_option <- function(option, opt_name = NULL) {
    if(!is.null(option)) {
      if(is.null(opt_name)) {
        opt_name <- deparse(substitute(option))
        opt_name <- str_c("roboplot.",opt_name)
      } else {
        opt_name <- str_c("roboplot.",opt_name)
      }
      setOption(opt_name, option)
      roboplotr_message(str_c("Roboplot option ",opt_name," set."))
      }
  }

  roboplotr_check_param(verbose, "character", allow_null = T)
  roboplotr_valid_strings(verbose, c("All","Alert","Warning"), any)
  set_roboplot_option(verbose, "verbose")

  if (reset) {
    .onLoad(override = T)
    roboplotr_message("Roboplot options reset.")
  } else {
    opts_names <- names(options())
    # reset session specific options
    widget_deps_names <- subset(opts_names, str_detect(opts_names, "^roboplot.widget.deps"))
    for(opt in widget_deps_names) {
      setOption(opt, NULL)
    }

    roboplotr_check_param(artefacts, "function", NULL, f.name = list(fun = first(substitute(artefacts)), check = "set_artefacts"))

    roboplotr_check_param(border_colors, "list", c("x","y"))
    roboplotr_valid_colors(border_colors)
    roboplotr_check_param(background_color, "character")
    roboplotr_valid_colors(background_color)

    roboplotr_check_param(caption_defaults, "list", c("prefix","lineend","updated"))
    if(!is.null(caption_defaults)) {
      `caption_defaults$prefix` <- caption_defaults$prefix
      `caption_defaults$lineend` <- caption_defaults$lineend
      `caption_defaults$updated` <- caption_defaults$updated
      roboplotr_check_param(`caption_defaults$prefix`, "character", allow_null = F)
      roboplotr_check_param(`caption_defaults$lineend`, "character", allow_null = F)
      roboplotr_check_param(`caption_defaults$updated`, "logical", allow_null = F)

    }

    roboplotr_check_param(dashtypes, "character", NULL)
    roboplotr_valid_strings(dashtypes,c("solid", "dash", "dot", "longdash", "dashdot", "longdashdot"))

    roboplotr_check_param(font_main, "function", NULL, f.name = list(fun = first(substitute(font_main)), check = "set_font"))
    roboplotr_check_param(font_title, "function", NULL, f.name = list(fun = first(substitute(font_title)), check = "set_font"))
    roboplotr_check_param(font_caption, "function", NULL, f.name = list(fun = first(substitute(font_caption)), check = "set_font"))

    roboplotr_check_param(grid_colors, "list", c("x","y"))
    roboplotr_valid_colors(grid_colors)

    roboplotr_check_param(height, "numeric")

    roboplotr_check_param(linewidth, "numeric")

    roboplotr_check_param(locale, "function", NULL, f.name = list(fun = first(substitute(locale)), check = "set_locale"))

    roboplotr_check_param(logo_file, "character")
    if(!is.null(logo_file)) {
      if (!file.exists(logo_file)) {
        stop("Given logo file does not seem to exist. Is the file path correct?", call. = F)
      }}

    roboplotr_check_param(modebar, "character", NULL)
    roboplotr_valid_strings(modebar, c("home","closest","compare","zoomin","zoomout","img_w","img_n","img_s","data_dl","robonomist"), any)

    roboplotr_check_param(patterns, "character", NULL)
    roboplotr_valid_strings(patterns,c("","/","\\","x","-","|","+","."))

    roboplotr_check_param(imgdl_wide, "function", NULL, f.name = list(fun = first(substitute(imgdl_wide)), check = "set_imgdl_layout"))
    roboplotr_check_param(imgdl_narrow, "function", NULL, f.name = list(fun = first(substitute(imgdl_narrow)), check = "set_imgdl_layout"))
    roboplotr_check_param(imgdl_small, "function", NULL, f.name = list(fun = first(substitute(imgdl_small)), check = "set_imgdl_layout"))

    roboplotr_check_param(tick_colors, "list", c("x","y"))
    roboplotr_valid_colors(tick_colors)

    roboplotr_check_param(trace_colors, "character", NULL)
    roboplotr_valid_colors(trace_colors)
    if(length(trace_colors) > 1 & length(trace_colors) < 5) { roboplotr_alert("Are you sure ",length(trace_colors)," color",ifelse(length(trace_colors)==1," is","s are")," enough?\n")}

    roboplotr_check_param(shinyapp, "logical", allow_null = F)

    roboplotr_check_param(width, "numeric")

    roboplotr_check_param(xaxis_ceiling, "character")
    roboplotr_valid_strings(xaxis_ceiling, c("default","days","months","weeks","quarters","years","guess"), any)

    set_roboplot_option(artefacts)
    set_roboplot_option(border_colors, "colors.border")
    set_roboplot_option(background_color, "colors.background")
    set_roboplot_option(caption_defaults, "caption")
    set_roboplot_option(dashtypes)
    set_roboplot_option(font_main, "font.main")
    set_roboplot_option(font_title, "font.title")
    set_roboplot_option(font_caption, "font.caption")
    set_roboplot_option(grid_colors, "colors.grid")
    set_roboplot_option(height)
    set_roboplot_option(locale)
    set_roboplot_option(linewidth)
    set_roboplot_option(logo_file, "logo")
    set_roboplot_option(modebar, "modebar.buttons")
    set_roboplot_option(imgdl_wide,"imgdl.wide")
    set_roboplot_option(imgdl_narrow,"imgdl.narrow")
    set_roboplot_option(imgdl_small,"imgdl.small")
    set_roboplot_option(patterns, "patterntypes")
    set_roboplot_option(tick_colors, "colors.ticks")
    set_roboplot_option(trace_colors, "colors.traces")
    set_roboplot_option(width)
    set_roboplot_option(xaxis_ceiling, "xaxis.ceiling")



    if(!str_detect(getOption("roboplot.logo"),"robonomist.png") & !"robonomist" %in% getOption("roboplot.modebar.buttons")) {
      setOption("roboplot.modebar.buttons", c(getOption("roboplot.modebar.buttons"),"robonomist"))
    }

    if(shinyapp) {

      roboplotr_alert("Reminder: set_roboplot_options() needs to be run inside the app ui header!")
      roboplotr_widget_deps(tempdir())
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

    }

  }
}

# Strips a string to filename format for roboplots
#
# @param string The string that will be trunctated to filename
#' @importFrom stringr str_extract_all str_replace_all str_c str_squish
roboplotr_string2filename <- function(string) {
  str_extract_all(string, "[a-z\uE5\uE4\uF6,A-Z\uC5\uC4\uD6,\\s,_,\\.,0-9,-]", simplify = T) |>
    str_c(collapse = "") |>
    str_squish() |>
    tolower() |>
    str_replace_all(c("\uE4" = "a", "\uE5" = "o", "\uF6" = "o", " |\\.|-" = "_", "," = ""))
}

#' Used to set locale parameters for [roboplot()] in [set_roboplot_options()]
#'
#' @param locale Character. Currently supports on only "en-GB", "en-US",
#' "sv-SE", or "fi-FI" (the default).
#' @examples
#' # You might want to display dates or numbers with some another default format
#' # for decimal marks or thousand marks.
#'
#' set_roboplot_options(
#'   locale = set_locale("en-GB"),
#'   caption_defaults = list(prefix = "Source: ", lineend = ".", updated = FALSE)
#'   )
#'
#' d <- energiantuonti |>
#'   dplyr::filter(Alue == "Kanada", Suunta == "Tuonti") |>
#'   dplyr::mutate(value = value * 1000000, Alue = "Canada")
#'
#' d |>
#'   roboplot(color = Alue,
#'            title = "Energy import",
#'            subtitle = "Mil. \u20AC",
#'            caption = "Statistics Finland")
#'
#' # This works on dates, too, if you need to show an exact date. In this
#' # example you wouldn't, and it only matters on the hoverlabel here, but let's
#' # force roboplotr() to display dates at by-day accuracy for the sake of example
#' # by using data frequency of "Daily".
#'
#' attr(d, "frequency") <- "Daily"
#'
#' d |> roboplot(color = Alue,
#'               title = "Energy import",
#'               subtitle = "Mil. \u20AC",
#'               caption = "Statistics Finland")
#'
#' # Revert to defaults:
#' set_roboplot_options(reset = TRUE)
#'
#' @return A list
#' @export
#' @importFrom dplyr case_when
set_locale <- function(locale = "fi-FI") {
  roboplotr_check_param(locale, "character", allow_null = F)
  roboplotr_valid_strings(locale, c("en-GB","en-US","sv-SE","fi-FI"), any)
  loc <- case_when(locale == "en-GB" ~ "en", locale == "en-US" ~ "en-US", locale == "sv-SE" ~ "sv", TRUE ~ "fi")
  sep <- case_when(loc %in% c("en", "en-US") ~ ",.", TRUE ~ ", ")
  dat <- case_when(loc == "en" ~ "%-d/%-m/%Y", loc == "en-US" ~ "%-m/%-d/%Y", TRUE ~ "%-d.%-m.%Y")
  list(locale = loc, separators = sep, date = dat)
}

#' @importFrom dplyr across everything matches mutate rename select
#' @importFrom rlang sym quo_name
#' @importFrom stringr str_c str_replace str_replace_all
#' @importFrom tidyr unite
roboplotr_transform_data_for_download <- function(d, color, pattern, plot_axes) {
  if (!is.null(color)) {  color <- as_name(color) }
  if (!is.null(pattern)) {  pattern <- as_name(pattern) }
  # d <- d |> rename(csv.data.tiedot = !! sym(quo_name(color)))
  # if(!is.null(facet_split)) { d <- unite(d, "csv.data.tiedot", .data$csv.data.tiedot, !!facet_split, sep = ", ")}
  # if(!is.null(pattern)) { if(quo_name(color) != quo_name(pattern)) { d <- unite(d, "csv.data.tiedot", .data$csv.data.tiedot, !!pattern, sep = ", ") }}
  # if(str_detect(plot_mode,"horizontal")) {
  #   if (quo_name(color) != quo_name(plot_yaxis)) {
  #     d <- unite(d, "csv.data.tiedot", .data[[plot_yaxis]], .data$csv.data.tiedot, sep = ", ")
  #   }
  #   }
  d <- d |>
    select(matches(c(plot_axes$y, color, pattern, plot_axes$x)), -matches("roboplot.topic")) |>
    mutate(
      across(!is.numeric, ~ as.character(.x) |> roboplotr_transform_string()), #note: semi-colon as colon for final csv
      across(is.numeric, ~ as.character(.x) |> str_replace_all("\\.", ","))
    )

  d

}

#' @importFrom stringr str_replace_all
roboplotr_transform_string <- function(string) {
  str_replace_all(string, c("[^[:alnum:]\\s\\,\\;\\',\\&,\\%,\\-]"= "_","'"="\u2019", "\\&" = "\u0026","\\%" = "\\u0025"))
}

#' @importFrom dplyr case_when
#' @importFrom padr get_interval
#' @importFrom stringr str_c
roboplotr_get_dateformat <- function(d, msg = T) {

  dateformats <- c("Annual" = "%Y",
                   "Quarterly" = "%YQ%q",
                   "Monthly" = "%m/%Y",
                   "Weekly" = "%YW%V",
                   "Daily" = getOption("roboplot.locale")$date)

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
  datecol <- map(names(d), ~ if("Date" %in% class(d[[.x]])) { .x }) |> roboplotr_compact() |> first()
  if(length(datecol) == 0) {
    tf <- NULL
  } else {
    tf <- if(is.null(d_attrs$frequency)) {
      wrn <- T
      get_padr_frequency(d[[datecol]])
    } else if (is.list(d_attrs$frequency)) {
      if(is.null(d_attrs$frequency$en)) {
        wrn <- T
        get_padr_frequency(d[[datecol]])
      } else{
        as.character(d_attrs$frequency$en)
      }
    } else {
      as.character(d_attrs$frequency)
    }
  }
  if(wrn == T & msg == T) {
    wrn <- if(is.null(tf)) {"Resorting to default %Y-%m-%d" } else { str_c("Guessing frequency \"",names(dateformats[tf]),"\" for date format ",dateformats[[tf]]) }
    roboplotr_message(str_c("No frequency attribute was detected for hoverlabel from data 'd', and has not been provided as \"frequency\" in the argument 'hovertext'.\n", wrn,"."))
  }
  tf
}

#' @importFrom crayon black green yellow red
#' @importFrom stringr str_c
roboplotr_messages <- function(string, type = c("message","alert","warning")) {
  type <- match.arg(type, c("message","alert","warning"))
  .fun <- switch(type, "message" = green, "alert" = yellow,"warning" = red)
  if(type == "message") {
    message(str_c(.fun(str_c("roboplotr ",type,": ")),black(string)))
  } else {
    message(.fun(str_c("roboplotr ",type,": ",string)))
  }
}

roboplotr_message <- function(...) { if(getOption("roboplot.verbose") == "All") { roboplotr_messages(str_c(...), "message") } }
roboplotr_alert <- function(...) { if(getOption("roboplot.verbose") %in% c("All","Alert")) { roboplotr_messages(str_c(...), "alert") } }
roboplotr_warning <- function(...) { roboplotr_messages(str_c(...), "warning") }

# Create a css file or string
# @param css_defs css style definitions. Each object you provide must be a list of three elements.
#   The first element will be a vector of the selectors to be styled (e.g. table, th, an id or html
#   class). If the first element is a vector of length greater than one then the selectors will be
#   comma separated in the css. The second element will be a vector of the css definitions and the
#   third element will a vector of the values of those definitions.
#
# @param file Character sting. If a file name is provided then the css code will be printed into
#   that file. If the argument is NULL (default) then a string will be returned.
#
#' @importFrom htmltools HTML
# @return css definitions.
#
# @examples
# \dontrun{
# roboplotr_get_css(list(list('table', c('text-align', 'font-size'), c('center', '20px')),
#          list('th', c('background-color', 'height'), c('lightgreen', '30px'))))
#
# roboplotr_get_css(list(list(c('table', 'td'), c('text-align', 'font-size'), c('center', '20px')),
#          list('th', c('background-color', 'height'), c('lightgreen', '30px'))))
#
# roboplotr_get_css(list(list('tr:hover', c('text-align', 'font-size'), c('center', '20px')),
#          list('th', c('background-color', 'height'), c('lightgreen', '30px'))))
# }

roboplotr_get_css <- function(css_defs, file = NULL) {

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

roboplotr_combine_words <- function (..., sep = ", ", and = " and ", before = "", after = before,
          oxford_comma = F)
{
  words <- c(...)
  n = length(words)
  if (n == 0)
    return(words)
  words = paste0(before, words, after)
  if (n == 1)
    return(words)
  if (n == 2)
    return(paste(words, collapse = if (is.null(and)) sep else and))
  if (oxford_comma && grepl("^ ", and) && grepl(" $", sep))
    and = gsub("^ ", "", and)
  words[n] = paste0(and, words[n])
  if (!oxford_comma) {
    words[n - 1] = paste0(words[n - 1:0], collapse = "")
    words = words[-n]
  }
  paste(words, collapse = sep)
}

roboplotr_compact <- function (l)
{
  Filter(Negate(is.null), l)
}
