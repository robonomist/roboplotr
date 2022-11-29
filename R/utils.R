#' Override the default options for colors, fonts etc. for any plot created with [roboplot()]
#'
#' @param border_colors,grid_colors,tick_colors List. Plot frame element colors. Values need to be hexadecimal colors or valid css colors, named "x" and "y".
#' @param background_color Character. Plot background color. Must be a hexadecimal color or a valid css color.
#' @param caption_defaults List. Used to parse caption. Values must be named "prefix", "lineend" and "updated". "prefix" is character, and added to caption text with ": ". "lineend" is character added to caption line ends. "updated" is logical that determines whether caption tries to guess latest update date from plot data.
#' @param dashtypes Character vector. Line trace linetypes in order of usage. Must contain all of "solid", "dash", "dot", "longdash", "dashdot", and "longdashdot" in any order.
#' @param font_main,font_title,font_caption Functions. Use [roboplot_set_font()].
#' @param height Numeric. Height of roboplotr plots in pixels.
#' @param linewidth Numeric. The default roboplotr line trace width.
#' @param logo_file Character. The filepath to the logo used in every plot.
#' @param modebar Character vector. Buttons contained in modebar in the given order. Must contain any of "home", "closest", "compare", "zoomin", "zoomout", "img_w", "img_n", "img_s", "data_dl" and "robonomist" in any order.
#' @param patterns Character vector. Line trace linetypes in order of usage. Must contain all of "", "/", "\\", "x", "-", "|", "+" and "." in any order.
#' @param trace_colors Character vector. Trace colors in order of usage. Needs to be a hexadecimal color or a valid css color. You should provide enough colors for most use cases, while roboplotr adds colors as needed.
#' @param xaxis_ceiling Character. Default rounding for yaxis limit. One of "default", "days", "months", "weeks", "quarters", "years" or "guess".
#' @param png_wide,png_narrow,png_small Functions. Use [roboplot_png_specs]. Controls the dimensions and fonts of png files downloaded through modebar buttons.
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
#' # Height can be controlled by-plot or globally.
#' #
#' d <- energiantuonti |>
#'   dplyr::filter(Alue %in% c("Kanada","Norja"), Suunta == "Tuonti")
#'
#' roboplot_set_options(
#'   border_colors = list(x = "#eed5d2", y = "#8b7d7b"),
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
#' roboplot_set_options(reset = TRUE)
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
#' # zoom. For downloading the plot data in .csv file format use "data_dl". Png
#' # image sizes are differentiated with "img_w"(ide), "img_n"(arrow) or
#' #"img_s"(mall).
#'
#' roboplot_set_options(logo_file =
#'                        system.file("images", "Rlogo.png", package = "roboplotr"),
#'                      modebar = c("home","closest","compare","zoomin","zoomout",
#'                                  "img_w","img_n","img_s","data_dl"),
#'                      verbose = "All")
#'
#' p <- d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus")
#'
#' p
#'
#' # REWRITE INCOMING
#' # Fonts are set with 'font_main', 'font_title' and 'font_caption', and are
#' # set using roboplotr::roboplot_set_fonts(), with examples under that
#' # documentation. When creating png files with
#' # roboplotr::roboplot_create_widgets(), you can specify the font sizes or
#' # large (wide or narrow) and small png files here separately, but no other
#' # font specifications are possible.
#'
#' #if(interactive()) {
#'  # roboplot_set_options(
#'    # png_large_fontsize = list(title = 12, main = 48, caption = 12),
#'    # png_small_fontsize = list(title = 12, main = 40, caption = 16),
#'  # )
#'
#'   #p <- d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus")
#'   #p |> roboplot_create_widget(filepath = tempdir(),
#'  #                             artefacts = c("png_small","png_wide"),
#'  #                             render = FALSE
#'   #)
#'   #utils::browseURL(paste0(tempdir(),"/energian_tuonti_pieni.png"))
#'   #utils::browseURL(paste0(tempdir(),"/energian_tuonti_levea.png"))
#' #}
#'
#' roboplot_set_options(reset = TRUE)
#'
#' # Captions are partly controlled by 'caption defaults', while you must
#' # provide the basic text by-plot in roboplotr::roboplot().
#' # roboplotr::roboplot_set_caption() is used on by-plot basis for more
#' # control, but you can provide global settings for some features. Provide a
#' # named list with all of "prefix", "lineend" and "updated", and captions will
#' # be changed accordingly. Use 'updated' = TRUE for roboplotr::roboplot() to
#' # try and extract the latest update from the data used.
#'
#' roboplot_set_options(
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
#' roboplot_set_options(
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
#' roboplot_set_options(reset = TRUE)
#'
#'
#' # When used inside shiny apps (assumed to use bs4Dash::dashboardPage()), run
#' # roboplotr::set_roboplot_options() in app ui header with 'shinyapp' = TRUE.
#'
#' if(interactive()) {
#'
#' ui <- bs4Dash::dashboardPage(
#'   bs4Dash::dashboardHeader(title = "Basic dashboard",
#'                            roboplot_set_options(shinyapp = TRUE)
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
roboplot_set_options <- function(
    border_colors = NULL,
    background_color = NULL,
    caption_defaults = NULL,
    dashtypes = NULL,
    font_main = NULL,
    font_title = NULL,
    font_caption = NULL,
    grid_colors = NULL,
    height = NULL,
    linewidth = NULL,
    logo_file = NULL,
    modebar = NULL,
    png_wide = NULL,
    png_narrow = NULL,
    png_small = NULL,
    patterns = NULL,
    tick_colors = NULL,
    trace_colors = NULL,
    xaxis_ceiling = NULL,
    verbose = NULL,
    shinyapp = F,
    reset = F
    ) {

  set_roboplot_option <- function(option, opt_name = NULL) {
    if(is.null(opt_name)) { opt_name <- substitute(option) }
    opt_name <- str_c("roboplot.",opt_name)
    if(!is.null(option)) { setOption(opt_name, option) }
    if(!is.null(option)) { roboplotr_message(str_c("Roboplot option ",opt_name," set.")) }
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
      roboplotr_check_param(`caption_defaults$updated`, "logical")

    }

    roboplotr_check_param(dashtypes, "character", NULL)
    roboplotr_valid_strings(dashtypes,c("solid", "dash", "dot", "longdash", "dashdot", "longdashdot"))

    roboplotr_check_param(font_main, "function", NULL, f.name = list(fun = first(substitute(font_main)), check = "roboplot_set_font"))
    roboplotr_check_param(font_title, "function", NULL, f.name = list(fun = first(substitute(font_title)), check = "roboplot_set_font"))
    roboplotr_check_param(font_caption, "function", NULL, f.name = list(fun = first(substitute(font_caption)), check = "roboplot_set_font"))

    roboplotr_check_param(grid_colors, "list", c("x","y"))
    roboplotr_valid_colors(grid_colors)

    roboplotr_check_param(height, "numeric")

    roboplotr_check_param(logo_file, "character")
    if(!is.null(logo_file)) {
      if (!file.exists(logo_file)) {
        stop("Given logo file does not seem to exist. Is the file path correct?", call. = F)
      }}

    roboplotr_check_param(modebar, "character", NULL)
    roboplotr_valid_strings(modebar, c("home","closest","compare","zoomin","zoomout","img_w","img_n","img_s","data_dl","robonomist"), any)

    roboplotr_check_param(patterns, "character", NULL)
    roboplotr_valid_strings(patterns,c("","/","\\","x","-","|","+","."))

    roboplotr_check_param(png_wide, "function", NULL, f.name = list(fun = first(substitute(png_wide)), check = "roboplot_png_specs"))
    roboplotr_check_param(png_narrow, "function", NULL, f.name = list(fun = first(substitute(png_narrow)), check = "roboplot_png_specs"))
    roboplotr_check_param(png_small, "function", NULL, f.name = list(fun = first(substitute(png_small)), check = "roboplot_png_specs"))

    roboplotr_check_param(tick_colors, "list", c("x","y"))
    roboplotr_valid_colors(tick_colors)

    roboplotr_check_param(trace_colors, "character", NULL)
    roboplotr_valid_colors(trace_colors)
    if(length(trace_colors) > 1 & length(trace_colors) < 5) { roboplotr_alert("Are you sure ",length(trace_colors)," color",ifelse(length(trace_colors)==1," is","s are")," enough?\n")}

    roboplotr_check_param(shinyapp, "logical", allow_null = F)

    roboplotr_check_param(xaxis_ceiling, "character")
    roboplotr_valid_strings(xaxis_ceiling, c("default","days","months","weeks","quarters","years","guess"), any)

    set_roboplot_option(border_colors, "colors.border")
    set_roboplot_option(background_color, "colors.background")
    set_roboplot_option(caption_defaults, "caption")
    set_roboplot_option(dashtypes)
    set_roboplot_option(font_main, "font.main")
    set_roboplot_option(font_title, "font.title")
    set_roboplot_option(font_caption, "font.caption")
    set_roboplot_option(grid_colors, "colors.grid")
    set_roboplot_option(height)
    set_roboplot_option(linewidth)
    set_roboplot_option(logo_file, "logo")
    set_roboplot_option(modebar, "modebar.buttons")
    set_roboplot_option(png_wide,"png.wide")
    set_roboplot_option(png_narrow,"png.narrow")
    set_roboplot_option(png_small,"png.small")
    # set_roboplot_option(png_large_fontsize, "png.font.size.lg")
    # set_roboplot_option(png_small_fontsize, "png.font.size.sm")
    set_roboplot_option(patterns, "patterntypes")
    set_roboplot_option(tick_colors, "colors.ticks")
    set_roboplot_option(trace_colors, "colors.traces")
    set_roboplot_option(xaxis_ceiling, "yaxis.ceiling")



    if(!str_detect(getOption("roboplot.logo"),"robonomist.png") & !"robonomist" %in% getOption("roboplot.modebar.buttons")) {
      setOption("roboplot.modebar.buttons", c(getOption("roboplot.modebar.buttons"),"robonomist"))
    }

    if(shinyapp) {

      roboplotr_alert("Reminder: roboplot_set_options() needs to be run inside the app ui header!")
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
  str_extract_all(string, "[a-z\uE5\uE4\uF6,A-Z\uC5\uC4\uD6,\\s,_,\\.,0-9]", simplify = T) |>
    str_c(collapse = "") |>
    str_squish() |>
    tolower() |>
    str_replace_all(c("\uE4" = "a", "\uE5" = "o", "\uF6" = "o", " |\\." = "_", "," = ""))
}


#' @importFrom dplyr across everything matches mutate rename select
#' @importFrom rlang sym quo_name
#' @importFrom stringr str_c str_replace str_replace_all
#' @importFrom tidyr unite
roboplotr_transform_data_for_download <- function(d, color, pattern, facet_split, plot_mode, plot_yaxis) {
  d <- d |> rename(csv.data.tiedot = !! sym(quo_name(color)))
  if(!is.null(facet_split)) { d <- unite(d, "csv.data.tiedot", .data$csv.data.tiedot, !!facet_split, sep = ", ")}
  if(!is.null(pattern)) { if(quo_name(color) != quo_name(pattern)) { d <- unite(d, "csv.data.tiedot", .data$csv.data.tiedot, !!pattern, sep = ", ") }}
  if(str_detect(plot_mode,"horizontal")) {
    if (quo_name(color) != quo_name(plot_yaxis)) {
      d <- unite(d, "csv.data.tiedot", .data[[plot_yaxis]], .data$csv.data.tiedot, sep = ", ")
    }
    }
  d |>
    select(matches(c("csv.data.tiedot","time","value"))) |>
    mutate(
      across(!matches(c("value","time")), ~ as.character(.x) |> str_replace_all("[^[:alnum:]]", "_")),
      time = str_c(.data$time),
      value = str_replace(.data$value, "\\.",",")
    )
}


#' @importFrom dplyr case_when
#' @importFrom padr get_interval
#' @importFrom stringr str_c
roboplotr_get_dateformat <- function(d, msg = T) {

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
