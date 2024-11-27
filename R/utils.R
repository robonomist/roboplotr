#' Global configuration of various `roboplotr` outputs
#'
#' Parameters to add and customize the various options of used by `roboplotr`.
#'
#' @param accessible Logical. Forces trace colors accessible against plot background.
#' @param artefacts Function. Control html and other file creation. Use [set_artefacts()].
#' @param border Function. Control plot borders. Use [set_border()].
#' @param grid Function. Control the grid behind the traces. Use [set_grid()].
#' @param background_color Character. Plot background color. Must be a hexadecimal
#' color or a valid css color.
#' @param caption_template Character. Template for [stringr::str_glue()] used to
#' parse captions.
#' @param dashtypes Character vector. Line trace linetypes in order of usage. Must
#' contain all of "solid", "dash", "dot", "longdash", "dashdot", and "longdashdot" in any order.
#' @param font_main,font_title,font_caption Functions. Use [set_font()].
#' @param height,width Double. Height and width of roboplotr plots in pixels.
#' Use NA for viewport size.
#' @param imgdl_wide,imgdl_narrow,imgdl_small Functions. Use [set_imgdl_layout].
#' Controls the dimensions and fonts of exports through modebar.
#' @param infobox Function. Defines the appearance of the infobox when `info_text`
#' is provided for [roboplots][roboplot()] or [robotables][robotable()]. Use [set_infobox()].
#' @param linewidth Numeric. The default `roboplot()` line trace width.
#' @param locale Function. Defines locale parameters as `roboplotr` needs them.
#' Use [set_locale()].
#' @param logo_file Character. The filepath to the logo used in every plot. Use
#' "none" for no logo.
#' @param markers Function. Control marker appearance. Use [set_markers()].
#' @param modebar Character vector or function. Use [set_modebar()], or provide
#' the buttons contained in modebar in the given order. Must contain any of "home",
#' "closest", "compare", "zoomin", "zoomout", "img_w", "img_n", "img_s", "data_dl"
#' and "robonomist" in any order.
#' @param patterns Character vector. Line trace linetypes in order of usage. Must
#' contain all of "", "/", "\\", "x", "-", "|", "+" and "." in any order.
#' @param rounding Double. Default rounding for numeric values across various roboplotr functions.
#' @param trace_border List. Borders for bars, pies and markers. Values must be
#' named "color" and "width". "color" needs to be a hexadecimal color or a
#' valid css color, "width" needs to be numeric.
#' @param trace_colors Character vector. Trace colors in order of usage. Needs to
#' be hexadecimal colors or valid css colors. You should provide enough colors for
#' most use cases, although `roboplotr` is able to extrapolate.
#' @param xaxis_ceiling Character. Default rounding for yaxis limit. One of "default",
#' "days", "months", "weeks", "quarters", "years" or "guess".
#' @param tidy_legend Logical. Controls whether the legend items will have matching
#' widths, making for neater legends, or containing text widths, saving space.
#' @param zeroline Function. Control the appearance of zeroline when set using [roboplot()]
#' parameter `zeroline`. Use [set_zeroline()].
#' @param verbose Character. Will roboplotr display all messages, alerts and warnings,
#' or warnings only? Must be one of "All", "Alert", or "Warning".
#' @param shinyapp Logical. Makes fonts, css and javascript available for shiny apps.
#' @param reset Logical or character. Resets options to roboplotr defaults or to
#' defaults you have previously specified with `set_roboplot_options(name)`.
#' @param name Character. Saves the current roboplot options as defaults for future
#' calls of `set_roboplot_options(reset)` or `roboplot(roboplot_options)`.
#' @param .defaults Deprecated. Use `name` instead.
#' @export
#' @examples
#' # Control global options for `roboplotr`. Some of these you can set
#' # also in `roboplot()`, `robotable()` or `robomap()`, some are available only
#' # globally.
#'
#'
#' # Basic plot frame colors for ticks, grid and border, background, height, and
#' # caption controls work like this:
#'
#' d <- energiantuonti |>
#'   dplyr::filter(Alue %in% c("USA","Norja"), Suunta == "Tuonti")
#'
#' set_roboplot_options(
#'   border = set_border(xcolor = "#eed5d2", ycolor = "#8b7d7b"),
#'   caption_template = "Lähde: {text}.",
#'   grid = set_grid(xcolor = "#9aff9a", ycolor = "cornsilk"),
#'   background_color = "ghostwhite",
#'   height = 700,
#' )
#'
#' d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus")
#'
#' # When testing different options, you can use 'reset' to reset default values.
#'
#' set_roboplot_options(reset = TRUE)
#'
#' d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus")
#'
#' # You can also name your options for later use. Set some options and reset them:
#'
#' set_roboplot_options(background_color = "whitesmoke", name = "smoke_background")
#' set_roboplot_options(reset = TRUE)
#'
#' d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus")
#'
#' # You can now reset to the named options:
#'
#' set_roboplot_options(reset = "smoke_background")
#' d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus")
#'
#' # You can also tell `roboplot()` to use a specified option set without changing
#' # them for all plots.
#'
#' set_roboplot_options(reset = TRUE)
#' d |> roboplot(Alue,
#'               "Energian tuonti",
#'               "Milj €",
#'               "Tilastokeskus",
#'               roboplot_options = "smoke_background")
#'
#'
#' # You can set the displayed lower right logo by 'logo_file'. Logo file needs
#' # the filepath to the logo used. The logo will be automatically scaled for
#' # `roboplotr` usage. If the logo is not the Robonomist logo, one will
#' # be automatically added to modebar (or you can add it manually as
#' # "robonomist").
#'
#' # Control modebar.
#'
#' set_roboplot_options(
#'   logo_file = system.file("images", "robonomist.png", package = "roboplotr"),
#'   modebar = set_modebar(
#'     buttons = c(
#'       "home",
#'       "closest",
#'       "compare",
#'       "zoomin",
#'       "zoomout",
#'       "img_w",
#'       "img_n",
#'       "img_s",
#'       "data_dl"
#'     )
#'   )
#' )
#'
#' d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus")
#'
#' # Files downloaded through plotly modebar require separate layout
#' # specifications which `roboplot()` automatically takes care of. Plot
#' # dimensions and font sizes might require extra specifications. Set these
#' # globally with `img_wide`, `img_narrow`, and / or `img_small` using
#' # `set_imgdl_layout()`.
#'
#' set_roboplot_options(imgdl_wide = set_imgdl_layout(width = 1600))
#'
#'
#' # Captions are partly controlled by `caption template`, while you will probably
#' # want to provide the actual content in `roboplot()`.
#'
#' set_roboplot_options(
#'   caption_template = "Lähteenä: {text}."
#' )
#'
#' d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus")
#'
#' # Trace appearance controls can be set globally.
#'
#' set_roboplot_options(
#'   dashtypes = c("longdash", "dashdot", "longdashdot", "solid", "dash", "dot"),
#'   linewidth = 4,
#'   markers = set_markers(symbol = "diamond", size = 12),
#'   patterns = c("x","-","|","+",".","","/","\\"),
#'   trace_colors = c("#027f93", "#153a42", "#f78b04"),
#'   xaxis_ceiling = "guess"
#' )
#'
#' d <- energiantuonti |> dplyr::filter(Alue %in% c("USA","Norja"))
#'
#' d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus", pattern = Suunta)
#'
#' d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus",
#'                    plot_type = "bar",
#'                    pattern = Suunta)
#'
#' d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus",
#'                    plot_mode = "scatter+line",
#'                    pattern = Suunta)
#'
#'
#' set_roboplot_options(reset = TRUE)
#'
#'
#' # When used inside shiny apps, run `set_roboplot_options()` in app ui head with
#' # `shinyapp` = TRUE.
#'
#' \dontrun{
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
#'     dplyr::filter(energiantuonti, Suunta == "Tuonti", Alue == "USA"),
#'     Alue, "Energian tuonti Yhdysvalloista",
#'     "Milj. €",
#'     "Tilastokeskus")
#' })
#' output$plot2 <- plotly::renderPlotly({
#'   roboplot(
#'     dplyr::filter(energiantuonti, Suunta == "Vienti", Alue == "USA"),
#'     Alue, "Energian vienti Yhdysvaltoihin",
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
#'
#' @importFrom htmltools singleton tagList tags
#' @importFrom purrr iwalk walk2
#' @importFrom shiny addResourcePath
#' @importFrom stringr str_c str_detect str_extract str_subset
set_roboplot_options <- function(
    accessible = NULL,
    artefacts = NULL,
    border = NULL,
    background_color = NULL,
    caption_template = NULL,
    dashtypes = NULL,
    font_main = NULL,
    font_title = NULL,
    font_caption = NULL,
    grid = NULL,
    height = NULL,
    imgdl_wide = NULL,
    imgdl_narrow = NULL,
    imgdl_small = NULL,
    infobox = NULL,
    linewidth = NULL,
    locale = NULL,
    logo_file = NULL,
    markers = NULL,
    modebar = NULL,
    name = NULL,
    patterns = NULL,
    rounding = NULL,
    tidy_legend = NULL,
    trace_border = NULL,
    trace_colors = NULL,
    xaxis_ceiling = NULL,
    zeroline = NULL,
    verbose = NULL,
    width = NULL,
    shinyapp = NULL,
    reset = NULL,
    .defaults
) {

  set_roboplot_option <- function(option, opt_name = NULL) {
    if(!is.null(option)) {
      if(is.null(opt_name)) {
        opt_name <- deparse(substitute(option))
        opt_name <- str_c("roboplot.",opt_name)
      } else {
        opt_name <- str_c("roboplot.",opt_name)
      }
      options(setNames(list(option), opt_name))
      roboplotr_message(str_c("Roboplot option ",opt_name," set."))
    }
  }

  roboplotr_override_webshot_screenshot()

  roboplotr_typecheck(verbose, "character", allow_null = T)
  roboplotr_valid_strings(verbose, c("All","Alert","Warning"), any)
  set_roboplot_option(verbose, "verbose")

  roboplotr_typecheck(reset, c("logical","character"), allow_null = T)

  if(is.logical(reset)) {
    if (reset) {
      if(interactive()) {
        deprecate_warn("2.3.0", "roboplotr::set_roboplot_options(reset = 'will only reset to \"roboplotr\" defaults when TRUE, use a named option for custom defaults')")
      }
      if(is.null(getOption("roboplotr.options.defaults"))) {
        .onLoad(override = T)
      } else {
        .roboplotr_defaults <- getOption("roboplotr.options.defaults")
        options(.roboplotr_defaults)
        options("roboplot.cur.options" = "defaults")
      }
      roboplotr_message("Roboplot options reset.")
    }
  } else if (is.character(reset)) {
    if(reset == "roboplotr") {
      .onLoad(override = T)
    } else {
      .this_reset <- getOption(str_glue("roboplotr.options.{reset}"))
      if(is.null(.this_reset)) {
        stop(str_glue("You have not specified reset options named {reset}! Use set_roboplot_options(name = \"{reset}\") to set them."), call. = F)
      }
      options(.this_reset)
      options("roboplot.cur.options" = reset)
    }
  }

    opts_names <- names(options())
    # reset session specific options
    widget_deps_names <- subset(opts_names, str_detect(opts_names, "^roboplot.widget.deps"))
    for(opt in widget_deps_names) {
      .Options[[opt]] <- NULL
    }

    # print(accessible)
    roboplotr_typecheck(accessible, "logical")

    roboplotr_typecheck(artefacts, "set_artefacts")

    roboplotr_typecheck(border, "set_border")

    roboplotr_typecheck(background_color, "character")
    roboplotr_valid_colors(background_color)

    roboplotr_typecheck(caption_template, "character")

    roboplotr_typecheck(dashtypes, "character", NULL)
    roboplotr_valid_strings(dashtypes,c("solid", "dash", "dot", "longdash", "dashdot", "longdashdot"))

    font_main <- substitute(font_main)
    if(!is.null(font_main)) {
      if(font_main[1] != "set_font()" & font_main[1] != "roboplotr::set_font()") { stop("Use 'roboplotr::set_font()' for font_main!", call. = F)}
      if(is.null(font_main$type)) { font_main$type <- "main" }
      font_main <- eval(font_main)
    }

    font_title <- substitute(font_title)
    if(!is.null(font_title)) {
      if(font_title[1] != "set_font()" & font_title[1] != "roboplotr::set_font()") { stop("Use 'roboplotr::set_font()' for font_title!", call. = F)}
      if(is.null(font_title$type)) { font_title$type <- "title" }
      font_title <- eval(font_title)
    }

    font_caption <- substitute(font_caption)
    if(!is.null(font_caption)) {
      if(font_caption[1] != "set_font()" & font_caption[1] != "roboplotr::set_font()") { stop("Use 'roboplotr::set_font()' for font_caption!", call. = F)}
      if(is.null(font_caption$type)) { font_caption$type <- "caption" }
      font_caption <- eval(font_caption)
    }

    roboplotr_typecheck(grid, "set_grid")

    roboplotr_typecheck(height, "numeric", allow_na = T)

    roboplotr_typecheck(infobox, "set_infobox")

    roboplotr_typecheck(linewidth, "numeric")

    roboplotr_typecheck(locale, "set_locale")

    roboplotr_typecheck(logo_file, "character")
    if(!is.null(logo_file)) {
      if(logo_file == "none") {
        logo_file <- system.file("images","none.png",package = "roboplotr")
      } else if (!file.exists(logo_file)) {
        stop("Given logo file does not seem to exist. Is the file path correct?", call. = F)
      }}

    roboplotr_typecheck(markers, "set_markers")

    roboplotr_typecheck(modebar, c("character","set_modebar"), size = NULL)

    if(!is.null(modebar)) {
      if(is.character(modebar)) {
        modebar <- set_modebar(modebar, .extra = "in set_roboplot_options(modebar = set_modebar)")
      }
    }

    roboplotr_typecheck(patterns, "character", NULL)
    roboplotr_valid_strings(patterns,c("","/","\\","x","-","|","+","."))

    roboplotr_typecheck(imgdl_wide, "set_imgdl_layout")
    roboplotr_typecheck(imgdl_narrow, "set_imgdl_layout")
    roboplotr_typecheck(imgdl_small, "set_imgdl_layout")

    roboplotr_typecheck(rounding, "numeric", allow_na = F)

    roboplotr_typecheck(tidy_legend, "logical")

    roboplotr_typecheck(trace_border, "list", allow_null = T)
    if(!is.null(trace_border)) {
      if(is.null(trace_border$color) | is.null(trace_border$width)) {
        stop("set_roboplot_options() `trace_border` must have character named `color` and numeric named `width`!", call. = F)
      }
      `trace_border$color` <- trace_border$color
      `trace_border$width` <- trace_border$width
      roboplotr_typecheck(`trace_border$color`, "character", allow_null = F, extra = "set_roboplot_options()")
      roboplotr_valid_colors(`trace_border$color`, message = "set_roboplot_options(trace_border)")
      roboplotr_typecheck(`trace_border$width`, "numeric", allow_null = F, extra = "set_roboplot_options()")
    }

    roboplotr_typecheck(trace_colors, "character", NULL)
    roboplotr_valid_colors(trace_colors)
    if (length(trace_colors) > 1 &
        length(trace_colors) < 5) {
      roboplotr_alert(
        str_glue(
          "Are you sure {length(trace_colors)} color{ifelse(length(trace_colors)==1,' is','s are')} enough?\n"
        )
      )
    }

    roboplotr_typecheck(shinyapp, "logical")

    roboplotr_typecheck(width, "numeric", allow_na = T)

    roboplotr_typecheck(xaxis_ceiling, "character")
    roboplotr_valid_strings(xaxis_ceiling, c("default","days","months","weeks","quarters","years","guess"), any)

    roboplotr_typecheck(zeroline, c("set_zeroline" = "list"))

    set_roboplot_option(accessible)
    set_roboplot_option(artefacts)
    set_roboplot_option(border)
    set_roboplot_option(background_color, "colors.background")
    set_roboplot_option(caption_template, "caption.template")
    set_roboplot_option(dashtypes)
    set_roboplot_option(font_main, "font.main")
    set_roboplot_option(font_title, "font.title")
    set_roboplot_option(font_caption, "font.caption")
    set_roboplot_option(grid)
    set_roboplot_option(height)
    set_roboplot_option(infobox)
    set_roboplot_option(locale)
    set_roboplot_option(tidy_legend, "legend.tidy")
    set_roboplot_option(linewidth)
    set_roboplot_option(logo_file, "logo")
    set_roboplot_option(markers)
    set_roboplot_option(modebar)
    set_roboplot_option(imgdl_wide,"imgdl.wide")
    set_roboplot_option(imgdl_narrow,"imgdl.narrow")
    set_roboplot_option(imgdl_small,"imgdl.small")
    set_roboplot_option(patterns, "patterntypes")
    set_roboplot_option(shinyapp)
    set_roboplot_option(trace_border, "trace.border")
    set_roboplot_option(trace_colors, "colors.traces")
    set_roboplot_option(zeroline)
    if(getOption("roboplot.accessible") == TRUE) {
      if(!identical(roboplotr_accessible_colors(getOption("roboplot.colors.traces"), background = getOption("roboplot.colors.background")),getOption("roboplot.colors.traces"))) {
        set_roboplot_option(roboplotr_accessible_colors(getOption("roboplot.colors.traces"), background = getOption("roboplot.colors.background")), "colors.traces")
      }
    }
    set_roboplot_option(width)
    set_roboplot_option(xaxis_ceiling, "xaxis.ceiling")



    if(!str_detect(getOption("roboplot.logo"),"robonomist") & !"robonomist" %in% getOption("roboplot.modebar")$buttons) {
      .modebar <- getOption("roboplot.modebar")
      .modebar$buttons <- c(.modebar$buttons, "robonomist")
      options("roboplot.modebar" = .modebar)
    }

    if(is_present(.defaults)) {
      deprecate_warn("2.3.0", "roboplotr::set_roboplot_options(.defaults)", "roboplotr::set_roboplot_options(name)")
      name <- .defaults
    }

    roboplotr_typecheck(name, c("logical","character"), allow_null = T)

    if(!is.null(name)) {
      if(is.logical(name)) {
        if(name) {
          deprecate_warn("2.3.0", "roboplotr::set_roboplot_options(name = 'must be a character')")
          .roboplot_options <- options() |> names() |> str_subset("roboplot\\.")
          .roboplot_options <- map(.roboplot_options, ~ getOption(.x)) |> setNames(.roboplot_options)
          options("roboplotr.options.defaults" = .roboplot_options)
          options("roboplot.cur.options" = "defaults")
        }
      } else if (is.character(name)) {
        .roboplot_options <- options() |> names() |> str_subset("roboplot\\.")
        .roboplot_options <- map(.roboplot_options, ~ getOption(.x)) |> setNames(.roboplot_options)
        options(setNames(list(.roboplot_options), str_glue("roboplotr.options.{name}")))
        options("roboplot.cur.options" = name)
      }
    }

    if(!is.null(shinyapp)) {
      if(shinyapp == T) {

        roboplotr_alert("Reminder: set_roboplot_options() needs to be run inside the app ui header at runtime!\n",
                        "Take care with roboplotr container css, all plotly plots inherit some but not all css.")
        roboplotr_widget_deps(tempdir())
        addResourcePath("roboplotr_js", system.file("www","js", package = "roboplotr"))
        addResourcePath("roboplotr_css", str_c(tempdir(),"/css"))

        tagList(
          singleton(
            tags$head(
              tagList(
                tags$script(type = "text/javascript", src = "roboplotr_js/relayout.js"),
                tags$link(rel = "stylesheet", type = "text/css", href = "roboplotr_css/style.css")
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

#' Locale configuration
#'
#' Parameters to add and customize the locale as used by `roboplotr`.
#'
#' @param locale Character. Currently supports on only "en-GB", "en-US",
#' "sv-SE", or "fi-FI" (the default).
#' @examples
#' # You might want to display dates or numbers with some another default format
#' # for decimal marks or thousand marks.
#'
#' set_roboplot_options(
#'   locale = set_locale("en-GB"),
#'   caption_template = "Source: {text}."
#'   )
#'
#' d <- energiantuonti |>
#'   dplyr::filter(Alue == "USA", Suunta == "Tuonti") |>
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
#' @returns A list of class roboplot.set_locale
#' @export
#' @importFrom dplyr case_when
set_locale <- function(locale = "fi-FI") {
  roboplotr_typecheck(locale, "character", allow_null = F, extra = "set_locale()")
  roboplotr_valid_strings(locale, c("en-GB","en-US","sv-SE","fi-FI"), any)
  loc <- case_when(locale == "en-GB" ~ "en", locale == "en-US" ~ "en-US", locale == "sv-SE" ~ "sv", TRUE ~ "fi")
  sep <- case_when(loc %in% c("en", "en-US") ~ ".,", TRUE ~ ", ")
  dat <- case_when(loc == "en" ~ "%-d/%-m/%Y", loc == "en-US" ~ "%-m/%-d/%Y", TRUE ~ "%-d.%-m.%Y")
  ylegendlabs <- case_when(loc == "en" ~ list(left = "Left Y-Axis", right = "Right Y-Axis"),
                           loc == "sv" ~ list(left = "V\uE4nster Y-axel", right = "H\uF6ger Y-axel"),
                           TRUE ~ list(left = "Vasen Y-akseli", right = "Oikea Y-akseli")
                           )

  robotable_labels <- case_when(
    loc == "en" ~list(
      search = "Search:",
      info = "Showing rows _START_-_END_ of _TOTAL_",
      lengthMenu = "Show _MENU_ rows per page",
      emptyTable = "No data available",
      first = "First",
      last = "Last"
    ),
    locale == "sv" ~ list(
      search = "S\u00f6k:",
      info = "Visar rader _START_-_END_ av _TOTAL_",
      lengthMenu = "Visa _MENU_ rader per sida",
      emptyTable = "Ingen data tillg\u00e4nglig",
      first = "F\u00f6rsta",
      last = "Sista"
    ),
    TRUE ~ list(
      search = "Etsi:",
      info = "N\u00e4ytet\u00e4\u00e4n rivit _START_-_END_ / _TOTAL_",
      lengthMenu = "N\u00e4yt\u00e4 _MENU_ rivi\u00e4 per sivu",
      emptyTable = "Tietoja ei saatavilla",
      first = "Ensimm\u00e4inen",
      last = "Viimeinen"
    )
  )
  .res <- list(locale = loc, separators = sep, date = dat, ylegendlabs = ylegendlabs, robotable_labels = robotable_labels)

  .res <- structure(.res, class = c("roboplotr","roboplotr.set_locale", class(.res)))

  .res

}

#' @importFrom dplyr across everything matches mutate rename select
#' @importFrom rlang sym quo_name
#' @importFrom stringr str_c str_replace str_replace_all
#' @importFrom tidyr unite
roboplotr_transform_data_for_download <- function(d, color, pattern, plot_axes) {

  if (!is.null(color)) {  color <- as_name(color) }
  if (!is.null(pattern)) {  pattern <- as_name(pattern) }

  d <- d |>
    select(matches(c(color, pattern, plot_axes$x, plot_axes$y)), -matches("roboplot.topic")) |>
    mutate(
      across(!is.numeric & !is.Date, ~ as.character(.x) |> roboplotr_transform_string()), #note: semi-colon as colon for final csv
      across(is.numeric, ~ as.character(.x) |> str_replace_all("\\.", ","))
    )

  d

}

#' @importFrom stringr str_replace_all
roboplotr_transform_string <- function(string) {
  str_replace_all(
    string,
    c(
      "\\<[^\\>]*\\>" = " ",
      "[^[:alnum:]\\s\\,\\;\\',\\&,\\%,\\-]" = "_",
      "'" = "\u2019",
      "\\&" = "\u0026",
      "\\%" = "\\u0025"
    )
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
                   "Daily" = getOption("roboplot.locale")$date)

  get_padr_frequency <- function(ts) {
    ts <- tryCatch(get_interval(ts), error = function(e) return( NA ))
    ts <- case_when(str_detect(ts, "^day") ~ "Daily",
                    str_detect(ts, "^week") ~ "Weekly",
                    str_detect(ts, "^month") ~ "Monthly",
                    str_detect(ts, "^quarter") ~ "Quarterly",
                    str_detect(ts, "^year") ~ "Annual",
                    TRUE ~ as.character(NA))
    if(!is.na(ts)) { ts } else { "Annual" }

  }
  d_attrs <- attributes(d)
  wrn <- F
  datecol <- map(names(d), ~ if(any(c("Date","POSIXct") %in% class(d[[.x]]))) { .x }) |> roboplotr_compact() |> first()
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

  if(!is.null(tf)) {
    if(!tf %in% c("Daily","Weekly","Monthly","Quarterly","Annual")) {
      wrn <- T
      tf <- get_padr_frequency(d[[datecol]])
    }
  }

  if(wrn == T & msg == T) {
    wrn <- if(is.null(tf)) {"Resorting to default %Y" } else { str_c("Guessing frequency \"",names(dateformats[tf]),"\" for date format ",dateformats[[tf]]) }
    roboplotr_message(str_c("No acceptable frequency attribute was detected for hoverlabel from data 'd', and has not been provided as \"frequency\" in the argument 'hovertext'.\n", wrn,"."))
  }
  tf
}


#' @importFrom rlang inform
roboplotr_message <-
  function(...) {
    if (getOption("roboplot.verbose") == "All") {
      inform(str_c(...), use_cli_format = T)
    }
  }

#' @importFrom rlang warn
roboplotr_alert <-
  function(...) {
    if (getOption("roboplot.verbose") %in% c("All", "Alert")) {
      warn(str_c(...), use_cli_format = T)
    }
  }

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

#' Rounding helper to preserve sums
#' @importFrom utils tail
#' @noRd
roboplotr_round <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

#' @importFrom stringr str_glue
roboplotr_ns_alert <- function(packages, msg) {
  .is <- map_lgl(packages, requireNamespace, quietly = TRUE)
  if (!all(.is)) {
    packages <- str_c("'", packages[!.is], "'")
    .many <- length(.is[!.is]) > 1
    .install <- ifelse(.many, str_glue("c({str_c(packages, collapse = ',')})"), str_c(packages, collapse = ','))
    .msg <- ifelse(is.null(msg), "this function", msg)
    stop(
      str_glue(
        "Package{ifelse(.many, 's','')} {roboplotr_combine_words(packages)} {ifelse(.many, 'are','is')} required for {.msg}. ",
        "Please install {ifelse(.many, 'them','it')} with install.packages({.install})."
      ),
      call. = F
    )
  }

}


roboplotr_plot_options <- function(roboplot_options) {

  roboplotr_typecheck(roboplot_options, "character", allow_null = T)

  if(!is.null(roboplot_options)) {
    .this_reset <- getOption(str_glue("roboplotr.options.{roboplot_options}"))
    if(is.null(.this_reset)) {
      stop(str_glue("You have not specified reset option named \"{roboplot_options}\"! Use set_roboplot_options(name = \"{roboplot_options}\") to set them."), call. = F)
    }
    options(.this_reset)
  }

}

# Used to temporarily override roboplot options for a single plot.
roboplotr_temp_options <- function(roboplot_options = NULL) {

  roboplotr_typecheck(roboplot_options,
                      "character",
                      allow_null = T,
                      extra = "in `roboplot()`")

  if(!is.null(roboplot_options)) {
    cur_options <- getOption("roboplot.cur.options")
    # print(cur_options)
    .this_reset <- getOption(str_glue("roboplotr.options.{roboplot_options}"))
    if(is.null(.this_reset)) {
      stop(str_glue("You have not specified roboplot options name \"{roboplot_options}\"! Use set_roboplot_options(name = \"{roboplot_options}\") to set them."), call. = F)
    }
    options(.this_reset)
  } else {
    cur_options <- NULL
  }

  list(roboplot_options = cur_options, reset = !is.null(roboplot_options))

}

roboplotr_reset_temp_options <- function(.reset) {
  if(.reset$reset) {
    set_roboplot_options(reset = .reset$roboplot_options)
  }
  }

