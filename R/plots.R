#' @importFrom dplyr case_when
#' @importFrom stringr str_length
#' @importFrom plotly plot_ly
#' @importFrom lubridate as_date today
roboplotr_config <- function(p,
                             title, subtitle = "", caption,
                             legend_position, legend_orientation, legend_order,
                             height,
                             width,
                             margin = NA,
                             zeroline = F,
                             enable_rangeslider = list(rangeslider = F, max = as_date(today)),
                             ticktypes,
                             container) {


  if(!is.logical(enable_rangeslider$rangeslider) & !is.character(enable_rangeslider$rangeslider)) {
    stop("Rangeslider must be TRUE, FALSE or a string interpretable as date, eg. \"2015-01-01\".", call. = F)
  } else if (is.logical(enable_rangeslider$rangeslider)) {
    enable_rangeslider <- enable_rangeslider$rangeslider
    slider_range <- NULL
  } else if (is.character(enable_rangeslider$rangeslider)) {
    if (is.na(suppressWarnings(as_date(enable_rangeslider$rangeslider)))) {
      stop("Rangeslider must be TRUE, FALSE or a string interpretable as date, eg. \"2015-01-01\".", call. = F)
    } else {
      slider_range <- list(enable_rangeslider$rangeslider, enable_rangeslider$max)
      enable_rangeslider <- T
    }
  }

  if(!is.list(margin)) {
    if(is.na(margin)) {
      margin <- list(t = 0, r = 10, b = 0, l = 20)
    }
  }

  p |>
    roboplotr_dependencies(title, subtitle, container) |>
    roboplotr_set_axis_ranges(ticktypes[c("xlim","ylim")]) |>
    roboplotr_grid() |>
    roboplotr_set_background() |>
    roboplotr_modebar(title, subtitle, height, width, ticktypes$dateformat) |>
    roboplotr_set_ticks(ticktypes) |>
    roboplotr_set_margin(margin) |>
    roboplotr_logo() |>
    roboplotr_legend(legend_position, legend_orientation, legend_order) |>
    roboplotr_title(title, subtitle) |>
    roboplotr_caption(caption) |>
    roboplotr_zeroline(zeroline) |>
    roboplotr_rangeslider(enable_rangeslider, slider_range = slider_range)
}


#' @importFrom dplyr add_row
#' @importFrom htmltools htmlDependency tagList tags
#' @importFrom htmlwidgets appendContent onRender
#' @importFrom R.utils setOption
#' @importFrom RCurl base64Encode
#' @importFrom shiny addResourcePath isRunning
#' @importFrom stringr str_c str_extract_all str_replace_all str_squish str_pad str_wrap
roboplotr_dependencies <- function(p, title, subtitle, container) {

  plot_title <- list(title,subtitle,getOption("roboplot.font.title")$bold)

  if(!isRunning()) {

    if(is.null(getOption("roboplot.widget.deps.session"))) {
      deps <- roboplotr_widget_deps()
      setOption("roboplot.widget.deps.session", deps)
    } else { deps <- getOption("roboplot.widget.deps.session") }

    p <- appendContent(
      p,
      tagList(
        tags$script(src = deps$js),
        tags$link(rel = "stylesheet", type = "text/css", href = deps$css)
      )
    )
  }

  rangeslider_sums <- F
  if(any(str_detect(p$plot_mode,"stack")) && any(p$trace_types == "bar")) { rangeslider_sums = T }
  pie_plot <- if(any(p$trace_types == "pie")) { T } else { F }

  # accessibility code, R above, js below..
  # ariaid <- str_pad(round(runif(1)*1000000),6,"left","0")
  # desc_string <- (function() {
  #   row.data <- p$data |> pmap(function(...) {
  #     as.character(list(...)) |> replace_na("NA") |> str_replace_all(c(";"=",","'"="\u2019")) |> str_c(collapse = ";")
  #   }) |> unlist() |> str_c(collapse = "\\n")
  #   col.names <- names(p$data) |> str_replace("csv\\.data\\.tiedot","tiedot") |> str_c(collapse = ";")
  #   str_c("<desc id = '",ariaid,"d'>A plotly plot displaying the following data\\:\\n",col.names,"\\n",row.data,"</desc>")
  # })()
  #
  # ttl_string <- str_c("<title id = '",ariaid,"t'>",title,", ",subtitle,"</title>")

  # let mainsvg = $(gd).find('svg.main-svg')[0]
  # let desc_string = data.dataString;
  # let title_string = data.titleString;
  # let arialabel_string = data.ariaID + 't ' + data.ariaID + 'd'
  # mainsvg.prepend(document.importNode(new DOMParser().parseFromString(desc_string, 'text/html').body.childNodes[0], true));
  # mainsvg.prepend(document.importNode(new DOMParser().parseFromString(title_string, 'text/html').head.childNodes[0], true));
  # mainsvg.setAttribute('aria-labelledby', arialabel_string);
  # mainsvg.setAttribute('role', 'img');

  # annotations are removed here due to plotly bug replicating annotations when font is provided as list of length > 1
  p |>
    onRender(jsCode = "
                        function (gd, params, data){
                        let plot_title = data.plotTitle;
                        for (i = gd.layout.annotations.length - 1; i >= 0; i--) {
        if(gd.layout.annotations[i].text == gd.layout.annotations[0].text && i > 0 ) {
          Plotly.relayout(gd, 'annotations[' + i + ']', 'remove');
        }
                        }
                        setVerticalLayout({'width': true}, gd, data.legendFontsize, plot_title, pie_plot = data.piePlot);
                        gd.on('plotly_relayout',function(eventdata) {
                        plotlyRelayoutEventFunction(eventdata, gd, data.legendFontsize, plot_title, data.rangesliderSums, pie_plot = data.piePlot);
                        })

                        if(data.container !== null) {
                        let el = gd.closest(data.container);

                          var observer = new MutationObserver(function(mutation) {
                          let nowactive = mutation[0].target.classList.contains('active')
                          let lastactive = mutation[0].oldValue.includes('active')
                          if(nowactive === true && lastactive === false) {
                            console.log('state change, inactive to active!')
                            plotlyRelayoutEventFunction({width: true}, gd, data.legendFontsize, plot_title, data.rangesliderSums, pie_plot = data.piePlot);
                            observer.disconnect();
                          }
                        });

                         observer.observe(el, {
                          attributes: true,
                          attributeFilter: ['class'],
                          attributeOldValue: true,
                        });

                        }

                        gd.on('plotly_afterplot', function() {
                        let thisclippath = $(gd).find('clipPath[id*=legend] > rect')
                        if(thisclippath.length > 0) {
                          thisclippath = thisclippath[0];
                          thiswidth = thisclippath.getAttribute('width');
//                          console.log('init width: ' + thiswidth)
                          thisclippath.setAttribute('width',Number(thiswidth)*1.05);
                          thiswidth = thisclippath.getAttribute('width');
 //                         console.log('recalc width: ' + thiswidth)
                        };
                        })


                            }", data = list(plotTitle = plot_title,
                                         rangesliderSums = rangeslider_sums,
                                         legendFontsize = getOption("roboplot.font.main")$size,
                                         piePlot = pie_plot,
                                         container = container#,
                                         # dataString = desc_string,
                                         # titleString = ttl_string,
                                         # ariaID = ariaid
                        ))
}

#
# clippath for onRender.. to-do
# let thisclippath = $(gd).find('clipPath[id*=legend] > rect')[0]
# let thiswidth = thisclippath.getAttribute('width')
# thisclippath.setAttribute('width',Number(thiswidth)*1.05)
# gd.on('plotly_afterplot', function() {
#   let thisclippath = $(gd).find('clipPath[id*=legend] > rect')[0]
#   let thiswidth = thisclippath.getAttribute('width')
#   thisclippath.setAttribute('width',Number(thiswidth)*1.05)
# })

#' Get dynamically scaling plotly plots with legend and caption positioning and font manipulation for different sizes.
#'
#' Wrapper for [plotly::plot_ly] for shorthand declaration of many layout and trace arguments.
#' Ensures proper scaling or elements when used in shiny apps, iframes, static image downloads and so on.
#'
#' @param d Data frame. Data to be plotted with at least the columns "time" (Date or POSIXt) and "value" (numeric). Other columns could be specified instead with 'plot_axes', using [set_axes()].
#' @param color Symbol, string, or function resulting in symbol or string. Variable from argument 'd' to use for trace color. If left NULL, the argument 'subtitle' will be used as a placeholder for determining color and hoverlabels.
#' @param pattern Symbol, string, or function resulting in symbol or string. Variable from argument 'd' to use for scatter plot linetype or bar plot pattern. Not supported for pie charts.
#' @param title,subtitle Characters. Labels for plot elements.
#' @param caption Function or character. Use [set_caption()].
#' @param legend_position,legend_orientation Characters. Currently only legend_position is used, and takes only "bottom" or NA for no legend. Legend is removed on default if the argument 'color' in argument 'd' has only one observation.
#' @param legend_title Logical or character. Use TRUE if you want the parameter 'color' to be the legend title. Use a character string if you want to provide your own legend title.
#' @param zeroline Logical or double. Determines zeroline inclusion, TRUE for zeroline, or double for exact placement.
#' @param rangeslider Logical or character in %Y-%m-%d format. Determines rangeslider inclusion. TRUE includes the rangeslider, a character string includes the rangeslider with the given date as a start date.
#' @param hovertext Function. Use [set_hovertext()].
#' @param highlight Double or list. Determines if a given trace is included in legend and assigned a color.
#' If double, traces with max(value) < highlight will be give trace color matching the grid color, and removed from the legend.
#' If function, it must return a logical and include named items "value" and ".fun", where .fun checks if given value will get a color or legend item.
#' Will not currently work with multiple patterns.
#' @param plot_type Character vector, named if length > 1. Determines the trace type for either the whole plot, or for all variables defined by color as name-value pairs.
#' @param plot_mode Character vector, named if length > 1. Controls plot specifics along with the parameter 'plot_type'. When 'plot_type' is "scatter", the available modes are "line" "scatter", "step" and "scatter+line".
#' When 'plot_type' is "bar", the available modes are "dodge" "stack", "horizontal". When 'plot_type' is "pie", the available modes are "normal" and "rotated".
#' You can give a single unnamed 'plot_mode' which is used as applicable, or name-value pairs, where names are items from parameter 'd' column described by parameter 'color', and values applicable plot modes listed above.
#' @param plot_axes Function. Function. Use [set_axes()].
#' @param trace_color Character vector, named if length > 1. Trace colors for all traces. Determines the trace type for either the whole plot, or for all variables defined by color as name-value pairs.
#' @param line_width Double vector, named if length > 1. Line width for all line traces. Determines the line width for either the whole plot, or for all variables defined by color as name-value pairs.
#' @param height,width Double. Height and width of the plot. Default width is NULL for responsive plots, give a value for static plot width.
#' @param facet_split Currently unused. Variable from argument 'd' to use for facet splits.
#' @param legend_maxwidth Double. Legend items (and y-axis values for horizontal barplots) longer than this will be collapsed with an ellipsis (Double).
#' @param xaxis_ceiling Character. One of "default", "days", "months", "weeks", "quarters", "years", or "guess"). How to round the upper bound of plot x-axis for other than bar plots if no axis limits are given.
#' @param secondary_yaxis Symbol, string, or function resulting in symbol or string. Variable from argument 'd' resulting in a maximum of two factor levels, determining which observations if any use a secondary y-axis.
#' Parameter 'zeroline' will be ignored. Cannot currently differentiate between the axes in legend, and right margin will not scale properly on zoom and possibly on image files downloaded through modebar.
#' @param artefacts Logical or function. Use [set_artefacts()] for fine-tuned control. Use TRUE instead for automated artefact creation or html and/or other files from the plot based on settings globally set by [set_roboplot_options()].
#' @param container Character. Experimental, might not work as intended. Use only with shiny apps. A css selector for the element in a shiny app where this [roboplot()] will be contained in. Used for relayouts if the plot is rendered while the container is not displayed.
#' @param ... Placeholder for other parameters.
#' @return A list of classes "plotly" and "html"
#' @examples
#' # The default use for roboplotr::roboplot is for line charts. Providing
#' # a title is mandatory, subtitle and color is optional but  very likely
#' # necessary.
#'
#' d <- energiantuonti |>
#'   dplyr::filter(Alue %in% c("Kanada","Norja","Yhdistynyt kuningaskunta"))
#' d1 <- d |> dplyr::filter(Suunta == "Tuonti")
#' d1 |> roboplot(color = Alue,
#'                title = "Energian tuonti",
#'                subtitle = "Milj. \u20AC",
#'                caption = "Lähde: Tilastokeskus.")
#'
#'
#' # Legend will automatically be omitted if only a single observation exists
#' # for 'color' is unless legend_position is given (currently only "bottom"
#' # works). Caption may be further specified with the helper function
#' # roboplotr::set_caption (see documentation for mote control).
#' d1 |>
#'   dplyr::filter(Alue == "Yhdistynyt kuningaskunta") |>
#'   roboplot(Alue,"Energian tuonti Yhdistyneest\uE4 kuningaskunnasta","Milj. \u20AC",
#'            caption = set_caption(text = "Tilastokeskus",
#'                                           updated = TRUE,
#'                                           .data = d1
#'            )
#'   )
#'
#'
#' # You can also use set_roboplot_options() to preconstruct some caption texts.
#'
#' set_roboplot_options(
#'   caption = list(prefix = "Lähde: ", lineend = ".", updated = FALSE)
#'   )
#'
#' # Legend can also be omitted by giving a legend_position of NA. Height and
#' # width can also be specified, while for most uses width specification is
#' # unnecessary, as roboplotr is designed for plots with responsive widths.
#' d1 |> roboplot(Alue,"Energian tuonti","Milj. \u20AC","Tilastokeskus",
#'                legend_position = NA,
#'                height = 600,
#'                width = 400
#' )
#'
#' # Pattern can be used in addition to color and you can control the ordering of
#' # the traces by transforming your variables to factors. You can also let
#' # roboplotr guess how much space is given to yaxis end in line plots, or give a
#' # string such as "weeks" or "days" to it. Message about missing frequency data
#' # can be silenced  by setting the information as an attribute of the used data.
#' d2 <- d |> dplyr::mutate(Alue = forcats::fct_reorder(Alue, value))
#' attr(d2, "frequency") <- "Quarterly"
#' d2 |> roboplot(Alue,"Energian tuonti ja vienti","Milj. \u20AC","Tilastokeskus",
#'                pattern = Suunta,
#'                xaxis_ceiling = "guess")
#'
#' # Bar plots use a pattern too
#' d2 |> roboplot(Alue,"Energian tuonti ja vienti","Milj. \u20AC","Tilastokeskus",
#'                pattern = Suunta,
#'                plot_type = "bar")
#'
#' # Scatter plots and bar plot may be combined, and colors determined by
#' # trace by giving named character vectors as the appropriate arguments.
#' # Barmode or scatter type is controlled by plot_mode.
#' d1 |> roboplot(Alue,"Energian tuonti ja vienti","Milj. \u20AC","Tilastokeskus",
#'              trace_color =  c("Kanada" = "red","Norja" = "blue", .other = "black"),
#'              plot_type = c("Norja" = "scatter","Kanada" = "bar",".other" = "scatter"),
#'              plot_mode = c("Yhdistynyt kuningaskunta" = "scatter",
#'                            "Norja" = "scatter+line"
#'              ))
#'
#'
#' # With single 'time' observation x-axis tickmarks lose tick labels and
#' # hovertemplate loses the time information. There are several places where
#' # this information fits nicely.
#' d3 <- d2 |> dplyr::filter(time == max(time))
#' d3 |>
#'   roboplot(Alue,
#'            glue::glue("Energian tuonti ja vienti vuonna {lubridate::year(max(d3$time))}"),
#'            glue::glue("Milj. \u20AC ({lubridate::year(max(d3$time))})"),
#'            pattern = Suunta,
#'            plot_type = "bar",
#'            caption = set_caption(text = "Tilastokeskus",
#'                                           updated = TRUE,
#'                                           .data = d1,
#'                                           line.end = "!",
#'                                           prepend = glue::glue(
#'                                             "Tieto vuodelta {lubridate::year(max(d3$time))}"),
#'                                           append = glue::glue(
#'                                             "Toistan, vuonna {lubridate::year(max(d3$time))}")
#'            ))
#'
#' # Plot axis can be controlled with roboplotr::set_axes (see
#' # documentation for more examples).
#' d2 |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue, "Energian tuonti","Milj. \u20AC","Tilastokeskus",
#'            plot_axes = set_axes(
#'              ytitle = "Arvo",
#'              xformat = "Vuonna %Y",
#'              ylim = c(-100,100))
#'   )
#'
#' # Bar plot can be horizontal with plot axis control and 'plot_mode' set to
#' # horizontal but then is better off with only a single 'time' observation. Long
#' # legend items and axis labels can be cut off with 'legend_maxwidth', while
#' # still showing the proper labels on hover.
#' d3 |>
#'   dplyr::mutate(Suunta = paste0(Suunta, " m\uE4\uE4r\uE4maittain")) |>
#'   roboplot(Suunta,
#'            glue::glue("Energian tuonti {lubridate::year(max(d$time))}"),
#'            "Milj. \u20AC","Tilastokeskus",
#'            plot_type = "bar",
#'            legend_maxwidth = 12,
#'            plot_mode = "horizontal",
#'            plot_axes = set_axes(
#'              y = "Alue",
#'              yticktype = "character",
#'              x = "value",
#'              xticktype = "numeric")
#'   )
#'
#' # Pie plots are possible too, but pattern is currently ignored by plotly library.
#' d3 |> roboplot(Alue,"Energian tuonti ja vienti","Milj. \u20AC","Tilastokeskus",
#'                pattern = Suunta,
#'                plot_type = "pie")
#'
#' # Pie plot can be centered to the first factor level of argument 'color' with
#' # with plot_mode "rotated".
#' d3 |> roboplot(Alue,"Energian tuonti ja vienti","Milj. \u20AC",
#'                     "Tilastokeskus",
#'                     plot_type = "pie",
#'                     plot_mode = "rotated")
#'
#' # You can give a highlight value if you don't have a pattern. Any trace with a
#' # "value" equal or higher than the given value will get colors as normal. Others
#' # get assigned a bacground grid color and no legend entry. Useful mostly with
#' # very large amounts of traces.
#'
#' d2 |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue, "Energian tuonti","Milj. \u20AC","Tilastokeskus",
#'            plot_type = "scatter",
#'            highlight = 160)
#'
#' # This works best with line plots, but can be included in other plots, too -
#' # with varying results, these are work in progress. Highlight can also be a list
#' # with "value" and ".fun" used to determine which traces are highlighted. The
#' # default usage is essentially list(value = highlight, .fun = sum).
#' d2 |> dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue, "Energian tuonti","Milj. \u20AC","Tilastokeskus",
#'            plot_type = "bar",
#'            highlight = list(value = 22, .fun = mean))
#'
#' # Rangeslider can be added as TRUE or FALSE, or as character in date format of
#' # %Y-%m-%d, in which case the given date will control where the rangeslider is
#' # initially set. Zeroline can be controlled in a similar way.
#' d2 |> dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue, "Energian tuonti","Milj. \u20AC","Tilastokeskus",
#'            rangeslider = "2014-01-01",
#'            zeroline = 128)
#'
#' # Secondary yaxis can be added to line plots when the corresponding variable
#' # only has two unique observations that is a subset of the variable 'color'.
#' # There is currently no way of differentiating between the axes in legend.
#' # Zeroline will not behave as expected, but will instead refer to right yaxis.
#' d2 |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   dplyr::mutate(sec_axis = ifelse(Alue == "Norja","Norja","Muu")) |>
#'   roboplot(Alue, "Energian tuonti","Milj. \u20AC","Tilastokeskus",
#'            plot_type = c("Norja" = "bar", ".other" = "scatter"),
#'            secondary_yaxis = sec_axis,
#'            zeroline = NA)
#'
#' # Finally, you may get html or other files from the plots you create either
#' # by using roboplotr::roboplot_create_artefacts() or simply using the
#' # parameter 'artefacts' here.
#' d2 |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   dplyr::mutate(sec_axis = ifelse(Alue == "Norja","Norja","Muu")) |>
#'   roboplot(Alue, "Energian tuonti","Milj. \u20AC","Tilastokeskus",
#'            plot_type = c("Norja" = "bar", ".other" = "scatter"),
#'            secondary_yaxis = sec_axis,
#'            zeroline = NA)
#' # Finally, you may get html or other files from the plots you create either
#' # by using roboplotr::roboplot_create_artefacts() or simply using the
#' # parameter 'artefacts' here. The global defaults or artefact creation are
#' # set with roboplotr::set_roboplot_options(), and for this example the
#' # default filepath will be changed to a temporary directory.
#'
#' set_roboplot_options(
#'   artefacts = set_artefacts(filepath = tempdir())
#' )
#'
#' d2 |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue,"Energian tuonti","Milj. \u20AC","Tilastokeskus",
#'   artefacts = TRUE)
#'
#' file.exists(paste0(tempdir(),"/energian_tuonti.html"))
#'
#' # Reset to defaults
#'
#' set_roboplot_options(reset = TRUE)
#'
#' # Further specifications for creating artefacts is defined under
#' # roboplotr::set_roboplot_options(), roboplotr::roboplot_create_widgets() and
#' # roboplotr::set_artefacts()
#'
#' # Using "container" is defined under roboplotr::set_roboplot_options() under
#' # as its usage is tied to using the 'shinyapp' parameter therein.
#' @export
#' @importFrom dplyr coalesce distinct group_split pull
#' @importFrom forcats fct_reorder
#' @importFrom lubridate as_date ceiling_date is.Date
#' @importFrom plotly partial_bundle
#' @importFrom purrr map2
#' @importFrom rlang as_label as_name enquo quo quo_get_env
#' @importFrom stats median runif setNames
#' @importFrom stringr str_c str_detect str_length str_pad str_replace
roboplot <- function(d,
                     color = NULL,
                     title = NULL,
                     subtitle = "",
                     caption = NULL,
                     legend_orientation = NULL,
                     legend_position = NULL,
                     trace_color = NULL,
                     highlight = NULL,
                     zeroline = F,
                     rangeslider = FALSE,
                     pattern = NULL,
                     line_width = getOption("roboplot.linewidth"),
                     hovertext = NULL,
                     plot_type = "scatter",
                     plot_mode = NULL,
                     plot_axes = set_axes(),
                     height = getOption("roboplot.height"),
                     facet_split = NULL,
                     legend_maxwidth = NULL,
                     xaxis_ceiling = getOption("roboplot.xaxis.ceiling"),
                     secondary_yaxis = NULL,
                     width = getOption("roboplot.width"),
                     legend_title = F,
                     artefacts = getOption("roboplot.artefacts")$auto,
                     container = getOption("roboplot.shinyapp")$container,
                     ...
){

  margin <- NA # will this be used at all? Probably not.

  if(missing(d)){
    stop("Argument 'd' must a data frame!", call. = F)
  }

  roboplotr_check_param(d, "data.frame", NULL, allow_null = F)

  if(is.null(title)) {
    title <- attributes(d)[c("title","robonomist_title")]
    if(!is.null(title$robonomist_title)) {
      roboplotr_message("Using the attribute \"robonomist_title\" for plot title.")
      title <- title$robonomist_title
    } else if (!is.null(title$title) & length(title$title != 1)) {
      roboplotr_alert("Using the attribute \"title\" as plot title.")
      title <- title$title
    } else {
      roboplotr_alert("Missing the title, using placeholder.")
      title <- "PLACEHOLDER"
    }
  }

  d_names <- names(d)

  roboplotr_check_param(plot_axes, "function", NULL, f.name = list(fun = substitute(plot_axes)[1], check = "set_axes"))

  if(!all(plot_axes[c("x","y")] %in% d_names)) {
    stop(str_c("'d' must be a tibble with columns named \"",plot_axes$x,"\" and \"",plot_axes$y,"\"."), call. = F)
  } else if (!all(class(d[[plot_axes$x]]) %in% plot_axes$xclass, class(d[[plot_axes$y]]) %in% plot_axes$yclass)) {
    stop(str_c("Argument 'd' column \"",plot_axes$x,"\" must be \"",
               roboplotr_combine_words(plot_axes$xclass, sep = "\", \"", and = "\", or \"", oxford_comma = F),
               "\", and the column \"",plot_axes$y,"\" must be \"",
               roboplotr_combine_words(plot_axes$yclass, sep = "\", \"", and = "\", or \"", oxford_comma = F)
               ,"\"."), call. = F)
  }

  color <- enquo(color)
  color <- roboplotr_check_valid_var(color, d_names)
  if(is.null(color)) { color <- quo(!!sym("roboplot.topic"))}

  if(as_name(color) == "roboplot.topic"){
    roboplotr_alert("Without an unquoted arg 'color' the variable named \"roboplot.topic\" is added to data 'd', using the argument 'title' trunctated to 30 characters as value for the variable.")
    d <- mutate(d, roboplot.topic = str_trunc(title,30))
  }

  pattern <- enquo(pattern)
  pattern <- roboplotr_check_valid_var(pattern, d_names)

  secondary_yaxis <- enquo(secondary_yaxis)
  secondary_yaxis <- roboplotr_check_valid_var(secondary_yaxis, d_names)

  if(!is.null(secondary_yaxis)) {
    if(!is.factor(pull(d,{{secondary_yaxis}}))) {
      if (pull(d,{{secondary_yaxis}}) |> unique() |> length() > 2) {
        stop("No more than two unique observations can be in the variable provided for 'secondary_yaxis'.", call. = F)
      }
      d <- d |> mutate({{secondary_yaxis}} := fct_reorder({{secondary_yaxis}}, .data$value, .desc = T, na.rm = T))
      if(unique(pull(d, !!secondary_yaxis)) |> length() > 1) {
        zeroline <- F
        rmargin <- (max(str_length(filter(d, as.numeric(!!secondary_yaxis) == max(as.numeric({{secondary_yaxis}})))$value), na.rm = T) * getOption("roboplot.font.main")$size / 1.5) |> max(30)
        margin <- list(t = 0, r = rmargin, b = 0, l = 20)
        roboplotr_alert("Secondary_yaxis cannot currently differentiate between the axes in legend, and right margin will not scale properly on zoom and possibly on image files downloaded through the modebar.")
      }
    }
  }

  roboplotr_check_param(hovertext, "function", NULL, f.name = list(fun = substitute(hovertext)[1], check = "set_hovertext"))

  if(is.null(hovertext)) {
    hovertext <- set_hovertext(roboplotr_get_dateformat(d),unit = tolower(subtitle))
  } else if (is.null(hovertext$dateformat)) {
    hovertext$dateformat <- roboplotr_hovertemplate_freq(roboplotr_get_dateformat(d))
  }

  roboplotr_check_param(caption, c("character","function"),size = 1, f.name = list(fun = substitute(caption)[1], check = "set_caption"))
  if(!is.null(caption)) {
    if(!is(substitute(caption)[1], "call")) {
      caption <- set_caption(caption,.data = d)
    }
  } else {
      cpt <- attributes(d)$source
      if(length(cpt) == 1) {
        roboplotr_message("Using the attribute \"source\" for plot caption.")
        caption <- set_caption(unlist(cpt)[1],.data = d)
      } else if (!is.null(cpt[[getOption("roboplot.locale")$locale]])) {
        roboplotr_message("Using the attribute \"source\" as plot caption.")
        caption <- set_caption(cpt[[getOption("roboplot.locale")$locale]][1],.data = d)
      } else {
        roboplotr_alert("Missing the caption, using placeholder.")
        caption <- set_caption("PLACEHOLDER",.data = d)
      }
  }
  roboplotr_check_param(xaxis_ceiling, "character", allow_null = F)
  if(!"date" %in% plot_axes$xticktype | !"time" %in% d_names) {
    if(!"default" %in% xaxis_ceiling) {
      roboplotr_alert("'xaxis_ceiling' is ignored if x-axis is not a date or a \"time\" column does not exist.")
    }
    xaxis_ceiling <- "default"
  } else {
    xaxis_ceiling <- match.arg(xaxis_ceiling, c("default","days","months","weeks","quarters","years","guess"))
  }
  if(xaxis_ceiling != "default" & all(is.na(plot_axes$xlim)) & !"bar" %in% plot_type & !"horizontal" %in% plot_mode) {
    if(xaxis_ceiling == "guess") {
      xaxis_ceiling <- roboplotr_guess_xaxis_ceiling(d, hovertext)
    }
    if(!is.null(xaxis_ceiling)) {
      plot_axes$xlim <- c(min(d$time), as_date(ceiling_date(max(d$time), xaxis_ceiling)))
    }
  } else if (xaxis_ceiling != "default" & (!any(is.na(plot_axes$xlim)) || any(c("bar","pie") %in% plot_type) || !"horizontal" %in% plot_mode)) {
    roboplotr_alert("'xaxis_ceiling' is ignored when \"bar\" or \"pie\" is in 'plot_type', \"horizontal\" is in 'plot_mode', or 'xlim' is provided in plot_axes.")
  }

  if(!is.null(facet_split)) {
    stop("Facet split currently unavailable!", call. = F)
    facet_split <- roboplotr_check_valid_var(facet_split, d_names)
    if(rangeslider == T | zeroline == T | any(!is.na(plot_axes$ylim))) roboplotr_alert("Rangeslider, zeroline and y-axis range are not currently enabled for faceted plots.")
    rangeslider <- F
    zeroline <- F
    ymin <- min(d$value)
    ymax <- max(d$value)
    axdif <- diff(c(ymin, ymax)) * 0.04
    ymin <- ymin - axdif
    ymax <- ymax + axdif
    plot_axes$ylim <- c(ymin, ymax)
  }


  if("horizontal" %in% plot_mode) {
    if(plot_axes$y == "value") { roboplotr_alert("Did you want \"value\" to be x-axis? Use the parameter 'plot_axes'.") }
  } else if (plot_axes$y != "value" & "bar" %in% plot_mode) {
    roboplotr_alert("Did you want a horizontal bar chart? Use the parameter 'plot_mode'.")
  }
  ticktypes <- append(plot_axes,list(dateformat = hovertext$dateformat, reverse = any(str_detect(plot_type, "bar")), pie = any(str_detect(plot_type, "pie"))))
  if((!plot_axes$yticktype %in% "numeric" | !plot_axes$xticktype %in% "date") & (zeroline != F | rangeslider != F)) {
    roboplotr_alert("Parameters 'zeroline' and 'rangeslider' are currently disabled when parameter 'plot_axis' xticktype is not date or yticktype is not numeric!")
    zeroline <- F
    rangeslider <- F
  }

  if(!is.factor(d[[as_name(color)]])) {
    d <- mutate(d, {{color}} := fct_reorder({{color}}, .data$value, .fun = mean, .na_rm = T) |> fct_rev())
  }

  d <- d |> group_by(!!color) |> filter(!all(is.na(.data$value))) |> ungroup() |> droplevels()

  unique_groups <- sort(unique(d[[as_name(color)]]))

  if(!all(plot_type %in% c("scatter","bar","pie"))) {
    stop("Plot type must be \"scatter\" or \"bar\", or a named character vector!", call. = F)
  } else if (length(plot_type) == 1 & is.null(names(plot_type))){
    d <- d |> mutate(roboplot.plot.type = plot_type)
    # } else if (!all(unique_groups %in% names(plot_type))) {
    #   stop(str_c("All variables in column \"",as_name(color),"\" must have a corresponding plot type!"), call. = F)
    # } else {
    #   d <- mutate(d, roboplot.plot.type = str_replace_all(!!color, plot_type))
    # }
  } else if (!all(unique_groups %in% names(plot_type)) & !(".other" %in% names(plot_type)) ) {
    stop(str_c("All variables in column \"",as_name(color),"\" must have a corresponding 'plot_type', or key \".other\" must be included!"), call. = F)
  } else {
    missing_groups <- unique_groups |> subset(!unique_groups %in% names(plot_type))
    if(length(missing_groups) > 0) {
      detected_widths <- map2(unname(plot_type), names(plot_type), function(pt,nm) {
        miss <- missing_groups |> subset(str_detect(missing_groups, str_c(nm, collapse = "|")))
        rep(pt, length(miss)) |> setNames(miss)
      }) |> roboplotr_compact() |> unlist()
      plot_type <- c(plot_type, detected_widths)
    }
    d <- mutate(d, roboplot.plot.type = plot_type[as.character(!!color)] |> coalesce(plot_type[".other"]))
  }

  d <- roboplotr_set_plot_mode(d,color,plot_mode)

  if(!all(typeof(line_width) == "double")) {
    stop("Line width must be a double, or a named double vector!", call. = F)
  } else if (length(line_width) == 1 & is.null(names(line_width))){
    d <- d |> mutate(roboplot.linewidth = line_width)
  } else {
    if(!all(unique_groups %in% names(line_width))) {
      line_width <- c(line_width, c(".other" = getOption("roboplot.linewidth")))
    }
    missing_groups <- unique_groups |> subset(!unique_groups %in% names(line_width))
    if(length(missing_groups) > 0) {
      detected_widths <- map2(unname(line_width), names(line_width), function(lw,nm) {
        miss <- missing_groups |> subset(str_detect(missing_groups, str_c(nm, collapse = "|")))
        rep(lw, length(miss)) |> setNames(miss)
      }) |> roboplotr_compact() |> unlist()
      line_width <- c(line_width, detected_widths)
    }
    d <- mutate(d, roboplot.linewidth = line_width[as.character(!!color)] |> coalesce(line_width[".other"]))
  }

  color_vector <- roboplotr_set_colors(trace_color, unique_groups, highlight, d, color)


  d <- d |> roboplotr_get_pattern(pattern) |> mutate(roboplot.trace.color = color_vector[!!color])

  if(is.null(legend_position) & length(unique_groups) < 2) { legend_position <- NA }

  # if(!is.null(facet_split)) {
  #   p <- roboplotr_get_facet_plot(d, facet_split, height, color, pattern, plot_type, trace_color, highlight, hovertext, plot_mode, ticktypes, plot_axes)
  # } else {
  p <- roboplotr_get_plot(d, height, color, pattern, plot_type, trace_color, highlight, hovertext, legend_maxwidth, secondary_yaxis, legend_position, ticktypes, width, legend_title)
  # }

  p$data <- roboplotr_transform_data_for_download(d, color, pattern, ticktypes)

  if(!isRunning()) { p$elementId <- str_c("widget_",roboplotr_string2filename(title),"_",str_pad(round(runif(1)*1000000),6,"left","0")) }
  p$title <- title
  p$trace_types <- distinct(d, !!color, .data$roboplot.plot.type) |> pull(2,1)
  p$plot_mode <- plot_mode

  maxtime <- if("time" %in% d_names & !identical(rangeslider, F)) { if(!is.na(plot_axes$xlim[2])) { plot_axes$xlim[2] } else { ceiling_date(max(d$time), roboplotr_guess_xaxis_ceiling(d, hovertext, what = "rangeslider")) } } else { NULL }
  mintime <- if("time" %in% d_names) { min(d$time) } else { NULL }

  # if only one group for color, remove legend as default
  legend_order <- ifelse(!any(c("bar","pie") %in% plot_type), "reversed", "normal")

  p <- p |>
    roboplotr_config(title = title, subtitle = subtitle, caption = caption,
                     legend_position = legend_position, legend_orientation = legend_orientation, legend_order = legend_order,
                     margin = margin,
                     height = height,
                     width = width,
                     zeroline = list(zeroline = zeroline, xrange = list(min = mintime, max = maxtime)),
                     enable_rangeslider = list(rangeslider = rangeslider, max = maxtime),
                     ticktypes = ticktypes,
                     container = container)

  if(getOption("roboplot.shinyapp")$shinyapp == F) {
     p <- partial_bundle(p, "basic")
  }

  ## add labels for facet plot. Has to be done here for the relayout js to work properly for captions.
  # if(!is.null(facet_split)) {
  #   yloc <- max(d$value)
  #   split_facet <- group_split(d,!!facet_split)
  #   for(i in seq(length(split_facet))) {
  #     xloc <- split_facet[[i]]$time |> unique() |> sort() |> median()
  #     ann_text <- split_facet[[i]][[as_name(facet_split)]] |> unique() |> str_pad(width = 10, side = "both", pad = " ")
  #     p <- p |>
  #       layout(annotations =
  #                list(text = ann_text, yref = str_c("y",i), xref = str_c("x",i), showarrow = F, y = yloc, x = xloc, bgcolor = "white", borderwidth = 1, borderpad = 4, bordercolor = "black"))
  #   }
  # }

  if(is.logical(artefacts)) {
    roboplotr_check_param(artefacts, c("logical"))
    if (artefacts == TRUE) {
      params <- getOption("roboplot.artefacts")
      roboplot_create_widget(p, NULL, params$filepath, params$render, params$self_contained, params$artefacts)
    } else { p }
  } else {
    roboplotr_check_param(artefacts, c("function"), NULL, f.name = list(fun = substitute(artefacts)[1], check = "set_artefacts"))
    roboplot_create_widget(p, artefacts$title, artefacts$filepath, artefacts$render, artefacts$self_contained, artefacts$artefacts)
  }

}


# #' @importFrom dplyr filter group_split
# #' @importFrom purrr map2
# #' @importFrom plotly add_trace layout plot_ly subplot
# #' @importFrom stringr str_replace_all
# roboplotr_get_facet_plot <- function(d, facet_split, height, color, pattern, plot_type, trace_color, highlight, hovertext, plot_mode, ticktypes, plot_axes) {
#
#   split_facet <- d |> group_split(!!facet_split)
#
#   p <- map2(split_facet,seq(length(split_facet)), function(facet, i) {
#
#     p <- plot_ly(facet, x = ~time, height = height, colors = pull(distinct(d,.data$roboplot.trace.color)))
#
#     split_d <- group_split(facet, !!color, !!pattern)
#     split_d <- if(any(plot_type == "scatter")) { rev(split_d) } else { split_d }
#
#     for (g in split_d) {
#       g.dash <- unique(d$roboplot.dash)
#       g.name <- unique(g[[as_name(color)]])
#       g.level <-  which(g.name == levels(g.name))
#       g.type <- unique(g$roboplot.plot.type)
#       g.linewidth <- unique(g$roboplot.linewidth)
#       legend.rank <- g.level * 100  + ( if(!is.null(pattern)){ which(g.dash == levels(g.dash)) * 10 } else  { 0 } )
#       show.legend <- if (i > 1) {F} else if(!is.null(trace_color)) { T } else { roboplotr_highlight_legend(highlight, filter(d, !!color == g.name)) }
#       p <- p |>
#         add_trace(data=g, y = ~value, text = g.name,
#                   texttemplate = NA,
#                   hovertemplate = roboplotr_hovertemplate(hovertext),
#                   line = if(g.type == "scatter") { list(width = g.linewidth, dash = g.dash) } else { NULL },
#                   offsetgroup = if(g.type == "bar") { g.name } else { NULL },
#                   legendgroup = g.name,
#                   legendrank = legend.rank,
#                   showlegend = show.legend,
#                   name = g.name,
#                   color = color,
#                   type = g.type, mode = if(g.type == "scatter") { "lines" } else { NULL }
#         )
#     }
#
#     if(any(!plot_mode %in% c("dodge","line","stack","horizontal"))) {
#       stop("Plot mode must be \"dodge\", \"line\", \"stack\" or \"horizontal\"!", call. = F)
#     } else {
#       p_mode <- ifelse(str_detect(plot_mode, "dodge|horizontal"), "group","relative")
#       p  <- layout(p, barmode = p_mode)
#     }
#     if(i > 1) {
#       p <- p |>
#         roboplotr_set_ticks(ticktypes = ticktypes) |>
#         layout(yaxis = list(range = plot_axes$ylim, showticklabels = F, showline = getOption("roboplot.colors.background")$y != getOption("roboplot.colors.border")$y))
#     }
#
#     p
#
#   })
#
#   subplot(p)
# }

#' @importFrom dplyr arrange desc distinct first group_split mutate pull slice_min summarize
#' @importFrom forcats fct_inorder
#' @importFrom plotly plot_ly layout subplot
#' @importFrom rlang := sym
#' @importFrom stats as.formula
#' @importFrom stringr str_replace_all str_trunc
roboplotr_get_plot <- function(d, height, color, pattern, plot_type, trace_color, highlight, hovertext, legend_maxwidth, secondary_yaxis, legend_position, ticktypes, width, legend_title) {

  plot_colors <- pull(distinct(d,.data$roboplot.trace.color, !!color),"roboplot.trace.color", name = !!color)

  trace_showlegend <- if(is.null(legend_position)) { T } else if (is.na(legend_position)) { F } else { T }

  p <- plot_ly(d, height = height, width = width, colors = plot_colors)

  p_mode <- ifelse(any(str_detect(d$roboplot.plot.mode, "dodge|horizontal")), "group","relative")
  p  <- layout(p, barmode = p_mode)

  d <- mutate(d,
              roboplot.plot.text = if (!is.null(pattern)) {
                if(quo_name(color) != quo_name(pattern)) {
                  str_c(!!color, ", ",!!pattern) |> str_remove(", alkuper\uE4inen")
                } else {!!color}
              } else {!!color},
              roboplot.legend.rank = ((as.numeric(!!color)-1) * 100) + ((as.numeric(.data$roboplot.dash)-1)*10))

  if("horizontal" %in% d$roboplot.plot.mode) {
    d <- roboplotr_get_bar_widths(d, ticktypes$y) |> arrange(desc(!!sym(ticktypes$y)))
    if(length(unique(d$roboplot.plot.text)) > 1) {
      d <- mutate(d, roboplot.horizontal.label = str_c(as.character(.data$roboplot.plot.text)))
    } else {
      d <- mutate(d, roboplot.horizontal.label = as.character(!!sym(ticktypes$y)))
    }
    if(!is.null(legend_maxwidth)) {
      d <- d |> mutate(roboplot.trunc = str_trunc(as.character((!!sym(ticktypes$y))), legend_maxwidth) |> fct_inorder())
    }
  }

  if("pie" %in% plot_type) {
    split_d <- d |> arrange(!!color) |> group_split(.data$roboplot.plot.type, .data$roboplot.dash)
  } else {
    split_d <- arrange(d, !!color) |> group_split(!!color, .data$roboplot.plot.type, .data$roboplot.dash)
  }

  if("scatter" %in% plot_type) { split_d <- rev(split_d)}

  rotation <- if("rotated" %in% d$roboplot.plot.mode) {
    -(group_by(d, !!color) |> summarize(value = sum(.data$value), .groups = "drop") |> mutate(value = .data$value / sum(.data$value)) |> slice_min(!!color) |> pull(.data$value) * 360 / 2)
  } else { 0 }

  if(!is.null(height) & !is.null(legend_position)) {
    if(length(split_d) > height / 50 & !is.na(legend_position)) {
      roboplotr_alert(str_c("This many legend items might make the legend too large to render for some widths, you might want to use 'height' of ",length(split_d) * 50,"."))
    }
  }
  trace_params <- map(split_d, function(g) {

    tracetype <- unique(g$roboplot.plot.type)
    hoverlab <- case_when(tracetype == "pie" ~ "label",
                          "horizontal" %in% g$roboplot.plot.type & is.null(pattern) & quo_name(color) != ticktypes$y ~ "text",
                          TRUE ~ "customdata")
    hovertemplate <- roboplotr_hovertemplate(hovertext, lab = hoverlab, ticktypes)
    marker_line_color <- NULL
    legend_rank <- mean(g$roboplot.legend.rank)
    if(tracetype == "pie") {
      g <- mutate(g, roboplot.bg.color = roboplotr_alter_color(.data$roboplot.trace.color,"dark"),
                  roboplot.tx.color = roboplotr_text_color_picker(roboplotr_alter_color(.data$roboplot.trace.color,"dark")),
                  roboplot.in.tx.color = roboplotr_text_color_picker(.data$roboplot.trace.color))
      background_color <- getOption("roboplot.colors.background")
      grid_color <- unique(getOption("roboplot.grid")[c("ycolor","xcolor")])[[1]]
      marker_line_color <- first(grid_color[grid_color != background_color])
      marker_line_color <- replace(marker_line_color, length(marker_line_color) == 0, roboplotr_alter_color(background_color,"darker"))
    }

    show.legend <- if(is.null(highlight)) { trace_showlegend } else { roboplotr_highlight_legend(highlight, g) }

    # ei syyst\uE4 tai toisesta toimi plotlyss\uE4 t\uE4ll\uE4 hetkell\uE4 kunnolla legendgrouptitle, perehdy
    # if(!is.null(secondary_yaxis)) {
    # legendgrouptitle <- ifelse(unique(as.numeric(pull(g, {{secondary_yaxis}}))) == 2, "<b>Oikea akseli</b>", "Vasen akseli")
    # } else {
    #       legendgrouptitle <- NULL
    #       }
    roboplotr_check_param(legend_title, c("logical","character"), allow_null = F)
    legendgrouptitle <- if (legend_title == F) { NULL } else if ( is.character(legend_title) ) { str_c("<b>",legend_title,"</b>") } else { str_c("<b>",as_name(color),"</b>") }

    plotting_params <- list(color = color, #!pie
                            customdata = if("horizontal" %in% g$roboplot.plot.mode) { ~ roboplot.horizontal.label } else { ~ roboplot.plot.text },
                            data=g,
                            direction = "clockwise", #pie
                            xhoverformat = if(ticktypes$xticktype == "date") { hovertext$dateformat } else { NULL },
                            hoverlabel = list(bgcolor = ~ roboplot.bg.color, color = ~ roboplot.tx.color), #pie
                            hovertemplate = hovertemplate,#if(length(unique(g$tFime))==1 & plot_mode != "horizontal") { ~ str_c(.data$roboplot.plot.text,"\n",format(round(.data$value,hovertext$rounding), scientific = F, big.mark = " ", decimal.mark = ","),hovertext$unit,"<extra></extra>") } else { hovertemplate },
                            insidetextfont = list(family = getOption("roboplot.font.main")$family, size = getOption("roboplot.font.main")$size, color = ~ roboplot.in.tx.color), #pie
                            labels = color, #pie
                            legendgroup = color,
                            legendrank = legend_rank,
                            line = ~ list(width = roboplot.linewidth, dash = roboplot.dash,
                                          smoothing = 0.5,
                                          shape = case_when(roboplot.plot.mode == "step" ~ "hv", roboplot.plot.mode == "smooth" ~ "spline", TRUE ~ "linear")), #scatter line
                            marker = list(colors = ~ roboplot.trace.color, line = list(color = marker_line_color, width = 1), pattern = list(shape = ~ roboplot.pattern)), #pie
                            mode = str_replace_all(unique(g$roboplot.plot.mode), c("^smooth$" = "line", "^line$" = "lines", "^step$" = "lines", "^scatter$" = "markers", "scatter\\+line" = "markers+lines")), #scatter
                            name = ~ if(!is.null(legend_maxwidth)) { str_trunc(as.character(roboplot.plot.text), legend_maxwidth) } else { roboplot.plot.text }, #!pie
                            offset = ~roboplot.bar.offset, #horizontal bar
                            offsetgroup = ~str_c(roboplot.pattern, roboplot.trace.color), #bar ## onko ok?? mieti
                            orientation = ifelse("horizontal" %in% g$roboplot.plot.mode & "bar" %in% g$roboplot.plot.type,"h","v"),
                            rotation = rotation, #pie
                            showlegend = show.legend,
                            sort = F, #pie
                            text = if("horizontal" %in% g$roboplot.plot.mode) { ~ roboplot.horizontal.label } else { ~ roboplot.plot.text },
                            textinfo = "percent", #pie
                            textposition = ifelse(tracetype == "bar", "none", "inside"), #pie and bar
                            texttemplate = if(tracetype == "pie") { NULL } else { NA },
                            type = ~ tracetype,
                            legendgrouptitle = list(text = legendgrouptitle, font = getOption("roboplot.font.main")[c("color","family","size")]),
                            values = as.formula(str_c("~",ticktypes$y)), # pie
                            width = ~roboplot.bar.width, #horizontal bar
                            x = as.formula(str_c("~",ticktypes$x)), #!pie
                            y = as.formula(str_c("~",ifelse(!is.null(legend_maxwidth) & ticktypes$y != "value", "roboplot.trunc", ticktypes$y))) #!pie
    )
    shared_params <- c("data","text","texttemplate","hovertemplate","legendgroup","showlegend","type","hoverinfo","legendgrouptitle","customdata")
    plotting_params <- if(tracetype %in% "scatter" & any(c("line","step","smooth") %in% g$roboplot.plot.mode)) {
      plotting_params[c(shared_params,"x","y","line","mode","name","color", "xhoverformat")]
    } else if(tracetype == "scatter" & "scatter+line" %in% g$roboplot.plot.mode) {
      plotting_params[c(shared_params,"x","y","line","mode","name","color", "xhoverformat")]
    } else if(tracetype == "scatter" & "scatter" %in% g$roboplot.plot.mode) {
      plotting_params[c(shared_params,"x","y","mode","name","color", "xhoverformat")]
    } else if (tracetype == "bar" & "horizontal" %in% g$roboplot.plot.mode) {
      plotting_params[c(shared_params,"x","y","offsetgroup","orientation","offset","width","color","name","textposition","marker")]
    } else if (tracetype == "bar") {
      plotting_params[c(shared_params,"x","y","offsetgroup","name","color", "textposition","marker","xhoverformat")]
    } else if (tracetype == "pie") {
      plotting_params[c(shared_params,"labels","textposition","textinfo","insidetextfont","direction","rotation","sort","hoverlabel","marker", "values")]
    }

    if(!is.null(secondary_yaxis)) {
      if(unique(as.numeric(pull(g, {{secondary_yaxis}}))) |> length() > 1) {
        stop("The column referred to by argument 'secondary_yaxis' must be plotted to one axis per trace. Try setting it to match the argument 'color'.", call. = F)
      }
      if(unique(as.numeric(pull(g, {{secondary_yaxis}}))) == 2 ) {
        plotting_params$yaxis <- "y2"
      }
    }

    plotting_params
  })


  for(par in trace_params) {
    p <- p |> roboplotr_add_trace(!!!par)
  }

  if(!is.null(secondary_yaxis)) {
    y2 <- roboplotr_get_tick_layout("double","y")
    y2 <- append(y2, list(overlaying = "y", side = "right", position = 1, anchor = "free", gridcolor = roboplotr_alter_color(getOption("roboplot.grid")$ycolor,"lighter"), size = 1))
    p <- p |> layout(yaxis2 = y2)
  }

  # hoverlabel font is determined currently here, move to an appropriate place..
  p |> layout(hoverlabel = list(font = getOption("roboplot.font.main")[c("family","size")])) |> config(responsive = ifelse(isRunning(),F,T))

}

#' @importFrom plotly add_trace
#' @importFrom rlang list2
roboplotr_add_trace <- function(...) {
  do.call(add_trace, list2(...))
}

#' @importFrom dplyr group_by mutate summarize
#' @importFrom purrr map_chr map2_chr map2_lgl
#' @importFrom rlang := as_name
#' @importFrom stringr str_glue str_replace_all
roboplotr_set_plot_mode <- function(d, color, plot_mode, groups) {

  plot_modes <- list(
    "scatter" = c("line","scatter","step","scatter+line","smooth"),
    "bar" = c("dodge","stack","horizontal"),
    "pie" = c("normal","rotated")
  )
  if(is.null(plot_mode)) {
    d <- mutate(d, roboplot.plot.mode = map_chr(.data$roboplot.plot.type, ~ plot_modes[[.x]][[1]]))
  } else {

    if (length(plot_mode) == 1 & is.null(names(plot_mode))){
      d <- d |> mutate(roboplot.plot.mode = plot_mode)
    } else {
      d <- d |> mutate(roboplot.plot.mode = map2_chr(.data$roboplot.plot.type, !!color, ~ ifelse(.y %in% names(plot_mode), str_replace_all(.y, plot_mode), plot_modes[[.x]][[1]])))
    }
  }

  modecheck <- distinct(d, !!color, .data$roboplot.plot.type, .data$roboplot.plot.mode) |>
    filter(map2_lgl(.data$roboplot.plot.type, .data$roboplot.plot.mode, ~ !.y %in% plot_modes[[.x]]))
  if(nrow(modecheck) > 0) {
    modecheck <- group_by(modecheck, .data$roboplot.plot.type) |> summarize("{{color}}" := roboplotr_combine_words(!!color))
    msg <- roboplotr_combine_words(
      map2_chr(
        modecheck$roboplot.plot.type,
        modecheck[[as_name(color)]],
        ~ str_glue("{as_name(color)} entry \"{.y}\" must have 'plot_mode' of {roboplotr:::roboplotr_combine_words(plot_modes[[.x]], and = ' or ')}")))
    stop(msg, call. = F)
  }
  d
}
