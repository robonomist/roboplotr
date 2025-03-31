#' @importFrom dplyr case_when
#' @importFrom stringr str_length
#' @importFrom plotly plot_ly
#' @importFrom purrr imap
#' @importFrom lubridate as_date today
roboplotr_config <- function(p,
                             title,
                             subtitle = "",
                             caption,
                             legend,
                             height,
                             width,
                             margin = NA,
                             zeroline = set_zeroline(F),
                             shadearea = NULL,
                             enable_rangeslider = list(rangeslider = F, max = as_date(today())),
                             ticktypes,
                             hovermode,
                             info_text,
                             modebar,
                             zoom) {
  if (!is.list(margin)) {
    if (is.na(margin)) {
      margin <- list(t = 0,
                     r = 5,
                     b = 0,
                     l = 5)
    }
  }

  p |>
    roboplotr_dependencies(title, subtitle, ticktypes, legend) |>
    roboplotr_grid() |>
    roboplotr_set_background() |>
    roboplotr_modebar(title,
                      subtitle,
                      caption,
                      height,
                      width,
                      ticktypes$dateformat,
                      info_text,
                      modebar,
                      legend
                      ) |>
    roboplotr_set_ticks(ticktypes) |>
    roboplotr_set_margin(margin, zoom) |>
    roboplotr_logo() |>
    roboplotr_legend(legend) |>
    roboplotr_title(title, subtitle) |>
    roboplotr_caption(caption) |>
    roboplotr_add_shapes(zeroline, shadearea) |>
    roboplotr_rangeslider(enable_rangeslider) |>
    roboplotr_set_axis_ranges(ticktypes[c("xlim", "ylim")], enable_rangeslider$rangeslider, hovermode) |>
    roboplotr_hoverlabel()
}


#' @importFrom dplyr add_row
#' @importFrom htmltools tagList tags
#' @importFrom htmlwidgets appendContent onRender
#' @importFrom RCurl base64Encode
#' @importFrom shiny isRunning
#' @importFrom stringr str_c str_extract_all str_replace_all str_squish str_pad str_wrap
roboplotr_dependencies <- function(p,
                                   title,
                                   subtitle,
                                   ticktypes,
                                   legend) {
  if (title$include == T) {
    plot_title <-
      list(title$title,
           subtitle,
           getOption("roboplot.font.title")$bold)
  } else {
    plot_title <-
      list("", subtitle, getOption("roboplot.font.title")$bold)
  }

  if (!getOption("roboplot.shinyapp")) {
    if (is.null(getOption("roboplot.widget.deps.session"))) {
      deps <- roboplotr_widget_deps()
      options("roboplot.widget.deps.session" = deps)
    } else {
      deps <- getOption("roboplot.widget.deps.session")
    }

    p <- appendContent(p, tagList(
      tags$script(src = deps$js),
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = deps$css
      )
    ))
  }

  rangeslider_sums <- F
  if (any(str_detect(p$plot_mode, "stack")) &&
      any(p$trace_types == "bar")) {
    rangeslider_sums = T
  }
  pie_plot <- if (any(p$trace_types == "pie")) {
    T
  } else {
    F
  }

  p |>
    onRender(
      jsCode = "function (gd, params, data){
                        let plot_title = data.plotTitle;
                        if (!gd._init_xrange && data.piePlot == false) {gd._init_xrange = { x0: gd.layout.xaxis.range[0], x1: gd.layout.xaxis.range[1] };}
                        if (!gd._init_yrange && data.piePlot == false) {gd._init_yrange = { x0: gd.layout.yaxis.range[0], x1: gd.layout.yaxis.range[1] };}
                        for (i = gd.layout.annotations.length - 1; i >= 0; i--) {
        if(gd.layout.annotations[i].text == gd.layout.annotations[0].text && i > 0 ) {
          Plotly.relayout(gd, 'annotations[' + i + ']', 'remove');
        }
                        }
                        let roboplot_logo = new Image();
                        roboplot_logo.src = gd.layout.images[0].source;
                        roboplot_logo = roboplot_logo.width / roboplot_logo.height
                        setVerticalLayout({'width': true}, gd, data.fonts, plot_title, pie_plot = data.piePlot, logo = roboplot_logo, tidy_legend = data.tidyLegend, legend_position = data.legendPosition);
                        setYPositions({'width': true}, gd, data.piePlot);

                        let isIntersecting = false;
                        let relayoutDone = true;

                        gd.on('plotly_relayout',function(eventdata) {
                        if(isIntersecting) {
                        plotlyRelayoutEventFunction(eventdata, gd, data.fonts, plot_title, data.rangesliderSums, pie_plot = data.piePlot, logo = roboplot_logo, tidy_legend = data.tidyLegend, legend_position = data.legendPosition);
                        } else (relayoutDone = true)
                        })


                        let observer = new IntersectionObserver(function(entries) {
                              // Check if the element is intersecting (visible)
                                                              isIntersecting = entries[0].isIntersecting;
                              if(entries[0].isIntersecting & relayoutDone) {
                                // Element is visible, handle the plot rendering or adjustment
//                                console.log(`relayout fired!`);
                                plotlyRelayoutEventFunction({width: true}, gd, data.fonts, plot_title, data.rangesliderSums, pie_plot = data.piePlot, logo = roboplot_logo, tidy_legend = data.tidyLegend, legend_position = data.legendPosition);
                                relayoutDone = false;
//                                observer.disconnect();
                              }
                            }, { threshold: [0,1], rootMargin: '50% 0px 50% 0px' });  // Adjust the threshold as needed

                       observer.observe(gd);  // Start observing the container


                        gd.on('plotly_afterplot', function() {
                        let thisscrollbar = $(gd).find('.scrollbar')
                        thisscrollbar.length > 0 ? thisscrollbar[0].style.visibility = 'hidden' : () => {}
                        let thisclippath = $(gd).find('clipPath[id*=legend] > rect')
                        if(thisclippath.length > 0) {
                          thisclippath = thisclippath[0];
                          thiswidth = thisclippath.getAttribute('width');
                          thisheight = thisclippath.getAttribute('height');
//                          console.log('init width: ' + thiswidth)
                          thisclippath.setAttribute('width',Number(thiswidth)*1.05);
                          thisclippath.setAttribute('height',Number(thisheight)*1.1);
//                          thiswidth = thisclippath.getAttribute('width');
 //                         console.log('recalc width: ' + thiswidth)
                        };
                        })


                            }",
      data = list(
        plotTitle = plot_title,
        rangesliderSums = rangeslider_sums,
        fonts = list(
          legend = getOption("roboplot.font.main")$size,
          x = ticktypes$xfont$size,
          y = ticktypes$yfont$size
        ),
        piePlot = pie_plot,
        tidyLegend = legend$tidy,
        legendPosition = legend$position
      )
    )
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

#' Comprehensive plotly wrapper function
#'
#' This function wraps numerous [Plotly](https://plotly-r.com/overview.html) features
#' into a single, well-parametrized interface. It also manages complex Plotly tasks,
#' such as resizing and customization, simplifying the creation of advanced visualizations.
#'
#' @param d Data frame. Data to be plotted with at least "time" (Date or POSIXt)
#' and "value" (numeric) columns. Other columns can be specified using `plot_axes`
#' via [set_axes()].
#' @param color Symbol or string. Column from `d` to use for trace color. If NULL,
#' `title` text is used for color and hover labels.
#' @param pattern Symbol or string. Column from `d` to use for scatter plot linetype
#' or bar plot pattern. Not supported for pie charts. Use [set_pattern()] for detailed
#' control.
#' @param title,caption Characters or functions. Labels for plot elements. Use
#' [set_title(include = FALSE)][set_title()] to omit the title from the displayed plot but include
#' it in modebar downloads, or alter the title's relative positioning. When using
#' [set_title(include = FALSE)][set_title()], you probably want to include a `title` for [roboplot()]'s
#' internal use. Use [set_caption()] to override any caption defaults set with [set_roboplot_options()].
#' @param subtitle Character. Label for plot element.
#' @param legend Character or function. Use [set_legend()]. If character, use "bottom",
#' or NA for no legend. The legend is removed by default if `color` in `d` has only
#' one observation.
#' @param legend_position Deprecated. Character. Use `legend` instead.
#' @param legend_maxwidth Deprecated. Numeric. Use `legend` instead.
#' @param zeroline Logical or numeric. TRUE to include zeroline, or numeric for
#' exact placement.
#' @param rangeslider Logical or character in %Y-%m-%d format. TRUE to include a
#' rangeslider, or a date string for the start date.
#' @param hovertext Function. Hovertext configuration parameters. Use [set_hovertext()].
#' @param highlight Numeric or list. Determines if a trace is included in the legend
#' and assigned a color. Traces with `max(value) < highlight` are given a trace
#' color matching the grid color and removed from the legend. Will not work with
#' multiple patterns.
#' @param plot_type Character vector, named if length > 1. Determines the trace
#' type for the plot or variables defined by `color`.
#' @param plot_mode Character vector, named if length > 1. Controls plot specifics
#' along with `plot_type`. Available modes vary by `plot_type`. For `plot_type`
#' "scatter", the available modes are "line" "scatter", "smooth", "step" and "scatter+line".
#' For `plot_type` of "bar", the available modes are "dodge" "stack", "horizontal",
#' "horizontalfill" and "horizontalstack". For `plot_type` "pie", the available
#' modes are "normal" and "rotated".
#' @param plot_axes Function. Axis configuration. Use [set_axes()].
#' @param trace_color Character vector, named if length > 1. Trace colors. When
#' named, must correspond to values in column in `d` referenced by `color`.
#' @param line_width Numeric vector, named if length > 1. Line width. When named,
#' must correspond to values in column in `d` referenced by `color`.
#' @param height,width Numeric. Height and width of the plot. Default width is NA
#' for responsive plots.
#' @param markers Function. Marker appearance parameters. Use [set_markers()].
#' @param facet_split Currently unused. Variable from `d` for facet splits.
#' @param shadearea Date, numeric or function. Highlight an area on the plot. Use
#' [set_shadearea()] for detailed control. Works with numeric or date x-axis and numeric y-axis.
#' @param error_bars Deprecated. Use `confidence_interval` instead.
#' @param confidence_interval Function. Confidence layer configuration. Use [set_confidence_interval()].
#' @param secondary_yaxis Character vector. Observations from `color` using a secondary
#' y-axis. Use `plot_axes` with [set_axes()] for more control.
#' @param xaxis_ceiling Character or date. Sets or rounds the upper bound of the
#' x-axis for non-bar plots. Use "guess" for automatic ceiling based on the data.
#' use "days", "weeks", "months", "quarters" or "years" for giving that much room,
#' preceded with a number like "2 weeks", for a specific amount of time. Use "default"
#' for the default set with [set_roboplot_options()].
#' @param artefacts Logical or function. Use [set_artefacts()] for detailed control.
#' TRUE for automated artefact creation based on [set_roboplot_options()].
#' @param labels Function. Use [set_labels()]. Control labels on plot traces. Use
#' params `title`, `plot_axes`, `caption` etc. to control other labels.
#' @param modebar Function. Use [set_modebar()].
#' @param info_text Character. Adds an info button to the modebar with this text, along with plot title and caption.
#' @param updatemenu Function. Use [set_updatemenu()] for detailed control.
#' @param hole Numeric. Hole size for pie charts. Between 0 and 0.9, default is 0.
#' @param zoom Character of function. Use [set_zoom()], or give "none", "scroll", or "drag".
#' @param roboplot_options Character. A name of roboplot options set with [set_roboplot_options()] param `name`.
#' See documentation of [set_roboplot_options()] for details.
#' @param ... Placeholder for other parameters.
#' @returns A list of classes "htmlwidget", "plotly", and  "roboplotr.roboplot"
#' @examples
#' # The default use for `roboplot()` is for line charts. Providing
#' # a title is mandatory, subtitle and color is optional but  very likely
#' # necessary. Use `set_roboplot_options()` to control the various global options.
#'
#'
#' d <- energiantuonti |>
#'   dplyr::filter(Alue %in% c("USA","Norja","Iso-Britannia"))
#' d1 <- d |> dplyr::filter(Suunta == "Tuonti")
#' d1 |> roboplot(color = Alue,
#'                title = "Energian tuonti",
#'                subtitle = "Milj. \u20AC",
#'                caption = "Lähde: Tilastokeskus.")
#'
#' # Legend will automatically be omitted if only a single observation exists
#' # for `color` is unless legend_position is given (currently only "bottom"
#' # works). Caption may be further specified with the helper function
#' # `set_caption()` (see documentation for more control).
#' d1 |>
#'   dplyr::filter(Alue == "Iso-Britannia") |>
#'   roboplot(Alue,"Energian tuonti Yhdistyneest\uE4 kuningaskunnasta","Milj. \u20AC",
#'            caption = set_caption(text = "Tilastokeskus")
#'   )
#'
#' # You can also use `set_roboplot_options()` to preconstruct some caption texts.
#'
#' set_roboplot_options(
#'   caption_template = "{prepend}.<br>Source: {caption}.<br>{append}.")
#'
#' d1 |>
#'   roboplot(Alue, "Energy import","Million euros",
#'            caption = set_caption(
#'              prepend ="Canada",
#'              caption = "Statistics Finland",
#'              append = paste0("(Customs Finland, International trade ",
#'                              "statistics;<br>Radiation and Nuclear Safety ",
#'                              "Authority; Gasum LLC)")))
#'
#' # Reset to defaults
#' set_roboplot_options(reset = TRUE)
#'
#' # You can omit caption by setting it to NA.
#'
#' d1 |> roboplot(Alue, "Energian tuonti","Milj. \u20AC",caption = NA)
#'
#' # Legend can also be omitted by giving `legend_position` of NA. Height and
#' # width can also be specified, while for most uses width specification is
#' # unnecessary, as roboplotr is designed for plots with responsive widths.
#' d1 |> roboplot(Alue,"Energian tuonti","Milj. \u20AC","Tilastokeskus",
#'                legend = set_legend(position = NA),
#'                height = 600,
#'                width = 400
#' )
#'
#' # For a long list of legend items, use `updatemenu`. See the documentation
#' # of `set_updatemenu()` function for more control.
#' energiantuonti |> roboplot(color = Suunta, updatemenu = Alue)
#'
#'
#' # `pattern` can be used in addition to color and you can control the ordering of
#' # the traces by transforming your variables to factors. You can also let
#' # `roboplot()` guess how much space is given to yaxis end in line plots, or give a
#' # string such as "weeks" or "days" with `xaxis_ceiling`. Message about missing
#' # frequency can be silenced  by setting the information as an attribute.
#'
#' d2 <- d |> dplyr::mutate(Alue = forcats::fct_reorder(Alue, value))
#' attr(d2, "frequency") <- "Quarterly"
#' d2 |> roboplot(Alue,"Energian tuonti ja vienti","Milj. \u20AC","Tilastokeskus",
#'                pattern = Suunta,
#'                xaxis_ceiling = "guess")
#'
#' # Bar plots use `pattern` too.
#' d2 |> roboplot(Alue,"Energian tuonti ja vienti","Milj. \u20AC","Tilastokeskus",
#'                pattern = Suunta,
#'                plot_type = "bar")
#' # Use `set_pattern()` if you want more options. See documentation for a more
#' # detailed explanation.
#' d2 |> roboplot(Alue,"Energian tuonti ja vienti","Milj. \u20AC","Tilastokeskus",
#'                               pattern = set_pattern(pattern = Suunta, sep = " - "),
#'                               xaxis_ceiling = "guess")
#' # Scatter plots and bar plot may be combined, and colors determined by
#' # trace by giving named character vectors as the appropriate arguments.
#' # Barmode or scatter type is controlled by `plot_mode`.
#' d1 |> roboplot(Alue,"Energian tuonti ja vienti","Milj. \u20AC","Tilastokeskus",
#'              trace_color =  c("USA" = "red","Norja" = "blue", .other = "black"),
#'              plot_type = c("Norja" = "scatter","USA" = "bar",".other" = "scatter"),
#'              plot_mode = c("Iso-Britannia" = "scatter",
#'                            "Norja" = "scatter+line", ".bar" = "dodge",
#'                            ".scatter" = "line"
#'              ))
#'
#' # If you omit ".other" from the trace colors,` roboplot()` will give the rest of
#' # of the traces colors from the default colors set in `set_roboplot_options()`
#' d1 |> roboplot(Alue,"Energian tuonti ja vienti","Milj. \u20AC","Tilastokeskus",
#'                trace_color = c("USA" = "pink"))
#'
#' # But if you need more control, you're better off just excplicity specifying
#' # the colors.
#' d1 |> roboplot(Alue,"Energian tuonti ja vienti","Milj. \u20AC","Tilastokeskus",
#'   trace_color =
#'     stats::setNames(c("#0052A5", "darkred", "#D52B1E"),
#'                     unique(d1$Alue))
#' )
#'
#' # With single "time" observation x-axis tickmarks lose tick labels. There are
#' # several places where this information fits nicely.
#' d3 <- d2 |> dplyr::filter(time == max(time))
#'
#' d3 |>
#'   roboplot(
#'     Alue,
#'     stringr::str_glue(
#'       "Energian tuonti ja vienti vuonna {lubridate::year(max(d3$time))}"
#'     ),
#'     stringr::str_glue("Milj. \u20AC ({lubridate::year(max(d3$time))})"),
#'     pattern = Suunta,
#'     plot_type = "bar",
#'     caption =
#'       set_caption(
#'         text = stringr::str_glue(
#'           "Tilastokeskus.<br>Tieto vuodelta {lubridate::year(max(d3$time))}"
#'         )
#'       )
#'  )
#'
#' # Plot axis can be controlled with `set_axes()` (see documentation).
#' d2 |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue, "Energian tuonti","Milj. \u20AC","Tilastokeskus",
#'            plot_axes = set_axes(
#'              ytitle = "Arvo",
#'              xformat = "Vuonna %Y",
#'              ylim = c(-100,100))
#'   )
#'
#' # Bar plot can be horizontal with plot axis control and `plot_mode` set to
#' # horizontal but then is better off with only a single "time" observation. Long
#' # legend items and axis labels can be cut off with `legend_maxwidth` in `legend`,
#' # while still showing the proper labels on hover. Control the order of the axes
#' # by explicitly setting them as factors.
#' d3 |>
#'   dplyr::mutate(Suunta = paste0(Suunta, " m\uE4\uE4r\uE4maittain")) |>
#'   dplyr::mutate(Alue = forcats::fct_reorder(Alue, value),
#'                 Suunta = forcats::fct_reorder(Suunta, value) |>
#'                   forcats::fct_rev()
#'   ) |>
#'   roboplot(Suunta,
#'            stringr::str_glue("Energian tuonti {lubridate::year(max(d$time))}"),
#'            "Milj. \u20AC","Tilastokeskus",
#'            plot_type = "bar",
#'            legend = set_legend(maxwidth = 12),
#'            plot_mode = "horizontal",
#'            plot_axes = set_axes(
#'              y = "Alue",
#'              x = "value"
#'              )
#'   )
#'
#' # If you want the bars to fill the available space in a horizontal bar chart,
#' # use `plot_mode` of "horizontalfill" instead of "horizontal".
#' d3 |>
#'   roboplot(Suunta,
#'            stringr::str_glue("Energian tuonti {lubridate::year(max(d$time))}"),
#'            "Milj. \u20AC","Tilastokeskus",
#'            plot_type = "bar",
#'            plot_mode = "horizontalfill",
#'            plot_axes = set_axes(
#'              y = "Alue",
#'              x = "value"
#'              )
#'   )
#'
#'
#' # Or stack the bars horizontally by using "horizontalstack". You might want to
#' # move title and caption to left edge of the container instead of the plot area
#' # with horizontal bars.
#' d3 |>
#'   roboplot(
#'     Suunta,
#'     set_title(
#'       stringr::str_glue("Energian tuonti {lubridate::year(max(d$time))}"),
#'       xref = "container"
#'     ),
#'     "Milj. \u20AC",
#'     set_caption("Tilastokeskus", xref = "container"),
#'     plot_type = "bar",
#'     plot_mode = "horizontalstack",
#'     plot_axes = set_axes(
#'       y = "Alue",
#'       x = "value"
#'     )
#'   )
#'
#' # You can use `secondary_yaxis` to define which observations from 'color' use
#' # go to a secondary yaxis on the right.
#' d2 |>
#'   dplyr::filter(Suunta == "Tuonti", Alue %in% c("Iso-Britannia", "USA", "Norja")) |>
#'   roboplot(Alue,
#'            "Energian tuonti",
#'            "Milj. \u20AC",
#'            "Tilastokeskus",
#'            secondary_yaxis = "Iso-Britannia",
#'            zeroline = 1000)
#'
#' # Furthermore, you can use `set_axes()` in `plot_axes` for further control, like
#' # titles. Documentation for `set_axes()` has more detailed examples.
#' d2 |>
#'   dplyr::filter(Suunta == "Tuonti", Alue %in% c("Iso-Britannia", "USA", "Norja")) |>
#'   roboplot(Alue,
#'            "Energian tuonti",
#'            "Milj. \u20AC",
#'            "Tilastokeskus",
#'            plot_axes = set_axes(y2 = "Iso-Britannia"))
#'
#' # Pie plots are possible too, but `pattern` is currently ignored by `plotly`
#' # and thus by `roboplot()`.
#' d3 |> roboplot(
#'   Alue,
#'   "Energian tuonti ja vienti",
#'   "Milj. \u20AC",
#'   "Tilastokeskus",
#'   pattern = Suunta,
#'   plot_type = "pie"
#' )
#'
#' # Aside from `pattern`, you might want to change `markers` used on "scatter"
#' # plots by using `set_markers()`. You can also include `confidence_interval` on any
#' # numeric axis by specifying them with `set_confidence_interval()`. See both functions
#' # for more details.
#' d2 |>
#' dplyr::filter(Alue == "Norja") |>
#'   dplyr::group_by(Suunta) |>
#'   dplyr::mutate(confidence_interval = sd(value)) |>
#'   dplyr::ungroup() |>
#'   roboplot(Suunta,
#'            plot_type = "scatter",
#'            plot_mode = "scatter",
#'            markers = set_markers("star", 12),
#'            confidence_interval = set_confidence_interval(type = "bars")
#'   )
#' # Pie plot can be centered to the first factor level of argument `color` with
#' # with `plot_mode` "rotated".
#' d3 |> roboplot(Alue,"Energian tuonti ja vienti","Milj. \u20AC",
#'                     "Tilastokeskus",
#'                     plot_type = "pie",
#'                     plot_mode = "rotated")
#'
#' # You can give a `highlight` value if you don't have a `pattern`. Any trace with
# # "value" equal or higher than the given value will get colors as normal. Others
#' # get assigned a background grid color and no legend entry. Useful mostly with
#' # very large amounts of traces.
#'
#' d2 |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue, "Energian tuonti","Milj. \u20AC","Tilastokeskus",
#'            plot_type = "scatter",
#'            highlight = 160)
#'
#' # This works best with line plots, but can be included in other plots, too -
#' # with varying results, these are work in progress. `highlight` can also be a
#' # list with "value" and ".fun" used to determine which traces are highlighted.
#' d2 |> dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue, "Energian tuonti","Milj. \u20AC","Tilastokeskus",
#'            plot_type = "bar",
#'            highlight = list(value = 22, .fun = mean))
#'
#' # `rangeslider` can be added as logical, or as character in date format of
#' # %Y-%m-%d, in which case the given date will control where the `rangeslider`
#' # start is initially set. `zeroline` can be controlled in a similar way.
#' d2 |> dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue, "Energian tuonti","Milj. \u20AC","Tilastokeskus",
#'            rangeslider = "2014-01-01",
#'            zeroline = 128)
#'
#' # `shadearea` can be used to draw attention to specific area of the plot.
#' d2 |> dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue, "Energian tuonti","Milj. \u20AC","Tilastokeskus",
#'            shadearea = "2019-01-01")
#' # Use` set_shadearea()` for more fine-tuned control.
#' d2 |> dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue, "Energian tuonti","Milj. \u20AC","Tilastokeskus",
#'            shadearea = set_shadearea(
#'              xmin = "2010-06-10",
#'              xmax = "2016-01-01",
#'              color = "green",
#'              opacity = 0.5,
#'              border = "gray",
#'              layer = "below"
#'            ))
#' # `roboplotr()` can't currently validate `xmin` and `xmax` for `shadearea`,
#' # and wrong input will probably just fail to produce the area.
#' d2 |> dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue, "Energian tuonti","Milj. \u20AC","Tilastokeskus",
#'            shadearea = set_shadearea(
#'              xmin = "182625",
#'            ))
#' # Finally, you may export from the plots you create either by using `create_widget()`
#' # or simply using the parameter `artefacts`. The global defaults of artefact
#' # creation are set with `set_roboplot_options()`, and for this example the
#' # default filepath will be changed to a temporary directory. See `set_artefacts()`
#' # and `create_widget()` documentation.
#' \dontrun{
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
#' }
#' @export
#' @importFrom dplyr coalesce distinct group_split pull
#' @importFrom forcats fct_reorder
#' @importFrom lifecycle deprecate_stop deprecate_warn deprecated is_present
#' @importFrom lubridate as_date ceiling_date is.Date
#' @importFrom plotly partial_bundle
#' @importFrom purrr map2
#' @importFrom rlang as_label as_name enquo eval_tidy get_expr quo quo_get_env
#' @importFrom stats median runif setNames
#' @importFrom stringr str_c str_detect str_length str_pad str_replace
roboplot <- function(d = NULL,
                     color = NULL,
                     title = NULL,
                     subtitle = "",
                     caption = NULL,
                     legend = NULL,
                     trace_color = NULL,
                     highlight = NULL,
                     zeroline = FALSE,
                     rangeslider = FALSE,
                     pattern = NULL,
                     line_width = getOption("roboplot.linewidth"),
                     hovertext = NULL,
                     plot_type = "scatter",
                     plot_mode = NULL,
                     plot_axes = set_axes(),
                     markers = set_markers(),
                     height = getOption("roboplot.height"),
                     error_bars = deprecated(),
                     confidence_interval = NULL,
                     facet_split = NULL,
                     xaxis_ceiling = getOption("roboplot.xaxis.ceiling"),
                     width = getOption("roboplot.width"),
                     shadearea = NULL,
                     secondary_yaxis = NULL,
                     modebar = NULL,
                     labels = NULL,
                     artefacts = getOption("roboplot.artefacts")$auto,
                     info_text = NULL,
                     updatemenu = NULL,
                     hole = NULL,
                     zoom = getOption("roboplot.zoom"),
                     roboplot_options = NULL,
                     legend_position = deprecated(),
                     legend_maxwidth = deprecated(),
                     ...) {

  .reset_options <- list(roboplot_options = NULL, reset = F)
  on.exit(roboplotr_reset_temp_options(.reset_options))

  margin <- NA # will this be used at all? Probably not.

  .reset_options <- roboplotr_temp_options(roboplot_options)

  roboplotr_typecheck(d, "data.frame", NULL, allow_null = F, allow_na = T)

  if (nrow(d) == 0) {
    return(roboplotr_empty_roboplot(title, caption, info_text))
  }

  title <- roboplotr_set_title(title, d, "in `roboplot()`")

  d_names <- names(d)

  d <- d |> mutate(across(where(is.numeric), as.numeric))

  roboplotr_typecheck(plot_axes, "set_axes", allow_null = F)

  roboplotr_typecheck(zeroline, c("numeric","logical","set_zeroline"), allow_null = F)

  if(!"roboplotr.set_zeroline" %in% class(zeroline)) {
    zeroline <- set_zeroline(zeroline)
  }

  if (!is.null(secondary_yaxis)) {
    # .plot_axes <- substitute(plot_axes)
    if (!is.null(plot_axes$y2)) {
      roboplotr_alert("roboplot() param 'secondary_yaxis' overrides y2 set with param 'plot_axes'")
    } else {
      roboplotr_message(
        "Use roboplot() param 'plot_axes' with set_axes() for more control over secondary yaxis. See documentation."
      )
    }
    # .plot_axes$y2 <- secondary_yaxis
    # plot_axes <- eval(.plot_axes)
    plot_axes$y2 <- secondary_yaxis
  }

  plot_axes <- roboplotr_set_ticktypes(d, plot_axes)

  if (!all(plot_axes[c("x", "y")] %in% d_names)) {
    stop(
      str_glue(
        "'d' must be a data frame with columns named \"{plot_axes$x}\"\"{plot_axes$y}\"!"
      ),
      call. = F
    )
  } else if (!all(
    class(d[[plot_axes$x]]) %in% str_replace(plot_axes$xclass, "log", "numeric"),
    class(d[[plot_axes$y]]) %in% str_replace(plot_axes$yclass, "log", "numeric")
  )) {
    xclasses <-
      roboplotr_combine_words(plot_axes$xclass, sep = '", "', and = '", or "')
    yclasses <-
      roboplotr_combine_words(plot_axes$yclass, sep = '", "', and = '", or "')
    stop(
      str_glue(
        "The data frame 'd' column {plot_axes$x} must be {xclasses} and column {plot_axes$y} must be {yclasses}"
      ),
      call. = F
    )
  }

  if(!"roboplotr.set_zoom" %in% class(zoom)) {zoom <- set_zoom(zoom)}

  color <- enquo(color)
  color <- roboplotr_check_valid_var(color, d_names)

  if (is.null(color)) {
    if (str_length(title$title) == 0) {
      stop(
        "You must either specify a `color` or a `title` with string length of more than 0 for the plot!",
        call. = FALSE
      )
    }
    color <- quo(!!sym("roboplot.topic"))
  }

  if (as_name(color) == "roboplot.topic") {
    roboplotr_alert(
      "You didn't specify a `color`, was this intentional?"
    )
    d <- mutate(d, roboplot.topic = str_trunc(title$title, 30))
  }

  unique_groups <- sort(unique(d[[as_name(color)]]))

  pattern <- enquo(pattern)

  if (quo_is_call(pattern)) {
    pattern <- eval_tidy(pattern)
    roboplotr_typecheck(pattern, "set_pattern")
    pattern_types <- pattern$pattern_types
    pattern_along <- pattern$pattern_along
    roboplotr_check_valid_var(pattern_along, d_names)
    pattern_showlegend <- pattern$show_legend
    pattern_sep <- pattern$pattern_sep
    pattern <- pattern$pattern
  } else {
    pattern_types <- NULL
    pattern_along <- NULL
    pattern_along <- enquo(pattern_along)
    pattern_showlegend <- NULL
    pattern_sep <- ", "
  }

  pattern <- roboplotr_check_valid_var(pattern, d_names)

  roboplotr_typecheck(markers, "set_markers")

  roboplotr_typecheck(width, "numeric", allow_na = T)
  roboplotr_typecheck(height, "numeric", allow_na = T)
  if (!is.null(width)) {
    if (is.na(width)) {
      width <- NULL
    }
  }
  if (!is.null(height)) {
    if (is.na(height)) {
      height <- NULL
    }
  }

  secondary_yaxis <- plot_axes$y2
  roboplotr_typecheck(secondary_yaxis,"character",size = NULL)
  roboplotr_valid_strings(secondary_yaxis, unique_groups, .fun = any, msg = "`secondary_yaxis` in roboplot()")

  if (!is.null(secondary_yaxis)) {
    if ("pie" %in% plot_type) {
      roboplotr_alert("secondary_yaxis is ignored with plot_mode \"pie\"!")
      secondary_yaxis <- NULL
    }

    if (is.logical(zeroline$position)) {
      if (zeroline$position == T)
        roboplotr_alert("When using set_axes(y2), zeroline specifications are ignored.")
    } else {
      roboplotr_alert("When using set_axes(y2), zeroline specifications are ignored.")
    }
  }

  if(is_present(error_bars)) {
    deprecate_stop("2.7.0", "roboplotr::roboplot(error_bars)", "roboplotr::roboplot(set_confidence_interval)")
  }


  if(!is.null(confidence_interval)) {
    if(all(is.logical(confidence_interval))) {
      confidence_interval <- set_confidence_interval("area")
    }
  }
  roboplotr_typecheck(confidence_interval, "set_confidence_interval")
  roboplotr_validate_confidence(confidence_interval, d, plot_axes)


  roboplotr_typecheck(modebar, "set_modebar")

  roboplotr_typecheck(hovertext, "set_hovertext")

  if (is.null(hovertext)) {
    hovertext <-
      set_hovertext(roboplotr_get_dateformat(d), unit = tolower(subtitle))
  } else if (is.null(hovertext$dateformat)) {
    hovertext$dateformat <-
      roboplotr_hovertemplate_freq(roboplotr_get_dateformat(d))
  }

  caption <- roboplotr_set_caption(caption, d, "in roboplot()")


  roboplotr_typecheck(xaxis_ceiling, c("character", "Date"), allow_null = F)
  if (!"date" %in% plot_axes$xticktype | !"time" %in% d_names) {
    if (!"default" %in% xaxis_ceiling) {
      roboplotr_alert(
        "'xaxis_ceiling' is ignored if x-axis is not a date or a \"time\" column does not exist."
      )
    }
    xaxis_ceiling <- "default"
  } else if (suppressWarnings(is.na(as_date(xaxis_ceiling)))) {
    `roboplot(xaxis_ceiling)` <- str_remove_all(xaxis_ceiling, "^[0-9]* ")
    roboplotr_valid_strings(
      `roboplot(xaxis_ceiling)`,
      c(
        "default",
        "days",
        "months",
        "weeks",
        "quarters",
        "years",
        "guess"
      ),
      .fun = any
    )
  } else {
    xaxis_ceiling <- as.character(xaxis_ceiling)
  }


  if (xaxis_ceiling != "default" &
      all(is.na(plot_axes$xlim)) &
      !"bar" %in% plot_type & !"horizontal" %in% plot_mode) {
    if (!is.null(xaxis_ceiling)) {
      plot_axes$xlim <-
        c(min(d$time),
          roboplotr_guess_xaxis_ceiling(d, hovertext, xaxis_ceiling))
    }
  } else if (xaxis_ceiling != "default" &
             (any(c("bar", "pie") %in% plot_type) ||
              !"horizontal" %in% plot_mode)) {
    roboplotr_alert(
      "'xaxis_ceiling' is ignored when \"bar\" or \"pie\" is in 'plot_type' or \"horizontal\" is in 'plot_mode'."
    )
  }

  plot_axes <- roboplotr_expand_axis_limits(plot_axes, d, zeroline)


  if (!is.null(facet_split)) {
    stop("Facet split currently unavailable!", call. = F)
    facet_split <- roboplotr_check_valid_var(facet_split, d_names)
    if (rangeslider == T |
        zeroline$position == T |
        any(!is.na(plot_axes$ylim)))
      roboplotr_alert("Rangeslider, zeroline and y-axis range are not currently enabled for faceted plots.")
    rangeslider <- F
    zeroline$position <- F
    ymin <- min(d$value)
    ymax <- max(d$value)
    axdif <- diff(c(ymin, ymax)) * 0.04
    ymin <- ymin - axdif
    ymax <- ymax + axdif
    plot_axes$ylim <- c(ymin, ymax)
  }


  if (any(c("horizontal", "horizontalfill", "horizontalstack") %in% plot_mode)) {
    if (plot_axes$y == "value") {
      roboplotr_alert("Did you want \"value\" to be x-axis? Use the parameter 'plot_axes'.")
    }
  } else if (plot_axes$y != "value" & "bar" %in% plot_mode) {
    roboplotr_alert("Did you want a horizontal bar chart? Use the parameter 'plot_mode'.")
  }

  ticktypes <-
    append(plot_axes,
           list(
             dateformat = hovertext$dateformat,
             reverse = any(str_detect(plot_type, "bar")),
             pie = any(str_detect(plot_type, "pie"))
           ))
  if ((!plot_axes$yticktype %in% "numeric" |
       !plot_axes$xticktype %in% "date") &
      (zeroline$position != F | rangeslider != F)) {
    roboplotr_alert(
      "Parameters 'zeroline' and 'rangeslider' are currently disabled when parameter 'plot_axis' xticktype is not date or yticktype is not numeric!"
    )
    zeroline$position <- F
    rangeslider <- F
  }

  if (!is.factor(d[[as_name(color)]])) {
    if (is.numeric(d[[plot_axes$y]])) {
      d <-
        mutate(d,
               {{color}} := fct_reorder({{color}}, as.numeric(.data[[plot_axes$y]]), .fun = mean, .na_rm = T) |> fct_rev())
      if(length(levels(d[[as_label(color)]])) > 1) {
        roboplotr_message(
          str_glue(
            "roboplotr arranged data 'd' column `{as_label(color)}` using mean of '{plot_axes$y}'. Relevel `{as_label(color)}` as factor with levels of your liking to control trace order."
          )
        )
      }
    } else {
      d <- mutate(d, {{color}} := fct_inorder({{color}}))
      roboplotr_message(str_glue(
        "roboplotr arranged data 'd' column '{as_label(color)}' as factor."
      ))
    }
  }

  if("value" %in% names(d)) {
    d <-
      d |> group_by(!!color) |> filter(!all(is.na(.data$value))) |> ungroup() |> droplevels()

  }

  if (!all(plot_type %in% c("scatter", "bar", "pie"))) {
    stop(
      "Plot type must be \"scatter\", \"bar\", or \"pie\", or a named character vector!",
      call. = F
    )
  } else if ("pie" %in% plot_type & length(unique(plot_type)) > 1) {
    stop("Roboplotr is unable to combine the plot_type \"pie\" with other plot types!",
         call. = F)
  } else if (length(plot_type) == 1 & is.null(names(plot_type))) {
    d <- d |> mutate(roboplot.plot.type = plot_type)
  } else if (!all(unique_groups %in% names(plot_type)) &
             !(".other" %in% names(plot_type))) {
    stop(
      str_c(
        "All variables in column \"",
        as_name(color),
        "\" must have a corresponding 'plot_type', or key \".other\" must be included!"
      ),
      call. = F
    )
  } else {
    missing_groups <-
      unique_groups |> subset(!unique_groups %in% names(plot_type))
    if (length(missing_groups) > 0) {
      detected_widths <-
        map2(unname(plot_type), names(plot_type), function(pt, nm) {
          miss <-
            missing_groups |> subset(str_detect(missing_groups, str_c(nm, collapse = "|")))
          rep(pt, length(miss)) |> setNames(miss)
        }) |> roboplotr_compact() |> unlist()
      plot_type <- c(plot_type, detected_widths)
    }
    d <-
      mutate(d, roboplot.plot.type = plot_type[as.character(!!color)] |> coalesce(plot_type[".other"]))
  }

  d <- roboplotr_set_plot_mode(d, color, plot_mode)

  if (!all(typeof(line_width) == "double")) {
    stop("Line width must be a double, or a named double vector!", call. = F)
  } else if (length(line_width) == 1 & is.null(names(line_width))) {
    d <- d |> mutate(roboplot.linewidth = line_width)
  } else {
    if (!all(unique_groups %in% names(line_width))) {
      line_width <-
        c(line_width, c(".other" = getOption("roboplot.linewidth")))
    }
    missing_groups <-
      unique_groups |> subset(!unique_groups %in% names(line_width))
    if (length(missing_groups) > 0) {
      detected_widths <-
        map2(unname(line_width), names(line_width), function(lw, nm) {
          miss <-
            missing_groups |> subset(str_detect(missing_groups, str_c(nm, collapse = "|")))
          rep(lw, length(miss)) |> setNames(miss)
        }) |> roboplotr_compact() |> unlist()
      line_width <- c(line_width, detected_widths)
    }
    d <-
      mutate(d, roboplot.linewidth = line_width[as.character(!!color)] |> coalesce(line_width[".other"]))
  }

  color_vector <-
    roboplotr_set_colors(trace_color, unique_groups, highlight, d, color)

  d <-
    d |>
    roboplotr_get_pattern(pattern, pattern_types) |>
    mutate(roboplot.trace.color = color_vector[as.character(!!color)])

  if (!quo_is_null(pattern_along)) {
    if (!all(unique(d$roboplot.plot.mode) == "line")) {
      roboplotr_alert(
        "'pattern_along' in set_pattern() was ignored, it currently only works with line traces only!"
      )
      pattern_showlegend <- NULL
    } else {
      roboplotr_alert(
        str_glue(
          "roboplotr attempts to make continous line with different patterns along '{as_name(pattern)}' over '{as_name(pattern_along)}. ",
          "Be sure to arrange the data in proper order. For complex data you are better off handling the data manipulation outside of roboplot()."
        )
      )
      d <- roboplotr_continuous_pattern(d, {{pattern_along}}, {{pattern}}, {{color}})
    }
  }

  roboplotr_typecheck(legend, c("character","set_legend"), allow_na = T, allow_null = T)

  if(!is.list(legend)) {
    if (all(is.null(legend),
            length(unique_groups) < 2,
            is.null(pattern))) {
      legend <- NA
    }
    legend <- set_legend(legend)
  }

  if(is_present(legend_position)) {
    deprecate_stop("2.7.0", "roboplotr::roboplot(legend_position)", "roboplotr::roboplot(legend)")
  }

  if(is_present(legend_maxwidth)) {
    deprecate_stop("2.7.0", "roboplotr::roboplot(legend_maxwidth)", "roboplotr::roboplot(legend)")
  }

  pattern_showlegend <- roboplotr_get_pattern_showlegend(d, pattern, pattern_showlegend, legend$position)

    if (legend$title == F) {
      legend$title <- NULL
    } else if (is.character(legend$title)) {
      legend$title <- str_c("<b>", legend$title, "</b>")
    } else {
      legend$title <- str_c("<b>", as_name(color), "</b>")
    }

  updatemenu <- roboplotr_get_updatemenu(enquo(updatemenu), d_names)

  p <-
    roboplotr_get_plot(
      d,
      height,
      color,
      pattern,
      plot_type,
      trace_color,
      highlight,
      hovertext,
      legend,
      secondary_yaxis,
      ticktypes,
      width,
      pattern_showlegend,
      pattern_sep,
      confidence_interval,
      markers,
      updatemenu,
      zoom,
      labels,
      hole = hole
    )
  # }

  p$data <-
    roboplotr_transform_data_for_download(d, color, pattern, ticktypes, confidence_interval)

  if (!getOption("roboplot.shinyapp")) {
    p$elementId <-
      str_c(
        "widget_",
        roboplotr_string2filename(title$title),
        "_",
        str_pad(round(runif(1) * 1000000), 6, "left", "0")
      )
  }
  p$title <- title$title
  p$trace_types <-
    distinct(d, !!color, .data$roboplot.plot.type) |> pull(2, 1)
  p$plot_mode <- plot_mode

  if ("time" %in% d_names & !identical(rangeslider, F)) {
    if (!is.na(plot_axes$xlim[2])) {
      maxtime <- as_date(plot_axes$xlim[2])
    } else {
      maxtime <- roboplotr_guess_xaxis_ceiling(d, hovertext, "guess", what = "rangeslider")
    }
  } else {
    maxtime <- NULL
  }
  if ("time" %in% d_names) {
    mintime <-   min(d$time)
  } else {
    mintime <- NULL
  }
  # if only one group for color, remove legend as default
  if (!is.null(secondary_yaxis)) {
    legend$order <- "grouped"
    # if(all(d$roboplot.plot.type == "bar")) {
    #   legend_order <- "reversed+grouped"
    # } else {
    #   legend_order <- "grouped"
    # }
  } else {
    legend$order <- "normal"
  }

  hover.mode <- "compare"
  if (!"horizontal" %in% plot_mode & any(plot_type == "bar")) {
    if (any(d[[ticktypes$y]] == 0)) {
      hover.mode <- "x"
    }
  }
  if (!is.null(shadearea)) {
    if (ticktypes$xticktype == "date" &
        ticktypes$yticktype == "numeric") {
      proper_shade <- c("Date", "character")
    } else if (ticktypes$xticktype == "numeric" &
               ticktypes$yticktype == "numeric") {
      proper_shade <- "numeric"
    } else {
      stop(
        "roboplotr::roboplot shadearea can't be used unless the x-axis is of type date or numeric and y-axis numeric.",
        call. = F
      )
    }

    roboplotr_typecheck(shadearea, c(proper_shade, "set_shadearea"))
    if (!is.list(shadearea)) {
      if (all(is.na(ticktypes$xlim))) {
        shadexmax <- NULL
      } else if (max(ticktypes$xlim, na.rm = T) > shadearea) {
        shadexmax <- max(ticktypes$xlim, na.rm = T)
      } else {
        shadexmax <- NULL
      }
      shadearea <- set_shadearea(xmin = shadearea, xmax = shadexmax)
    }
    shadearea <- roboplotr_shadearea(d, shadearea)
  }

  if (("pie" %in% plot_type |
       !is.null(secondary_yaxis)) && !legend$tidy) {
    roboplotr_message(
      "When using 'plot_mode' of \"pie\" or when providing a secondary_yaxis, 'tidy_legend = F' is ignored."
    )
  }

  p <- p |>
    roboplotr_config(
      title = title,
      subtitle = subtitle,
      caption = caption,
      legend = legend,
      margin = margin,
      height = height,
      width = width,
      zeroline = list(
        zeroline = zeroline,
        xrange = list(min = mintime, max = mintime)
      ),
      shadearea = shadearea,
      enable_rangeslider = list(rangeslider = rangeslider, max = maxtime),
      ticktypes = ticktypes,
      hovermode = hover.mode,
      info_text = info_text,
      modebar = modebar,
      zoom = zoom
    )

  if (getOption("roboplot.shinyapp") == F) {
    p <- partial_bundle(p, "basic")
    roboplotly_dep <- p$dependencies |>
      imap( ~ if (.x$name == "plotly-basic") {
        .y
      }) |>
      roboplotr_compact() |>
      unlist()
    p$dependencies[[roboplotly_dep]]$version <- "2.35.2"
    p$dependencies[[roboplotly_dep]]$script <- "plotly-basic-2.35.2.min.js"
  }

  p <- structure(p, class = c(class(p), "roboplotr","roboplotr.roboplot"))

  roboplotr_typecheck(artefacts, c("logical","set_artefacts"), allow_null = F)

  if (is.logical(artefacts)) {
    if (artefacts == TRUE) {
      params <- getOption("roboplot.artefacts")
      create_widget(
        p = p,
        title = NULL,
        filepath = params$filepath,
        render = params$render,
        self_contained = params$self_contained,
        zoom = params$zoom,
        artefacts = params$artefacts,
        height = params$height,
        width = params$width,
        delay = params$delay
      )
    } else {
      p
    }
  } else {
    create_widget(
      p = p,
      title = artefacts$title,
      filepath = artefacts$filepath,
      render = artefacts$render,
      self_contained = artefacts$self_contained,
      zoom = artefacts$zoom,
      artefacts = artefacts$artefacts,
      height = artefacts$height,
      width = artefacts$width,
      delay = artefacts$delay
    )
  }

}

#' @importFrom dplyr arrange desc distinct first group_split mutate na_if pull slice_min summarize
#' @importFrom forcats fct_inorder
#' @importFrom plotly plot_ly layout subplot
#' @importFrom rlang := quo_is_null sym
#' @importFrom stats as.formula
#' @importFrom stringr str_replace_all str_trunc
#' @importFrom tidyr fill
roboplotr_get_plot <-
  function(d,
           height,
           color,
           pattern,
           plot_type,
           trace_color,
           highlight,
           hovertext,
           legend,
           secondary_yaxis,
           ticktypes,
           width,
           pattern_showlegend,
           pattern_sep,
           confidence,
           markers,
           updatemenu,
           zoom,
           labels,
           hole
           ) {


    plot_colors <-
      pull(
        distinct(d, .data$roboplot.trace.color, !!color),
        "roboplot.trace.color",
        name = !!color
      )

      if (is.null(legend$position)) {
        trace_showlegend <-T
      } else if (is.na(legend$position)) {
        trace_showlegend <-F
      } else {
        trace_showlegend <-T
      }
    p <-
      plot_ly(d,
              height = height,
              width = width,
              colors = plot_colors)
    p_mode <-
      ifelse(any(str_detect(unique(
        d$roboplot.plot.mode
      ), "stack")), "relative", "group")

    p  <- layout(p, barmode = p_mode)

    d <- d |>
      mutate(
        roboplot.plot.text =
          if (is.na(pattern_sep)) {
            !!color
          } else if (!is.null(pattern) &
                     !all(quo_name(color) == quo_name(pattern))) {
            str_c(!!color, pattern_sep, !!pattern)
          } else {
            !!color
          },
        roboplot.legend.rank = ((as.numeric(!!color) - 1) * 100) + (coalesce(
          as.numeric(.data$roboplot.dash, .data$roboplot.pattern)
        ) - 1) * 10
      ) |>
      mutate(roboplot.legend.rank = (.data$roboplot.legend.rank + abs(min(
        .data$roboplot.legend.rank
      )) + 100))
    # print(d %>% distinct(!!color, roboplot.legend.rank) |> arrange(!!color))
    if (any(c("horizontal", "horizontalfill", "horizontalstack") %in% d$roboplot.plot.mode)) {
      if ("horizontalfill" %in% d$roboplot.plot.mode) {
        d <- roboplotr_get_bar_widths(d, ticktypes$y, color)
      }
      d <-  d |> arrange(desc(!!sym(ticktypes$y)))
      if (length(unique(d$roboplot.plot.text)) > 1) {
        d <-
          mutate(d, roboplot.horizontal.label = str_c(as.character(.data$roboplot.plot.text)))
      } else {
        d <-
          mutate(d, roboplot.horizontal.label = as.character(!!sym(ticktypes$y)))
      }
      if (!is.null(legend$maxwidth)) {
        d <-
          d |> mutate(roboplot.trunc = str_trunc(as.character((
            !!sym(ticktypes$y)
          )), legend$maxwidth) |> fct_inorder())
      }
    }

    if ("pie" %in% plot_type) {
      split_d <-
        d |> arrange(!!color) |> group_split(.data$roboplot.plot.type, .data$roboplot.dash)
      updatemenu <- NULL
    } else {
      split_d <-
        d |>
        mutate(arranger = as.numeric(!!color)) |>
        mutate(arranger = ifelse(
          .data$roboplot.plot.type == "bar",
          (max(.data$arranger[.data$roboplot.plot.type == "bar"]) + 1 -
             .data$arranger) + max(.data$arranger),
          .data$arranger
        ))

      if (!quo_is_null(updatemenu$buttons)) {
        split_d <- split_d |> mutate(roboplot.update.menu = !!updatemenu$buttons)
      } else {
        split_d <- split_d |> mutate(roboplot.update.menu = NA)
      }

      split_d <- split_d |>
        group_split(.data$arranger,
                    .data$roboplot.plot.type,
                    .data$roboplot.dash,
                    .data$roboplot.update.menu
                    )

      split_d <- rev(split_d)

      split_d <- roboplotr_get_confidence_areas(split_d, confidence, ticktypes)
      updatemenu <- roboplotr_set_updatemenu(d, split_d, updatemenu)
    }


    if ("rotated" %in% d$roboplot.plot.mode) {
      rotation <- -(
        group_by(d, !!color) |> summarize(value = sum(.data$value), .groups = "drop") |> mutate(value = .data$value / sum(.data$value)) |> slice_min(!!color) |> pull(.data$value) * 360 / 2
      )
    } else {
      rotation <- 0
    }

    if (!is.null(height) & !is.null(legend$position)) {
      if ((length(split_d) > height / 50 & !is.na(legend$position)) & is.null(updatemenu)) {
        roboplotr_alert(
          str_glue(
            "You have many legend items, you might want to use 'height' of {length(split_d) * 50}, or use param `updatemenu`."
          )
        )
      }
    }

    trace_params <- map(split_d, function(g) {
      tracetype <- unique(g$roboplot.plot.type)
      trace_labels <- roboplotr_trace_labels(tracetype, labels, names(d))
      hoverlab <- case_when(
        tracetype == "pie" ~ "label",
        any(
          c("horizontal", "horizontalfill", "horizontalstack") %in% g$roboplot.plot.type
        ) &
          is.null(pattern) & quo_name(color) != ticktypes$y ~ "text",
        TRUE ~ "customdata"
      )
      hovertemplate <-
        roboplotr_hovertemplate(hovertext, lab = hoverlab, ticktypes)
      if (any(unique(pull(g, !!color)) %in% secondary_yaxis)) {
        legend_rank <- mean(g$roboplot.legend.rank) + max(d$roboplot.legend.rank)
      } else {
        legend_rank <- mean(g$roboplot.legend.rank)
      }
      .fontsize <- getOption("roboplot.font.main")$size
      g <-
        mutate(
          g,
          roboplot.bg.color = roboplotr_alter_color(.data$roboplot.trace.color, "dark"),
          roboplot.tx.color = roboplotr_text_color_picker(.data$roboplot.bg.color, .fontsize)
        )
      text_inside <- any(tracetype == "pie" & trace_labels$style != "none", tracetype == "bar" & trace_labels$style %in% c("mini","auto","inside"))
      if (text_inside) {
        if(trace_labels$style == "mini") {
          g <- mutate(g, roboplot.in.tx.color = "#FFFFFF00")
        } else {
          g <-
            mutate(g,
                   roboplot.in.tx.color = roboplotr_text_color_picker(
                     .data$roboplot.trace.color,
                     getOption("roboplot.font.caption")$size
                   ))
        }
      }

      if(!is.null(attributes(g)$`roboplot.confidence.area`)) {
        show.legend <- attributes(g)$`roboplot.confidence.area`$show_legend
        g <- g |>
          mutate(
            roboplot.confidence.label = str_c(
              .data$roboplot.plot.text,
              "\n",
              .data[[attributes(g)$roboplot.confidence.area$col]]
            ),
            roboplot.plot.text = .data[[attributes(g)$roboplot.confidence.area$col]],
                 )
        if(!is.character(g[[attributes(g)$roboplot.confidence.area$col]])) {
          legend_rank <- legend_rank + 10 + as.numeric(unique(g[[attributes(g)$roboplot.confidence.area$col]]))
        } else {
          legend_rank <- legend_rank + 10 + 1
        }
      } else {
        if (is.null(highlight)) {
          if (!is.null(pattern_showlegend) & trace_showlegend) {
            show.legend <- pattern_showlegend[unique(g[[as_name(pattern)]]) |> as.character()]

          } else {
            show.legend <- trace_showlegend
          }
        } else {
          show.legend <- roboplotr_highlight_legend(highlight, g)
        }
      }


      .legendgrouptitle <- NULL
      if ("pie" %in% plot_type) {
        .legendgroup <- "pie"
      } else {
        .legendgroup <- as.character(unique(pull(g, {{color}})))
      }
      if (!is.null(secondary_yaxis)) {
        if (unique(pull(g, {{color}})) |> as.character() %in% secondary_yaxis) {
          .legendgrouptitle <- ticktypes$y2legend
          .legendgroup <- "R"
        } else {
          .legendgrouptitle <- ticktypes$ylegend
          .legendgroup <- "L"
        }
      }

      error_x <- NULL
      error_y <- NULL
      if (!is.null(confidence$type)) {
        if (confidence$type == "bars") {
          if (!quo_is_null(confidence$error_x)) {
            error_x <- list(array = confidence$error_x, color = confidence$xcolor)
          }
          if (!quo_is_null(confidence$error_y)) {
            error_y <- list(array = g[["roboplot.errorbar.max"]], arrayminus = g[["roboplot.errorbar.min"]], color = confidence$ycolor)
          }
        }
      }

      if(is.null(updatemenu)) {
        trace_visible <- T
      } else {
        trace_visible <- unique(g$roboplot.update.menu) == updatemenu$selected
      }

      roboplotr_typecheck(hole, "numeric", allow_null = TRUE)
      hole <- hole %||% 0
      roboplotr_is_between(hole, "roboplot()", lims = c(0,0.9))

      plotting_params <- list(
        color = color,
        constraintext = ifelse(!is.null(trace_labels$size), "none","both"),
        #!pie
        customdata =
          if(!is.null(attributes(g)$`roboplot.confidence.area`)) {
            ~ roboplot.confidence.label
          } else if (!quo_is_null(hovertext$col)) {
          as.formula(str_c("~", as_label(hovertext$col)))
          } else if (any(
          c("horizontal", "horizontalfill", "horizontalstack") %in% g$roboplot.plot.mode
        )) {
          ~ roboplot.horizontal.label
        } else {
          ~ roboplot.plot.text
        },
        data = g,
        direction = "clockwise",
        error_x = error_x,
        error_y = error_y,
        #error area fill
        fill = unlist(ifelse(!is.null(attributes(g)$`roboplot.confidence.area`), list("toself"),list(NULL))),
        fillcolor ~ color,
        #pie
        hole = hole,
        hoverlabel = list(
          bgcolor = ~ roboplot.bg.color,
          bordercolor = first(unique(getOption(
            "roboplot.grid"
          )[c("xcolor", "ycolor")]) |> unlist()),
          font = ~ append(
            getOption("roboplot.font.main")[c("family", "size")],
            list(color = roboplot.tx.color)
          )
        ),
        hoveron = "points+fills",
        #pie
        hovertemplate = hovertemplate,
        #if(length(unique(g$tFime))==1 & plot_mode != "horizontal") { ~ str_c(.data$roboplot.plot.text,"\n",format(round(.data$value,hovertext$rounding), scientific = F, big.mark = " ", decimal.mark = ","),hovertext$unit,"<extra></extra>") } else { hovertemplate },
        insidetextfont = list(
            family = getOption("roboplot.font.main")$family,
            size = trace_labels$size %||% getOption("roboplot.font.main")$size,
            color = ~ roboplot.in.tx.color
            # color = "white"
          ),
        #pie
        labels = color,
        #pie
        legendgroup = .legendgroup,
        legendgrouptitle = list(text = .legendgrouptitle, font = getOption("roboplot.font.main")[c("color", "family", "size")]),
        legendrank = legend_rank,
        line = ~ list(
          width = roboplot.linewidth,
          dash = roboplot.dash,
          smoothing = attributes(g)$roboplot.confidence.area$smoothing %||% 0.5,
          shape = case_when(
            !is.null(attributes(g)$`roboplot.confidence.area`) ~ "spline",
            roboplot.plot.mode == "step" ~ "hv",
            roboplot.plot.mode == "smooth" ~ "spline",
            TRUE ~ "linear"
          )
        ),
        #scatter line
        marker = list(
          colors = ~ roboplot.trace.color,
          line = getOption("roboplot.trace.border"),
          # pattern = if(is.null(pattern)) {
          #   list(shape = ~ roboplot.pattern)
          # } else {
          #   list(shape = arrange(g, as.character({{pattern}}))$roboplot.pattern)
          # }
          symbol = markers$symbol,
          size = markers$size,
          pattern = ~ list(shape = roboplot.pattern)
        ),
        #pie, bar
        mode = {
          .mode <- str_replace_all(
            unique(g$roboplot.plot.mode),
            c(
              "^smooth$" = "lines",
              "^line$" = "lines",
              "^step$" = "lines",
              "^scatter$" = "markers",
              "scatter\\+line" = "markers+lines"
            )
          )
          if(tracetype == "scatter" & trace_labels$style != "none") {
            .mode <- str_c(.mode, "+text")
          }
          .mode
          },
        #scatter
        name = ~ if (!is.null(legend$maxwidth)) {
          str_trunc(as.character(roboplot.plot.text), legend$maxwidth)
        } else {
          roboplot.plot.text
        },
        #!pie
        offset = if ("horizontalfill" %in% g$roboplot.plot.mode) {
          ~ roboplot.bar.offset
        } else {
          NULL
        },
        #horizontal bar
        offsetgroup = ~ str_c(roboplot.pattern, roboplot.trace.color),
        # error area fill
        opacity = attributes(g)$`roboplot.confidence.area`$opacity,
        #bar ## onko ok?? mieti
        orientation = ifelse(
          any(
            c("horizontal", "horizontalfill", "horizontalstack") %in% g$roboplot.plot.mode
          ) & "bar" %in% g$roboplot.plot.type,
          "h",
          "v"
        ),
        rotation = rotation,
        #pie
        showlegend = show.legend,
        sort = F,
        #pie
        text =
         if(any(c("horizontal", "horizontalfill", "horizontalstack") %in% g$roboplot.plot.mode)) {
          ~ roboplot.horizontal.label
        } else {
          ~ roboplot.plot.text
        },
        textfont = list(color = ~ trace_labels$color %||% roboplot.trace.color, size = ~ trace_labels$size %||% getOption("roboplot.font.main")$size),
        textinfo = "percent",
        #pie
        textposition = {
          if(tracetype == "scatter" & trace_labels$style == "auto") {
            .y <- round(g[[ticktypes$y]], max(hovertext$rounding-2, 0))
            fill(
              tibble(value = c(ifelse(.y[1] >= .y[2],1,-1), sign(diff(.y))) |> na_if(0)),
              .data$value)$value |> as.character() |> str_replace_all(c("-1" = "bottom center", "1" = "top center"))
          } else {
            case_when(
              trace_labels$style == "none" ~ "none",
              tracetype == "bar" & trace_labels$style == "mini" ~ "auto",
              tracetype == "bar" ~ trace_labels$style,
              tracetype == "scatter" & trace_labels$style == "last" ~ "middle right",
              tracetype == "scatter" & trace_labels$style == "auto" ~ "top center",
              # tracetype == "scatter" ~ "auto",
              tracetype == "pie" ~ "inside",
              TRUE ~ "none"
            )

          }
        #   tracetype == "pie" ~ "inside"
        #   trace_labels$style == "auto"
        #   case_when(
        #   "horizontal" %in% g$roboplot.plot.mode ~ "outside",
        #   tracetype == "pie" ~ "inside",
        #   TRUE ~ "none"
        # )
          },
        # textposition = "auto",
        #pie and bar
        texttemplate =  if(!quo_is_null(trace_labels$text_col)) {
          if (trace_labels$style == "last") {
            .y <- g[[as_label(trace_labels$text_col)]]
            .y[-length(.y)] <- ""
            .y } else {
              as.formula(str_c("~", as_label(trace_labels$text_col)))
            }
        } else if (tracetype == "pie") {
          NULL
        } else if (any(c("horizontal", "horizontalfill", "horizontalstack") %in% g$roboplot.plot.mode)) {
          "%{x:,.1f}"
        } else if(tracetype == "scatter" & trace_labels$style == "auto") {
          .y <- round(g[[ticktypes$y]], max(hovertext$rounding-2, 0))
          .p <- fill(
            tibble(value = c(ifelse(.y[1] >= .y[2],1,-1), sign(diff(.y))) |> na_if(0)),
            .data$value)$value
          .y <- tibble(check = c(ifelse(sign(.y[2] - .y[1]) < 0, -1, 1), sign(diff(.y))) |> na_if(0))  |> fill(.data$check) |>
            mutate(check = slider::slide_dbl(.data$check, ~ .x[[1]] != .x[[2]], .after = 1, .complete = T)) |> pull(.data$check)
          .y[c(1, length(.y))] <- 1
          .y <- ifelse(.y, g[[ticktypes$y]], NA) |> roboplotr_format_robotable_numeric(rounding = max(hovertext$rounding-1, 0), na_value = "")
          # map_chr(.y, ~ tags$tspan(as.character(.x)) |> as.character())
          ifelse(.p > 0, str_c(.y,"\n"), str_c("\n",.y))
        } else if (trace_labels$style == "last") {
          .y <- g[[ticktypes$y]]
          .y[-length(.y)] <- ""
          .y
        }  else {
          "%{y:,.1f}"
        },
        type = ~ tracetype,
        values = as.formula(str_c("~", ticktypes$y)),
        #updatemenus
        visible = trace_visible,
        # pie
        width = if ("horizontalfill" %in% g$roboplot.plot.mode) {
          ~ roboplot.bar.width
        } else {
          NULL
        },
        #horizontal bar
        x = as.formula(str_c("~", ticktypes$x)),
        xhoverformat = ifelse(ticktypes$xticktype == "date", ticktypes$dateformat, ""),
        #!pie
        y = as.formula(str_c(
          "~",
          ifelse(
            !is.null(legend$maxwidth) &
              ticktypes$y != "value",
            "roboplot.trunc",
            ticktypes$y
          )
        )) #!pie
      )
      shared_params <-
        c(
          "customdata",
          "data",
          "error_x",
          "error_y",
          "hoverlabel",
          "hovertemplate",
          "legendgroup",
          "legendgrouptitle",
          "legendrank",
          "showlegend",
          "text",
          "textposition",
          "texttemplate",
          "type",
          "visible"
        )

      if(!is.null(attributes(g)$`roboplot.confidence.area`)) {
        shared_params <- c(shared_params, "fill","fillcolor","opacity","hoveron")
      }

      if(text_inside) {
        shared_params <- c(shared_params, "insidetextfont")
      }
      if(!is.null(trace_labels$color) | !is.null(trace_labels$size)) {
        shared_params <- c(shared_params, "textfont")
      }
      if(!is.null(trace_labels$size) & tracetype == "bar") {
        shared_params <- c(shared_params, "constraintext")
      }

      if (tracetype %in% "scatter" &
          any(c("line", "step", "smooth") %in% g$roboplot.plot.mode)) {
        plotting_params <-
          plotting_params[c(shared_params,"x", "y", "line", "mode", "name", "color", "xhoverformat")]
      } else if (tracetype == "scatter" &
                 "scatter+line" %in% g$roboplot.plot.mode) {
        plotting_params <-
          plotting_params[c(shared_params, "x", "y", "line", "mode", "name", "marker", "color", "xhoverformat")]
      } else if (tracetype == "scatter" &
                 "scatter" %in% g$roboplot.plot.mode) {
        plotting_params <-
          plotting_params[c(shared_params, "x", "y", "mode", "name", "color", "marker", "xhoverformat")]
      } else if (tracetype == "bar" &
                 any(c("horizontal", "horizontalfill", "horizontalstack") %in% g$roboplot.plot.mode)) {
        plotting_params <-
          plotting_params[c(shared_params, "x", "y", "offsetgroup", "orientation", "offset", "width", "color", "name", "marker")]
      } else if (tracetype == "bar") {
        plotting_params <-
          plotting_params[c(shared_params,"x", "y", "offsetgroup", "name", "color", "marker", "xhoverformat")]
      } else if (tracetype == "pie") {
        plotting_params <-
          plotting_params[c(shared_params, "labels", "textinfo", "direction", "rotation", "sort", "marker", "values","hole")]
      }

      if (!is.null(secondary_yaxis)) {
        if (unique(pull(g, {{color}})) |> as.character() %in% secondary_yaxis) {
          plotting_params$yaxis <- "y2"
        }
      }

      plotting_params
    })

    add_trace_params <- function(p = p, trace_params = trace_params) {
      for (par in trace_params) {
        p <- p |> roboplotr_add_trace(!!!par)
      }
      p
    }
# browser()
    # map(trace_params, ~ names(.x)) |> unlist() |> unique() |> sort() |> print()

    p <- add_trace_params(p, trace_params)

    if (!is.null(secondary_yaxis)) {
      y2 <- roboplotr_get_tick_layout(
        ticktypes$yclass,
        "y",
        ticktypes$yformat,
        title = ticktypes$y2title,
        font = ticktypes$y2font,
        angle = ticktypes$yangle
      )
      y2 <-
        append(
          y2,
          list(
            overlaying = "y",
            side = "right",
            position = 1,
            anchor = "right",
            gridcolor = roboplotr_alter_color(getOption("roboplot.grid")$ycolor, "lighter"),
            size = 1
          )
        )

      p <- p |> layout(yaxis2 = y2)

    }

    p |>
      layout(updatemenus = updatemenu$menu) |>
      config(responsive = ifelse(isRunning(), F, T), scrollZoom = ifelse(zoom == "scroll", T, F))

  }

#' @importFrom plotly add_trace
#' @importFrom rlang list2
roboplotr_add_trace <- function(...) {
  do.call(add_trace, list2(...))
}

#' @importFrom dplyr group_by mutate summarize left_join
#' @importFrom purrr map_chr map2_chr map2_lgl
#' @importFrom rlang := as_name quo_name
#' @importFrom stringr str_glue str_replace str_replace_all
#' @importFrom tibble tibble
roboplotr_set_plot_mode <- function(d, color, plot_mode, groups) {
  plot_modes <- list(
    "scatter" = c("line", "scatter", "step", "scatter+line", "smooth"),
    "bar" = c(
      "dodge",
      "stack",
      "horizontal",
      "horizontalfill",
      "horizontalstack"
    ),
    "pie" = c("normal", "rotated")
  )
  if (is.null(plot_mode)) {
    d <-
      mutate(d,
             roboplot.plot.mode = map_chr(.data$roboplot.plot.type, ~ plot_modes[[.x]][[1]]))
  } else {
    if (length(plot_mode) == 1 & is.null(names(plot_mode))) {
      d <- d |> mutate(roboplot.plot.mode = plot_mode)
    } else {
      themodes <- tibble(.roboclr = names(plot_mode),
                         roboplot.plot.mode = as.character(plot_mode)) |>
        rename_with( ~ str_replace(.x, ".roboclr", quo_name(color))) |>
        full_join(d |> distinct({{color}}, .data$roboplot.plot.type), by = as_name(color)) |>
        mutate(
          roboplot.plot.mode = map2_chr(
            .data$roboplot.plot.mode,
            .data$roboplot.plot.type,
            ~ replace_na(.x, plot_mode[str_c(".", .y)])
          )
        ) |>
        filter(!is.na(.data$roboplot.plot.type))
      if (any(is.na(themodes$roboplot.plot.mode))) {
        msg <- themodes |> filter(is.na(.data$roboplot.plot.mode))
        def_modes <- roboplotr_combine_words(str_c('\".', unique(msg$roboplot.plot.type), '\"'))
        col_modes <- roboplotr_combine_words(str_c('\"', pull(msg[as_name(color)]), '\"'))
        stop(
          str_glue(
            "Please provide plot_mode for {def_modes}, and / or specify plot_mode(s) for any or all of {col_modes}."
          ),
          call. = F
        )
      }
      themodes <- themodes |> select(-.data$roboplot.plot.type)
      themodes[[quo_name(color)]] <- factor(themodes[[quo_name(color)]], levels = levels(d[[quo_name(color)]]))
      d <- d |>
        left_join(themodes, by = as_name(color)) |>
        mutate(
          roboplot.plot.mode = map2_chr(
            .data$roboplot.plot.mode,
            .data$roboplot.plot.type,
            ~ replace_na(.x, plot_modes[[.y]][[1]])
          )
        )
    }
  }

  modecheck <-
    distinct(d,!!color,
             .data$roboplot.plot.type,
             .data$roboplot.plot.mode) |>
    filter(
      map2_lgl(
        .data$roboplot.plot.type,
        .data$roboplot.plot.mode,
        ~ !.y %in% plot_modes[[.x]]
      )
    )
  if (nrow(modecheck) > 0) {
    modecheck <-
      group_by(modecheck, .data$roboplot.plot.type) |> summarize("{{color}}" := roboplotr_combine_words(!!color))
    msg <- roboplotr_combine_words(map2_chr(
      modecheck$roboplot.plot.type,
      modecheck[[as_name(color)]],
      ~ str_glue(
        "{as_name(color)} entry \"{.y}\" must have 'plot_mode' of {roboplotr:::roboplotr_combine_words(plot_modes[[.x]], and = ' or ')}"
      )
    ))
    stop(msg, call. = F)
  }
  d
}
