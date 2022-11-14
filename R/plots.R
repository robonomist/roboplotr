#' @importFrom dplyr case_when
#' @importFrom stringr str_length
#' @importFrom plotly plot_ly
#' @importFrom lubridate as_date today
roboplotr_config <- function(p,
                            title, subtitle = "", caption,
                            legend_position, legend_orientation, legend_order,
                            height,
                            margin = NA,
                            zeroline = F,
                            axis_range = list(x = c(NA,NA), y = c(NA,NA)),
                            enable_rangeslider = list(rangeslider = F, max = as_date(today)),
                            ticktypes = list(x = "time", y = "double")) {


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
      margin <- list(t = 0, r = 30, b = 0, l = 20)
    }
  }

  p |>
    roboplotr_dependencies(title, subtitle) |>
    roboplotr_set_axes(axis_range) |>
    roboplotr_set_grid() |>
    roboplotr_set_background() |>
    roboplotr_modebar(title, p$subtitle) |>
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
roboplotr_dependencies <- function(p, title, subtitle) {

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
  if(str_detect(p$plot_mode,"stack") && any(p$trace_types == "bar")) { rangeslider_sums = T }
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
    onRender(jsCode = str_c("
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
                        });
                        }"), data = list(plotTitle = plot_title,
                                         rangesliderSums = rangeslider_sums,
                                         legendFontsize = getOption("roboplot.font.main")$size,
                                         piePlot = pie_plot#,
                                         # dataString = desc_string,
                                         # titleString = ttl_string,
                                         # ariaID = ariaid
                        ))
}



#' Get properly size scaling plotly plots
#'
#' Wrapper for [plotly::plot_ly] for shorthand declaration of many layout and trace arguments.
#' Ensures proper scaling or elements when used in shiny apps, iframes, static image downloads and so on.
#'
#' @param d Data frame. Data to be plotted with at least the columns "time" (Date or POSIXt) and "value" (numeric).
#' @param color Expression. Variable from argument 'd' to use for trace color. If left NULL, the argument 'subtitle' will be used as a placeholder for determining color and hoverlabels.
#' @param pattern Expression. Variable from argument 'd' to use for linetype or bar pattern. Not supported for bar charts.
#' @param title,subtitle Characters. Labels for plot elements.
#' @param caption Function or character. Use [roboplot_set_caption()].
#' @param legend_position,legend_orientation Characters. Currently only legend_position is used, and takes only "bottom" or NA for no legend. Legend is removed on default if the argument 'color' in argument 'd' has only one observation.
#' @param zeroline Logical or double. Determines zeroline inclusion, TRUE for zeroline, or double for exact placement.
#' @param rangeslider Logical or character in %Y-%m-%d format. Determines rangeslider inclusion. TRUE includes the rangeslider, a character string includes the rangeslider with the given date as a start date.
#' @param axis_limits List. Determines the limits of the axes (list(x = c(NA,NA), y = c(NA,NA)))".
#' @param hovertext Function. Use [roboplot_set_hovertext()].
#' @param highlight Double or list. Determines if a given trace is included in legend and assigned a color.
#' If double, traces with max(value) < highlight will be give trace color matching the grid color, and removed from the legend.
#' If function, it must return a logical and include named items "value" and ".fun", where .fun checks if given value will get a color or legend item.
#' Will not currently work with multiple patterns.
#' @param plot_type Character vector, named if length > 1. Determines the trace type for either the whole plot, or for all variables defined by color as name-value pairs.
#' @param plot_mode Character. Determines the barmode for bars and scatters. Can be "scatter" or "line" or lines, "dodge", "horizontal" or "stack" for bars, and "rotated" for pies.
#' plot_mode "rotated" controls if the 0°-mark of a pie is centered on middle of the first item of the color variable as factor.
#' @param plot_yaxis Expression. Variable from argument 'd' to use as y-axis for a horizontal barplot. Disregarded for other plots.
#' @param trace_color Character vector, named if length > 1. Trace color for all trace. Determines the trace type for either the whole plot, or for all variables defined by color as name-value pairs.
#' @param line_width Double vector, named if length > 1. Line width for all line traces. Determines the line width for either the whole plot, or for all variables defined by color as name-value pairs.
#' @param height Double. Height of the plot.
#' @param facet_split Currently unused. Variable from argument 'd' to use for facet splits.
#' @param legend_maxwidth Double. Legend items (and y-axis values for horizontal barplots) longer than this will be collapsed with an ellipsis (Double).
#' @param xaxis_ceiling Character. One of "default", "days", "months", "weeks", "quarters", "years", or "guess"). How to round the upper bound of plot x-axis for other than bar plots if no axis limits are given.
#' @param secondary_yaxis Expression. Variable from argument 'd' resulting in a maximum of two factor levels, determining which observations if any use a secondary y-axis.
#' Parameter 'zeroline' will be ignored. Cannot currently differentiate between the axes in legend, and right margin will not scale properly on zoom and possibly on png generation.
#' @return A list of classes "plotly" and "html"
#' @examples
#' d <- energiantuonti |>
#'   dplyr::filter(Alue %in% c("Kanada","Norja","Yhdistynyt kuningaskunta"))
#' d1 <- d |> dplyr::filter(Suunta == "Tuonti")
#' p <- d1 |> roboplot(Alue,
#'                     title = "Energian tuonti",
#'                     subtitle = "Milj. €",
#'                     caption = "Tilastokeskus")
#'
#' p
#'
#' # Legend will automatically be omitted if only a single observation exists
#' # for 'color' is unless legend_position is given (currently only "bottom"
#' # works). Caption may be further specified with the helper function
#' # roboplot_set_caption.
#' p <- d1 |>
#'   dplyr::filter(Alue == "Yhdistynyt kuningaskunta") |>
#'   roboplot(Alue,"Energian tuonti Yhdistyneestä kuningaskunnasta","Milj. €",
#'            caption = roboplot_set_caption(text = "Tilastokeskus",
#'                                           updated = TRUE,
#'                                           .data = d1
#'            )
#'   )
#' p
#'
#' # Legend can also be omitted by giving a legend_position of NA. Height can
#' # also be specified.
#' p <- d1 |>
#'   roboplot(Alue,"Energian tuonti","Milj. €",
#'            caption = "Tilastokeskus",
#'            legend_position = NA,
#'            height = 600)
#' p
#'
#' # Pattern can be used for lines and ordering your variables to factors
#' # will affect the order in which the traces are added.
#' # You can also let roboplotr guess how much space is given to yaxis end
#' # in line plots, or give a string such as "weeks" or "days" to it.
#' # Message about missing frequency data can be silenced by setting the
#' # information as an attribute of the used data.
#' d2 <- d |> dplyr::mutate(Alue = forcats::fct_reorder(Alue, value))
#' attr(d2, "frequency") <- "Quarterly"
#' p <- d2 |>
#'   roboplot(Alue,"Energian tuonti ja vienti","Milj. €","Tilastokeskus",
#'            pattern = Suunta, xaxis_ceiling = "guess")
#' p
#'
#' # Bar plots use a pattern too
#' p <- d2 |>
#'   roboplot(Alue,"Energian tuonti ja vienti","Milj. €","Tilastokeskus",
#'            pattern = Suunta,
#'            plot_type = "bar")
#' p
#'
#' # Scatter plots and bar plot may be combined, and colors determined by
#' # trace by giving named character vectors as the appropriate arguments.
#' # Barmode or scatter type is controlled by plot_mode, where you can
#' # provide a character string combining the desired modes with "+". Plot mode
#' # cannot currently be controlled per trace.
#' p <- d1 |>
#'   roboplot(Alue,"Energian tuonti ja vienti","Milj. €","Tilastokeskus",
#'            trace_color =  c("Kanada" = "red","Norja" = "blue", .other = "black"),
#'            plot_mode = "scatter+stack",
#'            plot_type = c("Norja" = "bar","Kanada" = "scatter",".other" = "bar"))
#' p
#'
#'
#' # With single 'time' observation x-axis tickmarks lose tick labels and
#' # hovertemplate loses the time information. There are several places where
#' # this information fits nicely.
#' d3 <- d2 |> dplyr::filter(time == max(time))
#' p <- d3 |>
#'   roboplot(Alue,
#'            glue::glue("Energian tuonti ja vienti vuonna {lubridate::year(max(d3$time))}"),
#'            glue::glue("Milj. € ({lubridate::year(max(d3$time))})"),
#'            pattern = Suunta,
#'            plot_type = "bar",
#'            caption = roboplot_set_caption(text = "Tilastokeskus",
#'                                           updated = TRUE,
#'                                           .data = d1,
#'                                           line.end = "!",
#'                                           prepend = glue::glue(
#'                                             "Tieto vuodelta {lubridate::year(max(d3$time))}"),
#'                                           append = glue::glue(
#'                                             "Toistan, vuonna {lubridate::year(max(d3$time))}")
#'            ))
#' p
#'
#' # Bar plot can be horizontal but then is better off with only a single 'time'
#' # observation. Long legend items and axis labels can be cut off with
#' # legend_maxwidth, while still showing the proper labels on hover.
#' p <- d3 |>
#'   dplyr::mutate(Suunta = paste0(Suunta, " määrämaittain")) |>
#'   roboplot(Suunta,
#'            glue::glue("Energian tuonti {lubridate::year(max(d$time))}"),
#'            "Milj. €","Tilastokeskus",
#'            plot_type = "bar",
#'            legend_maxwidth = 12,
#'            plot_mode = "horizontal",
#'            plot_yaxis = Alue)
#' p
#'
#' # Pie plots are possible too, but pattern is currently ignored by plotly library.
#' p <- d3 |>
#'   roboplot(Alue,"Energian tuonti ja vienti yhteensä","Milj. €","Tilastokeskus",
#'            pattern = Suunta,
#'            plot_type = "pie")
#' p
#'
#' # Pie plot can be centered to the first factor level of argument 'color' with
#' # with plot_mode "rotated".
#' p <- d3 |>
#'   roboplot(Alue,"Energian tuonti ja vienti yhteensä","Milj. €",
#'            "Tilastokeskus",
#'            plot_type = "pie",
#'            plot_mode = "rotated")
#' p
#'
#' # You can give a highlight value if you don't have a pattern. Any trace with a
#' # "value" equal or higher than the given value will get colors as normal. Others
#' # get assigned a bacground grid color and no legend entry. Useful mostly with
#' # very large amounts of traces.
#' p <- d2 |> dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue, "Energian tuonti","Milj. €","Tilastokeskus",
#'            plot_type = "scatter",
#'            highlight = 160)
#' p
#'
#' # This works best with line plots, but can be included in other plots, too -
#' # with varying results, these are work in progress. Highlight can also be a list
#' # with "value" and ".fun" used to determine which traces are highlighted. The
#' # default usage is essentially list(value = highlight, .fun = sum).
#' p <- d2 |> dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue, "Energian tuonti","Milj. €","Tilastokeskus",
#'            plot_type = "bar",
#'            highlight = list(value = 22, .fun = mean))
#' p
#'
#' # Rangeslider can be added as TRUE or FALSE, or as character in date format of
#' # %Y-%m-%d, in which case the given date will control where the rangeslider is
#' # initially set. Zeroline can be controlled in a similar way.
#' p <- d2 |> dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue, "Energian tuonti","Milj. €","Tilastokeskus",
#'            rangeslider = "2014-01-01",
#'            zeroline = 128)
#' p
#'
#' # Secondary yaxis can be added to line plots when the corresponding variable
#' # only has two unique observations that is a subset of the variable 'color'.
#' # There is currently no way of differentiating between the axes in legend.
#' # Zeroline will not behave as expected, but will instead refer to right yaxis.
#' p <- d2 |> dplyr::filter(Suunta == "Tuonti") |>
#'   dplyr::mutate(sec_axis = ifelse(Alue == "Norja","Norja","Muu")) |>
#'   roboplot(Alue, "Energian tuonti","Milj. €","Tilastokeskus",
#'            plot_type = c("Norja" = "bar", ".other" = "scatter"),
#'            secondary_yaxis = sec_axis,
#'            zeroline = 80)
#' p
#' @export
#' @importFrom dplyr coalesce distinct group_split pull
#' @importFrom forcats fct_reorder
#' @importFrom lubridate as_date ceiling_date is.Date
#' @importFrom purrr map2
#' @importFrom rlang as_name enquo quo quo_get_env quo_name
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
                     axis_limits = list(x = c(NA,NA), y = c(NA,NA)),
                     plot_type = "scatter",
                     plot_mode = "line+dodge",
                     plot_yaxis,
                     height = getOption("roboplot.height"),
                     facet_split = NULL,
                     legend_maxwidth = NULL,
                     xaxis_ceiling = getOption("roboplot.yaxis.ceiling"),
                     secondary_yaxis = NULL
){

  margin <- NA # mieti mitä tällä tehdään, poistuuko kokonaan? Todenäköisesti

  if(missing(d)){
    stop("Argument 'd' must a data frame with columns named \"time\" and \"value\"!", call. = F)
  }

  roboplotr_check_param(d, "data.frame", NULL, allow_null = F)

  d_names <- names(d)


  if(!all(c("time","value") %in% d_names)) {
    stop("'d' must be a tibble with columns named \"time\" and \"value\".", call. = F)
  } else if (!all(class(d$time) %in% c("POSIXct","POSIXt","Date"), class(d$value) == "numeric")) {
    stop("Argument 'd' column \"time\" must be a \"Date\" or \"POSIXct\", and the column \"value\" must be numeric.", call. = F)
  }

  if(missing(color)) {
    color <- quo(!!sym(".topic"))
  } else {
    color <- roboplotr_check_valid_var(enquo(color), d_names)
  }

  if(as_name(color) == ".topic"){
    roboplotr_alert("Without an unquoted arg 'color' the variable named \".topic\" is added to data 'd', using the argument 'subtitle' trunctated to 30 characters as value for the variable.")
    d <- mutate(d, .topic = str_trunc(subtitle,30))
  }

  pattern <- roboplotr_check_valid_var(enquo(pattern), d_names)

  secondary_yaxis <- roboplotr_check_valid_var(enquo(secondary_yaxis), d_names)

  if(!is.null(secondary_yaxis)) {
    if(!is.factor(pull(d,{{secondary_yaxis}}))) {
      if (pull(d,{{secondary_yaxis}}) |> unique() |> length() > 2) {
        stop("No more than two unique observations can be in the variable provided for 'secondary_yaxis'.", call. = F)
      }
      d <- d |> mutate({{secondary_yaxis}} := fct_reorder({{secondary_yaxis}}, .data$value, .desc = T))
      if(unique(pull(d, !!secondary_yaxis)) |> length() > 1) {
        zeroline <- F
        rmargin <- (max(str_length(filter(d, as.numeric(!!secondary_yaxis) == max(as.numeric({{secondary_yaxis}})))$value), na.rm = T) * getOption("roboplot.font.main")$size / 1.5) |> max(30)
        margin <- list(t = 0, r = rmargin, b = 0, l = 20)
        roboplotr_alert("Secondary_yaxis cannot currently differentiate between the axes in legend, and right margin will not scale properly on zoom and possibly on png generation.")
      }
    }
  }

  roboplotr_check_param(hovertext, "function", NULL, f.name = list(fun = first(substitute(hovertext)), check = "roboplot_set_hovertext"))

    if(is.null(hovertext)) {
    hovertext <- roboplot_set_hovertext(roboplotr_get_dateformat(d),unit = tolower(subtitle))
  } else if (is.null(hovertext$dateformat)) {
    hovertext$dateformat <- roboplotr_hovertemplate_freq(roboplotr_get_dateformat(d))
  }

  roboplotr_check_param(caption, c("character","function"),size = 1, f.name = list(fun = substitute(hovertext), check = "roboplot_set_caption", var = substitute(caption)))
  if(!is.null(caption)) {
    if(!is(substitute(caption), "call")) {
      caption <- roboplot_set_caption(caption,.data = d)
    }
  }

  roboplotr_check_param(xaxis_ceiling, "character", size = NULL, allow_null = F)
  xaxis_ceiling <- match.arg(xaxis_ceiling, c("default","days","months","weeks","quarters","years","guess"))
  if(xaxis_ceiling != "default" & all(is.na(axis_limits$x)) & !"bar" %in% plot_type & !str_detect(plot_mode, "horizontal")) {
    if(xaxis_ceiling == "guess") {
      xaxis_ceiling <- roboplotr_guess_xaxis_ceiling(d, hovertext)
    }
    if(!is.null(xaxis_ceiling)) {
      axis_limits$x <- c(min(d$time), as_date(ceiling_date(max(d$time), xaxis_ceiling)))
    }
  } else if (xaxis_ceiling != "default" & (!any(is.na(axis_limits$x)) || any(c("bar","pie") %in% plot_type) || str_detect(plot_mode, "horizontal"))) {
    roboplotr_alert("'xaxis_ceiling' is ignored when \"bar\" or \"pie\" is in 'plot_type', \"horizontal\" is in 'plot_mode', or 'axis_limits' for x axis are provided.")
  }

  if(!is.null(facet_split)) {
    stop("Facet split currently unavailable!", call. = F)
    facet_split <- roboplotr_check_valid_var(enquo(facet_split), d_names)
    if(rangeslider == T | zeroline == T | any(!is.na(axis_limits$y))) roboplotr_alert("Rangeslider, zeroline and y-axis range are not currently enabled for faceted plots.")
    rangeslider <- F
    zeroline <- F
    ymin <- min(d$value)
    ymax <- max(d$value)
    axdif <- diff(c(ymin, ymax)) * 0.04
    ymin <- ymin - axdif
    ymax <- ymax + axdif
    axis_limits$y <- c(ymin, ymax)
  }


  if(str_detect(plot_mode, "horizontal")) {
    if(missing(plot_yaxis)) { stop("When plot mode is horizontal, provide the unquoted variable name used for y-axis as the argument 'plot_yaxis'!", call. = F) }
    xaxis <- "value"
    roboplotr_check_valid_var(enquo(plot_yaxis), d_names)
    plot_yaxis <- enquo(plot_yaxis)
    yaxis <- quo_name(plot_yaxis)
    ticktypes <- list(x="double", y = "character", dateformat = hovertext$dateformat)
  } else {
    xaxis <- "time"
    yaxis <- "value"
    ticktypes <- list(x="date",y = "double", dateformat = hovertext$dateformat)
  }

  if(!is.factor(d[[as_name(color)]])) {
    d <- mutate(d, {{color}} := fct_reorder({{color}}, .data$value, .desc = T))
  }

  d <- d|> group_by(!!color) |> filter(!all(is.na(.data$value))) |> ungroup() |> droplevels()

  unique_groups <- d[[as_name(color)]] |> unique() |> sort()

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




  if(!all(typeof(line_width) == "double")) {
    stop("Line width must be a double, or a named double vector!", call. = F)
  } else if (length(line_width) == 1 & is.null(names(line_width))){
    d <- d |> mutate(roboplot.linewidth = line_width)
  } else if (!all(unique_groups %in% names(line_width)) & !(".other" %in% names(line_width)) ) {
    stop(str_c("All variables in column \"",as_name(color),"\" must have a corresponding line width, or key \".other\" must be included!"), call. = F)
  } else {
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

  if(!is.null(facet_split)) {
    p <- roboplotr_get_facet_plot(d, facet_split, height, color, pattern, plot_type, trace_color, highlight, hovertext, plot_mode, ticktypes, axis_limits)
  } else {
    p <- roboplotr_get_plot(d, xaxis, yaxis, height, color, pattern, plot_type, trace_color, highlight, hovertext, plot_mode, legend_maxwidth, secondary_yaxis, legend_position)
  }

  p$data <- roboplotr_transform_data_for_download(d, color, pattern, facet_split, plot_mode, plot_yaxis)

  if(!isRunning()) { p$elementId <- str_c("widget_",roboplotr_string2filename(title),"_",str_pad(round(runif(1)*1000000),6,"left","0")) }
  p$title <- title
  p$subtitle <- subtitle
  p$trace_types <- distinct(d, !!color, .data$roboplot.plot.type) |> pull(2,1)
  p$plot_mode <- plot_mode

  maxtime <- max(d$time)

  # if only one group for color, remove legend as default
  legend_order <- ifelse("scatter" %in% plot_type, "reversed", "normal")

  p <- p |>
    roboplotr_config(title = title, subtitle = subtitle, caption = caption,
                    legend_position = legend_position, legend_orientation = legend_orientation, legend_order = legend_order,
                    margin = margin,
                    height = height,
                    axis_range = axis_limits,
                    zeroline = list(zeroline = zeroline, xrange = list(min = min(d$time), max = maxtime)),
                    enable_rangeslider = list(rangeslider = rangeslider, max = maxtime),
                    ticktypes = ticktypes)

  ## add labels for facet plot. Has to be done here for the relayout js to work properly for captions.
  if(!is.null(facet_split)) {
    yloc <- max(d$value)
    split_facet <- group_split(d,!!facet_split)
    for(i in seq(length(split_facet))) {
      xloc <- split_facet[[i]]$time |> unique() |> sort() |> median()
      ann_text <- split_facet[[i]][[as_name(facet_split)]] |> unique() |> str_pad(width = 10, side = "both", pad = " ")
      p <- p |>
        layout(annotations =
                 list(text = ann_text, yref = str_c("y",i), xref = str_c("x",i), showarrow = F, y = yloc, x = xloc, bgcolor = "white", borderwidth = 1, borderpad = 4, bordercolor = "black"))
    }
  }

  p

}


#' @importFrom dplyr filter group_split
#' @importFrom purrr map2
#' @importFrom plotly add_trace layout plot_ly subplot
#' @importFrom stringr str_replace_all
roboplotr_get_facet_plot <- function(d, facet_split, height, color, pattern, plot_type, trace_color, highlight, hovertext, plot_mode, ticktypes, axis_limits) {

  split_facet <- d |> group_split(!!facet_split)

  p <- map2(split_facet,seq(length(split_facet)), function(facet, i) {

    p <- plot_ly(facet, x = ~time, height = height, colors = pull(distinct(d,.data$roboplot.trace.color)))

    split_d <- group_split(facet, !!color, !!pattern)
    split_d <- if(any(plot_type == "scatter")) { rev(split_d) } else { split_d }

    for (g in split_d) {
      g.dash <- unique(d$roboplot.dash)
      g.name <- unique(g[[as_name(color)]])
      g.level <-  which(g.name == levels(g.name))
      g.type <- unique(g$roboplot.plot.type)
      g.linewidth <- unique(g$roboplot.linewidth)
      legend.rank <- g.level * 100  + ( if(!is.null(pattern)){ which(g.dash == levels(g.dash)) * 10 } else  { 0 } )
      show.legend <- if (i > 1) {F} else if(!is.null(trace_color)) { T } else { roboplotr_highlight_legend(highlight, filter(d, !!color == g.name)) }
      p <- p |>
        add_trace(data=g, y = ~value, text = g.name,
                  texttemplate = NA,
                  hovertemplate = roboplotr_hovertemplate(hovertext),
                  line = if(g.type == "scatter") { list(width = g.linewidth, dash = g.dash) } else { NULL },
                  offsetgroup = if(g.type == "bar") { g.name } else { NULL },
                  legendgroup = g.name,
                  legendrank = legend.rank,
                  showlegend = show.legend,
                  name = g.name,
                  color = color,
                  type = g.type, mode = if(g.type == "scatter") { "lines" } else { NULL }
        )
    }

    if(any(!plot_mode %in% c("dodge","line","stack","horizontal"))) {
      stop("Plot mode must be \"dodge\", \"line\", \"stack\" or \"horizontal\"!", call. = F)
    } else {
      p_mode <- ifelse(str_detect(plot_mode, "dodge|horizontal"), "group","relative")
      p  <- layout(p, barmode = p_mode)
    }
    if(i > 1) {
      p <- p |>
        roboplotr_set_ticks(ticktypes = ticktypes) |>
        layout(yaxis = list(range = axis_limits$y, showticklabels = F, showline = getOption("roboplot.colors.background")$y != getOption("roboplot.colors.border")$y))
    }

    p

  })

  subplot(p)
}

#' @importFrom dplyr arrange distinct first group_split mutate pull slice_min summarize
#' @importFrom forcats fct_inorder
#' @importFrom plotly plot_ly layout subplot
#' @importFrom rlang := sym
#' @importFrom stats as.formula
#' @importFrom stringr str_replace_all str_trunc
roboplotr_get_plot <- function(d, xaxis, yaxis, height, color, pattern, plot_type, trace_color, highlight, hovertext, plot_mode, legend_maxwidth, secondary_yaxis, legend_position) {

  plot_colors <- pull(distinct(d,.data$roboplot.trace.color, !!color))

  trace_showlegend <- if(is.null(legend_position)) { T } else if (is.na(legend_position)) { F } else { T }

  p <- plot_ly(d, height = height, colors = plot_colors)

  if("scatter" %in% plot_type & !str_detect(plot_mode, "line|scatter") | "bar" %in% plot_type & !str_detect(plot_mode, "dodge|stack|horizontal") ) {
    stop("Plot mode must be \"dodge\", \"line\", \"scatter\", \"stack\" or \"horizontal\", or a combination of two separated by \"+\" for different plot types !", call. = F)
  } else {
    p_mode <- ifelse(str_detect(plot_mode, "dodge|horizontal"), "group","relative")
    p  <- layout(p, barmode = p_mode)
  }

  d <- mutate(d,
              roboplot.plot.text = if (!is.null(pattern)) {
                if(quo_name(color) != quo_name(pattern)) {
                  str_c(!!color, ", ",!!pattern) |> str_remove(", alkuper\uE4inen")
                } else {!!color}
                } else {!!color},
              roboplot.legend.rank = ((as.numeric(!!color)-1) * 100) + ((as.numeric(.data$roboplot.dash)-1)*10))

  if(str_detect(plot_mode,"horizontal")) {
    d <- roboplotr_get_bar_widths(d, yaxis)
    if(length(unique(d$roboplot.plot.text)) > 1) {
      d <- mutate(d, roboplot.horizontal.label = str_c(as.character(!!sym(yaxis)),", ",as.character(.data$roboplot.plot.text)))
    } else {
      d <- mutate(d, roboplot.horizontal.label = as.character(!!sym(yaxis)))
    }
    if(!is.null(legend_maxwidth)) {
      d <- arrange(d, !!sym(yaxis)) |>
        mutate(roboplot.trunc = str_trunc(as.character((!!sym(yaxis))), legend_maxwidth) |> fct_inorder())
    }
  }

  if("pie" %in% plot_type) {
    split_d <- d |> arrange(!!color) |> group_split(.data$roboplot.plot.type, .data$roboplot.dash)
  } else {
    split_d <- group_split(d, !!color, .data$roboplot.plot.type, .data$roboplot.dash)
  }

  if("scatter" %in% plot_type) { split_d <- rev(split_d)}

  rotation <- if(str_detect(plot_mode, "rotated")) {
    -(group_by(d, !!color) |> summarize(value = sum(.data$value), .groups = "drop") |> mutate(value = .data$value / sum(.data$value)) |> slice_min(!!color) |> pull(.data$value) * 360 / 2)
  } else { 0 }

  if(length(split_d) > height / 50) {
    roboplotr_alert(str_c("This many legend items might make the legend too large to render for some widths, you might want to use 'height' of ",length(split_d) * 50,"."))
  }

  trace_params <- map(split_d, function(g) {

    tracetype <- unique(g$roboplot.plot.type)
    hovertime <- if (tracetype == "pie" | str_detect(plot_mode, "horizontal")) {NULL} else { "x" }
    hoverlab <- case_when(tracetype == "pie" ~ "label", str_detect(plot_mode, "horizontal") ~ "text", TRUE ~ "text")
    hoverval <- case_when(tracetype == "pie" ~ "value",  tracetype == "bar" & str_detect(plot_mode,"horizontal") ~ "x", TRUE ~ "y")
    hovertemplate <- roboplotr_hovertemplate(hovertext, val = hoverval, lab = hoverlab, time = hovertime)
    marker_line_color <- NULL
    legend_rank <- mean(g$roboplot.legend.rank)
    if(tracetype == "pie") {
      tx_colors <- roboplotr_text_color_picker(roboplotr_alter_color(plot_colors,"darker"))
      in_tx_colors <- roboplotr_text_color_picker(plot_colors)
      g <- mutate(g, roboplot.bg.color = roboplotr_alter_color(.data$roboplot.trace.color,"darker"),
                  roboplot.tx.color = tx_colors[!!color],
                  roboplot.in.tx.color = in_tx_colors[!!color])
      background_color <- getOption("roboplot.colors.background")
      grid_color <- getOption("roboplot.colors.grid")
      marker_line_color <- first(grid_color[grid_color != background_color])
      marker_line_color <- replace(marker_line_color, length(marker_line_color) == 0, roboplotr_alter_color(background_color,"darker"))
    }

    show.legend <- if(is.null(highlight)) { trace_showlegend } else { roboplotr_highlight_legend(highlight, g) }

    # ei syystä tai toisesta toimi plotlyssä tällä hetkellä kunnolla legendgrouptitle, perehdy
    # if(!is.null(secondary_yaxis)) {
        # legendgrouptitle <- ifelse(unique(as.numeric(pull(g, {{secondary_yaxis}}))) == 2, "<b>Oikea akseli</b>", "Vasen akseli")
    # } else {
    #       legendgrouptitle <- NULL
    #       }

    plotting_params <- list(color = color, #!pie
                            customdata = ~ roboplot.plot.text,
                            data=g,
                            direction = "clockwise", #pie
                            xhoverformat = roboplotr_hovertemplate_freq(hovertext$dateformat),
                            hoverlabel = list(family = getOption("roboplot.font.main")$family, size = getOption("roboplot.font.main")$size, bgcolor = ~ roboplot.bg.color, color = ~ roboplot.tx.color), #pie
                            hovertemplate = if(length(unique(g$time))==1 & plot_mode != "horizontal") { ~ str_c(.data$roboplot.plot.text,"\n",format(round(.data$value,hovertext$rounding), scientific = F, big.mark = " ", decimal.mark = ","),hovertext$unit,"<extra></extra>") } else { hovertemplate },
                            insidetextfont = list(family = getOption("roboplot.font.main")$family, size = getOption("roboplot.font.main")$size, color = ~ roboplot.in.tx.color), #pie
                            labels = color, #pie
                            legendgroup = color,
                            legendrank = legend_rank,
                            line = ~ list(width = roboplot.linewidth, dash = roboplot.dash), #scatter line
                            marker = list(colors = ~ roboplot.trace.color, line = list(color = marker_line_color, width = 1), pattern = list(shape = ~ roboplot.pattern)), #pie
                            mode = case_when(str_detect(plot_mode, "line") ~ "lines", TRUE ~ "markers") , #scatter
                            name = ~  if(!is.null(legend_maxwidth)) { str_trunc(as.character(roboplot.plot.text), legend_maxwidth) } else { roboplot.plot.text }, #!pie
                            offset = ~roboplot.bar.offset, #horizontal bar
                            offsetgroup = ~str_c(roboplot.pattern, roboplot.trace.color), #bar ## onko ok?? mieti
                            orientation = ifelse(str_detect(plot_mode,"horizontal") & plot_type == "bar","h","v"),
                            pattern = "x",
                            rotation = rotation, #pie
                            showlegend = show.legend,
                            sort = F, #pie
                            text = if(str_detect(plot_mode,"horizontal")) { ~ roboplot.horizontal.label } else { ~ roboplot.plot.text },
                            textinfo = "percent", #pie
                            textposition = ifelse(tracetype == "bar", "none", "inside"), #pie and bar
                            texttemplate = if(tracetype == "pie") { NULL } else { NA },
                            type = ~ tracetype,
                            # legendgrouptitle = list(text = legendgrouptitle, font = list(family = getOption("roboplot.font.main")$family, size = getOption("roboplot.font.main")$size)),
                            values = as.formula(str_c("~",yaxis)), # pie
                            width = ~roboplot.bar.width, #horizontal bar
                            x = as.formula(str_c("~",xaxis)), #!pie
                            y = as.formula(str_c("~",ifelse(!is.null(legend_maxwidth) & yaxis != "value", "roboplot.trunc", yaxis))) #!pie
    )
    shared_params <- c("data","text","texttemplate","hovertemplate","legendgroup","showlegend","type","hoverinfo") #"legendgrouptitle"
    plotting_params <- if(tracetype == "scatter" & str_detect(plot_mode,"line")) {
      plotting_params[c(shared_params,"x","y","line","mode","name","color", "xhoverformat")]
    } else if(tracetype == "scatter" & str_detect(plot_mode,"scatter")) {
      plotting_params[c(shared_params,"x","y","mode","name","color", "xhoverformat")]
    } else if (tracetype == "bar" & str_detect(plot_mode,"horizontal")) {
      plotting_params[c(shared_params,"x","y","offsetgroup","orientation","offset","width","color","name","textposition","marker","customdata")]
    } else if (tracetype == "bar") {
      plotting_params[c(shared_params,"x","y","offsetgroup","name","color", "textposition","marker")]
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
    y2 <- append(y2, list(overlaying = "y", side = "right", position = 1, anchor = "free", gridcolor = roboplotr_alter_color(getOption("roboplot.colors.grid")$y,"lighter"), size = 1))
    p <- p |> layout(yaxis2 = y2)
  }

  p

}

#' @importFrom plotly add_trace
#' @importFrom rlang list2
roboplotr_add_trace <- function(...) {
  do.call(add_trace, list2(...))
}
