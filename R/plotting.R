#' @importFrom plotly layout
roboplot_set_grid <- function(p, grid_color = getOption("roboplot.colors.grid"), border_color = getOption("roboplot.colors.border")) {
  p |> layout(
    xaxis = list(showgrid = TRUE, gridcolor = grid_color$x, linecolor = border_color$x, size = 1.5),
    yaxis = list(showgrid = TRUE, gridcolor = grid_color$y, linecolor = border_color$y, size = 1.5)
  )
}

#' @importFrom plotly layout
roboplot_set_background <- function(p, color = getOption("roboplot.colors.background")) {
  p |> layout(paper_bgcolor = color, plot_bgcolor = color)
}

#' @importFrom plotly layout config
roboplot_set_axes <- function(p, range = list(x = c(NA,NA), y = c(NA,NA))) {
  fixed_range <- if (any(c("zoom","zoomin2d","pan") %in% getOption("roboplot.modebar.buttons"))) { F } else { T }
  if(!"x" %in% names(range)) { range$x <- c(NA,NA) }
  if(!"y" %in% names(range)) { range$y <- c(NA,NA) }
  if(!all(is.na(range$y)) & any(is.na(range$y)) || !all(is.na(range$x)) & any(is.na(range$x))) {
    message("Provide both ends for any axis limits!")
  }
  config(p, locale = "fi") |>
    layout(separators = ", ") |>
    layout(xaxis = if(all(is.na(range$x))) { list(fixedrange = fixed_range) } else { list(fixedrange = fixed_range, range = range$x) },
           yaxis = list(fixedrange = fixed_range, range = range$y)) |>
    layout(hovermode = "compare") |>
    roboplot_set_axis_labels()

}

#' @importFrom stringr str_c str_extract_all str_replace_all str_squish str_wrap
#' @importFrom htmlwidgets JS
#' @importFrom plotly config
#' @importFrom purrr pmap
#' @importFrom tidyr drop_na
#' @importFrom fontawesome fa
roboplot_set_modebar <- function(p, title, subtitle) {

  if(is.na(title)) {
    dl_title <- "img"
  } else {
    dl_title <- str_extract_all(title, "[a-z\u00e5\u00e4\u00f6,A-Z\u00c5\u00c4\u00d6,\\s,_,\\.,0-9]", simplify = T) |>
      roboplot_string2filename()
  }

  js_string <- function(wd, ht, suffix, layout, ttl = dl_title) {
    t1 <- title
    t2 <- str_replace(str_wrap(p$title, round((p$title |> str_length())/2))|> str_replace_all(c("\n" = " ")),"\n","\\<br>") |> str_replace_all(c("<" = "\\<"))
    split_title <- list(
      str_c("\\<span>",getOption("roboplot.font.title")$bold(t1),"<span style='font-size: 75%'><br>",p$subtitle,"\\</span>\\</span>"),
      str_c("\\<span>",getOption("roboplot.font.title")$bold(t2),"\\<span style='font-size: 75%'>\\<br>",p$subtitle,"\\</span>\\</span>"))
    str_c('
          function(gd) {
          let oldlayout = JSON.parse(JSON.stringify(gd.layout))
          delete gd.layout.xaxis.rangeslider;
          delete gd.layout.height
          Plotly.relayout(gd, {height: ',ht,', width: ',wd,',
          "annotations[0].y": ',layout$caption,',
          "annotations[1].y": ',layout$caption,',
          "annotations[2].y": ',layout$caption,',
          "xaxis.tickfont.size": ',layout$main,',
          "yaxis.tickfont.size": ',layout$main,',
          "title.font.size": ',layout$title,'})
          setVerticalLayout({"width": true}, gd, ',layout$main,', ["',split_title[[1]],'","',split_title[[2]],'"], pie_plot = ',if(all(p$trace_types == "pie")) { "true" } else { "false" },')
          Plotly.downloadImage(gd, {format: "png", width: ',wd,', height: ',ht,', filename: "',ttl,'_',suffix,'"});
          Plotly.relayout(gd, oldlayout)
          delete oldlayout
          }
   ')
  }

  dl_icon <- function(fa_icon, scale = 0.032, translate = c(0,0)) {
    transform_string <- str_c("translate(",translate[1],",",translate[2],") scale(",scale,")")
    list(path = fa(name = fa_icon) |> as.character() |> str_extract("(?<=d\\=\")[^\"]{1,}") |> str_replace_all(" ",","),
         transform = transform_string)
  }

  dl_string <- (function() {
    row.data <- p$data |> drop_na() |> pmap(function(...) {
      as.character(list(...)) |> str_replace_all(c(";"=",","'"="\u2019")) |> str_c(collapse = ";")
    }) |> unlist() |> str_c(collapse = "\\n")
    col.names <- names(p$data) |> str_replace("csv\\.data\\.tiedot","tiedot") |> str_c(collapse = ";")
    str_c(col.names,"\\n",row.data)
  })()


  btn_list <- list(
    "home" = "resetViews",
    "zoom" = "zoom2d",
    "pan" = "pan2d",
    "zoomin" = "zoomIn2d",
    "zoomout" = "zoomOut2d",
    "closest" = "hoverClosestCartesian",
    "compare" = "hoverCompareCartesian",
    "img_w" = list(
      name = "Lataa kuva (leve\u00e4)",
      icon = dl_icon("image"),
      click = JS(js_string(1280,720,"levea",getOption("roboplot.png.font.size.lg")))
    ),
    "img_n" = list(
      name = "Lataa kuva (kapea)",
      icon = dl_icon("file-image", 0.025, c(2.7,2)),
      click = JS(js_string(810,720,"kapea",getOption("roboplot.png.font.size.lg")))),
    "img_s" = list(
      name = "Lataa kuva (pieni)",
      icon = dl_icon("twitter-square"),
      click = JS(js_string(889,500,"pieni",getOption("roboplot.png.font.size.sm")))),
    "data_dl" = list(
      name = "Lataa tiedot",
      icon = dl_icon("file-csv"),
      click = JS(str_c("
          function(gd) {
            let text = '",dl_string,"';
            //for(var i = 0; i < gd.data.length; i++){
              //console.log(gd.data[i])
              //var array1 = gd.data[i].y
              //array1.forEach(element => console.log(element));
              //console.log((gd.data[i].y))
              //text = text + gd.data[i].name + ';' + gd.data[i].x + '\\n';
              //text = text + gd.data[i].name + ';' + gd.data[i].y + '\\n';
            //};
            var blob = new Blob([text], {type: 'text/plain;charset=UTF-8'});
            var a = document.createElement('a');
            const object_URL = URL.createObjectURL(blob);
            a.href = object_URL;
            a.download = '",dl_title,"_data.csv';
            document.body.appendChild(a);
            a.click();
            URL.revokeObjectURL(object_URL);
          }
   "))
    ),
    "robonomist" = list(
      name = "Powered by Robonomist",
      icon = list(
        name = "Robonomist",
        svg = '<svg version="1.1" viewBox="0 0 71.447 32" xmlns="http://www.w3.org/2000/svg"><g id="XMLID_248_" transform="scale(.31159)"><polyline id="XMLID_132_" points="229.3 53.2 174.3 90.1 174.3 69.1 199.5 53.2 174.3 37.3 174.3 16.3"/><g id="XMLID_40_"><path id="XMLID_41_" d="m112 0c14.2 0 23.3 1.8 30.7 7 6.3 4.4 10.3 10.8 10.3 20.5 0 11.3-6.4 22.8-22.3 26.5l18.4 32.5c5 8.7 7.7 9.7 12.5 9.7v6.5h-27.3l-23.7-45.8h-7v27.6c0 10.5 0.7 11.7 9.9 11.7v6.5h-43.2v-6.7c10.3 0 11.3-1.6 11.3-11.9v-65.7c0-10.2-1-11.7-11.3-11.7v-6.7zm-4.8 7.9c-3.3 0-3.6 1.5-3.6 8.6v32.3h6.4c15.8 0 20.2-8.7 20.2-21.3 0-6.3-1.7-11.5-5-15-2.9-3-7-4.6-13-4.6z"/></g><polyline id="XMLID_130_" points="0 53.2 55 16.3 55 37.3 29.8 53.2 55 69.1 55 90.1"/></g></svg>'
      ),
      click = JS("function(gd) {window.open(\"https://robonomist.com\")  }")
    )
  )

  btn_list <- btn_list[getOption("roboplot.modebar.buttons")]
  p |> config(
    displaylogo = FALSE,
    modeBarButtons = list(unname(btn_list))
  )
}

#' @importFrom plotly layout
roboplot_set_ticks <- function(p, ticktypes) {
  background_color <- getOption("roboplot.colors.background")
  border_color <- getOption("roboplot.colors.border")
  tick_color <- getOption("roboplot.colors.ticks")
  set_ticks <- function(ticktype, axis) {
    if (ticktype == "date") {
      list(tickfont = getOption("roboplot.font.main"),
           mirror = TRUE,
           ticks = 'outside',
           type = "date",
           tickformatstops = list(
             list(dtickrange = list(NULL, 604800000),value = "%d.%m.%Y"),
             # end is year in milliseconds
             list(dtickrange = list(604800000, 3.15e10),value = "%m/%Y"),
             list(dtickrange = list(3.15e10, NULL),value = "%Y")
           ),
           tickcolor = tick_color[[axis]],
           showline = background_color != border_color[[axis]])
    } else if (ticktype == "double") {
      list(tickfont = getOption("roboplot.font.main"),
           tickformat = ",.3~f",
           ticksuffix = " ",
           mirror = TRUE,
           ticks = 'outside',
           tickcolor = tick_color[[axis]],
           showline = background_color != border_color[[axis]])
    } else if (ticktype == "character") {
      font <- getOption("roboplot.font.main")
      font$size <- font$size - 4
      list(tickfont = font,
           ticksuffix = " ",
           tickmode = "linear",
           mirror = TRUE,
           type = "category",
           ticks = 'outside',
           tickcolor = tick_color[[axis]],
           showline = background_color != border_color[[axis]])
    }

  }
  p |>
    layout(xaxis= set_ticks(ticktypes$x, "x"),
           yaxis= set_ticks(ticktypes$y, "y"))

}

#' @importFrom plotly layout
#' @importFrom stringr str_remove
roboplot_set_margin <-function(p, margin) {

  if (!is.list(margin)) {
    stop("Insert plot margins as list (default is list(t,r,b,l,pad))\nNote that top and bottom margins will be currently ignored but programmatically set instead.", call. = F)
  } else if (any(!names(margin) %in% c("t","r","b","l","pad")) | any(!is.double(unlist(margin)))) {
    stop("All plot margins must be of double type, and named one or more of t, r, b, l or pad.\nNote that top and bottom margins will be currently ignored but programmatically set instead.", call. = F)
  } else {
    drag_mode <- getOption("roboplot.modebar.buttons")
    drag_mode <- if (any(c("zoom","zoomin2d","pan") %in% drag_mode)) { str_remove(drag_mode[drag_mode %in% c("zoom","zoomin2d","pan")][1],"in2d") } else { F }
    p |>
      layout(
        margin = margin,
        autosize = T,
        dragmode = drag_mode
      )
    # p
  }
}

#' @importFrom plotly layout
#' @importFrom RCurl base64Encode
roboplot_add_logo <- function(p){

  image_file <- getOption("roboplot.logo")
  txt <- base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
  p |> plotly::layout(
    images = list(
      source = paste('data:image/png;base64', txt, sep=','),
      xref = "paper",
      yref = "paper",
      x = 1,
      sizex = 1,
      sizey = 1,
      xanchor="right",
      yanchor = "bottom"
    )
  )
}

#' @importFrom plotly layout
#' @importFrom dplyr case_when
#' @importFrom plotly rangeslider
roboplot_add_rangeslider <- function(p, enable = F, slider_range = NULL, height = 0.1) {
  if(enable == T) {
    height <- case_when(height > 0.5 ~ 0.5, height < 0.1 ~ 0.1, TRUE ~ height)
    p |> rangeslider(slider_range[1], slider_range[2], thickness = height)
  } else { p }
}

#' @importFrom dplyr case_when
#' @importFrom stringr str_length
#' @importFrom plotly plot_ly
#' @importFrom lubridate as_date today
roboplot_config <- function(p,
                            title, subtitle = "", caption,
                            legend_position, legend_orientation,
                            height, width = 800,
                            margin = NA,
                            zeroline = F,
                            axis_range = list(x = c(NA,NA), y = c(NA,NA)),
                            enable_rangeslider = list(rangeslider = F, max = as_date(today)),
                            ticktypes = list(x = "time", y = "double")) {

  if(missing(title) | missing(caption)) {
    stop("Title and caption must be strings, or NA for no plot title or plot caption.", call. = F)
  }

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
      margin <- list(t = 0, r = 20, b = 0, l = 20)
    }
  }

  p |>
    roboplot_attach_dependencies(title, subtitle) |>
    roboplot_set_axes(axis_range) |>
    roboplot_set_grid() |>
    roboplot_set_background() |>
    roboplot_set_modebar(title, p$subtitle) |>
    roboplot_set_ticks(ticktypes) |>
    roboplot_set_margin(margin) |>
    roboplot_add_logo() |>
    roboplot_set_legend(legend_position, legend_orientation) |>
    roboplot_set_title(title, subtitle) |>
    roboplot_set_caption(caption) |>
    roboplot_add_zeroline(zeroline) |>
    roboplot_add_rangeslider(enable_rangeslider, slider_range = slider_range)
}


#' @importFrom plotly layout
#' @importFrom stringr str_extract
roboplot_set_legend <- function(p, position, orientation) {
  if (is.na(position)) {
    p |> layout(showlegend = F)
  } else if (!position %in% c("right","bottom")) {
    stop("Position must be one of \"right\", \"left\", or NA")
  } else if (!orientation %in% c("vertical","horizontal","auto")) {
    stop("Orientation must be one of \"horizontal\", \"vertical\", or \"auto\"")
  } else {
    x.pos <- ifelse(position == "right", 100, 0)
    y.pos <- ifelse(position == "right", 1, 0) #-0.05
    orientation <- case_when(orientation == "auto" ~ ifelse(position == "right", "v","h"), TRUE ~ str_extract(orientation, "^(v|h)"))
    p |> layout(
      showlegend = T,
      legend = list(font = getOption("roboplot.font.main"),
                    bgcolor = 'rgba(0,0,0,0)',
                    x = x.pos, y = y.pos,
                    orientation = orientation,
                    xanchor = "left",
                    yanchor = "bottom"))
  }
}


#' @importFrom plotly layout
roboplot_set_caption <- function(p, caption) {
  if(is.na(caption)) {
    p
  } else if (!is.character(caption)) {
    stop("Caption and caption footer must be a string, or caption = NA for no plot caption.", call. = F)
  } else {
    p |>
      layout(
        annotations = list(
          x = 0, text = caption, align = "left",
          showarrow = F, xref = 'paper', yref = "paper",
          xanchor='left', yanchor = 'bottom', xshift=0, yshift=0,
          font = getOption("roboplot.font.caption")
        )
      )
  }
}

#' @importFrom plotly layout
#' @importFrom htmltools tag HTML
roboplot_set_title <- function(p, title, subtitle) {
  if (is.na(title)) {
    p
  } else if (!is.character(title) || !is.character(subtitle)) {
    stop("Title and subtitle must be strings, or title = NA for no plot title.", call. = F)
  } else {
    p <- p |>
      layout(
        title = list(
          text = paste0(
            "<span>",getOption("roboplot.font.title")$bold(title),
            tags$span(HTML(str_c("<br>",subtitle)), style = "font-size: 75%"),"</span>"
          ),
          font = getOption("roboplot.font.title"),
          xanchor = "left",
          x = 0,
          xref = "paper")
      )
  }
}

#' @importFrom plotly layout
#' @importFrom stats na.omit
roboplot_set_axis_labels <- function(p, label_x = NA, label_y = NA) {
  if (any(!is.na(c(label_x, label_y)))||any(is.null(c(label_x, label_y)))) {
    if(!any(is.character(na.omit(c(label_x, label_y))))) {
      stop("Axis labels must be strings or NA.", call. = F)
    }
  }
  p |>
    layout(
      xaxis = list(title = label_x),
      yaxis = list(title = label_y)
    )
}

#' @importFrom plotly layout
roboplot_add_zeroline <- function(p, z) {
  if(!is.logical(z$zeroline) & !is.double(z$zeroline)) {
    stop("Zeroline must be TRUE, FALSE or of type double.", call. = F)
  } else if (z$zeroline == F & !is.numeric(z$zeroline)) {
    p
  } else {
    zero_line <- ifelse(z$zeroline == T, 0, z$zeroline)
    p |> layout(shapes= list(type = "line", x0 = z$xrange$min, x1 = z$xrange$max, xref = "x", y0 = zero_line, y1 = zero_line, yref = "y", layer = "below", line = list(width = 2))) |>
      onRender(jsCode = "
function(gd, params, data) {
let zeroline_relayout = {'shapes[0].x0': gd.layout.xaxis.range[0], 'shapes[0].x1': gd.layout.xaxis.range[1]}
Plotly.relayout(gd, zeroline_relayout)

gd.on('plotly_afterplot', function() {
  var line_label = data.zeroline
  let yticks = $(gd).find('g.ytick text')
  let zeroline = $(gd).find('path.zl')
  if(line_label == 0) {
    if (zeroline.length > 0) {
    zeroline[0].style['stroke'] = 'black'
    }
  } else {
  let label_translate
  if (zeroline.length > 0) { zeroline[0].style['stroke'] = 'rgb(232, 232, 232)' };
  yticks.filter(function(d, i) {
  if(this.textContent.trim().replace(',','.').replace('\u2212','-').replace(' ','') == line_label) {
  label_translate = i.getAttribute('transform')
    let lines = $(gd).find('path.ygrid')
    lines.filter(function(d, i) {
    if(i.getAttribute('transform') == label_translate) {
      i.style['stroke'] = 'black'
    }
    })
  }
  })
  }

})
}
                                                         ", data = list(zeroline = zero_line))
  }
}

#' @importFrom htmltools htmlDependency tagList tags
#' @importFrom htmlwidgets appendContent onRender
#' @importFrom R.utils setOption
#' @importFrom RCurl base64Encode
#' @importFrom shiny addResourcePath isRunning
#' @importFrom stringr str_c str_extract_all str_replace_all str_squish str_wrap
roboplot_attach_dependencies <- function(p, title, subtitle) {
  t1 <- title
  t2 <- str_replace(str_wrap(title, round((title |> str_length())/2)),"\n","<br>")
  split_title <- list(
    str_c("<span>",getOption("roboplot.font.title")$bold(t1),"<span style=\"font-size: 75%\"><br>",subtitle,"</span></span>"),
    str_c("<span>",getOption("roboplot.font.title")$bold(t2),"<span style=\"font-size: 75%\"><br>",subtitle,"</span></span>"))

  if(!isRunning()) {

    if(is.null(getOption("roboplot.widget.deps.session"))) {
      deps <- roboplot_make_widget_deps()
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
  # else if (is.null(getOption("roboplot.widgets.deps.shiny"))) {
  #     roboplot_make_widget_deps(tempdir())
  #     addResourcePath("js", system.file("www","js", package = "roboplotr"))
  #     addResourcePath("fonts", file.path(tempdir(),"fonts"))
  #     addResourcePath("css", file.path(tempdir(),"css"))
  #     setOption("roboplot.widgets.deps.shiny", T)
  # }


  rangeslider_sums <- F
  if(p$plot_mode == "relative" && any(p$trace_types == "bar")) { rangeslider_sums = T }
  pie_plot <- if(any(p$trace_types == "pie")) { T } else { F }

  p |>
    onRender(jsCode = str_c("
                        function (gd, params, data){
                        let legendFontsize = 0;
                        console.log(JSON.stringify(gd.data))
                        if ('legend' in gd.layout) { legendFontsize = gd.layout.legend.font.size };
                        let alt_title = data.splitTitle;
                        setVerticalLayout({'width': true}, gd, legendFontsize, alt_title, pie_plot = data.piePlot);
                        gd.on('plotly_relayout',function(eventdata, lf = legendFontsize) {
                        plotlyRelayoutEventFunction(eventdata, gd, lf, alt_title, data.rangesliderSums, pie_plot = data.piePlot);
                        });
                        }"), data = list(splitTitle = split_title, rangesliderSums = rangeslider_sums, piePlot = pie_plot))
}


#' @importFrom stringr str_subset
roboplot_hovertemplate <- function(specs, type = "default") {

  hovertemplate_freq <- function(f) {
    if (is.null(f)) { "%Y-%m-%d" } else {
      switch(f,
             "Annual" = "%Y",
             "Quarterly" = "%YQ%q",
             "Monthly" = "%Y-%m-%d",
             "Weekly" = "%YW%V",
             "Daily" = "%d.%m.%Y",
             "%Y-%m-%d"
      )
    }
  }

  if (is.null(specs)) {
    NA
  } else if (!is.list(specs)) {
    stop("Hovertext must be either a list with any of \"rounding\", \"unit\", \"extra\" or \"dateformat\", or NULL", call. = F)
  } else if (!any(c("rounding","unit","","extra", "dateformat") %in% names(specs))) {
    stop("Hovertext must be either a list with any of \"rounding\", \"unit\", \"extra\" or \"dateformat\", or NULL", call. = F)
  } else {
    specs_template <- list(rounding = 1, unit = "", extra = "", dateformat = hovertemplate_freq("Monthly"))
    for (spec.name in (names(specs) |> str_subset("^(rounding|unit|extra|dateformat)$"))) {
      if(spec.name == "rounding") {
        if ((specs$rounding)%%1!=0) { stop("Hovertext rounding must be an integer.", call. = F) }
      } else if(spec.name == "dateformat") {
        if (is.null(specs$dateformat)) {
          specs$dateformat <- hovertemplate_freq(specs$dateformat)
        } else if (!is.character(specs$dateformat)) {
          stop("Hovertext dateformat must be one of \"Annual\", \"Quarterly\", \"Monthly\", \"Weekly\" or \"Daily\", or NULL.", call. = F)
        } else if (!(specs$dateformat %in% c("Annual", "Quarterly","Monthly","Weekly","Daily"))) {
          stop("Hovertext dateformat must be one of \"Annual\", \"Quarterly\", \"Monthly\", \"Weekly\" or \"Daily\", or NULL.", call. = F)
        } else {
          specs$dateformat <- hovertemplate_freq(specs$dateformat)
        }
      } else if (!is.character(specs[[spec.name]])) {
        stop(str_c("Hovertext ",spec.name," must be a string.", call. = F))
      }
      specs_template[[spec.name]] <- specs[[spec.name]]
    }
    if(type == "default") {
      paste0("%{text}<br>%{y:,.",specs_template$rounding,"f} ",specs_template$unit,"<br>%{x|",specs_template$dateformat,"}",ifelse(specs_template$extra == "", "", paste0("<br>",str_c(specs_template$extra, collapse = " "))),"<extra></extra>")
    } else if(type == "horizontal") {
      paste0("%{text}<br>%{customdata}<br>%{x}<extra></extra>")
    } else if(type == "pie") {
      paste0("%{text}<br>%{value:,.",specs_template$rounding,"f} ",specs_template$unit,"<extra></extra>")
    }
  }
}

#' Writes an html element for embedding, and optionally png files.
#'
#' @param p A plotly object.
#' @param title The filename of the html element (without file format). The function will clean the name up, or try to extract it from param p if missing.
#' @param filepath The path of the saved file.
#' @param render Logical. Is the plot rendered to viewer after saving the widget (default true). Returns the plot object nonetheless.
#' @param self_contained Logical. Will the html artefact have self-contained dependencies, increasing size. Default false.
#' @param png_artefacts Optional character vector of s(mall), n(arrow), and/or w(ide) corresponding to the expected .png sizes.
#' @examples
#' \dontrun{
#' p |> roboplot_create_widget()
#' }
#' @return The plotly object p.
#' @export
#' @importFrom dplyr first
#' @importFrom googleCloudStorageR gcs_get_global_bucket
#' @importFrom htmlwidgets saveWidget
#' @importFrom R.utils setOption
#' @importFrom stringr str_extract_all str_replace_all str_c str_squish

roboplot_create_widget <- function(p, title, filepath, render = T, self_contained = F, png_artefacts) {

  if (missing(title)) {
    title <- (p$x$layoutAttrs |> unlist())[grep("title.text", names((p$x$layoutAttrs |> unlist())))] |>
      str_extract_all("(?<=\\>)[^\\<\\>]{2,}(?=\\<)") |> unlist() |> first() |> str_c(collapse = "_") |>
      roboplot_string2filename()
    message(str_c("Using \"",title,"\" for htmlwidget filename.."))
  } else {
    title <- roboplot_string2filename(title)
  }

  filepath <- if(missing(filepath)) {
    if(isTRUE(getOption('knitr.in.progress'))) {
      tempdir()
    } else {
      getwd() }
  } else  { filepath }

  if (is.null(getOption(str_c("roboplot.widget.deps.",filepath)))) {
    roboplot_make_widget_deps(filepath = file.path(filepath,"plot_dependencies"))
    setOption(str_c("roboplot.widget.deps.",filepath), T)
  }# else {
    #print(getOption(str_c("roboplot.widget.deps.",filepath)))
    #}


  css_dep <- htmltools::htmlDependency("style", "0.1", src = c(href= "plot_dependencies/css"),  stylesheet = "style.css")
  js_dep <- htmltools::htmlDependency("js", "0.1", src = c(href= "plot_dependencies/js"),  script = "relayout.js")
  p$dependencies <- c(p$dependencies, list(css_dep, js_dep))
  detached_p <- p
  detached_p$append <- NULL
  detached_p |>
    saveWidget(file.path(filepath,str_c(title,".html")), selfcontained = self_contained, libdir = "plot_dependencies")


  if(!missing(png_artefacts)) {
    detached_p |> roboplot_automate_png(png_artefacts, filepath)
  }

  if(render == T) {
    p
  } else { invisible(p) }
}

#' Uses a headless browser to render the png files.
#'
#' @param p A plotly object.
#' @param artefacts A character vector of s(mall), n(arrow), and/or w(ide) corresponding to the expected .png sizes.
#' @param dl_path The path where the .png files will be downloaded to. Default is current working directory.
#' @return The plotly object p.
#' @importFrom chromote ChromoteSession
#' @importFrom htmlwidgets onRender
#' @importFrom knitr combine_words
#' @importFrom lubridate now as_datetime seconds
#' @importFrom plyr compact
#' @importFrom purrr map
#' @importFrom stringr str_replace_all str_c
roboplot_automate_png <- function(p, artefacts, dl_path = getwd()) {

  if(!any(artefacts %in% c("html","s","small","w","wide","n","narrow"))) {
    stop("\"png_artefacts\" must consist of one or more of s(mall), n(arrow) or w(ide), corresponding to desired .png size(s).", call. = F)
  }
  artefacts <- as.list(str_replace_all(artefacts, c("^s(|mall)$" = "pieni", "^w(|ide)$" = "leve\u00e4", "^n(|arrow)$" = "kapea")))

  p |> onRender(jsCode = str_c("function(gd,params,data) {
            if(data.includes('leve\u00e4')) {
              dlBtn = $(gd).find('[data-title=\"Lataa kuva (leve\u00e4)\"]')[0];
              dlBtn.click();
            };
            if(data.includes('kapea')) {
              dlBtn = $(gd).find('[data-title=\"Lataa kuva (kapea)\"]')[0];
              dlBtn.click();
            };
            if(data.includes('pieni')) {
              console.log('pieni')
              dlBtn = $(gd).find('[data-title=\"Lataa kuva (pieni)\"]')[0];
              dlBtn.click();
            };
    }"),  data = artefacts) |>
    roboplot_create_widget(title = "pngdl", filepath = tempdir(), self_contained = T, render = F)

  b <- ChromoteSession$new()
  b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = dl_path)
  b$Page$navigate(str_c("file://",file.path(tempdir(),"pngdl.html")))
  Sys.sleep(2)
  b$close()

  invisible(file.remove(file.path(tempdir(),"pngdl.html")))

  recent_files <- list.files(dl_path) |> map(~ {
    if (file.info(.x)$ctime |> as_datetime(tz = "UTC") >= now(tz = "UTC") - seconds(5)) { .x }
  }) |> compact()
  recent_length <- length(recent_files)
  if(recent_length > 0) {
    message(str_c("\nThe file",ifelse(recent_length > 1, "s",""),"\n", combine_words(recent_files,sep = ",\n", and = ", and\n"),"\n",
                  ifelse(recent_length > 1, "are","is")," in ",dl_path,"."))
  }

}

roboplot_get_frequency <- function(d) {
  d_attrs <- attributes(d)
  tf <- if(is.null(d_attrs$frequency)) {
    message("No frequency attribute detected for hovertext time format, resorting to default %Y/%m/%d.")
    NULL
  } else if (is.list(d_attrs$frequency)) {
    if(is.null(d_attrs$frequency$en)) {
      message("No frequency attribute detected for hovertext time format, resorting to default %Y/%m/%d.")
      NULL
    } else{
      as.character(d_attrs$frequency$en)
    }
  } else {
    as.character(d_attrs$frequency)
  }
  tf
}

roboplot_set_highlight <- function(highlight, df) {
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

#' @importFrom forcats fct_inorder fct_relevel fct_rev
roboplot_get_linetype <- function(linetype, d) {
  dashtypes <- getOption("roboplot.dashtypes")
  if(!is.null(linetype)) {
    if(!is.factor(d[[as_name(linetype)]])) {
      d[[as_name(linetype)]] <- fct_inorder(d[[as_name(linetype)]])
      d <- d |> mutate(roboplot.dash = dashtypes[!!linetype])
    }
  } else { d <- d|> mutate(roboplot.dash = "solid") }
  d <- mutate(d, roboplot.dash = fct_relevel(.data$roboplot.dash, dashtypes[dashtypes %in% .data]) |> fct_rev())
  d
}

#' Creates a plotly object with the desired visual specifications.
#'
#' Outputs a plotly object.
#'
#' @param d Data for plotting (Tibble).
#' @param color Mandatory. Tibble column from tibble d to use for color (Unquoted string).
#' @param linetype Optional. Tibble column from tibble d to use for linetype (Unquoted string).
#' @param title,subtitle,caption labeling for plot
#' @param legend_orientation,legend_position Legend positioning.
#' @param margin Plot margins if calculated margins do not work (List).
#' @param zeroline Determines zeroline inclusion (Logical or double).
#' @param rangeslider Determines rangeslider inclusion (Logical).
#' @param axis_limits Determines the limits of the axes (list(x = c(NA,NA), y = c(NA,NA)))".
#' @param hovertext A list describing hovertext items "list(rounding = 1, unit = "%", extra = "(ennuste)")".
#' @param highlight Either NULL, a double that limits, or a function that returns a logical determining if a given trace is included in legend and assigned a color.
#' @param plot_type Determines the trace type for either the whole plot, or for all variables defined by color as name-value pairs (Character vector).
#' @param plot_mode Determines the barmode for a barplot. Disregarded for scatterplots. (Character).
#' @param trace_color Trace color for all traces. Either a character string or a character vector of name-value pairs interpretable as colors (Character vector).
#' @param line_width Line width for line plots. Either a double or a double vector of name-value pairs (Double vector).
#' @param height Height of the plot.
#' @param facet_split The column from tibble d to use for facet split (Unquoted string).
#' @param pie_rotation Will pie charts be rotated to center the 0° point on middle of the first item of the color variable as factor (Logical).
#' @return plotly object
#' @examples
#' \dontrun{
#' d <- data("StatFin/kan/ntp/statfin_ntp_pxt_132h.px") |>
#' dplyr::filter(stringr::str_detect(taloustoimi, "B1GMH|P3KS14_S15"),
#' tiedot == "Kausitasoitettu ja työpäiväkorjattu sarja, viitevuosi 2015, miljoonaa euroa",
#' lubridate::year(time) >= 2010) |>
#' dplyr::group_by(taloustoimi) |>
#' dplyr::mutate(value = ((value/ lag(value, 4)) -1) * 100) |>
#' dplyr::ungroup() |>
#' dplyr::mutate(tiedot = dplyr::case_when(stringr::str_detect(taloustoimi, "B1GMH") ~ "BKT",
#' TRUE ~ "Yksityinen kulutus") |> forcats::as_factor()
#' p <- d |> roboplot(tiedot, "BKT ja kulutus", subtitle = "Vuosimuutos",
#' caption =  "Lähde: Tilastokeskus ja Etu",lwd = F,rangeslider = T,height = 600)
#' p
#' }
#' @export
#' @importFrom dplyr distinct group_split coalesce
#' @importFrom forcats fct_inorder
#' @importFrom plotly plot_ly add_trace
#' @importFrom plyr compact
#' @importFrom purrr map2
#' @importFrom rlang enquo as_name
#' @importFrom stats median runif setNames
#' @importFrom stringr str_c str_length str_pad str_replace

roboplot <- function(d,
                     color,
                     title,
                     subtitle = "",
                     caption,
                     legend_orientation = "auto",
                     legend_position = "bottom",
                     margin = NA,
                     trace_color = NULL,
                     highlight = NULL,
                     zeroline = F,
                     rangeslider = FALSE,
                     linetype,
                     line_width = getOption("roboplot.linewidth"),
                     hovertext,
                     axis_limits = list(x = c(NA,NA), y = c(NA,NA)),
                     plot_type = "scatter",
                     plot_mode = "dodge",
                     height = getOption("roboplot.height"),
                     facet_split,
                     pie_rotation = F
){


  if(missing(d) || missing(color)){
    stop("Data and unquoted color variable are needed!\nFor example: roboplot(a_tibble, color=tiedot)", call. = F)
  }

  if(missing(hovertext)) {
    d_attrs <- attributes(d)
    tf <- roboplot_get_frequency(d)
    hovertext <- list(rounding = 1, unit = "", extra = "", dateformat = tf)
  } else if (!"dateformat" %in% names(hovertext)) {
    hovertext$dateformat <- roboplot_get_frequency(d)
  }

  if(!missing(facet_split)) {
    stop("Facet split currently unavailable!", call. = F)
    facet_split <- enquo(facet_split)
    if(rangeslider == T | zeroline == T | any(!is.na(axis_limits$y))) message("Rangeslider, zeroline and y-axis range are not currently enabled for faceted plots.")
    rangeslider <- F
    zeroline <- F
    ymin <- min(d$value)
    ymax <- max(d$value)
    axdif <- diff(c(ymin, ymax)) * 0.04
    ymin <- ymin - axdif
    ymax <- ymax + axdif
    axis_limits$y <- c(ymin, ymax)
  }

  color <- enquo(color)

  linetype <- if (missing(linetype)) { NULL } else { enquo(linetype) }

  if(plot_mode == "horizontal") {
    if(is.null(names(plot_mode))) { stop("Horizontal plot mode needs the name of the y-axis categorial variable as the name of plot mode (ie. \"(c('tavara' = 'horizontal'\")", call. = F) }
    xaxis <- "value"
    yaxis <- names(plot_mode)
    ticktypes <- list(x="double", y = "character")
  } else {
    xaxis <- "time"
    yaxis <- "value"
    ticktypes <- list(x="date",y = "double")
  }

  if(!is.factor(d[[as_name(color)]])) {
    d[[as_name(color)]] <- fct_inorder(d[[as_name(color)]])
  }

  d <- d|> group_by(!!color) |> filter(!all(is.na(.data$value))) |> ungroup() |> droplevels()

  unique_groups <- d[[as_name(color)]] |> unique() |> sort()

  if(!all(plot_type %in% c("scatter","bar","pie"))) {
    stop("Plot type must be \"scatter\" or \"bar\", or a vector of name_value pairs!", call. = F)
  } else if (length(plot_type) == 1 & is.null(names(plot_type))){
    d <- d |> mutate(roboplot.plot.type = plot_type)
  } else if (!all(unique_groups %in% names(plot_type))) {
    stop(str_c("All variables in column \"",as_name(color),"\" must have a corresponding plot type!"), call. = F)
  } else {
    d <- mutate(d, roboplot.plot.type = str_replace_all(!!color, plot_type))
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
      }) |> compact() |> unlist()
      line_width <- c(line_width, detected_widths)
    }
    d <- mutate(d, roboplot.linewidth = line_width[as.character(!!color)] |> dplyr::coalesce(line_width[".other"]))


  }

  d <- roboplot_get_linetype(linetype, d)

  color_vector <- roboplot_set_colors(trace_color, unique_groups, highlight, d, color)

  d <- d |> mutate(roboplot.trace.color = color_vector[!!color])

  if(!missing(facet_split)) {
    p <- roboplot_get_facet_plot(d, facet_split, height, color, linetype, plot_type, trace_color, highlight, hovertext, plot_mode, ticktypes, axis_limits)
  } else {
    p <- roboplot_get_plot(d, xaxis, yaxis, height, color, linetype, plot_type, trace_color, highlight, hovertext, plot_mode, pie_rotation)
  }

  p$data <- roboplot_transform_data_for_download(d, color, linetype, facet_split, plot_mode)

  if(!isRunning()) { p$elementId <- str_c("widget_",roboplot_string2filename(title)) }
  p$title <- title
  p$subtitle <- subtitle
  p$trace_types <- distinct(d, !!color, .data$roboplot.plot.type) |> pull(2,1)
  p$plot_mode <- plot_mode

  maxtime <- max(d$time)
  p <- p |>
    roboplot_config(title = title, subtitle = subtitle, caption = caption,
                    legend_position = legend_position, legend_orientation = legend_orientation,
                    margin = margin,
                    height = height,
                    axis_range = axis_limits,
                    zeroline = list(zeroline = zeroline, xrange = list(min = min(d$time), max = maxtime)),
                    enable_rangeslider = list(rangeslider = rangeslider, max = maxtime),
                    ticktypes = ticktypes)

  ## add labels for facet plot. Has to be done here for the relayout js to work properly for captions.
  if(!missing(facet_split)) {
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
roboplot_get_facet_plot <- function(d, facet_split, height, color, linetype, plot_type, trace_color, highlight, hovertext, plot_mode, ticktypes, axis_limits) {

  split_facet <- d |> group_split(!!facet_split)

  p <- map2(split_facet,seq(length(split_facet)), function(facet, i) {

    p <- plot_ly(facet, x = ~time, height = height, colors = pull(distinct(d,.data$roboplot.trace.color)))

    split_d <- group_split(facet, !!color, !!linetype)
    split_d <- if(any(plot_type == "scatter")) { rev(split_d) } else { split_d }

    for (g in split_d) {
      g.dash <- unique(d$roboplot.dash)
      g.name <- unique(g[[as_name(color)]])
      g.level <-  which(g.name == levels(g.name))
      g.type <- unique(g$roboplot.plot.type)
      g.linewidth <- unique(g$roboplot.linewidth)
      legend.rank <- g.level * 100  + ( if(!is.null(linetype)){ which(g.dash == levels(g.dash)) * 10 } else  { 0 } )
      show.legend <- if (i > 1) {F} else if(!is.null(trace_color)) { T } else { roboplot_set_highlight(highlight, filter(d, !!color == g.name)) }
      p <- p |>
        add_trace(data=g, y = ~value, text = g.name,
                  texttemplate = NA,
                  hovertemplate = roboplot_hovertemplate(hovertext),
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

    if(!plot_mode %in% c("dodge","stack","horizontal")) {
      stop("Plot mode must be \"dodge\", \"stack\" or \"horizontal\"!", call. = F)
    } else {
      p_mode <- str_replace_all(plot_mode, c("dodge|horizontal" = "group", "stack" = "relative"))
      p  <- layout(p, barmode = p_mode)
    }
    if(i > 1) {
      p <- p |>
        roboplot_set_ticks(ticktypes = ticktypes) |>
        layout(yaxis = list(range = axis_limits$y, showticklabels = F, showline = getOption("roboplot.colors.background")$y != getOption("roboplot.colors.border")$y))
    }

    p

  })

  subplot(p)
}

#' @importFrom dplyr add_count group_by last mutate row_number ungroup
#' @importFrom rlang .data sym

get_bar_widths <- function(df, width_col) {
  get_offset <- function(the_count) {
    seq(-0.45, length.out = the_count, by = 1/the_count)
  }
  df |>
    add_count((!!sym(width_col)) , name = "roboplot.bar.width") |>
    group_by((!!sym(width_col)) ) |>
    mutate(roboplot.bar.offset = get_offset(max(.data$roboplot.bar.width)),
           roboplot.bar.width = ifelse(row_number() == last(row_number()), 1/.data$roboplot.bar.width*0.9, 1/.data$roboplot.bar.width)) |>
    ungroup()

}

#' @importFrom dplyr distinct first group_split mutate pull slice_min summarize
#' @importFrom forcats fct_relabel
#' @importFrom plotly plot_ly add_trace layout subplot
#' @importFrom stats as.formula
#' @importFrom stringr str_replace_all str_trunc
roboplot_get_plot <- function(d, xaxis, yaxis, height, color, linetype, plot_type, trace_color, highlight, hovertext, plot_mode, pie_rotation) {

  if(plot_mode == "horizontal") {
    d <- get_bar_widths(d, yaxis)
  }

  plot_colors <- pull(distinct(d,.data$roboplot.trace.color, !!color))

  p <- plot_ly(d, height = height, colors = plot_colors)

  d <- mutate(d,
              roboplot.plot.text = if (!is.null(linetype)) {str_c(!!color, ", ",tolower(!!linetype)) |> str_remove(", alkuper\uE4inen")} else {!!color},
              roboplot.legend.rank = ((max(as.numeric(!!color))-as.numeric(!!color)) * 100) + ((max(as.numeric(.data$roboplot.dash))-as.numeric(.data$roboplot.dash))*10))

  split_d <- group_split(d, if("pie" %in% plot_type) { NULL } else { !!color }, .data$roboplot.plot.type, !!linetype) %>% rev()

  for (g in split_d) {
    tracetype <- unique(g$roboplot.plot.type)
    hovertemplate <- roboplot_hovertemplate(hovertext, case_when(tracetype == "pie" ~ tracetype, plot_mode == "horizontal" ~ plot_mode, TRUE ~ "default"))
    marker_line_color <- first(getOption("roboplot.colors.grid"))
    legend_rank <- mean(g$roboplot.legend.rank)
    if(tracetype == "pie") {
      tx_colors <- roboplot_text_color_picker(roboplot_alter_color(plot_colors,"darker"))
      in_tx_colors <- roboplot_text_color_picker(plot_colors)
      g <- mutate(g, roboplot.bg.color = roboplot_alter_color(.data$roboplot.trace.color,"darker"),
                  roboplot.tx.color = tx_colors[!!color],
                  roboplot.in.tx.color = in_tx_colors[!!color])
      background_color <- getOption("roboplot.colors.background")
      grid_color <- getOption("roboplot.colors.grid")
      marker_line_color <- first(grid_color[grid_color != background_color])
      marker_line_color <- replace(marker_line_color, length(marker_line_color) == 0, roboplot_alter_color(background_color,"darker"))
    }

    rotation <- if(pie_rotation) {
      -(group_by(g, !!color) |> summarize(value = sum(.data$value), .groups = "drop") |> mutate(value = .data$value / sum(.data$value)) |> slice_min(!!color) |> pull(.data$value) * 360 / 2)
    } else { 0 }

    # print(distinct(g, !!color, !!linetype, roboplot.legend.rank))
    plotting_params <- list(color = color, #!pie
                            customdata = color,
                            data=g,
                            direction = "clockwise", #pie
                            hoverlabel = list(family = getOption("roboplot.font.main")$family, size = getOption("roboplot.font.main")$size, bgcolor = ~ roboplot.bg.color, color = ~ roboplot.tx.color), #pie
                            hovertemplate = hovertemplate,
                            insidetextfont = list(family = getOption("roboplot.font.main")$family, size = getOption("roboplot.font.main")$size, color = ~ roboplot.in.tx.color), #pie
                            labels = color, #pie
                            legendgroup = color,
                            legendrank = legend_rank,
                            line = ~ list(width = roboplot.linewidth, dash = roboplot.dash), #scatter
                            marker = list(colors = ~ roboplot.trace.color, line = list(color = marker_line_color, width = 1)), #pie
                            mode = "lines", #scatter
                            name = ~ roboplot.plot.text, #!pie
                            offset = ~roboplot.bar.offset, #horizontal bar
                            offsetgroup = color, #bar ## onko ok?? mieti
                            orientation = ifelse(plot_mode == "horizontal" & plot_type == "bar","h","v"),
                            rotation = rotation, #pie
                            showlegend = T,
                            sort = F, #pie
                            text = ~roboplot.plot.text,
                            textinfo = "percent", #pie
                            textposition = ifelse(tracetype == "bar", "none", "inside"), #pie and bar
                            texttemplate = if(tracetype == "pie") { NULL } else { NA },
                            type = ~ tracetype,
                            values = as.formula(str_c("~",yaxis)), # pie
                            width = ~roboplot.bar.width, #horizontal bar
                            x = as.formula(str_c("~",xaxis)), #!pie
                            y = as.formula(str_c("~",yaxis)) #!pie
                            )
    shared_params <- c("data","text","texttemplate","hovertemplate","legendgroup","showlegend","type")
    plotting_params <- if(tracetype == "scatter") {
      plotting_params[c(shared_params,"x","y","line","mode","name","color")]
    } else if (tracetype == "bar" & plot_mode == "horizontal") {
      plotting_params[c(shared_params,"x","y","offsetgroup","orientation","offset","width","name","color", "textposition")]
    } else if (tracetype == "bar") {
      plotting_params[c(shared_params,"x","y","offsetgroup","name","color", "textposition")]
    } else if (tracetype == "pie") {
      plotting_params[c(shared_params,"labels","textposition","textinfo","insidetextfont","direction","rotation","sort","hoverlabel","marker", "values")]
    }
    p <- p |> roboplot_add_trace(!!!plotting_params)
  }

  if(!plot_mode %in% c("dodge","stack","horizontal")) {
    stop("Plot mode must be \"dodge\", \"stack\" or \"horizontal\"!", call. = F)
  } else {
    plot_mode <- str_replace_all(plot_mode, c("dodge|horizontal" = "group", "stack" = "relative"))
    p  <- layout(p, barmode = plot_mode)
  }

  p |> layout(legend = list(traceorder = ifelse(length(split_d) == 1, "normal", "reversed")))

}

#' @importFrom plotly add_trace
#' @importFrom rlang list2
roboplot_add_trace <- function(...) {
  do.call(add_trace, list2(...))
}

#' Create a css file or string
#'
#' \code{roboplot_make_css} will create a css file or string which can also be used in shiny
#'
#' \code{roboplot_make_css} will create a css file or string which can also be used in shiny. If the argument
#'   file is provided the css code will be printed out to the file. The file can then be used in
#'   shiny with the \code{includeCSS} function. Alternatively there are two ways to use
#'   \code{make_css} in order to add css to your shiny app. If you have a very small css file or you
#'   are just testing your app you can use \code{tags$style} with \code{make_css} directly. There is
#'   an example in the examples section. Another way (which will make your code cleaner) is to
#'   create your css in global.R assign it to a variable and then use that variable with
#'   \code{tags$style}. There is another example on the examples section. Keep in mind that for
#'   complete shiny apps it is best practice to use a file and load it with \code{includeCSS}. This
#'   will be faster as well as it won't run the code to create the css file each time.
#'
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

#' @importFrom farver decode_colour
#' @importFrom RCurl base64Encode
#' @importFrom stringr str_c
#' @importFrom stats setNames
#'
roboplot_make_widget_deps <- function(filepath = NULL) {

  js_file <- system.file("www/js","relayout.js", package = "roboplotr")

  if(!is.null(filepath)) {
    dir.create(filepath, showWarnings = F)
    dir.create(file.path(filepath,"fonts"), showWarnings = F)
    dir.create(file.path(filepath,"css"), showWarnings = F)
    dir.create(file.path(filepath,"js"), showWarnings = F)
    invisible(file.copy(js_file, file.path(filepath,"js","relayout.js"), overwrite = T))
  }

  font_strings <- NULL

  set_font_strings <- function(this_opt, filepath, el_name) {
    if(!is.null(filepath)) {
      invisible(file.copy(this_opt$path, file.path(filepath,"fonts",str_extract(this_opt$path,"[^/]*$"))))
      font_string <- list(file.path("..","fonts",str_extract(this_opt$path,"[^/]*$"))) |> setNames(el_name)
      font_string
    } else {
      base_font <- base64Encode(readBin(this_opt$path, "raw", file.info(this_opt$path)[1, "size"]), "txt")
      font_string <- list(str_c('data:vnd.ms-opentype;base64', base_font, sep=',')) |> setNames(el_name)
      font_string
    }}

  for(opt in c("main", "caption","title")) {

    opt_name <- str_c("roboplot.font.",opt)

    this_opt <- getOption(opt_name)

    if(!is.null(this_opt$path)) {
      font_strings <- append(font_strings, set_font_strings(this_opt, filepath, opt))
    }

  }

  rangeslider_mask <- decode_colour(getOption("roboplot.colors.background")) |> str_c(collapse = ", ")
  rangeslider_mask_css <- list(".rangeslider-mask-min, .rangeslider-mask-max",
                               c("fill", "fill-opacity"),
                               c(str_c("rgb(",rangeslider_mask,") !important"),"0.7 !important"))

  font_strings <- map2(font_strings, names(font_strings), ~ list('@font-face', c('font-family', 'src'), c(str_c('roboplot-',.y), str_c("url('",.x,"')")))) |>
    unname()

  css_list <- map(c(font_strings,list(rangeslider_mask_css)), ~.x)
  css_string <- roboplot_make_css(css_list,
                                  file = if(!is.null(filepath)) { file.path(filepath,"css/style.css") } else NULL)

  if(is.null(filepath)) {
    list("css" = str_c('data:text/css;base64', base64Encode(css_string), sep=','),
         "js" =  str_c('data:application/javascript;base64', base64Encode(readBin(js_file, "raw", file.info(js_file)[1, "size"]), "txt"), sep=',')
    )
  }

}
