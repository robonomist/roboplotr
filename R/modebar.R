#' @importFrom dplyr mutate
#' @importFrom fontawesome fa
#' @importFrom htmlwidgets JS
#' @importFrom lubridate as_date quarter
#' @importFrom plotly config
#' @importFrom purrr pmap
#' @importFrom stringr str_c str_extract_all str_replace_all str_squish str_wrap
#' @importFrom tidyr drop_na
roboplotr_modebar <- function(p, title, subtitle, caption, height, width, dateformat, info_text = NULL, modebar, legend) {

  if(is.null(modebar)) {
    modebar <- getOption("roboplot.modebar")
  }
  if(modebar$display == "none") {
    return(config(p, displayModeBar = F))
  }

  if(!is.null(modebar$title)) {
    dl_title <- modebar$title
  } else {
    dl_title <- roboplotr_string2filename(title$title)
  }

  js_string <- function(layout, ttl = dl_title) {
    tf <- getOption("roboplot.font.title")
    mf <- getOption("roboplot.font.main")
    cf <- getOption("roboplot.font.caption")
    plot_title <- list(title$title,subtitle,tf$bold)
    ht <- ifelse(is.null(height), 'null', as.character(height))
    wt <- ifelse(is.null(width), 'null', as.character(width))
    pie <-ifelse(all(p$trace_types == "pie"),"true","false")
    tidy <-ifelse(legend$tidy,"true","false")
    zm <- modebar$zoom
    str_glue('
          function(gd, params) {
          let oldlayout = JSON.parse(JSON.stringify(gd.layout))
          delete gd.layout.xaxis.rangeslider;
          delete gd.layout.height;
          delete gd.layout.width;
          Plotly.relayout(gd, {height: <{layout$y}, width: <{layout$x},
          "annotations[0].font.size": <{layout$caption},
          "xaxis.tickfont.size": <{layout$main},
          "yaxis.tickfont.size": <{layout$main},
          "title.font.size": <{layout$title}})
          let roboplot_logo = new Image();
          roboplot_logo.src = gd.layout.images[0].source;
          roboplot_logo = roboplot_logo.width / roboplot_logo.height
          $(gd).find("div.modebar").css("display","none")
          setVerticalLayout({"width": true}, gd, {legend: <{layout$main}, x: <{layout$main}, y: <{layout$main}}, ["<{plot_title[[1]]}","<{plot_title[[2]]}",<{tolower(plot_title[[3]])}], pie_plot = <{pie}, logo = roboplot_logo, tidy_legend = <{tidy}, legend_position = "<{legend$position}")
          setYPositions({"width": true}, gd, <{pie});
          Plotly.downloadImage(gd, {scale: "<{zm}", format: "<{layout$type}", width: <{layout$x}, height: <{layout$y}, filename: "<{ttl}<{layout$suffix}"});
          $(gd).find("div.modebar").css("display","initial")
          Plotly.relayout(gd, oldlayout)
          delete oldlayout
          }
   ',.open = "<{")
  }

  dl_icon <- function(fa_icon, scale = 0.032, translate = c(0,0)) {
    transform_string <- str_c("translate(",translate[1],",",translate[2],") scale(",scale,")")
    list(path = fa(name = fa_icon) |> as.character() |> str_extract("(?<=d\\=\")[^\"]{1,}") |> str_replace_all(" ",","),
         transform = transform_string)
  }

  dl_string <- (function() {
    d <- p$data

    if("time" %in% names(d) & !is.null(dateformat)) {
      if(str_detect(dateformat,"Q")) {
        d <- d |> mutate(time = str_c(format(as_date(.data$time), "%YQ"),lubridate::quarter(.data$time)))
      } else {
        if(str_detect(dateformat, "-")) {
          dateformat <- "%Y-%m-%d"
        }
        d <- d |> mutate(time = format(as_date(.data$time), dateformat))
      }
    }
    row.data <- d |> pmap(function(...) {
      as.character(list(...)) |> replace_na("NA") |> str_c(collapse = ";")
    }) |> unlist() |> str_c(collapse = "\\n")
    col.names <- names(d) |>  str_c(collapse = ";")
    ti <- roboplotr_transform_string(title$title)
    su <- roboplotr_transform_string(subtitle)
    str_c(str_c(ti,", ",su),str_c(rep(";",length(names(p$data))-2),collapse = ""),"\\n",col.names,"\\n",row.data)
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
      click = JS(js_string(getOption("roboplot.imgdl.wide")))
    ),
    "img_n" = list(
      name = "Lataa kuva (kapea)",
      icon = dl_icon("file-image", 0.025, c(2.7,2)),
      click = JS(js_string(getOption("roboplot.imgdl.narrow")))),
    "img_s" = list(
      name = "Lataa kuva (pieni)",
      icon = dl_icon("twitter-square"),
      click = JS(js_string(getOption("roboplot.imgdl.small")))),
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
            var blob = new Blob([\"\uFEFF\" + text], {type: 'text/plain;charset=UTF-8'});
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


  btn_list <- btn_list[modebar$buttons]

  if(!is.null(info_text)) {
    modal_id <- str_c("roboplot-info-", str_remove(runif(1), "\\."))
    main_font <- getOption("roboplot.font.main")
    title_font <- getOption("roboplot.font.title")
    m.specs <- getOption("roboplot.infobox")
    .bold <- ifelse(title_font$bold, tags$b, tags$span)
    modal_html <- tags$div(
      id = str_glue("{modal_id}_infomodal"),
      style = "position: absolute; top: 4%; left: 6%; width: 50%;",
      tags$div(
        style = str_glue(
          "position: absolute;
               z-index: 9999;
               background-color: {m.specs$background};
               color: {m.specs$font};
               padding: 5px;
               border: {m.specs$border_width}px solid {m.specs$border};
               box-shadow: 0 4px 8px {m.specs$background};"
        ),
        tags$span(
          id = str_glue("{modal_id}_infomodal-close"),
          fa(
            "times-circle",
            fill = m.specs$font,
            height = str_glue("{title_font$size}px")
          ),
          style = "top: 5px; right: 10px; font-size: 24px; cursor: pointer; float: right;"
        ),
        .bold(
          HTML(title$title),
          tags$br(),
          tags$span(HTML(subtitle), style = "font-size: 75%"),
          style = str_glue(
            "font-family: {title_font$family};font-size: {title_font$size}px;"
          )
        ),
        tags$span(
          style = str_glue(
            "font-family: {main_font$family}; font-size: {main_font$size}px;"
          ),
          tags$p(HTML(as.character(info_text))),
          tags$p(HTML(caption$text))

        )
      )
    )


    info_btn <- list("info" = list(
      name = "Info",
      icon = dl_icon("circle-info"),
      click = JS(str_glue("
      function(gd) {
      var existingModal = document.getElementById('{<modal_id}_infomodal_container');
      if (existingModal) {
        existingModal.style.display = existingModal.style.display === 'none' ? 'block' : 'none';
      } else {
        var modal = document.createElement('div');
        modal.id = '{<modal_id}_infomodal_container';
        modal.innerHTML = `{<modal_html}`;
        modalparent = gd.querySelector('.plot-container');
        gd.appendChild(modal);
        document.getElementById('{<modal_id}_infomodal-close').addEventListener('click', function() {
          modal.style.display = 'none';
        });
      }
}", .open = "{<")
      )
    ))

    append_location <- ifelse("robonomist" %in% names(btn_list), length(btn_list)-1, length(btn_list))
    btn_list <- append(btn_list,info_btn,after = append_location)

  }

  p <- p |> config(
    displaylogo = FALSE,
    modeBarButtons = list(unname(btn_list))
  )

  if(modebar$display == "constant") {
    p <- config(p, displayModeBar = T)
  }

  p
}

#' @importFrom DT JS
#' @importFrom fontawesome fa
#' @importFrom stringr str_detect str_glue
roboplotr_robotable_modebar <- function(d, id, title, info_text) {

  # xportoptionsiin pitää vielä lisätä otsikoinnit, optional infotekstit jne

  xportoptions <- (function() {
    the_cols <- seq(to = length(d)) - 1
    list(columns = subset(the_cols,!the_cols %in% as.numeric(names(
      attributes(d)$dt_orders
    ))))
  })()

  modebar_buttons <- list(
    "csv" = list(
      filename = roboplotr_string2filename(title$title),
      extend = 'csv',
      text = fa("file-csv", height = "12pt"),
      exportOptions = xportoptions

    ),
    "info" = list(
      extend = "collection",
      text = fa("circle-info", height = "12pt"),
      # action = JS(str_glue("function(e, dt, node, config) {alert('{<info_text}');}", .open = "{<"))
      action = JS(str_glue('function(e, dt, node, config) {$("#{<id}_infomodal").toggle();}', .open = "{<"))
    ),
    "robonomist" = list(
      extend = "collection",
      text = '<svg version="1.1" viewBox="0 0 71.447 32" style = "width: 12pt;padding-bottom: 2px" xmlns="http://www.w3.org/2000/svg"><path transform="scale(.31159)"  d="M 229.3 53.2 L 174.3 90.1 L 174.3 69.1 L 199.5 53.2 L 174.3 37.3 L 174.3 16.3 M112 0c14.2 0 23.3 1.8 30.7 7 6.3 4.4 10.3 10.8 10.3 20.5 0 11.3-6.4 22.8-22.3 26.5l18.4 32.5c5 8.7 7.7 9.7 12.5 9.7v6.5h-27.3l-23.7-45.8h-7v27.6c0 10.5 0.7 11.7 9.9 11.7v6.5h-43.2v-6.7c10.3 0 11.3-1.6 11.3-11.9v-65.7c0-10.2-1-11.7-11.3-11.7v-6.7zm-4.8 7.9c-3.3 0-3.6 1.5-3.6 8.6v32.3h6.4c15.8 0 20.2-8.7 20.2-21.3 0-6.3-1.7-11.5-5-15-2.9-3-7-4.6-13-4.6z M 0 53.2 L 55 16.3 L 55 37.3 L 29.8 53.2 L 55 69.1 L 55 90.1"/></svg>',
      action = JS("function(e, dt, node, config) {window.open(\"https://robonomist.com\")  }")
    ))

  modebar_buttons[c(
    "csv",
    ifelse(!is.null(info_text),"info",""),
    ifelse(str_detect(getOption("roboplot.logo"),"robonomist"),"","robonomist"))] |>
    roboplotr_compact() |>
    unname()
}


#' Font configuration for modebar exports.
#'
#' Parameters to customize plot layout when exported from modebar.
#'
#' @param height,width Numeric. The dimensions for the export file in pixels.
#' @param mainfont,titlefont,captionfont Numeric. The font sizes used in the export
#' the modebar button will produce.
#' @param suffix Character. Suffix attached after the name of the export.
#' @param format Character. One of "png", "svg", "jpg", or "webp". Defines the file
#' format of the export.
#' @examples
#' # Used inside `set_roboplot_options()` to control modebar export specifications
#' # globally. Parameters `x` and `y` control the dimensions. Override plot font
#' # sizes with `mainfont`, `titlefont`, and `captionfont`. You can give a suffix
#' # for export filenames (the filename is derived from title), and provide any
#' # one of "png","svg","jpeg", or "webp" the for `format`.
#'
#' \dontrun{
#' set_roboplot_options(
#'   imgdl_wide =
#'     set_imgdl_layout(
#'       width = 1400,
#'       height = 350,
#'       mainfont = 16,
#'       titlefont = 24,
#'       captionfont = 14,
#'       suffix = "",
#'       format = "svg")
#' )
#'
#' # Create a `roboplot()` and navigate to it, and download your static
#' # image by clicking the appropriate button in the modebar.
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(
#'     color = Alue,
#'     title = "Energian tuonti",
#'     subtitle = "Milj. €",
#'     caption = "Tilastokeskus",
#'     artefacts = set_artefacts("html", filepath = tempdir())
#'   )
#'   utils::browseURL(paste0(tempdir(),"/energian_tuonti.html"))
#'
#' # Static image downloads are controlled through the modebar, so if you use
#' # other than "img_wide" you need to use `set_roboplot_options()` to
#' # control the modebar accordingly in addition to giving the downloaded image
#' # specifications (`roboplot()` does have some defaults in place
#' # for every size, so you can skip image specifications even if you do add
#' # use other image download buttons in the modebar). With multiple image
#' # download buttons the `suffix` parameter is especially handy.
#'
#' set_roboplot_options(
#'   imgdl_wide =
#'     set_imgdl_layout(
#'       width = 1400,
#'       height = 350,
#'       mainfont = 16,
#'       titlefont = 24,
#'       captionfont = 14,
#'       suffix = "_wide"),
#'   imgdl_narrow =
#'     set_imgdl_layout(
#'       width = 400,
#'       height = 600,
#'       mainfont = 16,
#'       titlefont = 24,
#'       captionfont = 14,
#'       suffix = "_narrow"),
#'   modebar = c("img_w","img_n","img_s")
#' )
#'
#'   # Create a `roboplot()` and navigate to it, and download any of the
#'   # static images through the modebar.
#'   energiantuonti |>
#'     dplyr::filter(Suunta == "Tuonti") |>
#'     roboplot(
#'       color = Alue,
#'       title = "Energian tuonti",
#'       subtitle = "Milj. €",
#'       caption = "Tilastokeskus",
#'       artefacts = set_artefacts("html", filepath = tempdir())
#'     )
#'   utils::browseURL(paste0(tempdir(),"/energian_tuonti.html"))
#'
#' # Revert to defaults:
#' set_roboplot_options(reset = TRUE)
#' }
#' @returns A list of class of roboplotr.set_imgdl_layout
#' @export
set_imgdl_layout <- function(
    width = 1280,
    height = 720,
    mainfont = getOption("roboplot.font.main")$size,
    titlefont = getOption("roboplot.font.title")$size,
    captionfont = getOption("roboplot.font.caption")$size,
    suffix = "_img",
    format = "png") {
  roboplotr_typecheck(width, "numeric", allow_null = F)
  roboplotr_typecheck(height, "numeric", allow_null = F)
  roboplotr_typecheck(mainfont, "numeric", allow_null = F)
  roboplotr_typecheck(titlefont, "numeric", allow_null = F)
  roboplotr_typecheck(captionfont, "numeric", allow_null = F)
  roboplotr_typecheck(suffix, "character", allow_null = F)
  roboplotr_typecheck(format, "character", allow_null = F)
  roboplotr_valid_strings(format, c("png","svg","jpeg", "webp"), any)

  .res <- list(x = width, y = height, main = mainfont, title = titlefont, caption = captionfont, suffix = suffix, type = format)

  .res <- structure(.res, class = c("roboplotr","roboplotr.set_imgdl_layout", class(.res)))

  .res
}

#' Info box configuration for [roboplot()] or [robotable()]
#'
#' Global parameters to add and customize info boxes in [roboplots][roboplot()]
#' and [robomaps][robomap()].
#'
#' @importFrom dplyr first
#' @param background,border Characters. Colors used for the corresponding
#' element of the infopopup of [robotable()]s and [roboplot()]s. Must be a
#' hexadecimal color or a valid css color.
#' @param border_width Integer. The border width of infomodal.
#' @returns A list of class roboplot.set_infobox.
#' @export
set_infobox <-
  function(background = first(unlist(unique(getOption('roboplot.grid')[c('xcolor', 'ycolor')]))),
           border = first(unlist(unique(getOption('roboplot.border')[c('xcolor', 'ycolor')]))),
           border_width = 1) {
    font <- roboplotr_text_color_picker(background,getOption("roboplot.font.main")$size)
    roboplotr_typecheck(background, "character", allow_null = F)
    roboplotr_typecheck(border, "character", allow_null = F)
    roboplotr_valid_colors(c(background,border), message = "Colors in set_infobox()")
    roboplotr_typecheck(border_width, "numeric", allow_null = F)

    .res <- list(
      background = background,
      border = border,
      font = font,
      border_width = round(border_width)
    )

    .res <- structure(.res, class = c("roboplotr","roboplotr.set_infobox", class(.res)))

    .res
  }


#' Modebar configuration
#'
#' Parameters to add and customize modebars in [roboplots][roboplot()].
#'
#' @param buttons Character vector. Buttons contained in modebar in the given order.
#' Must contain any of "home", "closest", "compare", "zoomin", "zoomout", "img_w",
#' "img_n", "img_s", "data_dl" and "robonomist" in any order.
#' @param display Character. One of "constant", "hover" or "none". Controls whether
#' modebar is visible always, only on hover, or never. Whatever the choice, static
#' exports will not display a modebar.
#' @param title. This will be the title used for static files downloaded through
#' modebar. Control image download title extension specifications with `set_imgdl_layout()`.
#' [roboplot][roboplot()] `title` will be used if no title is given here.
#' @param zoom Integer. Static plot downloaded through modebar will be magnified
#' by this multiplier. Note that this will affect final file dimensions if you have
#' specified some with [set_roboplot_options][set_roboplot_options()]. Default is
#' 1.
#' @param ... Placeholder for other parameters.
#' @returns A list.
#' @export
set_modebar <- function(buttons = getOption("roboplot.modebar")$buttons,
                        display = getOption("roboplot.modebar")$display,
                        title = getOption("roboplot.modebar")$title,
                        zoom = getOption("roboplot.modebar")$zoom,
                        ...) {
  args <- list(...)

  if (!is.null(args$.extra)) {
    .extra <- args$.extra
  } else {
    .extra <- "in set_modebar()"
  }

  roboplotr_typecheck(buttons, "character", NULL, extra = .extra)

  valid_buttons <- c(
    "home",
    "closest",
    "compare",
    "zoomin",
    "zoomout",
    "pan",
    "img_w",
    "img_n",
    "img_s",
    "data_dl",
    "robonomist"
  )

  roboplotr_valid_strings(
    buttons,
    valid_buttons,
    any
  )

  if(any(!buttons %in% valid_buttons)) {
    roboplotr_alert(
      str_c(
        "Invalid modebar button(s) given. Valid buttons are: ",
        str_c(valid_buttons, collapse = ", ")
      ))
  }

  buttons <- buttons[buttons %in% valid_buttons]

  if (!str_detect(getOption("roboplot.logo"), "robonomist") &
      !"robonomist" %in% buttons) {
    buttons <- c(buttons, "robonomist")
  }

  roboplotr_typecheck(display,
                      "character",
                      allow_null = F,
                      extra = .extra)

  roboplotr_valid_strings(display, c("constant", "hover", "none"), .fun = any)

  roboplotr_typecheck(title,
                      "character",
                      allow_null = T,
                      extra = .extra)

  roboplotr_typecheck(zoom, "integer", allow_null = F)

  .res <- list(buttons = buttons,
               display = display,
               title = title,
               zoom = zoom
               )

  .res <- structure(.res, class = c("roboplotr", "roboplotr.set_modebar", class(.res)))

  .res

}

roboplotr_robomap_modebar <- function(map, title, zoom) {

  map <- map |>
    addEasyButton(
      easyButton(
        id = "reset-view",
        icon = "fa-home",
        title = "Kohdista",
        position = "topright",
        onClick = JS(
          "function(btn, map) {
          // Reset to the initial view
          map.setView(map.initialCenter, map.initialZoom);
        }"
        )
      )
     )# |>
    # addEasyButton(
    #   easyButton(
    #     id = "download-map",
    #     icon = fa("file-image", vertical_align = "0em"),
    #     title = "Lataa kartta",
    #     position = "topright",
    #     onClick = JS(
    #       str_c(
    #         "function(btn, map) {
    #       // Check if html2canvas is already loaded
    #       if (typeof html2canvas === 'undefined') {
    #         // Load html2canvas script if it's not already included
    #         var scriptHtml2Canvas = document.createElement('script');
    #         scriptHtml2Canvas.src = 'https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js';
    #         document.head.appendChild(scriptHtml2Canvas);
    #
    #         // Wait for the script to load before proceeding
    #         scriptHtml2Canvas.onload = function() {
    #           captureMap();
    #         };
    #       } else {
    #         captureMap();
    #       }
    #
    #       // Function to capture the map and download as PNG
    #       function captureMap() {
    #         // Hide navigation controls before capture
    #         var controls = document.querySelectorAll('.leaflet-top.leaflet-right');
    #         controls.forEach(function(control) {
    #           control.style.display = 'none';
    #         });
    #
    #         // Set the map container style to avoid shifting during capture
    #         var mapContainer = document.querySelector('.leaflet-container');
    #         var originalPosition = mapContainer.style.position;
    #         var originalTop = mapContainer.style.top;
    #         var originalLeft = mapContainer.style.left;
    #         mapContainer.style.position = 'absolute';
    #         mapContainer.style.top = '0px';
    #         mapContainer.style.left = '0px';
    #
    #         // Use html2canvas to capture the map
    #         html2canvas(mapContainer, {
    #           useCORS: true,  // Enable CORS to capture tiles from other domains
    #           allowTaint: true
    #         }).then(function(canvas) {
    #           // Create a download link
    #           var link = document.createElement('a');
    #           link.href = canvas.toDataURL('image/png');
    #           link.download = '",
    #         roboplotr_string2filename(str_remove_all(title,"<[^>]*>")),
    #         ".png';  // Filename for the download
    #           link.click();  // Trigger the download
    #         }).finally(function() {
    #           // Restore visibility of navigation controls after capture
    #           controls.forEach(function(control) {
    #             control.style.display = 'flex';
    #           });
    #           // Restore the original position style of the map container
    #           mapContainer.style.position = originalPosition;
    #           mapContainer.style.top = originalTop;
    #           mapContainer.style.left = originalLeft;
    #         });
    #       }
    #     }
    #   "
    #       )
    #     )
    #   )
    # )

  if (zoom) {
    map <- map |>
      addEasyButton(
        easyButton(
          # Add zoom controls manually
          id = "map-plus",
          icon = "fa-plus",
          title = "L\u00e4henn\u00e4",
          onClick = JS("function(btn, map){ map.zoomIn(); }"),
          position = "topright"
        )
      ) |>
      addEasyButton(
        easyButton(
          id = "map-minus",
          icon = "fa-minus",
          title = "Loitonna",
          onClick = JS("function(btn, map){ map.zoomOut(); }"),
          position = "topright"
        )
      )
  }

  map <-  map |>
    addEasyButton(
      easyButton(
        id = "robonomist-link",
        icon = '<svg version="1.1" viewBox="0 0 71.447 32" style = "width: 12pt;padding-bottom: 2px" xmlns="http://www.w3.org/2000/svg"><path transform="scale(.31159)"  d="M 229.3 53.2 L 174.3 90.1 L 174.3 69.1 L 199.5 53.2 L 174.3 37.3 L 174.3 16.3 M112 0c14.2 0 23.3 1.8 30.7 7 6.3 4.4 10.3 10.8 10.3 20.5 0 11.3-6.4 22.8-22.3 26.5l18.4 32.5c5 8.7 7.7 9.7 12.5 9.7v6.5h-27.3l-23.7-45.8h-7v27.6c0 10.5 0.7 11.7 9.9 11.7v6.5h-43.2v-6.7c10.3 0 11.3-1.6 11.3-11.9v-65.7c0-10.2-1-11.7-11.3-11.7v-6.7zm-4.8 7.9c-3.3 0-3.6 1.5-3.6 8.6v32.3h6.4c15.8 0 20.2-8.7 20.2-21.3 0-6.3-1.7-11.5-5-15-2.9-3-7-4.6-13-4.6z M 0 53.2 L 55 16.3 L 55 37.3 L 29.8 53.2 L 55 69.1 L 55 90.1"/></svg>',
        title = "Robonomist",
        onClick = JS(
          "function(btn, map) {window.open(\"https://robonomist.com\")  }"
        ),
        position = "topright"
      )
    )

  map
}
