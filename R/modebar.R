#' @importFrom dplyr mutate
#' @importFrom fontawesome fa
#' @importFrom htmlwidgets JS
#' @importFrom lubridate as_date quarter
#' @importFrom plotly config
#' @importFrom purrr pmap
#' @importFrom stringr str_c str_extract_all str_replace_all str_squish str_wrap
#' @importFrom tidyr drop_na
roboplotr_modebar <- function(p, title, subtitle, caption, height, width, dateformat, info_text = NULL) {

  if(is.null(title)) {
    dl_title <- "img"
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
    str_c('
          function(gd, params) {
          let oldlayout = JSON.parse(JSON.stringify(gd.layout))
          delete gd.layout.xaxis.rangeslider;
          delete gd.layout.height;
          delete gd.layout.width;
          Plotly.relayout(gd, {height: ',layout$y,', width: ',layout$x,',
          "annotations[0].font.size": ',layout$caption,',
          "xaxis.tickfont.size": ',layout$main,',
          "yaxis.tickfont.size": ',layout$main,',
          "title.font.size": ',layout$title,'})
          setVerticalLayout({"width": true}, gd, ',layout$main,', ["',plot_title[[1]],'","',plot_title[[2]],'",',tolower(plot_title[[3]]),'], pie_plot = ',if(all(p$trace_types == "pie")) { "true" } else { "false" },')
          Plotly.downloadImage(gd, {scale: "1", format: "',layout$type,'", width: ',layout$x,', height: ',layout$y,', filename: "',ttl,layout$suffix,'"});
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
    d <- p$data
    if("time" %in% names(d) & !is.null(dateformat)) {
      if(str_detect(dateformat,"Q")) {
        d <- d |> mutate(time = str_c(format(as_date(.data$time), "%YQ"),lubridate::quarter(.data$time)))
      } else {
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

  if(!is.null(info_text)) {
    modal_id <- str_c("roboplot-info-", str_remove(runif(1), "\\."))
    main_font <- getOption("roboplot.font.main")
    title_font <- getOption("roboplot.font.title")
    modal_color <- getOption('roboplot.trace.border')$color
    .bold <- ifelse(title_font$bold, tags$b, tags$span)
    modal_html <- tags$div(
      id = str_glue("{modal_id}_infomodal"),
      style = "position: absolute; top: 20px; left: 5px; width: 50%;",
      tags$div(
        style = str_glue(
          "position: absolute;
               z-index: 9999;
               background-color: {modal_color};
               color: {roboplotr_text_color_picker(modal_color)};
               padding: 5px;
               border: 1px solid {getOption('roboplot.border')[c('xcolor','ycolor')] |> unique() |> unlist() |> first()};
               box-shadow: 0 4px 8px {modal_color};"
        ),
        tags$span(
          id = str_glue("{modal_id}_infomodal-close"),
          fa(
            "times-circle",
            fill = main_font$color,
            height = str_glue("{title_font$size}px")
          ),
          style = "top: 5px; right: 10px; font-size: 24px; cursor: pointer; float: right;"
        ),
        .bold(
          title$title,
          tags$br(),
          tags$span(subtitle, style = "font-size: 75%"),
          style = str_glue(
            "font-family: {title_font$family};font-size: {title_font$size}px;"
          )
        ),
        tags$span(
          style = str_glue(
            "font-family: {main_font$family}; font-size: {main_font$size}px;"
          ),
          tags$p(HTML(info_text)),
          tags$p(caption)

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
        modal.style.display = 'block'; // Show the modal
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

  p |> config(
    displaylogo = FALSE,
    modeBarButtons = list(unname(btn_list))
  )
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


#' Png downloads in [roboplot()]
#'
#' Use in [set_roboplot_options()] to get a list used for [roboplot()] relayouts for downloaded image files when the appropriate modebar button is pressed
#'
#' @param height,width Doubles. The dimensions for the image file in pixels the modebar button will produce.
#' @param mainfont,titlefont,captionfont Doubles. The font sizes used in the image file the modebar button will produce.
#' @param suffix Character. Suffix attached after the name of the downloaded image file.
#' @param format Character. One of "png", "svg", "jpg", or "webp". Defines the file format of the downloaded image file.
#' @examples
#' # Used inside roboplotr::set_roboplot_options() to control image download
#' # specifications. Parameters 'x' and 'y' control the image dimensions. The
#' # image uses the font specifications of the main plot, but you might need to
#' # alter the font sizes with 'mainfont', 'titlefont' and 'captionfont'. You
#' # can give a suffix for image download filenames (the filename is derived
#' # from the plot title), and provide any one of "png","svg","jpeg", or "webp"
#' # for the file format.
#'
#' set_roboplot_options(
#'   caption = list(prefix = "Lähde: ", lineend = ".", updated = FALSE),
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
#' if(interactive()) {
#'
#'   p <- d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus")
#'   p |> roboplot_create_widget(filepath = tempdir(),
#'                               artefacts = c("img_w")
#'   )
#'   utils::browseURL(paste0(tempdir(),"/energian_tuonti.svg"))
#' }
#'
#' # The plotly image downloads are controlled by the modebar, so if you use
#' # other than 'img_wide' you need to use roboplotr::set_roboplot_options to
#' # control the modebar accordingly in addition to giving the downloaded image
#' # specifications (roboplotr::roboplot() does have some defaults in place
#' # for every size, so you can skip image specifications even if you do add
#' # use other image download buttons in the modebar). With multiple image
#' # download buttons the 'suffix' parameter is especially handy.
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
#' if(interactive()) {
#'
#'   p <- d |> roboplot(Alue, "Energian tuonti", "Milj €", "Tilastokeskus")
#'   p |> roboplot_create_widget(filepath = tempdir(),
#'                               artefacts = c("img_w","img_n","img_s")
#'   )
#'   utils::browseURL(paste0(tempdir(),"/energian_tuonti_narrow.png"))
#'   utils::browseURL(paste0(tempdir(),"/energian_tuonti_pieni.png"))
#'   utils::browseURL(paste0(tempdir(),"/energian_tuonti_wide.png"))
#' }
#'
#' # Revert to defaults:
#' set_roboplot_options(reset = TRUE)
#' @returns A list
#' @export
set_imgdl_layout <- function(
    width = 1280,
    height = 720,
    mainfont = getOption("roboplot.font.main")$size,
    titlefont = getOption("roboplot.font.title")$size,
    captionfont = getOption("roboplot.font.caption")$size,
    suffix = "_img",
    format = "png") {
  roboplotr_check_param(width, "numeric", allow_null = F)
  roboplotr_check_param(height, "numeric", allow_null = F)
  roboplotr_check_param(mainfont, "numeric", allow_null = F)
  roboplotr_check_param(titlefont, "numeric", allow_null = F)
  roboplotr_check_param(captionfont, "numeric", allow_null = F)
  roboplotr_check_param(suffix, "character", allow_null = F)
  roboplotr_check_param(format, "character", allow_null = F)
  roboplotr_valid_strings(format, c("png","svg","jpeg", "webp"), any)
  list(x = width, y = height, main = mainfont, title = titlefont, caption = captionfont, suffix = suffix, type = format)
}
