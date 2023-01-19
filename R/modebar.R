#' @importFrom fontawesome fa
#' @importFrom htmlwidgets JS
#' @importFrom plotly config
#' @importFrom purrr pmap
#' @importFrom stringr str_c str_extract_all str_replace_all str_squish str_wrap
#' @importFrom tidyr drop_na
roboplotr_modebar <- function(p, title, subtitle, height, width) {

  if(is.null(title)) {
    dl_title <- "img"
  } else {
    dl_title <- roboplotr_string2filename(title)
  }

  js_string <- function(layout, ttl = dl_title) {
    tf <- getOption("roboplot.font.title")
    mf <- getOption("roboplot.font.main")
    cf <- getOption("roboplot.font.caption")
    plot_title <- list(title,subtitle,tf$bold)
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
          setVerticalLayout({"width": true}, gd, ',layout$main,', ["',plot_title[[1]],'","',plot_title[[2]],'","',plot_title[[3]],'"], pie_plot = ',if(all(p$trace_types == "pie")) { "true" } else { "false" },')
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
    row.data <- p$data |> pmap(function(...) {
      as.character(list(...)) |> replace_na("NA") |> str_c(collapse = ";")
    }) |> unlist() |> str_c(collapse = "\\n")
    col.names <- names(p$data) |>  str_c(collapse = ";")
    ti <- roboplotr_transform_string(title)
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
  p |> config(
    displaylogo = FALSE,
    modeBarButtons = list(unname(btn_list))
  )
}


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
