#' @importFrom stringr str_c str_extract_all str_replace_all str_squish str_wrap
#' @importFrom htmlwidgets JS
#' @importFrom plotly config
#' @importFrom purrr pmap
#' @importFrom tidyr drop_na
#' @importFrom fontawesome fa
roboplotr_modebar <- function(p, title, subtitle) {

  if(is.null(title)) {
    dl_title <- "img"
  } else {
    dl_title <- str_extract_all(title, "[a-z\u00e5\u00e4\u00f6,A-Z\u00c5\u00c4\u00d6,\\s,_,\\.,0-9]", simplify = T) |>
      roboplotr_string2filename()
  }

  js_string <- function(wd, ht, suffix, layout, ttl = dl_title) {
    plot_title <- list(title,subtitle,getOption("roboplot.font.title")$bold)
    str_c('
          function(gd) {
          let oldlayout = JSON.parse(JSON.stringify(gd.layout))
          delete gd.layout.xaxis.rangeslider;
          delete gd.layout.height
          Plotly.relayout(gd, {height: ',ht,', width: ',wd,',
          "font": ',layout$caption,',
          "xaxis.tickfont.size": ',layout$main,',
          "yaxis.tickfont.size": ',layout$main,',
          "title.font.size": ',layout$title,'})
          setVerticalLayout({"width": true}, gd, ',layout$main,', ["',plot_title[[1]],'","',plot_title[[2]],'","',plot_title[[3]],'"], pie_plot = ',if(all(p$trace_types == "pie")) { "true" } else { "false" },')
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
