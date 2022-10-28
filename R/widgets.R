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
#' @importFrom htmltools htmlDependency
#' @importFrom htmlwidgets saveWidget
#' @importFrom R.utils setOption
#' @importFrom stringr str_extract_all str_replace_all str_c str_squish

roboplot_create_widget <- function(p, title, filepath, render = T, self_contained = F, png_artefacts) {

  if (missing(title)) {
    title <- (p$x$layoutAttrs |> unlist())[grep("title.text", names((p$x$layoutAttrs |> unlist())))] |>
      str_extract_all("(?<=\\>)[^\\<\\>]{2,}(?=\\<)") |> unlist() |> first() |> str_c(collapse = "_") |>
      roboplotr_string2filename()
    roboplotr_message(str_c("Using \"",title,"\" for htmlwidget filename.."))
  } else {
    title <- roboplotr_string2filename(title)
  }

  filepath <- if(missing(filepath)) {
    if(isTRUE(getOption('knitr.in.progress'))) {
      tempdir()
    } else {
      getwd() }
  } else  { filepath }

  if (is.null(getOption(str_c("roboplot.widget.deps.",filepath)))) {
    # print(str_c("widget deps in path ",filepath, " missing "))
    roboplotr_widget_deps(filepath = file.path(filepath,"plot_dependencies"))
    setOption(str_c("roboplot.widget.deps.",filepath), T)
  }# else {
  #print(getOption(str_c("roboplot.widget.deps.",filepath)))
  #}

  css_dep <- htmlDependency("style", "0.1", src = c(href= "plot_dependencies/css"),  stylesheet = "style.css")
  js_dep <- htmlDependency("js", "0.1", src = c(href= "plot_dependencies/js"),  script = "relayout.js")
  p$dependencies <- c(p$dependencies, list(css_dep, js_dep))
  detached_p <- p
  detached_p$append <- NULL
  detached_p |>
    saveWidget(file.path(filepath,str_c(title,".html")), selfcontained = self_contained, libdir = "plot_dependencies")


  if(!missing(png_artefacts)) {
    detached_p |> roboplotr_automate_png(png_artefacts, filepath)
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
#' @importFrom purrr map
#' @importFrom stringr str_replace_all str_c
roboplotr_automate_png <- function(p, artefacts, dl_path = getwd()) {

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
  }) |> roboplotr_compact()
  recent_length <- length(recent_files)
  if(recent_length > 0) {
    roboplotr_message(str_c("\nThe file",ifelse(recent_length > 1, "s",""),"\n", combine_words(recent_files,sep = ",\n", and = ", and\n"),"\n",
                  ifelse(recent_length > 1, "are","is")," in ",dl_path,"."))
  }

}

#' @importFrom farver decode_colour
#' @importFrom RCurl base64Encode
#' @importFrom stringr str_c
#' @importFrom stats setNames
#'
roboplotr_widget_deps <- function(filepath = NULL) {

  js_file <- system.file("www/js","relayout.js", package = "roboplotr")

  if(!is.null(filepath)) {
    dir.create(filepath, showWarnings = F)
    dir.create(file.path(filepath,"fonts"), showWarnings = F)
    dir.create(file.path(filepath,"css"), showWarnings = F)
    dir.create(file.path(filepath,"js"), showWarnings = F)
    invisible(file.copy(js_file, file.path(filepath,"js","relayout.js"), overwrite = T))
  }

  font_strings <- NULL

  set_font_strings <- function(this_opt, filepath) {
    if(!is.null(filepath)) {
      invisible(file.copy(this_opt$path, file.path(filepath,"fonts",str_extract(this_opt$path,"[^/]*$"))))
      font_string <- list(file.path("..","fonts",str_extract(this_opt$path,"[^/]*$"))) |> setNames(this_opt$family)
      font_string
    } else {
      base_font <- base64Encode(readBin(this_opt$path, "raw", file.info(this_opt$path)[1, "size"]), "txt")
      font_string <- list(str_c('data:vnd.ms-opentype;base64', base_font, sep=',')) |> setNames(this_opt$family)
      font_string
    }}

  for(opt in c("main", "caption","title")) {

    opt_name <- str_c("roboplot.font.",opt)

    this_opt <- getOption(opt_name)

    if(!is.null(this_opt$path)) {
      font_strings <- append(font_strings, set_font_strings(this_opt, filepath))
    }

  }

  rangeslider_mask <- decode_colour(getOption("roboplot.colors.background")) |> str_c(collapse = ", ")
  rangeslider_mask_css <- list(".rangeslider-mask-min, .rangeslider-mask-max",
                               c("fill", "fill-opacity"),
                               c(str_c("rgb(",rangeslider_mask,") !important"),"0.7 !important"))

  font_strings <- map2(font_strings, names(font_strings), ~ list('@font-face', c('font-family', 'src'), c(.y, str_c("url('",.x,"')")))) |>
    unname()

  css_list <- map(c(font_strings,list(rangeslider_mask_css)), ~.x)
  css_string <- roboplotr_get_css(css_list,
                                  file = if(!is.null(filepath)) { file.path(filepath,"css/style.css") } else NULL)

  if(is.null(filepath)) {
    list("css" = str_c('data:text/css;base64', base64Encode(css_string), sep=','),
         "js" =  str_c('data:application/javascript;base64', base64Encode(readBin(js_file, "raw", file.info(js_file)[1, "size"]), "txt"), sep=',')
    )
  }

}

#' Uploads the html elements and dependencies to cloud storage. DO NOT USE! WORK IN PROGRESS
#'
#' @param files_path The folder where the artefacts to be uploaded are located.
#' @param upload_path The gcs folder where the artefacts will be uploaded to.
#' @param overwrite If named files exist in the cloud storage, will they be overwritten.
#' @importFrom knitr current_input
#' @importFrom stringr str_remove str_replace_all str_c str_detect
#' @importFrom dplyr case_when
#' @importFrom googleCloudStorageR gcs_metadata_object gcs_upload gcs_get_global_bucket gcs_auth gcs_global_bucket gcs_list_objects
#' @importFrom purrr walk
roboplotr_upload_widgets <- function(files_path, upload_path, overwrite = FALSE) {

  if (length(Sys.glob(file.path(getwd() |> str_remove("(?<=pttrobo).{1,}"),"robottiperhe-*.json"))) != 0){
    aut_file <- Sys.glob(file.path(getwd() |> str_remove("(?<=pttrobo).{1,}"),"robottiperhe-*.json"))
  } else {
    aut_file <- Sys.glob(file.path("~" |> str_remove("(?<=pttrobo).{1,}"),"robottiperhe-*.json"))
  }

  tryCatch(gcs_auth(aut_file), error = function(e) {
    str <- str_c("Do you have the proper authorisation file in the directory?\n")
    stop(str, call. = F)
  })
  suppressMessages(gcs_global_bucket("pttry"))

  is_knitting <- isTRUE(getOption('knitr.in.progress'))

  if(missing(files_path)) {
    if(is_knitting == T) {
      cur_input <- current_input()
      files_path <- tempdir()
    } else {
      stop("Give the path to the files you wish to upload. Careful! This will upload every .html, .css, .map, .scss, .txt and .js file in the given path!", call. = F)
    }
  } else {
    upl_files <-  list.files(path = files_path, recursive = T, full.names = T) |> str_subset("\\.(css|js|map|scss|html|txt)$") |> str_c(collapse = ", ")
    roboplotr_warning(str_c("Give the path to the files you wish to upload. Careful! This will upload all of ",upl_files,"!\nType \"upload\" to continue:"))
    ans <- readline(" ")
    if (ans != "upload") { stop("Canceled", call. = F) }
  }

  if (missing(upload_path) & !is_knitting) {
    stop("Give the path to the folder in the upload bucket where you wish to upload the files to.", call. = F)
  }

  artefact_files <- list.files(files_path, recursive = T) |> str_subset("\\.(css|js|map|scss|html|txt)$")
  if(overwrite == FALSE) { message("Overwrite is set to false, set overwrite = T in roboplotr_upload_widgets if you want to overwrite existing uploads.") }
  walk(artefact_files, function(artefact_file) {
    upload_file <- if(is_knitting) {
      prefix <- cur_input |> str_remove("\\.Rmd$") |> str_replace_all("/","_") |> str_c("_artefacts")
      file.path("ennustekuvat",prefix,artefact_file)
    } else {
      file.path(upload_path,artefact_file)
    }
    obj.existence <- suppressMessages(gcs_list_objects(prefix = upload_file) |> nrow() |> as.logical())
    # print(artefact_file)
    # print(upload_file)
    if(obj.existence == TRUE & overwrite == FALSE) {
      roboplotr_warning(str_c("The file ",upload_file, " already exists!"))
    } else {
      if(obj.existence == TRUE) {
        roboplotr_alert(str_c("Overwriting previous upload of ",upload_file))
      } else {
        roboplotr_message(str_c("Uploading ",upload_file))
      }
      upload_type <- case_when(str_detect(upload_file, "css$") ~ "text/css",
                               str_detect(upload_file, "js$") ~ "text/javascript",
                               str_detect(upload_file, "txt$") ~ "text/plain",
                               str_detect(upload_file, "map$") ~ "application/json",
                               TRUE ~ as.character(NA))
      if(is.na(upload_type)) { upload_type <- NULL}
      meta <- gcs_metadata_object(artefact_file, cacheControl = "public, max-age=600")
      meta[["name"]] <- str_replace_all(upload_file, c("\\%C3\\%B6" = "\u00f6", "\\%C3\\%A4" = "\u00e4", "\\%2F" = "/"))
      gcs_upload(file.path(files_path,artefact_file), gcs_get_global_bucket(), name = upload_file, type = upload_type, object_metadata = meta, predefinedAcl="bucketLevel")
    }

  })
}
