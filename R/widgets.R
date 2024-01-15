#' File creation from [roboplot()]s
#'
#' Write html and and other files from [roboplot()] plots
#'
#' @param p A plotly object.
#' @param title Character. The filename of the artefact(s) created
#' (without file format). Will be formatted with underscores, and the title
#' of argument 'p' will be used if no title is provided.
#' @param filepath Character. The filepath to the created artefacts.
#' @param render Logical. Controls if the plot saved will be displayed in
#' viewer. Plot will be returned silently in either case.
#' @param self_contained Logical. Controls if the plot dependencies will be
#' saved in an adjacent directory "plot_dependencies" or contained within the
#' file, increasing size.
#' @param artefacts Character vector. Controls what artefacts are saved. One or
#' more of "html", "png", "jpg", "jpge", "webp", or "pdf".
#' @param zoom Numeric. Controls the zoom level of static images if any are
#' defined with 'artefacts'. Default 1.
#' @param width,height Numeric. Sets the size of any static images created. Any
#' artefacts created with [roboplot()]'s 'artefacts' parameter will use the given
#' dimensions, if any, for that plot.
#' @examples
#' # Saving roboplotr::roboplot() plots as files can be controlled by setting
#' # global options with roboplotr::set_roboplot_options() (see documentation),
#' # and using roboplotr::set_artefacts() in roboplotr::roboplot() parameter
#' # 'artefacts', but you can use this function as well. Control location of the
#' # files with 'filepath'. Default 'filepath' will be the current working
#' # directory. If a 'title' is not provided, it will be parsed from plot title.
#'
#' d <- energiantuonti |> dplyr::filter(Alue == "Kanada",Suunta == "Tuonti")
#'
#' d |>
#'   roboplot(
#'     Alue, "Energian tuonti Kanadasta", "Milj €", "Tilastokeskus"
#'     ) |>
#'   roboplot_create_widget(filepath = tempdir())
#'
#' file.exists(paste0(tempdir(),"/energian_tuonti_kanadasta.html"))
#'
#' # You can provide the filename as string and
#' # roboplotr::roboplot_create_widget() will parse the filename from that. The
#' # plot will always be silently returned, but 'render' controls whether it
#' # will be displayed in viewer on widget creation. Normally
#' # roboplotr::roboplot() html widgets will have dependencies contained in an
#' # external folder "plot_dependencies", but elements can be bundled within the
#' # widgets with 'self_contained' (if you want to forward the file, perhaps).
#'
#' d |>
#'   roboplot(
#'     Alue, "Kanadan energiantuonti", "Milj €", "Tilastokeskus"
#'     ) |>
#'   roboplot_create_widget(
#'     title = "Energian tuonti - Kanada",
#'     filepath = tempdir(),
#'     render = FALSE,
#'     self_contained = FALSE
#'   )
#'
#' file.exists(paste0(tempdir(),"/energian_tuonti_kanada.html"))
#'
#' # If you want to create static plot files, use a character vector of file
#' # types in 'artefacts'. Exclude "html" if you want to create static plots
#' # only. Possible filetypes are "html","png","jpg","jpge","webp","pdf". The
#' # static files created this way will respect the plot layout specifications
#' # of the roboplotr::roboplot() plot, unlike the ones downloaded through
#' # modebar (set those with roboplotr::set_roboplot_options()). Note that modebar
#' # gives access to svg file format, while automating it the file creation with
#' # roboplotr::roboplot_create_widget() or roboplotr::roboplot() allows for pdf
#' # files.
#'
#' if(interactive()) {
#'   d |>
#'     roboplot(
#'       color = Alue,
#'       title = "Kanadan energiantuonti",
#'       subtitle = "Milj €",
#'       caption = "Tilastokeskus",
#'       width = 400,
#'       height = 800
#'     ) |>
#'     roboplot_create_widget(filepath = tempdir(), artefacts = "pdf")
#'
#'   utils::browseURL(paste0(tempdir(), "/kanadan_energiantuonti.pdf"))
#' }
#' @return A list of classes "plotly" and "html"
#' @export
#' @importFrom dplyr first
#' @importFrom htmltools htmlDependency
#' @importFrom htmlwidgets saveWidget
#' @importFrom stringr str_extract_all str_replace_all str_c str_squish
#' @importFrom widgetframe frameableWidget

roboplot_create_widget <- function(
    p,
    title = NULL,
    filepath = getOption("roboplot.artefacts")$filepath,
    render = getOption("roboplot.artefacts")$render,
    self_contained = getOption("roboplot.artefacts")$self_contained,
    zoom = getOption("roboplot.artefacts")$zoom,
    artefacts = getOption("roboplot.artefacts")$artefacts,
    width = getOption("roboplot.artefacts")$width,
    height = getOption("roboplot.artefacts")$height
    ) {

  roboplotr_check_param(artefacts, "character", size = NULL, allow_null = F, allow_na = F)
  roboplotr_valid_strings(artefacts, c("html","png","jpg","jpge","webp","pdf"), .fun = any)
  roboplotr_check_param(zoom, "numeric", allow_null = F, allow_na = F)
  roboplotr_check_param(width, "numeric", allow_null = T, allow_na = F)
  roboplotr_check_param(height, "numeric", allow_null = T, allow_na = F)

  if(is.null(width)) { width <- getOption("roboplot.artefacts")$width }
  if(is.null(height)) { height <- getOption("roboplot.artefacts")$height }

  if (is.null(title)) {
    title <- (p$x[c("layout","layoutAttrs")] |> unlist())[str_subset(names((p$x[c("layout","layoutAttrs")] |> unlist())),"(?<!axis)\\.title\\.text")] |>
      first() |>
      str_extract_all("(?<=\\>)[^\\<\\>]{2,}(?=\\<)") |> unlist() |> first() |> str_c(collapse = "_")
    roboplotr_message(str_c("Using \"",roboplotr_string2filename(title),"\" for htmlwidget filename.."))
  } else {
    roboplotr_check_param(title, "character", allow_null = F, allow_na = F)
  }

  widget_title <- title
  title <- roboplotr_string2filename(title)

  if("html" %in% artefacts) {
    roboplotr_widget_deps(filepath = file.path(filepath,"plot_dependencies"))
    css_dep <- htmlDependency("style", "0.1", src = c(href= "plot_dependencies/css"),  stylesheet = "style.css")
    js_dep <- htmlDependency("js", "0.1", src = c(href= "plot_dependencies/js"),  script = "relayout.js")
    p$dependencies <- c(p$dependencies, list(css_dep, js_dep))
  }
  detached_p <- p
  detached_p$append <- NULL

  if(any(artefacts != "html")) {
    .images <- subset(artefacts, artefacts != "html")
    detached_p |> roboplotr_static_image(.images, title, zoom, filepath, width, height)
  }

  if("html" %in% artefacts) {
    detached_p |>
      frameableWidget() |>
      saveWidget(file.path(filepath,str_c(title,".html")), selfcontained = self_contained, libdir = if(self_contained) { NULL} else { "plot_dependencies" }, title = widget_title)
  }

  if(render == T) {
    p
  } else { invisible(p) }
}

#' Artefact control for [roboplot()]
#'
#' Set global parameters in [set_roboplot_options()] for artefact creation
#' of [roboplot()] plots.
#'
#' @param auto Logical. Whether [roboplot()] will create artefacts automatically.
#' @inheritParams roboplot_create_widget
#' @examples
#' # Used to set global defaults for widget or other artefact creation. Any of
#' # these can be overridden by roboplotr::roboplot(). Only supposed to be
#' # called inside roboplotr::set_roboplot_options() or roboplotr::roboplot().
#' # Use 'filepath' to control which directory the artefacts are created to,
#' # 'render' to control if the roboplot() plot will be rendered on artefact
#' # creation, 'self_contained' to control if html plot dependencies are placed
#' # in an adjacent directory or contained within the html file, 'artefacts'
#' # (one of "html", "png","jpg", "jpge", "jpge", "webp" or "pdf) to control
#' # what artefacts are created, and 'zoom' to set static artefact zoom level.
#'
#' # roboplot_create_widget() shows how the parameters are used.
#' @return A list.
#' @export
set_artefacts <- function(
    artefacts = getOption("roboplot.artefacts")$artefacts,
    title = NULL,
    filepath = getOption("roboplot.artefacts")$filepath,
    render = getOption("roboplot.artefacts")$render,
    self_contained = getOption("roboplot.artefacts")$self_contained,
    zoom = getOption("roboplot.artefacts")$zoom,
    auto = getOption("roboplot.artefacts")$auto,
    width = getOption("roboplot.artefacts")$width,
    height = getOption("roboplot.artefacts")$height
) {
  roboplotr_check_param(filepath, "character", allow_null = F)
  roboplotr_check_param(render, "logical", allow_null = F)
  roboplotr_check_param(self_contained, "logical", allow_null = F)
  roboplotr_check_param(artefacts, "character", size = NULL, allow_null = F)
  roboplotr_valid_strings(artefacts, c("html","png","jpg","jpge","webp","pdf"), .fun = any)
  roboplotr_check_param(title, "character", allow_null = T)
  roboplotr_check_param(zoom, "numeric", allow_null = F, allow_na = F)
  roboplotr_check_param(width, "numeric", allow_null = F, allow_na = F)
  roboplotr_check_param(height, "numeric", allow_null = F, allow_na = F)

  list(
    auto = auto,
    filepath = filepath,
    render = render,
    self_contained = self_contained,
    artefacts = artefacts,
    zoom = zoom,
    title = title,
    width = round(width),
    height = round(height)
  )
}


#' @importFrom plotly config
#' @importFrom webshot2 webshot
roboplotr_static_image <-
  function(p,
           artefacts,
           title,
           zoom,
           dl_path = getwd(),
           width = getOption("roboplot.artefacts")$width,
           height = getOption("roboplot.artefacts")$height) {

    .static_images <-
      str_c(dl_path, str_c(title, artefacts, sep = "."), sep = "/")

    p |>
      config(displayModeBar = F) |>
      roboplot_create_widget(
        title = "imgdl",
        filepath = tempdir(),
        self_contained = F,
        render = F,
        artefacts = "html"
      )

    for (.img in .static_images) {
      webshot(
        file.path(tempdir(), "imgdl.html"),
        file = .img,
        vwidth = width,
        vheight = height,
        zoom = zoom,
        quiet = T
      )
    }

    invisible(file.remove(file.path(tempdir(), "imgdl.html")))

  }


#' @importFrom RCurl base64Encode
#' @importFrom stats setNames
#' @importFrom stringr str_extract
roboplotr_set_font_string <- function(this_opt, filepath) {
  if(!is.null(filepath)) {
    invisible(file.copy(this_opt$path, file.path(filepath,"fonts",str_extract(this_opt$path,"[^/]*$"))))
    font_string <- list(file.path("..","fonts",str_extract(this_opt$path,"[^/]*$"))) |> setNames(this_opt$font_face)
    font_string
  # } else if (!is.null(this_opt$google_font)) {
  } else {
    base_font <- base64Encode(readBin(this_opt$path, "raw", file.info(this_opt$path)[1, "size"]), "txt")
    font_string <- list(str_c('data:vnd.ms-opentype;base64', base_font, sep=',')) |> setNames(this_opt$font_face)
    font_string
  }}

#' @importFrom farver decode_colour
#' @importFrom RCurl base64Encode
#' @importFrom stringr str_c str_glue
#' @importFrom stats setNames
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

  for(opt in c("main", "caption","title")) {

    opt_name <- str_c("roboplot.font.",opt)

    this_opt <- getOption(opt_name)

    if(!is.null(this_opt$path)) {
      font_strings <- append(font_strings, roboplotr_set_font_string(this_opt, filepath))
    } else if(!is.null(this_opt$google_font)) {
      font_strings <- append(font_strings, this_opt$google_font$url |> setNames(this_opt$font_face))
    }

  }


  rangeslider_mask <- decode_colour(getOption("roboplot.colors.background")) |> str_c(collapse = ", ")
  rangeslider_mask_css <- list(".rangeslider-mask-min, .rangeslider-mask-max",
                               c("fill", "fill-opacity"),
                               c(str_c("rgb(",rangeslider_mask,") !important"),"0.7 !important"))

  font_strings <- map2(font_strings, names(font_strings), ~ list('@font-face', c('font-family', 'src'), c(.y, str_c("url('",.x,"')")))) |>
    unname() |> unique()

  modebar_labcolor <- unlist(unique(getOption("roboplot.grid")[c("xcolor","ycolor")]))[1]
  modebar_lab <-
    list(
      ".js-plotly-plot .plotly [data-title]::after",
      c("margin-right", "background", "color", "font-family"),
      c(
        "-13px !important",
        str_glue("{modebar_labcolor} !important"),
        str_glue("{roboplotr:::roboplotr_text_color_picker(modebar_labcolor)} !important"),
        str_glue("{getOption('roboplot.font.caption')$family} !important")
      )
    )

  modebar_labpointer <-
    list(".js-plotly-plot .plotly [data-title]::before",
         "border-color",
          str_glue("transparent transparent {modebar_labcolor} !important"))


  css_list <-
    map(c(
      font_strings,
      list(rangeslider_mask_css),
      list(modebar_lab),
      list(modebar_labpointer)
    ), ~ .x)

  css_string <- roboplotr_get_css(css_list,
                                  file = if(!is.null(filepath)) { file.path(filepath,"css/style.css") } else NULL)

  if(is.null(filepath)) {
    list("css" = str_c('data:text/css;base64', base64Encode(css_string), sep=','),
         "js" =  str_c('data:application/javascript;base64', base64Encode(readBin(js_file, "raw", file.info(js_file)[1, "size"]), "txt"), sep=',')
    )
  }

}

# Uploads the html elements and dependencies to cloud storage. DO NOT USE! WORK IN PROGRESS
#
# @param files_path The folder where the artefacts to be uploaded are located.
# @param upload_path The gcs folder where the artefacts will be uploaded to.
# @param overwrite If named files exist in the cloud storage, will they be overwritten.
# @importFrom knitr current_input
# @importFrom stringr str_remove str_replace_all str_c str_detect
# @importFrom dplyr case_when
# @importFrom googleCloudStorageR gcs_metadata_object gcs_upload gcs_get_global_bucket gcs_auth gcs_global_bucket gcs_list_objects
# @importFrom purrr walk
# roboplotr_upload_widgets <- function(files_path, upload_path, overwrite = FALSE) {
#
#   if (length(Sys.glob(file.path(getwd() |> str_remove("(?<=pttrobo).{1,}"),"robottiperhe-*.json"))) != 0){
#     aut_file <- Sys.glob(file.path(getwd() |> str_remove("(?<=pttrobo).{1,}"),"robottiperhe-*.json"))
#   } else {
#     aut_file <- Sys.glob(file.path("~" |> str_remove("(?<=pttrobo).{1,}"),"robottiperhe-*.json"))
#   }
#
#   tryCatch(gcs_auth(aut_file), error = function(e) {
#     str <- str_c("Do you have the proper authorisation file in the directory?\n")
#     stop(str, call. = F)
#   })
#   suppressMessages(gcs_global_bucket("pttry"))
#
#   is_knitting <- isTRUE(getOption('knitr.in.progress'))
#
#   if(missing(files_path)) {
#     if(is_knitting == T) {
#       cur_input <- current_input()
#       files_path <- tempdir()
#     } else {
#       stop("Give the path to the files you wish to upload. Careful! This will upload every .html, .css, .map, .scss, .txt and .js file in the given path!", call. = F)
#     }
#   } else {
#     upl_files <-  list.files(path = files_path, recursive = T, full.names = T) |> str_subset("\\.(css|js|map|scss|html|txt)$") |> str_c(collapse = ", ")
#     roboplotr_warning(str_c("Give the path to the files you wish to upload. Careful! This will upload all of ",upl_files,"!\nType \"upload\" to continue:"))
#     ans <- readline(" ")
#     if (ans != "upload") { stop("Canceled", call. = F) }
#   }
#
#   if (missing(upload_path) & !is_knitting) {
#     stop("Give the path to the folder in the upload bucket where you wish to upload the files to.", call. = F)
#   }
#
#   artefact_files <- list.files(files_path, recursive = T) |> str_subset("\\.(css|js|map|scss|html|txt)$")
#   if(overwrite == FALSE) { message("Overwrite is set to false, set overwrite = T in roboplotr_upload_widgets if you want to overwrite existing uploads.") }
#   walk(artefact_files, function(artefact_file) {
#     upload_file <- if(is_knitting) {
#       prefix <- cur_input |> str_remove("\\.Rmd$") |> str_replace_all("/","_") |> str_c("_artefacts")
#       file.path("ennustekuvat",prefix,artefact_file)
#     } else {
#       file.path(upload_path,artefact_file)
#     }
#     obj.existence <- suppressMessages(gcs_list_objects(prefix = upload_file) |> nrow() |> as.logical())
#     # print(artefact_file)
#     # print(upload_file)
#     if(obj.existence == TRUE & overwrite == FALSE) {
#       roboplotr_warning(str_c("The file ",upload_file, " already exists!"))
#     } else {
#       if(obj.existence == TRUE) {
#         roboplotr_alert(str_c("Overwriting previous upload of ",upload_file))
#       } else {
#         roboplotr_message(str_c("Uploading ",upload_file))
#       }
#       upload_type <- case_when(str_detect(upload_file, "css$") ~ "text/css",
#                                str_detect(upload_file, "js$") ~ "text/javascript",
#                                str_detect(upload_file, "txt$") ~ "text/plain",
#                                str_detect(upload_file, "map$") ~ "application/json",
#                                TRUE ~ as.character(NA))
#       if(is.na(upload_type)) { upload_type <- NULL}
#       meta <- gcs_metadata_object(artefact_file, cacheControl = "public, max-age=600")
#       meta[["name"]] <- str_replace_all(upload_file, c("\\%C3\\%B6" = "\u00f6", "\\%C3\\%A4" = "\u00e4", "\\%2F" = "/"))
#       gcs_upload(file.path(files_path,artefact_file), gcs_get_global_bucket(), name = upload_file, type = upload_type, object_metadata = meta, predefinedAcl="bucketLevel")
#     }
#
#   })
# }
