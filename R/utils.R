#' Override the default options for colors, fonts etc. for any plot created with roboplot()
#'
#' @param roboplot_options List. The roboplot options to be set.
#' @param notify Logical. Controls the message for set options.
#' @param shinyapp Logical. Makes fonts, css and javascript available for shiny apps.
#' @export
#' @importFrom knitr combine_words
#' @importFrom purrr iwalk
#' @importFrom stringr str_c str_detect str_extract str_subset
#' @importFrom R.utils setOption
roboplot_set_options <- function(roboplot_options, notify = T, shinyapp = F) {
  opts_names <- names(options())

  # reset session specific options

  widget_deps_names <- subset(opts_names, str_detect(opts_names, "^roboplot.widget.deps"))
  for(opt in widget_deps_names) {
    setOption(opt, NULL)
    }

  opts_names <- subset(opts_names, str_detect(opts_names, "^roboplot"))
  opts_error <- function() stop(str_c("Roboplot options must be a named list with names among\n",combine_words(opts_names, and = " and/or "),"!\n"), call. = F)
  if(!is.list(roboplot_options)) { opts_error() }
  if(is.null(names(roboplot_options))) {
    opts_error()
  } else if (!all(names(roboplot_options) %in% opts_names)) {
    opts_error()
  }

  check_colors <- function() {
    color_options <- roboplot_options[str_subset(names(roboplot_options), "roboplot.colors")]
    list_options <- color_options[str_subset(names(color_options), "ticks|grid|border")]
    if(length(list_options) > 0) {
      for(op in list_options) {
        if(!all(c("x","y") == names(op)) | !is.list(op)) { stop("Any color roboplot options for border, grid and ticks must be a named list with names x and y!", call. = F) }
        }
    }
    if(length(color_options) > 0) {
      if(any(roboplot_are_colors(unlist(color_options))) == F) { stop ("Any color roboplot_options must be hexadecimal values or among strings provided by grDevices::colors!", call. = F) }
    }
  }

  check_dashtypes <- function() {
    dashtype_options <- roboplot_options[str_subset(names(roboplot_options), "roboplot.dashtypes")]
    if(length(dashtype_options) > 0) {
      valid_dashtypes <- c("solid", "dash", "dot", "longdash", "dashdot", "longdashdot")
      if(!all(dashtype_options %in% valid_dashtypes) | !all(valid_dashtypes %in% dashtype_options)) { stop (str_c("Dashtype roboplot options must contain all of ",combine_words(valid_dashtypes)," in any order!"), call. = F) }
    }
  }

  check_modebar_buttons <- function() {
    button_options <- roboplot_options[str_detect(names(roboplot_options),"roboplot.modebar.buttons")]
    valid_buttons <- c("closest","compare","img_w","img_n","img_s","data_dl","robonomist")
    if(!any(unlist(button_options) %in% valid_buttons)) {
      stop(str_c("Roboplot modebar button options must be one or more of ",combine_words(valid_buttons),"!"), call. = F)
    }
  }

  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

  check_png_fonts <- function() {
    png_fonts <- roboplot_options[str_detect(names(roboplot_options),"roboplot.png.font.size")]
    valid_png_fonts <- c("title","main","caption")
    for(png_font in png_fonts) {
      if(!is.list(png_font) | !all(names(png_font) == valid_png_fonts) | any(!is.wholenumber(unlist(png_font)))) {
        stop(str_c("Roboplot png fontsize options must be one or more of ",combine_words(valid_png_fonts)," and be whole numbers!"), call. = F)
        }
    }
  }

  check_fonts <- function(roboplot_options) {

    font_options <- roboplot_options[str_detect(names(roboplot_options),"roboplot.font")]

    for(i in seq_len(length(font_options)) ) {

      opt_name <- names(font_options[i])
      opt <- font_options[[i]]

      if(is.null(opt) | !any(c("size","path","color") %in% names(opt))) {
        stop(str_c(opt_name, " must be a list with \"size\", \"path\" and \"color\", and optionally \"bold\" used with titles."), call. = F)
      }

      for(name in names(opt)) {
        if(is.null(opt[[name]])) { stop(str_c(opt_name," options can't be NULL!"), call. = F) }
      }

      if(!opt$path %in% c("serif","sans-serif")) {
        if (!file.exists(opt$path) | !tools::file_ext(tolower(opt$path)) %in% c("otf","ttf")) {
          stop (str_c(opt_name, " path must be \"roboplot-main\", \"serif\", \"sans-serif\", or a filepath to a file with .otf or .ttf extension."), call. = F)
        } else {
          opt$family <- str_c("roboplot-",str_extract(opt_name,"[^\\.]*$"))
        }
      } else if (opt$path %in% c("serif","sans-serif")) {
        opt$family <- opt$path
        opt$path <- NULL
      }

      if(!is.wholenumber(opt$size)) {
        stop(str_c(opt_name, "  size must be a whole number."), call. = F)
      }

      if(roboplot_are_colors(opt$color) == F & length(opt$color) != 1) {
        stop(str_c(opt_name, " colors must be 6-character hexadecimal colors or among strings provided by grDevices::colors!."), call. = F)
      }

      if(str_detect(opt_name,"title") & is.logical(opt$bold)) {
        if(opt$bold == F) {
         opt$bold <- function(x) { x }
        } else {
          opt$bold <- function(x) { paste0("<b>",x,"</b>") }
        }
      } else if(str_detect(opt_name,"title") &!is.logical(opt$bold)) { stop("roboplot.font.main$bold must be TRUE or FALSE.") }

      roboplot_options[[opt_name]] <- opt

    }

    roboplot_options
  }
  roboplot_options <- check_fonts(roboplot_options)
  check_modebar_buttons()
  check_colors()

  iwalk(roboplot_options, ~ setOption(names(roboplot_options[.y]), .x))

  if(notify == T) { message(str_c("Roboplot option",if(length(roboplot_options) == 1) {""} else {"s"}," ",combine_words(names(roboplot_options))," set.")) }

  if(!str_detect(getOption("roboplot.logo"),"robonomist.png")) {
    setOption("roboplot.modebar.buttons", c(getOption("roboplot.modebar.buttons"),"robonomist"))
  }

  if(shinyapp ) {
    roboplot_make_widget_deps(tempdir())
    addResourcePath("js", system.file("www","js", package = "roboplotr"))
    addResourcePath("fonts", file.path(tempdir(),"fonts"))
    addResourcePath("css", file.path(tempdir(),"css"))
  }

}

#' Strips a string to filename format for roboplots
#'
#' @param string The string that will be trunctated to filename
#' @importFrom stringr str_extract_all str_replace_all str_c str_squish
roboplot_string2filename <- function(string) {
  str_extract_all(string, "[a-z\uE5\uE4\uF6,A-Z\uC5\uC4\uD6,\\s,_,\\.,0-9]", simplify = T) |>
    str_c(collapse = "") |>
    str_squish() |>
    tolower() |>
    str_replace_all(c("\uE4" = "a", "\uE5" = "o", "\uF6" = "o", " |\\." = "_", "," = ""))
}


#' @importFrom rlang sym quo_name
#' @importFrom tidyr unite
#' @importFrom dplyr across everything mutate rename select
#' @importFrom stringr str_replace
roboplot_transform_data_for_download <- function(d, color, linetype, facet_split, plot_mode) {
  d <- d |> rename(csv.data.tiedot = !! sym(quo_name(color)))
  if(!missing(facet_split)) { d <- unite(d, "csv.data.tiedot", .data$csv.data.tiedot, !!facet_split, sep = ", ")}
  if(!is.null(linetype)) { d <- unite(d, "csv.data.tiedot", .data$csv.data.tiedot, !!linetype, sep = ", ")}
  if(plot_mode == "horizontal" & quo_name(color) != quo_name(names(plot_mode))) { d <- unite(d, "csv.data.tiedot", .data$csv.data.tiedot, sym(quo_name(names(plot_mode))))}
  d |>
    select(.data$csv.data.tiedot, .data$time, .data$value) |>
    mutate(
      across(everything(), ~ as.character(.x)),
      value = str_replace(.data$value, "\\.",",")
    )
}

#' Uploads the html elements and dependencies to cloud storage. DO NOT USE! WORK IN PROGRESS
#'
#' @param files_path The folder where the artefacts to be uploaded are located.
#' @param upload_path The gcs folder where the artefacts will be uploaded to.
#' @param overwrite If named files exist in the cloud storage, will they be overwritten.
#' @export
#' @importFrom knitr current_input
#' @importFrom stringr str_remove str_replace_all str_c str_detect
#' @importFrom dplyr case_when
#' @importFrom googleCloudStorageR gcs_metadata_object gcs_upload gcs_get_global_bucket gcs_auth gcs_global_bucket gcs_list_objects
#' @importFrom purrr walk
roboplot_upload_widgets <- function(files_path, upload_path, overwrite = FALSE) {

  if (length(Sys.glob(file.path(getwd() |> str_remove("(?<=pttrobo).{1,}"),"robottiperhe-*.json"))) != 0){
    aut_file <- Sys.glob(file.path(getwd() |> str_remove("(?<=pttrobo).{1,}"),"robottiperhe-*.json"))
  } else {
    aut_file <- Sys.glob(file.path("~" |> str_remove("(?<=pttrobo).{1,}"),"robottiperhe-*.json"))
  }

  tryCatch(gcs_auth(aut_file), error = function(e) {
    str <- paste0("Do you have the proper authorisation file in the directory?\n")
    stop(str, call. = F)
  })
  suppressMessages(gcs_global_bucket("pttry"))

  is_knitting <- isTRUE(getOption('knitr.in.progress'))

  if(missing(files_path)) {
    if(is_knitting == T) {
      cur_input <- knitr::current_input()
      files_path <- tempdir()
    } else {
      stop("Give the path to the files you wish to upload. Careful! This will upload every .html, .css, .map, .scss, .txt and .js file in the given path!", call. = F)
    }
  } else {
    upl_files <-  list.files(path = files_path, recursive = T, full.names = T) |> str_subset("\\.(css|js|map|scss|html|txt)$") |> str_c(collapse = ", ")
    message(str_c("Give the path to the files you wish to upload. Careful! This will upload all of ",upl_files,"!\nType \"upload\" to continue:"))
    ans <- readline(" ")
    if (ans != "upload") { stop("Canceled", call. = F) }
  }

  if (missing(upload_path) & !is_knitting) {
    stop("Give the path to the folder in the upload bucket where you wish to upload the files to.", call. = F)
  }

  artefact_files <- list.files(files_path, recursive = T) |> str_subset("\\.(css|js|map|scss|html|txt)$")
  if(overwrite == FALSE) { message("Overwrite is set to false, set overwrite = T in roboplot_upload_widgets if you want to overwrite existing uploads.") }
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
      message(str_c("The file ",upload_file, " already exists!"))
    } else {
      if(obj.existence == TRUE) {
        message(str_c("Overwriting previous upload of ",upload_file))
      } else {
        message(str_c("Uploading ",upload_file))
      }
      upload_type <- dplyr::case_when(str_detect(upload_file, "css$") ~ "text/css",
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
