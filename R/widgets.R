#' Export visualizations to various formats
#'
#' Write html and and other files from [roboplots][roboplot()], [robotables][robotable()]
#' or [robomaps][robomap()].
#'
#' @param p A plotly object.
#' @param title Character. The filename of the artefact(s) created
#' (without file format). Will be formatted with underscores, and the `title` text
#' of `p` will be used if no title is provided.
#' @param filepath Character. The filepath to the created artefacts.
#' @param render Logical. Controls if the artefact will be displayed in
#' viewer. Will be returned silently in either case.
#' @param self_contained Logical. Controls whether artefact's dependencies' are
#' saved in an adjacent directory or contained within the
#' file, increasing size.
#' @param artefacts Character vector. Controls what artefacts are saved. One or
#' more of "html", "png", "jpg", "jpge", "webp", or "pdf".
#' @param zoom Numeric. Controls the zoom level of static plots if any are
#' defined with 'artefacts'. Default 1.
#' @param width,height Numeric. Sets the size of any static plots created. Any
#' artefacts created with [roboplot()]'s `artefacts` parameter will use the given
#' dimensions, if any, for that plot.
#' @examples
#' set_roboplot_options(verbose = "Warning", .default = TRUE)
#' # Saving `roboplot()` plots as files can be controlled by setting global options
#' # with `set_roboplot_options()`, and using `set_artefacts()` in `roboplot()`
#' # `artefacts`, but you can use this function as well. Control location of the
#' # files with `filepath` (default is current working directory).
#' \dontrun{
#' d <- energiantuonti |> dplyr::filter(Alue == "Kanada",Suunta == "Tuonti")
#'
#' d |>
#'   roboplot(
#'     Alue, "Energian tuonti Kanadasta", "Milj €", "Tilastokeskus"
#'     ) |>
#'   create_widget(filepath = tempdir())
#'
#' file.exists(paste0(tempdir(),"/energian_tuonti_kanadasta.html"))
#'
#' # You can provide the filename as string and `create_widget()` will parse the
#' # filename from that. The plot will always be silently returned, but `render`
#' # controls whether it will be displayed in viewer on widget creation. Normally
#' # `roboplotr` html widgets will have dependencies contained in an
#' # external folder, but they can be bundled within with 'self_contained'.
#'
#' d |>
#'   roboplot(
#'     Alue, "Kanadan energiantuonti", "Milj €", "Tilastokeskus"
#'     ) |>
#'   create_widget(
#'     title = "Energian tuonti - Kanada",
#'     filepath = tempdir(),
#'     render = FALSE,
#'     self_contained = FALSE
#'   )
#'
#' file.exists(paste0(tempdir(),"/energian_tuonti_kanada.html"))
#'
#' # If you want to create non-interactive files, use a character vector of file
#' # types in 'artefacts'. Possible filetypes are "html","png","jpg","jpge","webp",
#' # "pdf". The static files created this way will respect the plot layout specifications
#' # of the `roboplot()` plot, unlike the ones exported with modebar. Note that
#' # modebar gives access to svg file format, while automating it the file creation with
#' # `create_widget()` or `roboplot()` allows for pdf files.
#'
#'   d |>
#'     roboplot(
#'       color = Alue,
#'       title = "Kanadan energiantuonti",
#'       subtitle = "Milj €",
#'       caption = "Tilastokeskus",
#'       width = 400,
#'       height = 800
#'     ) |>
#'     create_widget(filepath = tempdir(), artefacts = "pdf")
#'
#'   utils::browseURL(paste0(tempdir(), "/kanadan_energiantuonti.pdf"))
#' }
#' @returns What was passed as `p`.
#' @export
#' @importFrom dplyr first
#' @importFrom htmltools htmlDependency
#' @importFrom htmlwidgets saveWidget
#' @importFrom stringr str_extract_all str_replace_all str_c str_squish
#' @importFrom utils packageVersion
#' @importFrom widgetframe frameableWidget

create_widget <- function(
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
  is.robotable <- "datatables" %in% class(p)

  roboplotr_typecheck(artefacts, "character", size = NULL, allow_null = F)
  roboplotr_valid_strings(artefacts, c("html","png","jpg","jpge","webp","pdf"), .fun = any)
  roboplotr_typecheck(zoom, "numeric", allow_null = F)
  roboplotr_typecheck(width, "numeric", allow_null = T)
  roboplotr_typecheck(height, "numeric", allow_null = T)

  if(!dir.exists(filepath)) {
    stop(str_glue("Does the directory {filepath} exist?"), call. = T)
  }

  if(is.null(width)) { width <- getOption("roboplot.artefacts")$width }
  if(is.null(height)) { height <- getOption("roboplot.artefacts")$height }

  if (is.null(title)) {
    if(is.robotable) { stop("You must provide a title for a robotable widget when creating artefacts!", call. = F) }
    if(!is.null(p$title)) {
      title <- p$title
    } else {
      title <- (p$x[c("layout","layoutAttrs")] |> unlist())[str_subset(names((p$x[c("layout","layoutAttrs")] |> unlist())),"(?<!axis)\\.title\\.text")] |>
        first() |>
        str_extract_all("(?<=\\>)[^\\<\\>]{2,}(?=\\<)") |> unlist() |> first() |> str_c(collapse = "_")
    }
    roboplotr_message(str_c("Using \"",roboplotr_string2filename(title),"\" for htmlwidget filename.."))
  } else {
    roboplotr_typecheck(title, "character", allow_null = F)
  }

  widget_title <- title
  title <- roboplotr_string2filename(title)
  detached_p <- structure(p, class = str_subset(class(p), "roboplotr", negate = T))
  if(!is.robotable) {
    detached_p$append <- NULL
    if("html" %in% artefacts) {
      roboplotr_widget_deps(filepath = file.path(filepath,"plot_dependencies"))
      css_dep <- htmlDependency("style", packageVersion("roboplotr"), src = c(href= "plot_dependencies/css"),  stylesheet = "style.css")
      js_dep <- htmlDependency("js", packageVersion("roboplotr"), src = c(href= "plot_dependencies/js"),  script = "relayout.js")
      detached_p$dependencies <- c(detached_p$dependencies, list(css_dep, js_dep))
    }
  }

  if(any(artefacts != "html")) {
    if(is.robotable) {
      roboplotr_alert("Other artefacts than \"html\" are ignored with robotable!")
    } else {
      .images <- subset(artefacts, artefacts != "html")
      detached_p |> roboplotr_static_image(.images, title, zoom, filepath, width, height)
    }
  }

  if("html" %in% artefacts) {
    if(self_contained) {
      .libdir <- NULL
      } else {
      .libdir <- ifelse(is.robotable, "tbl_dependencies", "plot_dependencies")
      }
    detached_p |>
      frameableWidget() |>
      saveWidget(file.path(filepath,str_c(title,".html")), selfcontained = self_contained, libdir =.libdir, title = widget_title)
  }

  if(render == T) {
    p
  } else {
    invisible(p)
  }
}

#' Visualization export configuration
#'
#' Parameters to configure exports from [roboplots][roboplot()] or [robotables][robotable()].
#'
#' @param auto Logical. Whether [roboplot][roboplot()] or [robotable][robotable()]
#' will create artefacts automatically.
#' @inheritParams create_widget
#' @examples
#' # Used to set global defaults for widget or other artefact creation. Any of
#' # these can be overridden by `roboplot()`. Use `filepath` to control which
#' # directory the artefacts are created to, `render` to control if the object will
#' # be rendered on artefact creation, `self_contained` to control if html plot
#' # dependencies are placed in an adjacent directory or contained within the html file,
#' # `artefacts` (one of "html", "png","jpg", "jpge", "jpge", "webp" or "pdf) to
#' # control what artefacts are created, and 'zoom' to set static artefact zoom level.
#'
#' # create_widget() shows how the parameters are used.
#' @returns A list of class roboplot.set_artefacts.
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
  roboplotr_typecheck(filepath, "character", allow_null = F)
  roboplotr_typecheck(render, "logical", allow_null = F)
  roboplotr_typecheck(self_contained, "logical", allow_null = F)
  roboplotr_typecheck(artefacts, "character", size = NULL, allow_null = F)
  roboplotr_valid_strings(artefacts, c("html","png","jpg","jpge","webp","pdf"), .fun = any)
  roboplotr_typecheck(title, "character", allow_null = T)
  roboplotr_typecheck(zoom, "numeric", allow_null = F)
  roboplotr_typecheck(width, "numeric", allow_null = F)
  roboplotr_typecheck(height, "numeric", allow_null = F)

  .res <- list(
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

  .res <- structure(.res, class = c("roboplotr","roboplotr.set_artefacts", class(.res)))

  .res
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

    rm_rangeslider <- function(p) {
      p$x$layout$xaxis$rangeslider = NULL
      p
    }
    p |>
      config(displayModeBar = F) |>
      rm_rangeslider() |>
      create_widget(
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
        str_glue("{roboplotr_text_color_picker(modebar_labcolor,getOption('roboplot.font.caption')$size)} !important"),
        str_glue("{getOption('roboplot.font.caption')$family} !important")
      )
    )

  modebar_labpointer <-
    list(".js-plotly-plot .plotly [data-title]::before",
         "border-color",
          str_glue("transparent transparent {modebar_labcolor} !important"))

  plotly_position <-
    list(".plotly.html-widget",
         "position",
         "relative")


  css_list <-
    map(c(
      font_strings,
      list(rangeslider_mask_css),
      list(modebar_lab),
      list(modebar_labpointer),
      list(plotly_position)
    ), ~ .x)

  css_string <- roboplotr_get_css(css_list,
                                  file = if(!is.null(filepath)) { file.path(filepath,"css/style.css") } else NULL)

  if(is.null(filepath)) {
    list("css" = str_c('data:text/css;base64', base64Encode(css_string), sep=','),
         "js" =  str_c('data:application/javascript;base64', base64Encode(readBin(js_file, "raw", file.info(js_file)[1, "size"]), "txt"), sep=',')
    )
  }

}
