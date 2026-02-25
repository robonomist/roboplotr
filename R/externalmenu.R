#' Set up an external menu for filtering plot data based on a categorical variable.
#' This function creates a configuration for an external menu that can be used to filter plot data based on a categorical variable. The menu will be rendered as a draggable overlay on the plot, with checkboxes for each unique value of the specified variable. The menu can be toggled from the modebar.
#' @param col Symbol or character. Column from `d` of `roboplot()` that contains the categorical variable for filtering. This column will be used to create the checkboxes in the menu.
#' @param title Logical. Whether to include a title in the menu. Default is TRUE.
#' @param selected Character vector. Initial selection of items in the menu. If NULL (default), all items are selected initially. If `max_items` is set, the initial selection will be trimmed to the first `max_items` values found in the data.
#' @param max_items Numeric. Maximum number of items that can be selected at once. If NULL (default), there is no limit.
#' @param box A list of styling options for the menu box. Use `set_infobox()` to create this list.
#' @param btn A list of styling options for the box selector. Use ` set_infobox()` to create this list.
#' @examples
#' 
#' # You might want to make items selectable outside of the legend selection. Use
#' # the parameter `externalmenu` with `set_externalmenu()` to provide the column 
#' # that controls visiblity of items in the plot available from the `roboplot` 
#' # modebar. See more examples in the `set_externalmenu()` documentation.
#' 
#' energiantuonti |>
#'   dplyr::filter(time == max(time)) |>
#'   roboplot(Suunta, plot_type = "bar", plot_mode = "horizontal",
#'            plot_axes = set_axes(x = "value", y = "Alue"),
#'            externalmenu = set_externalmenu(Alue)
#'   )
#' 
#' 
#' # You might want to make items selectable outside of the legend selection. Use
#' # the parameter `externalmenu` with `set_externalmenu()` to provide the column 
#' # that controls visiblity of items in the plot. This way you can construct, 
#' # for example, a horizontal bar plot where you have the legend controlling
#' # the color variable, but still filter the items in the y-axis through the 
#' # external menu, available from the `roboplot` modebar.
#' 
#' energiantuonti |>
#'   dplyr::filter(time == max(time)) |>
#'   roboplot(Suunta, plot_type = "bar", plot_mode = "horizontal",
#'            plot_axes = set_axes(x = "value", y = "Alue"),
#'            externalmenu = set_externalmenu(Alue)
#'            )
#' 
#' # You can combine external menu with pattern to have the pattern variable 
#' # controlled by the external menu.
#' 
#' energiantuonti |>
#'   roboplot(Alue,
#'            pattern = Suunta,
#'            externalmenu = set_externalmenu(Suunta)
#'   )
#' 
#' # You could also use the external menu to control the visibility of items in the 
#' # legend to save space on the plot itself. This is not as obvious to user and 
#' # the external menu won't have the legend colors available, but can be useful 
#' # in some situations.
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(Alue, 
#'            legend = set_legend(position = "none"),
#'            externalmenu = set_externalmenu(Alue)
#'   )
#' 
#' # Any arbitary column works, but the results in the plot are up to the user.
#' 
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Vienti") |>
#'   dplyr::mutate(menu = purrr::map_chr(Alue, ~ sample(c("a","b","c","d"),1))) |>
#'   roboplot(Alue,
#'            externalmenu = set_externalmenu(menu)
#'   )
#' 
#' # You can even filter for the dates, but remember that the external menu is 
#' # not meant for filtering the data, but rather for controlling the visibility 
#' # of items in the plot. If you want to filter the data, it's better to do it 
#' # before plotting, or use something more appropriate like a rangeslider.
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Vienti") |>
#'   roboplot(Alue,
#'            externalmenu = set_externalmenu(time),
#'            rangeslider = T
#'   )
#' 
#' # Or at least construct a reasonable filter for an appropriate plot.
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Vienti", Alue %in% c("Venäjä", "Kanada")) |>
#'   dplyr::mutate(Vuosi = lubridate::year(time)) |>
#'   roboplot(Alue,
#'            plot_type = "bar",
#'            externalmenu = set_externalmenu(Vuosi)
#'   )
#' 
#' # Or some other arbitary grouping
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Vienti") |>
#'   dplyr::mutate(Suunta = ifelse(Alue %in% c("Venäjä","Viro","Latvia"), "Itä", "Länsi")) |>
#'   roboplot(Alue, 
#'            externalmenu = set_externalmenu(Suunta))
#' 
#' # You can control the appearance of the externalmenu somewhat by using 
#' # `set_infobox()` with set_externalmenu(box, btn), or omit the title with 
#' # `set_externalmenu(title = FALSE)`.
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Tuonti") |>
#'   roboplot(
#'     Alue,
#'     legend = set_legend(position = "none"),
#'     externalmenu = set_externalmenu(
#'       Alue,
#'       title = F,
#'       box = set_infobox(background = "orange"),
#'       btn = set_infobox(
#'         border = "brown",
#'         border_width = 4,
#'         background = "red"
#'       )
#'     )
#'   )
#' 
#' # Technically, nothing prevents you from using the external menu along with 
#' # legend, but it might be quite confusing for the user.
#' 
#' energiantuonti |>
#'   dplyr::filter(Suunta == "Vienti") |>
#'   roboplot(Alue,
#'            externalmenu = set_externalmenu(Alue)
#'   )
#' 
#' @export
set_externalmenu <- function(
    col = NULL,
    title = TRUE,
    box = set_infobox(background = getOption("roboplot.colors.background")),
    btn = set_infobox(),
    max_items = NULL,
    selected = NULL
) {
  
  roboplotr_typecheck(title, "logical", allow_null = F)
  roboplotr_typecheck(max_items, "numeric", allow_null = T)
  roboplotr_typecheck(selected, size = NULL, "character", allow_null = T)
  col <- enquo(col)
  if (quo_is_null(col)) return(NULL)
  
  item_font <- set_font(color = box$font)
  btn_font <- set_font(font = getOption("roboplot.font.title")$.font, color = btn$font)
  
  box$font <- item_font
  btn$font <- btn_font

  checkmark_size = 1
  checkmark_color = getOption("roboplot.colors.traces")[1]
  
  .res <- list(
    id = paste0("roboplot-externalmenu-", gsub("\\.", "", as.character(runif(1)))),
    col = col,
    title = title,
    box = box,
    btn = btn,
    selected = selected,
    `max-items`= max_items,
    checkmark = list(size = checkmark_size, color = checkmark_color)
  )
  
  .res <- structure(.res, class = c("roboplotr", "roboplotr.set_externalmenu", class(.res)))
  
  .res
  
}

#' @importFrom rlang quo_get_expr
roboplotr_validate_externalmenu <- function(externalmenu, d_names) {

  if(quo_is_null(externalmenu)) return(NULL)
  
  if (!quo_is_symbolic(externalmenu)) {
    externalmenu <- sym(rlang::quo_get_expr(externalmenu))
  }
  if(as_label(externalmenu) %in% d_names) {
    externalmenu <- set_externalmenu(!!externalmenu)
  } else {
    tryCatch({
      externalmenu <- eval_tidy(externalmenu)
    }, error = function(e) {
      stop("The `externalmenu` argument must be a column name from the data or a call to `roboplot::set_externalmenu()`.", call. = F)
    })
  }
  roboplotr_typecheck(externalmenu, "set_externalmenu")
  externalmenu
  
}

roboplotr_set_external_menu <- function(p, externalmenu, d) {
  
  if (is.null(externalmenu)) return(p)

  roboplotr_typecheck(externalmenu, "set_externalmenu")
  col <- externalmenu$col
  roboplotr_check_valid_var(col, names(d), where = "set_externalmenu")

  externalmenu$col <- as_label(externalmenu$col)
  
  roboplotr_valid_strings(externalmenu$selected, unique(d[[externalmenu$col]]), .fun = any, msg = "`set_externalmenu(selected)`")
  externalmenu$`max-items` <- round(min(c(length(unique(d[[externalmenu$col]])),externalmenu$`max-items`)))
  if(externalmenu$`max-items` == 0) {
    externalmenu$`max-items` <- NULL
  }
  
  externalmenu$close <- fa(
    "times-circle",
    fill = externalmenu$box$font$color,
    height = str_c(externalmenu$btn$font$size+2,"px")
  )
  externalmenu$grip <- fa(
    "grip",
    fill = externalmenu$box$font$color,
    height = str_c(round(externalmenu$btn$font$size*1.5),"px")
  )
  
  externalmenu$title_select <- getOption("roboplot.locale")$externalmenu$select
  externalmenu$title_deselect <- getOption("roboplot.locale")$externalmenu$deselect
  externalmenu$limit_reached <- getOption("roboplot.locale")$externalmenu$limit_reached
  externalmenu$selected_label <- getOption("roboplot.locale")$externalmenu$selected
  externalmenu$categoryorder <- (function() {if (is.factor(d[[externalmenu$col]])) levels(d[[externalmenu$col]]) else NULL})()
  
  
  p$x$roboplot_externalmenu <- externalmenu
  
  p |> onRender(
    "function(el, x) {setExternalMenu(el, x)}"
  )
  
  
}


roboplotr_modebar_externalmenu_button <- function(btn_list, externalmenu) {
  if (is.null(externalmenu) || is.null(externalmenu$id)) return(btn_list)
  
  btn <- list("externalmenu" = list(
    name = getOption("roboplot.locale")$modebar_label$filter,
    icon = roboplotr_modebar_dl_icon("filter"),
    click = JS(str_glue(
      "function(gd) {
         var el = document.getElementById('<{externalmenu$id}');
         if (!el) return;
         el.style.display = (el.style.display === 'none' || el.style.display === '') ? 'block' : 'none';
       }", .open = "<{"
    ))
  ))
  
  append_location <- ifelse("robonomist" %in% names(btn_list), length(btn_list) - 1, length(btn_list))
  append(btn_list, btn, after = append_location)
}


roboplotr_prefilter_externalmenu <- function(pb, externalmenu) {
  
  if(is.null(externalmenu)) { return (pb) }
  
  if(is.null(externalmenu$selected)) { return(pb) }

  sel <- as.character(externalmenu$selected)
  
  for (i in seq_along(pb$x$data)) {
    tr <- pb$x$data[[i]]
    
    # must have per-point customdata for your filter; skip traces without it
    cd <- tr$customdata
    if (is.null(cd)) next
    
    # --- stash full arrays so JS can restore later ---
    tr$meta <- tr$meta %||% list()
    tr$meta$extmenu_full <- list(
      x = tr$x,
      y = tr$y,
      text = tr$text,
      hovertext = tr$hovertext,
      customdata = tr$customdata
    )
    
    # --- compute keep index (supports scalar customdata or list-of-vectors) ---
    keep <- vapply(cd, function(v) {
      if (is.null(v)) return(FALSE)
      if (is.list(v) || length(v) > 1 && !is.character(v) && !is.numeric(v)) {
        # rare; ignore
        return(FALSE)
      }
      # handle list element being a vector (multi-category point)
      vv <- as.character(unlist(v))
      any(vv %in% sel)
    }, logical(1))
    
    # if cd is not a list (e.g. atomic vector), vapply above won't work; handle:
    if (!is.list(cd)) {
      keep <- as.character(cd) %in% sel
    }
    
    # --- subset arrays (only if lengths match / exist) ---
    if (!is.null(tr$x)) tr$x <- tr$x[keep]
    if (!is.null(tr$y)) tr$y <- tr$y[keep]
    if (!is.null(tr$text) && length(tr$text) == length(keep)) tr$text <- tr$text[keep]
    if (!is.null(tr$hovertext) && length(tr$hovertext) == length(keep)) tr$hovertext <- tr$hovertext[keep]
    tr$customdata <- if (is.list(cd)) cd[keep] else cd[keep]
    
    pb$x$data[[i]] <- tr
  }
  
  pb
}
