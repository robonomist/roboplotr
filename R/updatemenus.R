# What follows is the documentation for the function below:

#' Set up an update menu for a plot
#' @importFrom rlang enquo
#' @export
#' @param buttons A column from param "d" of call to [roboplot()] that will be used to create the buttons in the update menu. If the column is not a factor, it will be converted to one.
#' @param selected The default selected button. Default is the first level of `buttons`.
#' @param position The position of the update menu. Default is "topleft". Options are "topleft", "topright", "bottomleft", "bottomright".
#' @param opacity The opacity of the update menu background. Default is 0.9.
#' @return A list of class `roboplotr.set_updatemenu` that can be passed to the `updatemenu` parameter of [roboplot()].
#' @examples
#' # For a long list of legend items, use `set_updatemenu()`.
#' energiantuonti |> roboplot(color = Suunta, updatemenu = set_updatemenu(Alue))
#' # Control position and default selection
#' energiantuonti |>
#'   roboplot(color = Suunta,
#'            updatemenu = set_updatemenu(Alue, selected = "Ruotsi", position = "topright"))
#' # `roboplot()` uses the `subtitle` parameter for hovertext unit labeling. If 
#' # your updatemenu refers various units, you need to pass the correct labeling 
#' # with `set_hovertext()`.
#' energiantuonti |>
#'   dplyr::filter(Alue %in% c("USA", "EU-maat", "Belgia", "Ruotsi")) |>
#'   dplyr::group_by(Alue, Suunta) |>
#'   dplyr::mutate(indeksi = value / mean(value[lubridate::year(time) == 2015]) * 100,
#'                 Suunta = tolower(Suunta)) |>
#'   dplyr::ungroup() |>
#'   tidyr::pivot_longer(c(value, indeksi), names_to = "Tiedot") |>
#'   dplyr::mutate(Tiedot = ifelse(Tiedot == "value", "Arvo", "Indeksi (2015 = 100)")) |>
#'   tidyr::drop_na() |>
#'   dplyr::mutate(hoverlabel = stringr::str_glue("{Alue}, {Suunta}\n{Tiedot}")) |>
#'   roboplot(
#'     Alue,
#'     "Energiantuonti",
#'     pattern = set_pattern(Suunta, sep = ": "),
#'     updatemenu = set_updatemenu(Tiedot, position = "topright"),
#'     hovertext = set_hovertext(unit = "", text_col = hoverlabel)
#'   )
set_updatemenu <- function(buttons, selected = NULL, position = "topleft", opacity = 0.9) {
  
  roboplotr_typecheck(selected, "character")
  roboplotr_typecheck(position, "character")
  roboplotr_typecheck(opacity, "numeric")
  roboplotr_is_between(opacity, "set_updatemenu()")
  roboplotr_valid_strings(position, c("topleft","topright","bottomleft","bottomright"), any)
  .res <- list(
    buttons = enquo(buttons),
    selected = selected,
    position = position
    )
  
  .res <- structure(.res, class = c("roboplotr", "roboplotr.set_updatemenu", class(.res)))
  
  .res
  
}


#' @importFrom farver decode_colour encode_colour
#' @importFrom forcats fct_inorder
#' @importFrom rlang as_label
roboplotr_set_updatemenu <- function(d, split_d, updatemenu) {
  
  if (!quo_is_null(updatemenu$buttons)) {
    if (!is.factor(d[[as_label(updatemenu$buttons)]])) {
      d[[as_label(updatemenu$buttons)]] <- fct_inorder(d[[as_label(updatemenu$buttons)]])
    }
    if(is.null(updatemenu$selected)) {
      selected <- levels(d[[as_label(updatemenu$buttons)]])[1]
    } else {
      selected <- updatemenu$selected
      roboplotr_valid_strings(selected, levels(d[[as_label(updatemenu$buttons)]]), any, "`set_updatemenu()` param `selected`")
      
    }
    
    labels <- levels(d[[as_label(updatemenu$buttons)]]) |> sort()
    
    btns <- map(labels, function(btn) {
      menu_index <- map_lgl(split_d, ~ unique(.x[["roboplot.update.menu"]]) == btn)
      list(method = "update",
           args = list(list(visible = menu_index)),
           label = btn)
    })
    
    xanchor <- str_extract(updatemenu$position, "left|right")
    yanchor <- str_extract(updatemenu$position, "top|bottom")
    menu <- list(list(
      type = "dropdown",
      buttons = btns,
      direction = "down",
      font = getOption("roboplot.font.main")[c("color","family","size")],
      bgcolor = getOption("roboplot.colors.background") |> decode_colour() |> encode_colour(alpha = updatemenu$opacity),
      active = which(selected == labels)-1,
      showactive = FALSE,
      xanchor = xanchor,
      yanchor = yanchor
    ))
    
    list(
      "menu" = menu,
      "selected" = selected
    )

  } else {
    NULL
  }
  
}


#' @importFrom rlang as_quosure
roboplotr_get_updatemenu <- function(updatemenu, d_names, externalmenu) {
  
  if(!is.null(externalmenu) & !quo_is_null(updatemenu)) {
    roboplotr_message("Using `roboplot(externalmenu)` overrides any `updatemenu` specifications.")
    return(list(buttons = as_quosure(NULL),
         selected = NULL,
         position = "topleft",
         opacity = 0.9
    ))
  }
  
  # updatemenu <- enquo(updatemenu)
  if (quo_is_call(updatemenu)) {
    updatemenu <- eval_tidy(updatemenu)
    roboplotr_typecheck(updatemenu, "set_updatemenu")
    buttons <- updatemenu$buttons
    roboplotr_check_valid_var(buttons, d_names, where = "`set_updatemenu()`")
    rm(buttons)
  } else {
    roboplotr_check_valid_var(updatemenu, d_names, where = "`roboplot()`")
    updatemenu <- list(buttons = updatemenu,
                       selected = NULL,
                       position = "topleft",
                       opacity = 0.9
                       )
  }
  updatemenu
}
  

