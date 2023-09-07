#' @export
set_robotable_labels <-
  function(search = "Etsi:",
           info = "Näytetään rivit _START_-_END_ / _TOTAL_",
           lengthMenu = "Näytä _MENU_ riviä per sivu",
           first = "Ensimmäinen",
           last = "Viimeinen",
           `next` = ">>",
           previous = "<<") {
    list(
      search = search,
      info = info,
      lengthMenu = lengthMenu,
      paginate = list(
        first = first,
        last = last,
        `next` = `next`,
        previous = previous
      )
    )
  }

#' @importFrom lubridate quarter year
#' @importFrom stringr str_glue
roboplotr_format_robotable_date <- function(date_col, dateformat) {
  if (dateformat == "%YQ%q")
  {
    str_glue(
      '<span data-order="{date_col}">{format(date_col, str_c(year(date_col), "Q", quarter(date_col)))}</span>'
    )
  }
  else {
    str_glue('<span data-order="{date_col}">{format(date_col, dateformat)}</span>')
  }
}

#' @importFrom stringr str_glue
roboplotr_format_robotable_numeric <-
  function(num,
           rounding,
           flag = "",
           unit = "") {
    str_glue(
      '<span data-order="{as.numeric(as_factor(num))}">{formatC(round(as.numeric(num), rounding),big.mark = " ", decimal.mark = ",", format = "fg", flag = flag)}{unit}</span>'
    )
  }

#' @importFrom dplyr across mutate
roboplotr_robotable_cellformat <-
  function(d, rounding, flag, unit) {

    if (!is.null(names(unit))) {
      if (!all(names(d) %in% c(names(unit))) &
          !".default" %in% names(unit)) {
        unit <- c(unit, ".default" = "")
      }
      unitless_cols <- names(d) |> subset(!names(d) %in% names(unit))
      unit <-
        unit |> subset(!".default" == names(unit)) |> c(rep(unit[[".default"]], length(unitless_cols)) |> setNames(unitless_cols))
    }  else { unit <- rep(unit, length(names(d))) |> setNames(names(d))}

    if (!is.null(names(flag))) {
      if (!all(names(d) %in% c(names(flag))) &
          !".default" %in% names(flag)) {
        flag <- c(flag, ".default" = "")
      }
      flagless_cols <- names(d) |> subset(!names(d) %in% names(flag))
      flag <-
        flag |> subset(!".default" == names(flag)) |> c(rep(flag[[".default"]], length(flagless_cols)) |> setNames(flagless_cols))
    } else { flag <- rep(flag, length(names(d))) |> setNames(names(d))}
    d <- d |>
        mutate(across(
          where(is.numeric),
          ~ roboplotr_format_robotable_numeric(., rounding, flag[[cur_column()]], unit[[cur_column()]])
        ))

    if ("Date" %in% unlist(map(d, class))) {
      dateformat <-
        roboplotr:::roboplotr_get_dateformat(d) |> roboplotr:::roboplotr_hovertemplate_freq()
      d <- d |>
        mutate(across(
          where(is.Date),
          ~ roboplotr_format_robotable_date(., dateformat)
        ))
    }

    d
  }

#' @importFrom stringr str_glue
roboplotr_set_robotable_nav <-
  function(font = getOption("roboplot.font.main"),
           title_font = getOption("roboplot.font.title"),
           caption_font = getOption("roboplot.font.caption")) {
    JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({",
      str_glue(
        "'background-color': '{getOption('roboplot.colors.background')}','font-size': '{font$size+2}px', 'font-family': '{font$family}', 'color': '{font$color}'"
      ),
      "});",
      "$('#DataTables_Table_0_length,.dataTables_filter,.dataTables_filter input').css({",
      str_glue(
        "'background-color': '{getOption('roboplot.colors.background')}','font-size': '{font$size}px', 'font-family': '{font$family}', 'color': '{font$color}'"
      ),
      "});",
      "$('.dataTables_info,#DataTables_Table_0_paginate,#DataTables_Table_0_length select,#DataTables_Table_0_length select option').css({",
      str_glue(
        "'background-color': '{getOption('roboplot.colors.background')}','font-size': '{font$size-1}px', 'font-family': '{font$family}', 'color': '{font$color}'"
      ),
      "});",
      "$('.dataTables_scrollFoot').css({",
      str_glue(
        "'background-color': '{getOption('roboplot.colors.background')}','font-size': '{caption_font$size}px', 'font-family': '{caption_font$family}', 'color': '{caption_font$color}'"
      ),
      "});",
      "$('.roboplot_table-title').css({",
      str_glue(
        "'background-color': '{getOption('roboplot.colors.background')}','font-size': '{title_font$size}px', 'font-family': '{title_font$family}', 'color': '{title_font$color}','margin-bottom': '-2px','text-align': 'left'"
      ),
      "});",
      "$('.roboplot_table-subtitle').css({",
      str_glue(
        "'background-color': '{getOption('roboplot.colors.background')}','font-size': '{round(0.75*title_font$size)}px', 'margin-top': '-{title_font$size-round(0.75*title_font$size)}px', 'font-family': '{title_font$family}', 'color': '{title_font$color}','text-align': 'left'"
      ),
      "});",
      "}"
    )
  }

#' @importFrom DT formatStyle
#' @importFrom stringr str_glue
roboplotr_set_robotable_fonts <-
  function(tbl, ncols, font = getOption("roboplot.font.main")) {
    tbl |> formatStyle(
      ncols,
      color = font$color,
      backgroundColor = getOption("roboplot.colors.background"),
      `font-size` = str_glue('{font$size}px'),
      `font-family` = font$family
    )
  }

#' @importFrom DT datatable tableFooter tableHeader
#' @importFrom htmltools HTML tags withTags
#' @importFrom stringr str_glue str_remove_all str_split str_width
robotable <-
  function(d,
           title = "",
           subtitle = "",
           caption,
           rounding = 1,
           flag = "",
           unit = "") {
    d <- d |> roboplotr_robotable_cellformat(rounding, flag, unit)

    # d <- setNames(d, txt_hyphenate(names(d)))
    # joku tässä ei nyt täsmää leveyden määrittelyssä
    col_widths <- map2(d, names(d), ~ {
      if (any(is.na(.x))) {
        roboplotr:::roboplotr_alert(
          str_glue(
            "Some NA values exist in column {.y}! This might cause issues with fitting the column widths. You might want to consider replacing them with \" \" or NA-strings."
          )
        )
      }
      c(unlist(str_split(.y, " |\\&shy;|-")), unlist(str_split(str_remove_all(.x, "\\<[^\\<]*\\>"),"(?<![0-9]) (?![0-9])"))) |> str_width() |> max()
    }) |> unlist()

    col_widths <- round(col_widths / sum(col_widths) * 100)
    column_defs <-
      lapply(names(col_widths), function(name, widths) {
        list(targets = which(names(widths) == name) - 1,
             # Convert 1-based R index to 0-based JavaScript index
             width = str_c(widths[[name]], "%"))
      }, widths = col_widths)
    .footer <-
      (function(text = caption) {
        str_glue(getOption("roboplot.caption.template"))
      })()
    sketch <- withTags(table(tableHeader(d, escape = F),
                             tableFooter(.footer)))
    if (str_length(subtitle) > 0) {
      subtitle <-
        tags$caption(HTML(str_c("<br>", subtitle)), class = "roboplot_table-subtitle")
    }
    .bold <-
      ifelse(getOption("roboplot.font.title")$bold, tags$b, tags$span)
    d |> datatable(
      # width = getOption("roboplot.width"),
      # height = getOption("roboplot.height"),
      container = sketch,
      rownames = F,
      escape = F,
      caption = tags$caption(.bold(title), subtitle, class = "roboplot_table-title"),
      options = list(
        autoWidth = TRUE,
        columnDefs = column_defs,
        initComplete = roboplotr_set_robotable_nav(),
        language = set_robotable_labels()
      )
    ) |>
      roboplotr_set_robotable_fonts(seq(ncol(d)))
  }

# roboplotr::set_roboplot_options(
#   font_title = roboplotr::set_font(font = "Arial", size = 22),
#   font_main = roboplotr::set_font(font = "Arial", size = 12)
# )
#
# #
# roboplotr::energiantuonti |>
#   # filter(month(time) == 1) |>
#   roboplotr:::robotable(subtitle = "asdf", caption = "Tilastokeskus")
#
# roboplotr::energiantuonti |>
#   # filter(month(time) == 1) |>
#   roboplotr::roboplot(Alue, pattern = Suunta)
