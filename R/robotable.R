#' @export
set_robotable_labels <-
  function(search = "Etsi:",
           info = "Näytetään rivit _START_-_END_ / _TOTAL_",
           lengthMenu = "Näytä _MENU_ riviä per sivu",
           first = "Ensimmäinen",
           last = "Viimeinen",
           decimal = ",",
           thousands = " ",
           `next` = ">>",
           previous = "<<") {
    list(
      search = search,
      info = info,
      lengthMenu = lengthMenu,
      thousands = thousands,
      decimal = decimal,
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
      '{formatC(round(as.numeric(num), rounding),big.mark = " ", decimal.mark = ",", format = "fg", flag = flag)}{unit}'
    )
  }

#' @importFrom dplyr across bind_cols cur_column mutate rename_with
roboplotr_robotable_cellformat <-
  function(d, rounding, flag, unit) {
    if (!is.null(names(unit))) {
      if (!all(names(d) %in% c(names(unit))) &
          !".default" %in% names(unit)) {
        unit <- c(unit, ".default" = "")
      }
      unitless_cols <-
        names(d) |> subset(!names(d) %in% names(unit))
      unit <-
        unit |> subset(!".default" == names(unit)) |> c(rep(unit[[".default"]], length(unitless_cols)) |> setNames(unitless_cols))
    }  else {
      unit <- rep(unit, length(names(d))) |> setNames(names(d))
    }

    if (!is.null(names(flag))) {
      if (!all(names(d) %in% c(names(flag))) &
          !".default" %in% names(flag)) {
        flag <- c(flag, ".default" = "")
      }
      flagless_cols <-
        names(d) |> subset(!names(d) %in% names(flag))
      flag <-
        flag |> subset(!".default" == names(flag)) |> c(rep(flag[[".default"]], length(flagless_cols)) |> setNames(flagless_cols))
    } else {
      flag <- rep(flag, length(names(d))) |> setNames(names(d))
    }

    order_cols <-
      d |> select(where(is.numeric)) |> rename_with( ~ str_c(".order_", .x))
    num_cols <- d |> select(where(is.numeric)) |> names()
    dt_orders <-
      (map_dbl(num_cols, ~ which(.x == names(d))) - 1) |> setNames(seq(to = order_cols |> length()) +
                                                                     length(d) - 1)

    d <- d |>
      mutate(across(
        where(is.numeric),
        ~ roboplotr_format_robotable_numeric(., rounding, flag[[cur_column()]], unit[[cur_column()]])
      )) |>
      bind_cols(order_cols)

    attr(d, "dt_orders") <- dt_orders

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

#' @importFrom purrr map
#' @importFrom stringr str_c str_glue
roboplotr_set_specific_css <-
  function(css_target = "css_target",
           ...,
           exit = NULL) {
    this_css <- list(...)
    css_specs <-
      map2(this_css, names(this_css), ~ str_glue("'{.y}': '{.x}'")) |>
      unlist() |>
      str_c(collapse = ",")
    split_css <- css_target |> str_split(" ") |> unlist()
    hover <- str_detect(first(split_css), "hover$")
    if (hover) {
      first_css <- first(split_css) |> str_remove(":hover$")
      rest_css <- split_css[-1] |> str_c(collapse = " ")
      old_css <- names(this_css) |> str_c(collapse = ",")
      exit <- map2(exit, names(exit), ~ str_glue("'{.y}': '{.x}'")) |>
        unlist() |>
        str_c(collapse = ",")
      str_glue(
        "
      $('{<first_css}').hover(
        function() { // Mouse enter
          let target = $(this).find('{<rest_css}');
          target.css({{<css_specs} });
        },
        function() { // Mouse leave
        let target = $(this).find('{<rest_css}');
        target.css({{<exit} });
        }
      );
    ",
        .open = "{<"
      )
    } else {
      str_glue("$('{<css_target}').css({{<css_specs}});", .open = "{<")
    }

  }

#' @importFrom stringr str_glue
roboplotr_set_robotable_css <-
  function(font = getOption("roboplot.font.main"),
           title_font = getOption("roboplot.font.title"),
           caption_font = getOption("roboplot.font.caption"),
           title = ""
           ) {
    JS(
      "function(settings, json) {",
      roboplotr_set_specific_css(
        '.dt-button > span > svg > path',
        'fill' = 'none',
        'transition' =  'opacity 0.3s ease-in-out width 0.3s ease-in-out'
      ),
      roboplotr_set_specific_css(
        '.dataTables_wrapper .dt-buttons',
        'position' = 'absolute',
        'top' = '0px',
        'right' = '0px',
        'z-index' = '2'
      ),
      if(str_length(title) == 0 & getOption("roboplot.shinyapp")$shinyapp == F) {
        roboplotr_set_specific_css('.dataTables_wrapper',
                                   'padding-top' = '18px')
      } else {
        ""
      },
      roboplotr_set_specific_css(
        ".dt-buttons .dt-button",
        "border" = "none",
        "height" = "18px",
        "background" = "none",
        "display" = "inline-block",
        "margin-right" = "2px",
        "padding-left" = " 0px",
        "padding-top" = "0px",
        "opacity" = "0",
        "width" = "18px",
        "transition" = "opacity 0.3s ease-in-out, width 0.3s ease-in-out"
      ),

      roboplotr_set_specific_css(
        '.dataTables_wrapper:hover .dt-buttons .dt-button',
        "border" = "none",
        "height" = "18px",
        "background" = "none",
        "margin-right" = "2px",
        "opacity" = "1",
        "width" = "18px",
        "transition" = "opacity 0.3s ease-in-out, width 0.3s ease-in-out",
        exit = c("opacity" = "0")
      ),
      roboplotr_set_specific_css(
        ".dataTables_wrapper:hover > .dt-buttons > .dt-button > span > svg > path",
        "fill" = "rgba(68, 68, 68, 0.3)",
        "transition" = "opacity 0.3s ease-in-out, width 0.3s ease-in-out",
        exit = c("fill" = "none")
      ),
      roboplotr_set_specific_css(
        ".dt-button:hover > span > svg > path",
        "fill" = "rgba(68, 68, 68, 0.7)",
        "transition" = "opacity 0.3s ease-in-out, width 0.3s ease-in-out",
        exit = c("fill" = "rgba(68, 68, 68, 0.3)")
      ),
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
      roboplotr_set_specific_css(".dataTables_info", "margin-right" = '5px', "padding-top" = "6px", "margin-bottom" = "6px"),
      roboplotr_set_specific_css(".dataTables_length", "margin-bottom" = '5px'),
      roboplotr_set_specific_css(".dataTables_filter input", "padding" = '4px 5px 5px 5px'),
      roboplotr_set_specific_css(".dataTables_paginate", "margin-top" = '-5px', 'margin-bottom' = '6px', "padding-top" = "0px"),
      "$('.roboplot_table-footer').css({",
      str_glue(
        "'background-color': '{getOption('roboplot.colors.background')}','font-size': '{caption_font$size}px', 'font-family': '{caption_font$family}', 'color': '{caption_font$color}', 'font-weight': 'normal'"
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
    fontsize <- font$size
    tbl |> formatStyle(
      ncols,
      color = font$color,
      backgroundColor = getOption("roboplot.colors.background"),
      fontSize = fontsize,
      fontFamily = font$family
    )
  }

#' @importFrom DT datatable tableFooter tableHeader
#' @importFrom htmltools HTML tags withTags
#' @importFrom stringr str_glue str_remove_all str_split str_width
#' @export
robotable <-
  function(d,
           title = "",
           subtitle = "",
           caption,
           rounding = 1,
           width = getOption("roboplot.width"),
           height = NULL,
           flag = "",
           unit = "") {

    d <- d |> roboplotr_robotable_cellformat(rounding, flag, unit)

    # d <- setNames(d, txt_hyphenate(names(d)))
    # joku tässä ei nyt täsmää leveyden määrittelyssä
    # col_widths <- map2(d, names(d), ~ {
    #   if (any(is.na(.x))) {
    #     # roboplotr:::roboplotr_alert(
    #     #   str_glue(
    #     #     "Some NA values exist in column {.y}! This might cause issues with fitting the column widths. You might want to consider replacing them with \" \" or NA-strings."
    #     #   )
    #     # )
    #   }
    #   if(str_detect(.y, "^\\.order")) { 0 } else {
    #     c(unlist(str_split(.y, " |\\&shy;|-")), unlist(str_split(
    #       str_remove_all(.x, "\\<[^\\<]*\\>"),
    #       "(?<![0-9]) (?![0-9])"
    #     ))) |> str_width() |> max()
    #   }
    # }) |> unlist()
    #
    # col_widths <- round(col_widths / sum(col_widths) * 100)

    # tämä skulaa osittain autowidthin kanssa, mutta ei nyt tee mitään,
    # autowidth pitää fiksata
    # width_defs <-
    #   lapply(names(col_widths), function(name, widths) {
    #     list(targets = which(names(widths) == name) - 1,
    #          # Convert 1-based R index to 0-based JavaScript index
    #          width = str_c(widths[[name]], "%"))
    #   }, widths = col_widths)

    order_defs <-
      map2(
        attributes(d)$dt_orders,
        names(attributes(d)$dt_orders),
        ~
          list(orderData = .y, targets = .x)
      ) |> unname() |>
      append(list(list(
        visible = F, targets = as.numeric(names(attributes(d)$dt_orders))
      )))

    # column_defs <- append(width_defs, order_defs)
    column_defs <- order_defs

    .footer <-
      str_glue(getOption("roboplot.caption.template"), text = caption)

    .bold <-
      ifelse(getOption("roboplot.font.title")$bold, tags$b, tags$span)

    sketch <-
      tagList(tags$table(
        tags$caption(tags$span(.bold(title)),tags$br(),tags$span(subtitle, class = "roboplot_table-subtitle"), class = "roboplot_table-title"),
        tags$thead(tags$tr(
        map(names(d), ~ HTML(.x)) |> map(tags$th) |> tagList()
      )),
      tags$tfoot(tags$tr(
        tags$th(style = "vertical-align: top",
          class = "roboplot_table-footer",
          colspan = names(d) |> stringr::str_subset("^.order", negate = T) |> length(),
          tags$span(.footer),
          robotable_logo()
        ),
      ))
      )
      )
    if (str_length(subtitle) > 0) {
      subtitle <-
        tags$caption(HTML(str_c("<br>", subtitle)), class = "roboplot_table-subtitle")
    }
    xportoptions <- (function() {
      the_cols <- seq(to = length(d)) - 1
      list(columns = subset(the_cols,!the_cols %in% as.numeric(names(
        attributes(d)$dt_orders
      ))))
    })()
    # names(d |> select(where(is.numeric))) # tässä alkua columnien uudelleennimeämiselle.. pitäisi valita numeric
    # columnit csv:tä muodostaessa, ja antaa lukumuotoiltujen sarakkeiden nimet näille
    d |>
      datatable(
        width = width,
        height = height,
        container = sketch,
        rownames = F,
        escape = F,
        extensions = "Buttons",
        options = list(
          # tämä pitää speksata TRUE, että column width manuaalinen määritys toimii,
          # mutta otsikkorivin asettelussa menee jotain pieleen
          # autoWidth = TRUE,
          columnDefs = column_defs,
          #tämä ei shinyn kanssa toimi oikein?
          # fillContainer = T,
          initComplete = roboplotr_set_robotable_css(title = title),
          language = set_robotable_labels(),
          dom = ifelse(nrow(d) > 10, "Btiprlf", "Bt"),
          buttons = list(
            list(
              filename = roboplotr_string2filename(title),
              extend = 'csv',
              text = fa("file-csv", height = "12pt"),
              exportOptions = xportoptions

            )
          )
        )
      ) |>
      roboplotr_set_robotable_fonts(seq(ncol(d)))


  }
