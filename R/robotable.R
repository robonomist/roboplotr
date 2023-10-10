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
      map2(this_css, names(this_css), ~ str_glue("\n{.y}: {.x}")) |>
      # map2(this_css, names(this_css), ~ str_glue("'{.y}': '{.x}'")) |>
      unlist() |>
      str_c(collapse = ";")
    str_glue("{<css_target} {{<css_specs}}", .open = "{<")
  }

#' @importFrom stringr str_glue
roboplotr_set_robotable_css <-
  function(
    id = "robotable-id",
    font = getOption("roboplot.font.main"),
    title_font = getOption("roboplot.font.title"),
    caption_font = getOption("roboplot.font.caption"),
    title = ""
  ) {
    str_c(
      roboplotr_set_specific_css(
        str_glue('#{id}_wrapper .dt-buttons .dt-button span svg path'),
        'fill' = 'none',
        'transition' =  'opacity 0.3s ease-in-out width 0.3s ease-in-out'
      ),
      roboplotr_set_specific_css(
        str_glue('#{id}_wrapper .dt-buttons'),
        'position' = 'absolute',
        'top' = '0px',
        'right' = '0px',
        'z-index' = '2'
      ),
      if(str_length(title) == 0 & getOption("roboplot.shinyapp")$shinyapp == F) {
        roboplotr_set_specific_css(str_glue('#{id}_wrapper'),
                                   'padding-top' = '18px')
      } else {
        ""
      },
      roboplotr_set_specific_css(
        str_glue("#{id}_wrapper .dt-buttons .dt-button"),
        "border" = "none",
        "height" = "18px",
        "background" = "none",
        "display" = "inline-block",
        "margin-right" = "2px",
        "color" = getOption("roboplot.colors.background"),
        "padding-left" = " 0px",
        "padding-top" = "0px",
        "opacity" = "0",
        "width" = "18px",
        "transition" = "opacity 0.3s ease-in-out, width 0.3s ease-in-out"
      ),

      roboplotr_set_specific_css(
        str_glue("#{id}_wrapper:hover .dt-buttons .dt-button"),
        "border" = "none",
        "height" = "18px",
        "background" = "none",
        "margin-right" = "2px",
        "opacity" = "1",
        "width" = "18px",
        "transition" = "opacity 0.3s ease-in-out, width 0.3s ease-in-out"
      ),
      roboplotr_set_specific_css(
        str_glue("#{id}_wrapper:hover .dt-buttons .dt-button span svg path"),
        "fill" = "rgba(68, 68, 68, 0.3)",
        "transition" = "opacity 0.3s ease-in-out, width 0.3s ease-in-out"
      ),
      roboplotr_set_specific_css(
        str_glue("#{id}_wrapper .dt-buttons .dt-button:hover span svg path"),
        "fill" = "rgba(68, 68, 68, 0.7)",
        "transition" = "opacity 0.3s ease-in-out, width 0.3s ease-in-out"
      ),
      roboplotr_set_specific_css(
        str_glue("#{id}_wrapper .dataTables_scrollHead, #{id}_wrapper .thead"),
        'background-color' = getOption('roboplot.colors.background'),
        'font-size' = str_glue('{font$size+2}px'),
        'font-family' = font$family,
        'color' = font$color
      ),
      roboplotr_set_specific_css(
        str_glue(
          '#{id}_length,#{id}_filter,#{id}_filter input'
        ),
        'background-color' = getOption('roboplot.colors.background'),
        'font-size' = str_glue('{font$size+2}px'),
        'font-family' = font$family,
        'color' = font$color
      ),
      roboplotr_set_specific_css(
        str_glue("#{id}_info"),
        "margin-right" = '5px',
        "padding-top" = "6px",
        "margin-bottom" = "6px"
      ),
      roboplotr_set_specific_css(
        str_glue(
          '#{id}_info,#{id}_length label,#{id}_paginate,#{id}_length select,#{id}_length select option,#{id}_filter,#{id}_filter label'
        ),
        'background-color' = getOption('roboplot.colors.background'),
        'font-size' = str_glue('{font$size-1}px'),
        'font-family' = font$family,
        'color' = font$color
      ),
      roboplotr_set_specific_css(str_glue("#{id}_length"), "margin-bottom" = '5px'),
      roboplotr_set_specific_css(str_glue("#{id}_filter"), "margin-top" = '-4px'),
      roboplotr_set_specific_css(str_glue("#{id}_filter input"), "padding" = '4px 5px 5px 5px'),
      roboplotr_set_specific_css(
        str_glue("#{id}_paginate"),
        "margin-top" = '-5px',
        'margin-bottom' = '6px',
        "padding-top" = "0px"
      ),
      roboplotr_set_specific_css(
        str_glue("#{id}_footer"),
        "vertical-align" = "top",
        "background-color" = getOption('roboplot.colors.background'),
        'font-size' = str_glue('{caption_font$size}px'),
        'font-family' = caption_font$family,
        'color' = caption_font$color,
        'font-weight' = 'normal'
      ),
      roboplotr_set_specific_css(
        str_glue("#{id}_title"),
        "background-color" = getOption('roboplot.colors.background'),
        'font-size' = str_glue('{title_font$size}px'),
        'font-family' = title_font$family,
        'color' = title_font$color,
        'margin-bottom' = "-2px",
        "text-align" = "left"
      ),
      roboplotr_set_specific_css(
        str_glue("#{id}_subtitle"),
        "background-color" = getOption('roboplot.colors.background'),
        'font-size' = str_glue('{round(0.75*title_font$size)}px'),
        'font-family' = title_font$family,
        'color' = title_font$color,
        'font-weight' = 'normal',
        'margin-top' = str_glue('-{title_font$size-round(0.75*title_font$size)}px'),
        "text-align" = "left"
      ),
      roboplotr_set_specific_css(
        str_glue("#{id}_length label"),
        "font-weight" = "normal", "padding-top" = "2px"
      ),
      roboplotr_set_specific_css(
        str_glue("#{id}_filter label"),
        "font-weight" = "normal"
      )
      ,
      collapse = "\n")
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
           title = NULL,
           subtitle = "",
           caption,
           rounding = 1,
           width = getOption("roboplot.width"),
           height = NULL,
           flag = "",
           unit = "",
           info_text = NULL
           ) {

    if (is.null(title)) {
      title <- attributes(d)[c("title", "robonomist_title")]
      if (!is.null(title$robonomist_title)) {
        roboplotr_message("Using the attribute \"robonomist_title\" for plot title.")
        title <- set_title(title$robonomist_title)
      } else if (!is.null(title$title) & length(title$title != 1)) {
        roboplotr_alert("Using the attribute \"title\" as plot title.")
        title <- set_title(title$title)
      } else {
        roboplotr_alert("Missing the title, using placeholder.")
        title <- set_title("PLACEHOLDER")
      }
    } else if (is.character(title)) {
      title <- set_title(title = title, include = T)
    } else {
      roboplotr_check_param(title, c("character,","function"), NULL,  f.name = list(fun = substitute(title)[1], check = "set_title"))
    }

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

    robotable_id <- str_c("robotable-",str_remove(runif(1),"\\."))

    if(!is.null(info_text)) {
      main_font <- getOption("roboplot.font.main")
      title_font <- getOption("roboplot.font.title")
      modal_html <- tags$div(
        id = str_glue("{robotable_id}_infomodal"),
        style = "display: none; position: absolute; top: 20px; left: 5px; width: 50%;",
        tags$div(style = str_glue(
                   "position: absolute;
               z-index: 9999;
               background-color: {getOption('roboplot.trace.border')$color};
               padding: 5px;
               border: 1px solid {getOption('roboplot.border')[c('xcolor','ycolor')] |> unique() |> unlist() |> first()};
               box-shadow: 0 4px 8px {getOption('roboplot.trace.border')$color};"),
                 .bold(title$title,tags$br(),tags$span(subtitle, style = "font-size: 75%"),
                       style = str_glue("font-family: {title_font$family};font-size: {title_font$size}px; color: {main_font$color};")
                       ),
                 tags$span(
                   style = str_glue("font-family: {main_font$family}; font-size: {main_font$size}px; color: {main_font$color};"),
                   tags$p(info_text),
                   tags$p(str_glue(getOption("roboplot.caption.template"), text = caption))

                 )
                 )
      )
    } else {
      modal_html <- NULL
    }

    if (str_length(subtitle) > 0) {
      subtitle <-
        tagList(tags$br(),
                tags$span(subtitle, id = str_glue("{robotable_id}_subtitle")))
    } else {
      subtitle <- NULL
    }

    sketch <-
      tagList(
        tags$div(class = "robotable-container", style = "position: relative",
          tags$table(
            tags$style(roboplotr_set_robotable_css(robotable_id)),
            id = robotable_id,
            modal_html,
            tags$caption(
              id = str_glue("{robotable_id}_title"),
              if(title$include == T) tags$span(.bold(title$title)) else {NULL},
              subtitle
            ),
            tags$thead(tags$tr(
              map(names(d), ~ HTML(.x)) |> map(tags$th) |> tagList()
            )),
            tags$tfoot(tags$tr(
              tags$th(
                #style = "vertical-align: top",
                id = str_glue("{robotable_id}_footer"),
                colspan = names(d) |> stringr::str_subset("^.order", negate = T) |> length(),
                tags$span(.footer),
                robotable_logo()
              ),
            ))
          )
        ))

    xportoptions <- (function() {
      the_cols <- seq(to = length(d)) - 1
      list(columns = subset(the_cols,!the_cols %in% as.numeric(names(
        attributes(d)$dt_orders
      ))))
    })()
    # names(d |> select(where(is.numeric))) # tässä alkua columnien uudelleennimeämiselle.. pitäisi valita numeric
    # columnit csv:tä muodostaessa, ja antaa lukumuotoiltujen sarakkeiden nimet näille


    modebar_buttons <- list(
      "csv" = list(
        filename = roboplotr_string2filename(title$title),
        extend = 'csv',
        text = fa("file-csv", height = "12pt"),
        exportOptions = xportoptions

      ),
    "info" = list(
      extend = "collection",
      text = fa("circle-info", height = "12pt"),
      # action = JS(str_glue("function(e, dt, node, config) {alert('{<info_text}');}", .open = "{<"))
      action = JS(str_glue('function(e, dt, node, config) {$("#{<robotable_id}_infomodal").toggle();}', .open = "{<"))
    ),
    "robonomist" = list(
      extend = "collection",
      text = '<svg version="1.1" viewBox="0 0 71.447 32" width = "12pt" xmlns="http://www.w3.org/2000/svg"><path transform="scale(.31159)"  d="M 229.3 53.2 L 174.3 90.1 L 174.3 69.1 L 199.5 53.2 L 174.3 37.3 L 174.3 16.3 M112 0c14.2 0 23.3 1.8 30.7 7 6.3 4.4 10.3 10.8 10.3 20.5 0 11.3-6.4 22.8-22.3 26.5l18.4 32.5c5 8.7 7.7 9.7 12.5 9.7v6.5h-27.3l-23.7-45.8h-7v27.6c0 10.5 0.7 11.7 9.9 11.7v6.5h-43.2v-6.7c10.3 0 11.3-1.6 11.3-11.9v-65.7c0-10.2-1-11.7-11.3-11.7v-6.7zm-4.8 7.9c-3.3 0-3.6 1.5-3.6 8.6v32.3h6.4c15.8 0 20.2-8.7 20.2-21.3 0-6.3-1.7-11.5-5-15-2.9-3-7-4.6-13-4.6z M 0 53.2 L 55 16.3 L 55 37.3 L 29.8 53.2 L 55 69.1 L 55 90.1"/></svg>',
      action = JS("function(e, dt, node, config) {window.open(\"https://robonomist.com\")  }")
    ))

    modebar_buttons <- modebar_buttons[c(
      "csv",
      ifelse(!is.null(info_text),"info",""),
      ifelse(str_detect(getOption("roboplot.logo"),"robonomist"),"","robonomist"))] |>
      roboplotr_compact() |>
      unname()

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
          # initComplete = roboplotr_set_robotable_css(title = title),
          language = set_robotable_labels(),
          dom = ifelse(nrow(d) > 10, "Btiprlf", "Bt"),
          buttons = modebar_buttons
        )
      ) |>
      roboplotr_set_robotable_fonts(seq(ncol(d)))


  }
