#' Set labels for robotable items like search box and pagination.
#' @param search Character. Label for search box.
#' @param info,lengthMenu,first,last,next,previous Character. Table navigation items.
#' @param emptyTable Character. The text shown when the table is empty.
#' @importFrom purrr walk
#' @export
set_robotable_labels <-
  function(search = "Etsi:",
           info = "N\u00e4ytet\u00e4\u00e4n rivit _START_-_END_ / _TOTAL_",
           lengthMenu = "N\u00e4yt\u00e4 _MENU_ rivi\u00e4 per sivu",
           emptyTable = "Tietoja ei saatavilla",
           first = "Ensimm\u00e4inen",
           last = "Viimeinen",
           `next` = ">>",
           previous = "<<") {
    separators <- unlist(str_split(getOption("roboplot.locale")$separators, ""))
    walk(
      list(search, lengthMenu, emptyTable, first, last, `next`, previous),
      ~ roboplotr_check_param(.x, "character", allow_null = F)
    )
    list(
      search = search,
      info = info,
      lengthMenu = lengthMenu,
      thousands = separators[2],
      decimal = separators[1],
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
      '<span data-order="{date_col}">{format(date_col, str_c(year(date_col), "Q", quarter(date_col))) |> replace_na(" ")}</span>'
    )
  }
  else {
    str_glue('<span data-order="{date_col}">{format(date_col, dateformat) |> replace_na(" ")}</span>')
  }
}

#' @importFrom dplyr if_else
#' @importFrom stringr str_glue
roboplotr_format_robotable_numeric <-
  function(num,
           rounding,
           flag = "",
           unit = "",
           na_value = ""
           ) {
    dplyr::if_else(is.na(num), na_value,
              str_glue(
                '{formatC(round(num, rounding), big.mark = " ", decimal.mark = ",", format = "fg", flag = flag)}{unit}'
              ))
  }

#' @importFrom dplyr across bind_cols bind_rows cur_column mutate rename_with where
roboplotr_robotable_cellformat <-
  function(d, rounding, flag, unit, na_value, dateformat) {
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
        ~ roboplotr_format_robotable_numeric(., rounding, flag[[cur_column()]], unit[[cur_column()]], na_value)
      )) |>
      bind_cols(order_cols)

    attr(d, "dt_orders") <- dt_orders

    if ("Date" %in% unlist(map(d, class))) {
      if(is.null(dateformat)) {
        dateformat <-
          roboplotr_get_dateformat(d) |> roboplotr_hovertemplate_freq()
      }
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
  function(id = "robotable-id",
           font = getOption("roboplot.font.main"),
           title_font = getOption("roboplot.font.title"),
           caption_font = getOption("roboplot.font.caption"),
           title = "") {
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
      if (str_length(title) == 0 &
          getOption("roboplot.shinyapp")$shinyapp == F) {
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
        str_glue(
          "#{id}_wrapper .dataTables_scrollHead, #{id}_wrapper .thead, #{id}_wrapper thead"
        ),
        'background-color' = getOption('roboplot.colors.background'),
        'font-size' = str_glue('{font$size+2}px'),
        'font-family' = font$family,
        'color' = font$color
      ),
      roboplotr_set_specific_css(
        str_glue('#{id}_length,#{id}_filter,#{id}_filter input'),
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
        "text-align" = "left",
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
        "font-weight" = "normal",
        "padding-top" = "2px"
      ),
      roboplotr_set_specific_css(str_glue("#{id}_filter label"),
                                 "font-weight" = "normal")
      ,
      collapse = "\n"
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

#' Automated datatable tables.
#'
#' Wrapper for [DT::datatable] for shorthand declaration of many layout arguments.
#' Automates many formatting options.
#'
#' @param d Data frame. Data to be created a table from.
#' @param title,subtitle Characters. Labels for plot elements. Optionally, use [set_title()] for the title if you want to omit the title from the displayed plot, but include it for any downloads through the modebar.
#' @param caption Function or character. Use [set_caption()].
#' @param height,width Double. Height and width of the table. Default width is NULL for responsive plots, give a value for static table dimensions.
#' @param flag,unit Character vectors. Use "+" for flag if you want to flag a numeric column for positive values. Unit is prepended to values in a numeric column. Name the units with column names from data 'd' if you want to target specific columns.
#' @param rounding Double. Controls the rounding of numeric columns.
#' @param dateformat Character. Controls how to format dates displayed on the table. Robotable tries to determine the proper format if left NULL.
#' @param na_value Character. How NA values are displayed.
#' @param pagelength Double. Controls how many rows are displayed on the table. If data 'd' contains more rows than this, [robotable()] automatically adds navigation.
#' @param info_text Character. Optional. If included, this text will be displayed with a popup when the info button in the table modebar is clicked.
#' @param heatmap Function. Use [set_heatmap()]. Displays any numeric values as a heatmap.
#' @param na_value Character. The displayed value of all NA values in the table.
#' @param artefacts Function. Use [set_artefacts()]. Controls artefact creation. Currently unable to make static files, only html files.
#' @return A list of classes "datatable" and "htmlwidget"
#' @importFrom DT datatable tableFooter tableHeader
#' @importFrom htmltools HTML tags withTags
#' @importFrom stringr str_glue str_remove_all str_split str_width
#' @export
#' @examples
#' # You can use roboplotr::robotable() to create html tables
#' #
#' energiantuonti |> robotable()
#' #
#' # No matter if you have any number date, character or numeric columns, robotable()
#' # handles the formatting automatically.
#' #
#' d <- energiantuonti |>
#'   dplyr::filter(Alue %in% c("Ruotsi","Kanada")) |>
#'   tidyr::unite(Tiedot, Alue, Suunta, sep = ", ") |>
#'   dplyr::arrange(Tiedot, time) |>
#'   tidyr::pivot_wider(names_from = Tiedot) |>
#'   dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) |>
#'   dplyr::arrange(time)
#
#' d |> robotable()
#' #
#' # You can use the pagelength parameter to control how many rows are shown at once,
#' # info_text to add any information you wish describe about the table, and
#' # heatmap parameter if you want to color-code any numeric values across the
#' # table. See [set_heatmap()] documentation for further examples.
#' #
#' d |>
#'  robotable(heatmap = set_heatmap(),
#'            pagelength = 50,
#'            info_text = "Testing the info button.")
#'
robotable <-
  function(d,
           title = NULL,
           subtitle = "",
           caption = NULL,
           rounding = 1,
           width = getOption("roboplot.width"),
           height = NULL,
           flag = "",
           unit = "",
           dateformat = NULL,
           pagelength = 10,
           info_text = NULL,
           heatmap = NULL,
           na_value = "",
           artefacts = getOption("roboplot.artefacts")$auto
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
      roboplotr_check_param(
        title,
        c("character,", "function"),
        NULL,
        f.name = list(fun = substitute(title)[1], check = "set_title")
      )
    }

    roboplotr_check_param(
      caption,
      c("character", "function"),
      size = 1,
      f.name = list(fun = substitute(caption)[1], check = "set_caption")
    )

    if (!is.null(caption)) {
      if (!is(substitute(caption)[1], "call")) {
        caption <- set_caption(text = caption)
      }
    } else {
      cpt <- attributes(d)$source
      if (length(cpt) == 1) {
        roboplotr_message("Using the attribute \"source\" for plot caption.")
        caption <- set_caption(text = unlist(cpt)[1])
      } else if (!is.null(cpt[[getOption("roboplot.locale")$locale]])) {
        roboplotr_message("Using the attribute \"source\" as plot caption.")
        caption <-
          set_caption(text = cpt[[getOption("roboplot.locale")$locale]][1])
      } else {
        roboplotr_alert("Missing the caption, using placeholder.")
        caption <- set_caption(text = "PLACEHOLDER")
      }
    }

    roboplotr_check_param(heatmap,
                          c("function"),
                          NULL,
                          f.name = list(fun = substitute(heatmap)[1], check = "set_heatmap"))

    d <- d |> roboplotr_robotable_cellformat(rounding, flag, unit, na_value, dateformat)

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

    center_defs <- (function() {
      dnames <- names(d)
      center_defs <- (which(subset(dnames, !str_detect(dnames, "^\\.")) %in% dnames)[-1])-1
      list(list(className = "dt-right", targets = center_defs))
    })()

    column_defs <- append(order_defs, center_defs)

    .footer <- caption

    .bold <-
      ifelse(getOption("roboplot.font.title")$bold, tags$b, tags$span)

    robotable_id <- str_c("robotable-", str_remove(runif(1), "\\."))

    if (!is.null(info_text)) {
      jsCode <-  str_glue(
        "$(document).ready(function() {
        $('#{<robotable_id}_infomodal-close').on('click', function() {
          $('#{<robotable_id}_infomodal').hide();
        });
      });
      ",
        .open = "{<"
      )
      main_font <- getOption("roboplot.font.main")
      title_font <- getOption("roboplot.font.title")
      m.specs <- getOption("roboplot.infobox")
      modal_html <- tags$div(
        tags$script(HTML(jsCode)),
        id = str_glue("{robotable_id}_infomodal"),
        style = "display: none; position: absolute; top: 20px; left: 5px; width: 50%;",
        tags$div(
          style = str_glue(
            "position: absolute;
               z-index: 9999;
               background-color: {m.specs$background};
               color: {m.specs$font};
               padding: 5px;
               border: {m.specs$border_width}px solid {m.specs$border};
               box-shadow: 0 4px 8px {m.specs$background};"
          ),
          tags$span(
            id = str_glue("{robotable_id}_infomodal-close"),
            fa(
              "times-circle",
              fill = m.specs$font,
              height = str_glue("{title_font$size}px")
            ),
            style = "top: 5px; right: 10px; font-size: 24px; cursor: pointer; float: right;"
          ),
          .bold(
            HTML(title$title),
            tags$br(),
            tags$span(HTML(subtitle), style = "font-size: 75%"),
            style = str_glue(
              "font-family: {title_font$family};font-size: {title_font$size}px;"
            )
          ),
          tags$span(
            style = str_glue(
              "font-family: {main_font$family}; font-size: {main_font$size}px;"
            ),
            tags$p(HTML(as.character(info_text))),
            tags$p(HTML(caption))

          )
        )
      )
    } else {
      modal_html <- NULL
    }

    if (str_length(subtitle) > 0) {
      subtitle <-
        tagList(tags$br(),
                tags$span(HTML(subtitle), id = str_glue("{robotable_id}_subtitle")))
    } else {
      subtitle <- NULL
    }

    sketch <-
      tagList(tags$div(
        class = "robotable-container",
        style = "position: relative",
        tags$table(
          tags$style(roboplotr_set_robotable_css(robotable_id)),
          id = robotable_id,
          modal_html,
          tags$span(
            id = str_glue("{robotable_id}_title"),
            if (title$include == T)
              tags$span(.bold(title$title))
            else {
              NULL
            },
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
              tags$span(HTML(.footer)),
              robotable_logo()
            ),
          ))
        )
      ))

    robotable_buttons <-
      roboplotr_robotable_modebar(d, robotable_id, title, info_text)

    getMagnitudes <- function(value) {
      # Initialize an empty vector to store magnitudes
      magnitudes <- numeric(0)

      # Find the largest magnitude less than the given value
      currentMagnitude <- 10^floor(log10(value))

      # Loop to add magnitudes from the largest to 10
      while(currentMagnitude >= 10) {
        magnitudes <- c(magnitudes, currentMagnitude)
        currentMagnitude <- currentMagnitude / 10
      }

      rev(magnitudes)
    }

    roboplotr_pagelength <- function(pagelength, rows) {
      hi_bound <- 10^(nchar(rows)-1)
      length_menu <- getMagnitudes(hi_bound)
      if(!is.null(pagelength)) {
        length_menu[[1]] <- pagelength
      }
      length_menu <- subset(length_menu, length_menu >= pagelength)
      length_menu <- list(c(length_menu, -1),c(as.character(length_menu), "Kaikki"))
      list(pagelength = length_menu[[1]][[1]], lengthmenu = length_menu)
    }
    .pagination <- roboplotr_pagelength(pagelength, nrow(d))

    dt <- d |>
      datatable(
        width = width,
        height = height,
        container = sketch,
        rownames = F,
        escape = F,
        extensions = "Buttons",
        options = list(
          columnDefs = column_defs,
          language = set_robotable_labels(),
          pageLength = .pagination$pagelength,
          lengthMenu = .pagination$lengthmenu,
          dom = ifelse(nrow(d) > pagelength, "Btiprlf", "Bt"),
          buttons = robotable_buttons
        )
      ) |>
      roboplotr_set_robotable_fonts(seq(ncol(d)))

    dt <- roboplotr_tbl_heatmap(d, dt, heatmap)

    if (is.logical(artefacts)) {
      roboplotr_check_param(artefacts, c("logical"))
      if (artefacts == TRUE) {
        params <- getOption("roboplot.artefacts")
        create_widget(
          p = dt,
          title = title$title,
          filepath = params$filepath,
          render = params$render,
          self_contained = params$self_contained,
          artefacts = "html"
        )
      } else {
        dt
      }
    } else {
      roboplotr_check_param(
        artefacts,
        c("function"),
        NULL,
        f.name = list(fun = substitute(artefacts)[1], check = "set_artefacts")
      )
      if(is.null(artefacts$title)) {
        .title <- title$title
      } else {
        .title <- artefacts$title
      }
      create_widget(
        p = dt,
        title = .title,
        filepath = artefacts$filepath,
        render = artefacts$render,
        self_contained = artefacts$self_contained,
        artefacts = "html"
      )
    }


  }
