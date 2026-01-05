#' Table label configuration
#'
#' Parameters to add and customize labeling in [robotables][robotable()].
#' @param search Character. Label for search box.
#' @param info,lengthMenu,first,last,next,previous Character. Table navigation items.
#' @param emptyTable Character. The text shown when the table is empty.
#' @export
set_robotable_labels <-
  function(search = getOption("roboplot.locale")$robotable_labels$search,
           info = getOption("roboplot.locale")$robotable_labels$info,
           lengthMenu = getOption("roboplot.locale")$robotable_labels$lengthMenu,
           emptyTable = getOption("roboplot.locale")$robotable_labels$emptyTable,
           first = getOption("roboplot.locale")$robotable_labels$first,
           last = getOption("roboplot.locale")$robotable_labels$last,
           `next` = ">>",
           previous = "<<") {

    arggs <- as.list(environment())
    for (.name in names(arggs)) {
      Parameter <- arggs[[.name]]
      roboplotr_typecheck(Parameter, "character", allow_null = F,extra = str_glue("`{.name}` in set_robotable_labels()"))
      if(length(arggs[[.name]]) > 0) {
        assign(.name, as(arggs[[.name]], "character"))
      }
    }

    separators <- unlist(str_split(getOption("roboplot.locale")$separators, ""))

    .res <- list(
      search = arggs$search,
      info = arggs$info,
      lengthMenu = arggs$lengthMenu,
      emptyTable = arggs$emptyTable,
      thousands = separators[2],
      decimal = separators[1],
      paginate = list(
        first = arggs$first,
        last = arggs$last,
        `next` = arggs$`next`,
        previous = arggs$previous
      )
    )

    .res <- structure(.res, class = c("roboplotr", "roboplotr.set_robotable_labels", class(.res)))

    .res
  }

#' @importFrom lubridate quarter year
#' @importFrom stringr str_glue
roboplotr_format_robotable_date <- function(date_col, dateformat) {

  dateformat <- str_replace_all(dateformat, "%-","%")

  if(str_detect(dateformat, "%q")) {
    vec1 <- quarter(date_col) |> as.character()
    vec2 <- format(date_col, dateformat)
    pattern <- "%q"
    res <- map2_chr(vec1, vec2, ~ str_replace_all(.y, "%q", .x))
  } else {
    res <- format(date_col, dateformat)
  }

  res |> replace_na(" ")
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
    if_else(is.na(num), na_value,
              str_glue(
                '{formatC(round(num, rounding), big.mark = " ", decimal.mark = ",", format = "f", digits = rounding, flag = flag)}{unit}'
              ))
  }

#' @importFrom dplyr across bind_cols bind_rows cur_column mutate rename_with where
roboplotr_robotable_cellformat <-
  function(d, rounding, flag, unit, na_value, dateformat) {

    # Check if the rounding, flag, and unit parameters are valid
    if (!is.null(names(unit))) {
      if (!all(names(d) %in% c(names(unit))) &
          !".default" %in% names(unit)) {
        unit <- c(unit, ".default" = "")
      }
      unitless_cols <-
        names(d) |> subset(!names(d) %in% names(unit))
      unit <-
        unit |> subset(!".default" == names(unit)) |> c(rep(subset(unit, names(unit) == ".default"), length(unitless_cols)) |> setNames(unitless_cols))
    }  else {
      unit <- rep(unit, length(names(d))) |> setNames(names(d))
    }

    if (!is.null(names(rounding))) {
      roboplotr_typecheck(rounding, "numeric",size = NULL)
      .num_cols <- d |> select(where(is.numeric)) |> names()
      rounding <- setNames(as.numeric(rounding), names(rounding))
      if (!all(.num_cols %in% names(rounding)) &
          !".default" %in% names(rounding)) {
        roboplotr_message(str_glue("Some numeric columns specified in robotable() data `d` were not provided `rounding`! robotable() used {round(getOption('roboplot.rounding'))} (reset the global default with `set_roboplot_options(rounding)`) for those columns. Use \".default\" to set a default value for all numeric columns not specified in `rounding`."))
        rounding <- c(rounding, ".default" = round(getOption("roboplot.rounding")))
      }
      roundless_cols <-
        .num_cols |> subset(!.num_cols %in% names(rounding))
      rounding <-
        rounding |> subset(!".default" == names(rounding)) |> c(rep(subset(rounding, names(rounding) == ".default"), length(roundless_cols)) |> setNames(roundless_cols))
    }  else {
      roboplotr_typecheck(rounding, "numeric", 1)
      .num_cols <- d |> select(where(is.numeric)) |> names()
      rounding <- rep(as.numeric(rounding),length(.num_cols)) |> setNames(.num_cols)
    }

    if (!is.null(names(flag))) {
      if (!all(names(d) %in% c(names(flag))) &
          !".default" %in% names(flag)) {
        flag <- c(flag, ".default" = "")
      }
      flagless_cols <-
        names(d) |> subset(!names(d) %in% names(flag))
      flag <-
        flag |> subset(!".default" == names(flag)) |> c(rep(subset(flag, names(flag) == ".default"), length(flagless_cols)) |> setNames(flagless_cols))
    } else {
      flag <- rep(flag, length(names(d))) |> setNames(names(d))
    }

    if(d |> select(where( ~ is.numeric(.x) | is.Date(.x) | is.factor(.x))) |> ncol() > 0) {

      order_cols <-
        d |> select(where( ~ is.numeric(.x) | is.Date(.x) | is.factor(.x))) |> rename_with( ~ str_c(".order_", .x)) |>
        mutate(across(where( ~ is.Date(.x) | is.factor(.x)), as.numeric))
      sortee_cols <- d |> select(where( ~ is.numeric(.x) | is.Date(.x) | is.factor(.x))) |> names()

      dt_orders <-
        (map_dbl(sortee_cols, ~ which(.x == names(d))) - 1) |> setNames(seq(to = length(order_cols)) +
                                                                       length(d) - 1)

      if(is.null(dateformat)) {
        dateformat <-
          roboplotr_get_dateformat(d) |> roboplotr_hovertemplate_freq()
      }

      d <- d |>
        mutate(
          across(where(is.numeric), ~ {
            .cur <- cur_column()
            roboplotr_format_robotable_numeric(., rounding[[.cur]], flag[[.cur]], unit[[.cur]], na_value)
          }
        ),
        across(where(is.Date),~ roboplotr_format_robotable_date(., dateformat))
        ) |>
        bind_cols(order_cols)
    } else {
      dt_orders <- NULL
    }

    attr(d, "dt_orders") <- dt_orders

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



    hex8_to_opaque_hex <- function(hex8) {
      bg <- decode_colour(getOption("roboplot.colors.background"))
      rgba <- decode_colour(hex8, alpha = TRUE)
      rgb_final <- (1 - rgba[, 4]) * bg + rgba[, 1:3] * rgba[, 4]
      encode_colour(rgb_final)
    }

    .stripe_color <- list(color = decode_colour(getOption("roboplot.table_options")$stripecolor), opacity = getOption("roboplot.table_options")$stripeopacity)
    .stripeashex <- encode_colour(.stripe_color$color, alpha = .stripe_color$opacity) |> hex8_to_opaque_hex()
    .stripe_font <- roboplotr:::roboplotr_text_color_picker(.stripeashex)
    .stripe_color <- str_c(c(as.character(.stripe_color$color),.stripe_color$opacity), collapse = ',')

    res <- str_c(
      roboplotr_set_specific_css(
        str_glue('#{id}'),
        'width' = '100%!important',
        'font-size' = str_glue('{font$size+2}px'),
        'font-family' = font$family
        ),
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
          getOption("roboplot.shinyapp") == F) {
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
        str_glue("#{id}_wrapper table.dataTable.display> tbody > tr.odd > *, {id}_wrapper table.dataTable.striped> tbody > tr.odd > *"),
        "box-shadow" = str_glue("inset 0 0 0 9999px rgba({.stripe_color})!important")
      ),
      roboplotr_set_specific_css(
        str_glue("#{id}_wrapper .dataTables_scroll"),
        "margin-bottom" = "6px"
      ),
      roboplotr_set_specific_css(
        str_glue("#{id}_wrapper .dataTables_scrollBody"),
        "height" = "unset!important",
        "border-bottom" = "none"
      ),
      roboplotr_set_specific_css(
        str_glue("#{id}_wrapper .dataTables_scrollFoot table"),
        "border" = "none"
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
        str_glue("#{id} tr:last-of-type td"),
        "border-bottom" = "1pt solid rgba(0, 0, 0, 0.3)"
      ),
      roboplotr_set_specific_css(
        str_glue("#{id}_footer-container tr:hover"),
        "--dt-row-hover" = "none"
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
    HTML(res)
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

#' Comprehensive DT wrapper function
#'
#' This function wraps numerous [DT](https://rstudio.github.io/DT/)
#' features into a single, well-parametrized interface.
#'
#' @param d Data frame. Data to be created a table from.
#' @param title,subtitle Characters. Labels for plot elements.
#' @param caption Function or character. Use [set_caption()].
#' @param height,width Numeric. Height and width of the table. Default width is
#' NULL for responsive tables, give a value for static table dimensions.
#' @param flag,unit Character vectors. Use "+" for flag if you want to flag a numeric
#' column for positive values. Unit is prepended to values in a numeric column.
#' Name the units with column names from `d` if you want to target specific columns.
#' @param rounding Double, named if vector. Controls the rounding of numeric columns.
#' Give a named vector for specific columns. Default is set with [set_roboplot_options()]
#' @param dateformat Character. Controls how to format dates displayed on the table.
#' `robotable()` attempts to determine the proper format if left NULL.
#' @param na_value Character. How NA values are displayed.
#' @param pagelength Numeric. Controls how many rows are displayed on the table.
#' If `d` contains more rows than this, [robotable()] adds navigation elements.
#' @param info_text Character. Optional. If included, this text will be displayed
#' with a popup when the info button in the table modebar is clicked.
#' @param heatmap Function. Use [set_heatmap()]. Color-codes numeric columns.
#' @param labels Function. Use [set_labels()]. Sets robotable labels for pagination and search.
#' @param artefacts Function. Use [set_artefacts()]. Controls file exports.
#' Currently unable to make static exports, only html files.
#' @param class Character vector. Controls the basic datatable appearance. You may
#' combine any of "cell-border", "compact","hover","nowrap","row-border" and / or
#' "stripe". The default use is "stripe", "hover" and "row-border".
#' @param searchable,sortable Logical. Control whether the [robotable()] columns
#' have searching or sorting when navigation is displayed.
#' @param col_widths Named numeric vector. Must sum to 100 or lower. Sets the percentage
#' widths taken by column by name. Columns not named will have the excess space
#' divided evenly between them. For narrow screens the widths cannot be adhered to.
#' @param responsive Character or logical. If logical, the columns collapse for narrow
#' screens. If character, provide the names of the columns you want to prioritize.
#' @param ... Placeholder for other parameters.
#' @returns A list of classes datatable, htmlwidget, and roboplotr.robotable
#' @importFrom DT datatable tableFooter tableHeader
#' @importFrom htmltools HTML tags withTags
#' @importFrom stringr str_glue str_remove_all str_split str_width
#' @export
#' @examples
#' # You can use `robotable()` to create html tables
#' #
#' energiantuonti |> robotable()
#' #
#' # No matter if you have any number date, character or numeric columns, robotable()
#' # handles the formatting automatically.
#' #
#' d <- energiantuonti |>
#'   dplyr::filter(Alue %in% c("Ruotsi","USA")) |>
#'   tidyr::unite(Tiedot, Alue, Suunta, sep = ", ") |>
#'   dplyr::arrange(Tiedot, time) |>
#'   tidyr::pivot_wider(names_from = Tiedot) |>
#'   dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) |>
#'   dplyr::arrange(time)
#
#' d |> robotable()
#' #
#' # You can use `pagelength` to control how many rows are shown at once,
#' # `info_text` to add any information you wish describe about the table, and
#' # `heatmap` parameter if you want to color-code any numeric values across the
#' # table. See [set_heatmap()].
#' #
#' d |>
#'  robotable(heatmap = set_heatmap(),
#'            pagelength = 50,
#'            info_text = "Testing the info button.")
#'
#' # You can use `class` to control visuals, providing a character vector of html
#' # classes used by the table. Available options are "cell-border", "compact",
#' # "hover", "nowrap", "row-border" and "stripe", with the default being "stripe",
#' # "hover" and "row-border". Column widths can be specified by giving a named
#' # vector where the values are percentages of total width. Navigating options
#' # can be disabled by 'searchable' and 'sortable'.
#'
#' d |> robotable(
#'   class = c("compact", "nowrap"),
#'   searchable = FALSE,
#'   sortable = FALSE,
#'   col_widths = c("USA, Vienti" = 50)
#' )
#'
#' # You can use `responsive` to let columns collapse on narrow screens.
#'
#' energiantuonti |> robotable(responsive = TRUE)
#'
#' # Specify column priority.
#'
#' energiantuonti |> robotable(responsive = c("Alue", "value", "Suunta"))
#'
#' # Note that the underlying DT library does not support -not- using the first
#' # column as the top priority.
#'
#' energiantuonti |> robotable(responsive = "value")
#'
robotable <-
  function(d,
           title = NULL,
           subtitle = "",
           caption = NULL,
           rounding = getOption("roboplot.rounding"),
           width = getOption("roboplot.width"),
           height = NULL,
           flag = "",
           unit = "",
           dateformat = NULL,
           pagelength = 10,
           info_text = NULL,
           heatmap = NULL,
           na_value = "",
           class = NULL,
           searchable = T,
           sortable = T,
           labels = set_robotable_labels(),
           col_widths = NULL,
           responsive = NULL,
           artefacts = getOption("roboplot.artefacts")$auto,
           ...
           ) {

    title <- roboplotr_set_title(title, d, "in `robotable()`")

    roboplotr_typecheck(class, "character", NULL)
    roboplotr_valid_strings(class, c("cell-border", "compact","hover","nowrap","row-border","stripe"), any, "robotable() class")
    if(is.null(class)) { class <- "display" } else {
      class <- unique(class) |> str_c(collapse = " ")
    }
    roboplotr_typecheck(searchable, "logical",allow_null = F)
    roboplotr_typecheck(sortable, "logical",allow_null = F)
    roboplotr_typecheck(col_widths, "numeric", NULL)
    if(!is.null(col_widths)) {
      if (all(!names(col_widths) %in% names(d))) {
        stop("The param 'col_widths' must be a numeric vector with names matching the names from param 'd' or robotable()!", call. = F)
      } else if (any(!names(col_widths) %in% names(d))) {
        roboplotr_alert("Some columns specified in robotable() 'col_widths' are not in the data!")
      }
      col_widths <- col_widths[names(col_widths) %in% names(d)]
    if(sum(col_widths) > 100) {
        if(any(!names(d) %in% names(col_widths))) {
          .col_widths_msg <- " Remember to leave room for columns not defined in 'col_widths'."
        } else {
          .col_widths_msg <- ""
        }
        stop(str_glue("The param 'col_widths' must sum to 100 or lower!{.col_widths_msg}"), call. = F)
      }
    }

    caption <- roboplotr_set_caption(caption, d, "in robotable()")$text

    roboplotr_typecheck(heatmap, "set_heatmap")
    roboplotr_typecheck(labels, "set_robotable_labels")

    d <- d |> roboplotr_robotable_cellformat(rounding, flag, unit, na_value, dateformat)

    responsive_defs <- NULL

    if(!is.null(responsive)) {
      roboplotr_typecheck(responsive, c("character", "logical"), NULL)
      if(!is.logical(responsive)) {
        roboplotr_valid_strings(responsive, names(d), any, "robotable(responsive) responsive")
        responsive <- responsive[responsive %in% names(d)]
        responsive_defs <- imap(responsive, ~ list(responsivePriority = .y, targets = which(.x == names(d))-1))
        responsive <- T
      } else if (length(responsive) > 1) {
        stop("`robotable(responsive)` must be of length 1 if logical, or provide the names of the columns you want to prioritize!", call. = F)
      }

    } else {
      responsive <- F
    }

    order_defs <-
      map2(
        attributes(d)$dt_orders,
        names(attributes(d)$dt_orders),
        ~
          list(orderData = as.numeric(.y), targets = .x)
      ) |> unname() |>
      append(list(list(
        visible = F, targets = as.numeric(names(attributes(d)$dt_orders))
      )))

    center_defs <- (function() {
      dnames <- names(d)
      center_defs <- ((which(subset(dnames, !str_detect(dnames, "^\\.")) %in% dnames)[-1])-1)
      list(list(className = "dt-right", targets = center_defs),
           list(className = "dt-left", targets = 0)
           )
    })()

    colwidth_defs <- (function() {
      dnames <- names(d)
      if(!is.null(col_widths)) {
        col_names <- dnames[!dnames %in% names(col_widths)]
        colwidth_defs <- which(dnames %in% subset(col_names, !str_detect(col_names, "^\\.")))-1
        widths <- rep((100-sum(col_widths))/length(colwidth_defs),length(colwidth_defs)) |> roboplotr_round()
      } else {
        col_names <- dnames
        colwidth_defs <- which(dnames %in% subset(col_names, !str_detect(col_names, "^\\.")))-1
        widths <- rep(100/length(colwidth_defs),length(colwidth_defs)) |> roboplotr_round()
      }
      colwidth_defs <- widths |> setNames(colwidth_defs)
      colwidth_defs <- colwidth_defs |> unique() |> map( ~ list(
        width = as.character(str_glue("{.x}%")), targets = as.numeric(names(colwidth_defs[colwidth_defs == .x]))
      ))
      if(!is.null(col_widths)) {
        col_widths <- col_widths |> unique() |> map( ~ list(
          width = as.character(str_glue("{.x}%")), targets = which(dnames %in% names(col_widths[col_widths == .x]))-1
        ))
        colwidth_defs <- colwidth_defs |> append(col_widths)
      }
      colwidth_defs
    })()


    column_defs <- append(order_defs, center_defs) |> append(colwidth_defs) |> append(responsive_defs)

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
        style = str_glue("position: relative; background: {getOption('roboplot.colors.background')};"),
        roboplotr_get_robotable_google_fonts(),
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
          tags$tbody(),
          tags$tfoot(
            tags$tbody(
              id = str_glue("{robotable_id}_footer-container"),
              tags$th(
              id = str_glue("{robotable_id}_footer"),
              colspan = names(d) |> stringr::str_subset("^.order", negate = T) |> length(),
              tags$span(HTML(.footer)),
              robotable_logo()
            )
          )
          )
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


    .dom <- case_when(
      nrow(d) <= pagelength ~ "Bt",
      searchable == F ~ "Btiprl",
      TRUE ~ "Btiprlf"
    )

    preloadJS <- JS("
function reloadCSS(href, callback) {
  let links = Array.from(document.querySelectorAll('link[rel=\"stylesheet\"]')).filter(function(link) {
    return link.href.includes(href);
  });

  if (links.length > 0) {
    let link = links[0];
    let newLink = document.createElement('link');
    newLink.rel = 'stylesheet';
    newLink.href = href + '?v=' + new Date().getTime();

    newLink.onload = function() {
      link.parentNode.removeChild(link);
      document.head.appendChild(newLink);
      if (typeof callback === 'function') callback();
    };

    document.head.appendChild(newLink);
  } else {
    if (typeof callback === 'function') callback();
  }
}

function preInitFunction(settings, json) {
  console.log('preInit function started');

  reloadCSS('tbl_dependencies/dt-core-1.13.6/css/jquery.dataTables.min.css', function() {
    console.log('CSS reloaded, proceeding with DataTable initialization.');
    settings.oApi._fnInitComplete(settings);
  });
}
")

    is_responsive <- function(responsive) {
      if(is.null(responsive)) {NULL} else { "Responsive" }
    }
    
    dt <- d |>
      # filter(row_number() <= 100) |>
      datatable(
        class = class,
        width = width,
        height = height,
        container = sketch,
        rownames = FALSE,
        escape = FALSE,
        extensions = c("Buttons", is_responsive(responsive)),
        options = list(
          responsive = responsive,
          autoWidth = T,
          scrollX = T,
          scrollY = "100%",
          scrollCollapse = T,
          buttons = robotable_buttons,
          columnDefs = column_defs,
          dom = .dom,
          language = labels,
          lengthMenu = .pagination$lengthmenu,
          ordering = sortable,
          pageLength = .pagination$pagelength,
          preInit = preloadJS
          )
      ) |>
      roboplotr_set_robotable_fonts(seq(ncol(d)))
    
    dt

    dt <- roboplotr_tbl_heatmap(d, dt, heatmap)

    dt <- structure(dt, class = c(class(dt), "roboplotr","roboplotr.robotable"))

    roboplotr_typecheck(artefacts, c("logical","set_artefacts"))

    if (is.logical(artefacts)) {
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


#' @importFrom htmltools HTML tags
#' @importFrom purrr map
#' @importFrom stringr str_c str_extract str_glue
roboplotr_get_robotable_google_fonts <- function() {

  .fonts <- options()[c(
    "roboplot.font.main",
    "roboplot.font.title",
    "roboplot.font.caption"
  )] |> map( ~ if (!is.null(.x$google_font)) {
    roboplotr_set_specific_css("@font-face", "font-family" = str_extract(.x$family, "[^,]*(?=,)"),
                               "src" = str_glue("url('{.x$google_font$url}')"))
  } else if (!is.null(.x$path)) {
    .f <- roboplotr_set_font_string(.x, NULL)
    roboplotr_set_specific_css("@font-face", "font-family" = names(.f), "src" = str_glue("url('{.f[[1]]}')"))
  }) |>
    unique() |>
    unlist()

  if(length(.fonts) == 0) {
    NULL
  } else {
    tags$style(HTML(str_c(.fonts, collapse= ";")))
  }
}

#' Set `robotable()` global styling defaults.
#'
#' Control stripe appearance only at the moment.
#'
#' @param stripecolor Character. A valid css color name or 6-digit hex color.
#' @param stripeopacity Numeric. A number between 0 and 1.
#' @param ... Placeholder for future table_options
#' @export

set_table_options <- function(
    stripecolor = getOption("roboplot.table_defaults")$stripecolor,
    stripeopacity = getOption("roboplot.table_defaults")$stripeopacity,
    ...
) {

  roboplotr_typecheck(stripecolor, "character", allow_null = F)
  roboplotr_typecheck(stripeopacity, "numeric", allow_null = F)
  roboplotr_valid_colors(stripecolor)
  roboplotr_is_between(stripeopacity, "set_table_options", c(0, 1))

  .res <- list(stripecolor = stripecolor,
              stripeopacity = stripeopacity)

  .res <- structure(.res, class = c("roboplotr", "roboplotr.set_table_options", class(.res)))

  .res
}
