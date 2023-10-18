# this <- nettomuutto_map_data |>
#   filter(Alue %in% "Espoo") |>
#   group_by(across(!matches(c("time", "value")))) |>
#   summarize(value = sum(value), .groups = "drop")
#' Automated leaflet maps.
#'
#' Wrapper for [leaflet::leaflet] for shorthand declaration of many map layout arguments.
#' Automates many formatting options.
#'
#' @param d Data frame. Data to be created a table from.
#' @param area Symbol, string, or function resulting in symbol or string. Variable from argument 'd' to use to identify the areas described by the map. This must be of class sfc_MULTIPOLYGON.
#' @param title,subtitle Characters. Labels for plot elements. Optionally, use [set_title()] for the title if you want to omit the title from the displayed plot, but include it for any downloads through the modebar.
#' @param map_opacity Double. Value from 0 to 1, defining how opaque the underlying map tiles are. 0 removes the map layer.
#' @param map_colors Character vector. Colors used for map tiles, from highest to lowest values. [robomap()] will extrapolate as many colors as necessary. Must be a hexadecimal color strings or a valid css color strings.
#' @return A list of classes "leaflet" and "htmlwidget"
#' @importFrom htmltools HTML tags
#' @importFrom leaflet addControl addLegend addPolygons addTiles colorNumeric leaflet tileOptions
#' @importFrom purrr map
#' @importFrom stringr str_glue str_remove
#' @export
#' @examples
#' # You can use roboplotr::robomap() to create html maps. Note that very large
#' # number of geoms makes for slow rendering maps.
#'
#' vaesto_postinumeroittain |> robomap(Postinumeroalue)
#'
#' # Currently robomap() only supports very little customization.
#' vaesto_postinumeroittain |>
#'   dplyr::filter(Alue == "Espoo") |>
#'   robomap(Postinumeroalue, title = "Väestö Espoossa", caption = "Tilastokeskus")
#'
#' # robomap() adjusts legend item rounding based on the largest value in the "value"
#' # column of data 'd'. Control trace colors currently with global colors only.
#'
#' set_roboplot_options(trace_colors = c("lightgreen","darkred"), trace_border = list(width = 2, color = "black"))
#'
#' vaesto_postinumeroittain |>
#'   dplyr::slice_min(value, n = 30) |>
#'   robomap(Postinumeroalue, title = "Vähäväkilukuiset postinumerot", caption = "Tilastokeskus")
#'

robomap <-
  function(d,
           area,
           title = NULL,
           caption = NULL,
           hovertemplate = list(flag = "", unit = ""),
           map_opacity = 0.7,
           map_colors = getOption("roboplot.colors.traces")) {
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

    d <- d |>
      mutate(
        leafletlabel = str_c({
          {
            area
          }
        }, "<br>", roboplotr:::roboplotr_format_robotable_numeric(value, flag = hovertemplate$flag), " ", {
          hovertemplate$unit
        }),
        leafletlabel = map(leafletlabel, HTML)
      )

    bins <-
      round(rev(seq(
        min(d$value),
        max(d$value),
        length.out = min(round(length(
          d$value |> unique()
        )), 5) + 1
      ))) |> unique()

    if (length(bins) == 1) {
      map_palette <-
        colorNumeric(map_colors,
                     domain = d$value,
                     reverse = T)
    } else {
      map_palette <- leaflet::colorBin(
        map_colors,
        bins = bins,
        domain = d$value,
        reverse = T
      )
    }

    robomap_id <- str_c("robomap-", str_remove(runif(1), "\\."))

    this_map <-
      leaflet(d,
              elementId = robomap_id,
              width = "100%",
              # height = "100%"
              ) %>%
      addPolygons(
        color = getOption("roboplot.trace.border")$color,
        weight = getOption("roboplot.grid")$xwidth,
        # remove polygon borders
        fillColor = ~ map_palette(value),
        fillOpacity = 0.9,
        smoothFactor = 0.5,
        label = ~ leafletlabel,
        # weight = getOption("roboplot.trace.border")$width
      )

    if (map_opacity > 0) {
      this_map <-
        this_map |> addTiles(options = tileOptions(opacity = map_opacity, noWrap = T))
    }

    caption <- tags$span(
      style = str_glue(
        'opacity: 1; font-size: {getOption("roboplot.font.caption")$size}px;'
      ),
      caption
    )

    mainfontsize <- getOption("roboplot.font.main")$size
    control_style <- tagList(
      tags$style(
        roboplotr_set_specific_css(
          str_glue(".{robomap_id}-info"),
          # "width" = "40%",
          "background" = getOption("roboplot.colors.background"),
          "opacity" = "0.7",
          "font-size" = str_glue('{getOption("roboplot.font.main")$size}px'),
          "font-family" = getOption("roboplot.font.main")$family,
          "color" = getOption("roboplot.font.main")$color,
          "padding" = str_glue(
            '{round(mainfontsize/3)}px {mainfontsize}px {round(mainfontsize/3)}px {round(mainfontsize/2)}px'
          ),
          "border-radius" = "5px"
        )
      ),
      tags$style(roboplotr_set_specific_css(
        str_glue(".{robomap_id}-info-control"),
        "display" = "none"
      )),
      tags$style(
        roboplotr_set_specific_css(
          str_glue("#{robomap_id}_leaflet-container"),
          "background" = getOption("roboplot.colors.background")
        )
      )
    )

    this_map <- this_map |>
      addControl(
        control_style,
        position = "topleft",
        className = str_glue("{robomap_id}-info-control")
      ) |>
      addControl(
        html = caption,
        position = "bottomright",
        className = str_glue("{robomap_id}-info")
      )

    if (length(bins) == 1) {
      this_map <- this_map |>
        addLegend(
          className = str_glue("{robomap_id}-info legend"),
          na.label = "",
          opacity = 0.9,
          position = "bottomright",
          labels = roboplotr_format_robotable_numeric(unique(d$value)),
          colors = map_palette(d$value),
          values = ~ value,
          title = title$title
        )
    } else {
      round_bins <- function(vals) {
        maxval <- max(vals)
        num_digits <- nchar(maxval)
        # Determine the rounding magnitude
        round_magnitude <- switch(
          num_digits,
          '1' = 1,
          # Units
          '2' = 1,
          # Tens
          '3' = 10,
          # Hundreds
          '4' = 100,
          # Thousands
          '5' = 1000,
          # Tens of thousands
          10000             # Default for larger values
        )

        # Adjust the maximum bin
        ceiling(vals / round_magnitude) * round_magnitude

      }
      this_map <- this_map |>
        addLegend(
          className = str_glue("{robomap_id}-info legend"),
          na.label = "",
          opacity = 0.9,
          labFormat = function(type, cuts) {
            str_c(
              roboplotr_format_robotable_numeric(round_bins(tail(cuts,-1) - c(
                rep(-1, length(cuts) - 2), 0
              ))),
              " – ",
              roboplotr_format_robotable_numeric(round_bins(head(cuts,-1)))
            ) |> map(~ tags$span(.x, style = "white-space: nowrap;") |> as.character() |> HTML()) |>
              reduce(c)
          },
          position = "bottomright",
          pal = map_palette,
          values = ~ value,
          title = title$title
        )
    }
    this_map |>
      onRender(
        str_glue(
          "function(el, x) {
    var leafletContainer = $(el).closest('.leaflet-container');
    leafletContainer.attr('id', '{<robomap_id}_leaflet-container');
  }",
          .open = "{<"
        )
      )
  }
#
# robomap(this, Postinumeroalue)
