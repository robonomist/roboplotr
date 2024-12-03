#' Raster layer for data contour maps
#'
#' @importFrom leaflet addRasterImage
#' @importFrom methods as
#' @importFrom tidyr unnest
#' @importFrom utils capture.output
#' @noRd
roboplotr_map_rasterlayer <- function(map,
                                      d,
                                      data_contour = F,
                                      opacity,
                                      robomap_palette) {
  if (data_contour == TRUE) {
    roboplotr_ns_alert(
      c("gstat", "progress", "raster", "scales", "sf", "sp"),
      "usage of raster layers in `robomap()`"
    )

    d <- d |> mutate(area = as.numeric(sf::st_area(d)))

    d <- d |> mutate(
      area = .data$area / sum(.data$area),
      num_points = round(scales::rescale(.data$area, c(1, sqrt(
        sqrt(sqrt(max(d$area)))
      ))))
    )

    show.pb <- getOption("roboplot.verbose") == "All"

    if (show.pb) {
      bpb <- progress::progress_bar$new(total = (2 * nrow(d)) + 3, format = ":info for contour map.. [:bar]")
    }

    interior_df <- d |>
      mutate(sampled_points_interior = map2(.data$geom, .data$num_points, ~ {
        if (show.pb) {
          bpb$tick(token = list(info = "Determining boundaries"))
        }
        sf::st_sample(.x, size = .y)
      })) |>
      unnest(.data$sampled_points_interior) |>
      sf::st_sf()

    boundary_df <- d |>
      mutate(sampled_points_boundary = map2(.data$geom, .data$num_points, ~ {
        if (show.pb) {
          bpb$tick(token = list(info = "Determining boundaries"))
        }
        sf::st_sample(.x, size = max(ceiling(.y * 0.2), 1), type = "regular")
      })) |>
      unnest(.data$sampled_points_boundary) |>
      sf::st_sf()

    sampled_df <- bind_rows(interior_df, boundary_df)

    sampled_sp <- as(sampled_df, "Spatial")

    sp::proj4string(sampled_sp) <- sf::st_crs(d)$proj4string

    bb <- sf::st_bbox(d$geom)
    grd <- expand.grid(lon = seq(bb['xmin'], bb['xmax'], by = 0.1),
                       lat = seq(bb['ymin'], bb['ymax'], by = 0.1))
    sp::coordinates(grd) <- ~ lon + lat
    sp::gridded(grd) <- TRUE
    sp::proj4string(grd) <- sp::proj4string(sampled_sp)
    if (show.pb) {
      bpb$tick(token = list(info = "Creating raster layer"))
    }
    capture.output(
      idw_model <- gstat::idw(
        formula = robomap.value ~ 1,
        locations = sampled_sp,
        newdata = grd
      )
    )
    idw_raster <- raster::raster(idw_model)

    if (show.pb) {
      bpb$tick(token = list(info = "Masking raster layer"))
    }

    muni_mask <- as(sf::st_union(d), "Spatial")

    idw_masked <- raster::mask(idw_raster, muni_mask)

    template_raster <- raster::raster(raster::extent(idw_masked),
                                      ncol = idw_masked@ncols * 7,
                                      nrow = idw_masked@nrows * 7)
    raster::crs(template_raster) <- raster::crs(idw_masked)
    higher_res <- raster::resample(idw_masked, template_raster, method =
                                     "bilinear")

    smoothed_raster <- raster::focal(higher_res,
                                     w = matrix(1, 5, 5),
                                     fun = mean,
                                     na.rm = TRUE)
    smoothed_clipped <- raster::mask(smoothed_raster, muni_mask)
    map <- map |>
      addRasterImage(smoothed_clipped, colors = robomap_palette, opacity = opacity)
    if (show.pb) {
      bpb$terminate()
    }
    map
  }
  else {
    map
  }
}

#' @importFrom leaflet addCircleMarkers
roboplotr_map_markerlayer <- function(map, d, markers, size_scale = c(1, 12)) {
  roboplotr_ns_alert("scales", "usage of markers in `robomap()`")
  if (!markers) {
    map
  } else {
    if (!all(c("lon", "lat") %in% names(d))) {
      stop(
        "Currently robomap needs longitude and latitude as columns named \"lon\" and \"lat\"!",
        call. = F
      )
    }
    size_scale <- function(value) {
      scales::rescale(value, to = size_scale)
    }
    map |>
      addCircleMarkers(
        lng = ~ lon,
        lat =  ~ lat,
        stroke = TRUE,
        color = unique(getOption("roboplot.grid")[c("xcolor", "ycolor")]) |> first(),
        fillColor = getOption("roboplot.colors.background"),
        fillOpacity = 1,
        radius = ~ size_scale(robomap.value),
        weight = unique(getOption("roboplot.grid")[c("xwidth", "ywidth")]) |> unlist() |> max(),
        label = ~ leafletlabel
      )
  }
}

#' @importFrom leaflet addPolygons labelOptions
roboplotr_map_polygonlayer <- function(map,
                                       data_contour,
                                       map_opacity,
                                       robomap_palette,
                                       border_width) {
  map |>
    addPolygons(
      color = getOption("roboplot.trace.border")$color,
      weight = border_width,
      fillColor = robomap_palette,
      #~ if (data_contour) { NULL } else { robomap.value },
      fillOpacity = ifelse(data_contour, 0, map_opacity),
      label = ~ leafletlabel,
      labelOptions = labelOptions(
        style = list(
          "background" = getOption("roboplot.colors.background"),
          "font-size" = str_glue('{getOption("roboplot.font.main")$size}px'),
          "font-family" = getOption("roboplot.font.main")$family,
          "color" = roboplotr_text_color_picker(
            getOption("roboplot.colors.background"),
            getOption("roboplot.font.main")$size
          ),
          "border" = str_glue(
            '{max(getOption("roboplot.border")[c("xwidth","ywidth")] |> unlist())}pt solid {first(unique(getOption("roboplot.border")[c("xcolor","ycolor")]))}'
          )
        )
      )
    )
}

#' @importFrom leaflet addProviderTiles addTiles providerTileOptions tileOptions
roboplotr_map_tilelayer <- function(map, tile_opacity, wrap = F, provider = NULL) {
  if(!is.null(provider)) {
    # print("provider")

    map_providers <- c(
      "normal" = "OpenStreetMap",
      "minimalist" = "CartoDB.Positron",
      "dark" = "CartoDB.DarkMatter",
      "topo" = "Esri.WorldTopoMap",
      "street" = "Esri.WorldStreetMap",
      "satellite" = "Esri.WorldImagery",
      "grayscale" = "Esri.WorldGrayCanvas"
    )

    roboplotr_typecheck(provider, "character", size = 1, allow_null = T)
    roboplotr_valid_strings(provider, names(map_providers), any)

    map |>
      addProviderTiles(
        map_providers[[provider]],
        options = providerTileOptions(opacity = tile_opacity, noWrap = !wrap)
      )

  } else if (tile_opacity > 0) {
    map |>
      addTiles(options = tileOptions(opacity = tile_opacity, noWrap = !wrap))
  } else {
    map
  }


}

roboplotr_round_magnitude <- function(vals, rounding, .fun = ceiling) {
  map(vals, function(val) {
    num_digits <- nchar(abs(round(val, 0)))
    if (num_digits == 1 | all(val < 1, val > -1)) {
      if (str_detect(deparse(.fun), "round")) {
        .fun(val, rounding)
      } else {
        .fun(val)
      }
    } else {
      .round_magnitude <- case_when(
        num_digits %in% c(1, 2) ~ 1,
        num_digits == 3 ~ 10,
        num_digits == 4 ~ 100,
        num_digits == 5 ~ 1000,
        num_digits == 6 ~ 10000,
        TRUE ~ 100000
      )
      if (str_detect(deparse(.fun), "round")) {
        .fun(val / .round_magnitude, rounding) * .round_magnitude
      } else {
        .fun(val / .round_magnitude) * .round_magnitude
      }
    }
  }) |> unlist()

}

#' Comprehensive leaflet wrapper function
#'
#' This function wraps numerous [Leaflet](https://rstudio.github.io/leaflet/articles/leaflet.html)
#' features into a single, well-parametrized interface.
#'
#' @param d Data frame. Data to be created a map from.
#' @param area Symbol or string. Variable from argument 'd' to use to identify the
#' areas described by the map. This must be of class sfc_MULTIPOLYGON.
#' @param title,subtitle Characters. Labels for plot elements.
#' @param caption Function or character. Use a string, or [set_caption()].
#' @param map_opacity,tile_opacity Numeric. Values from 0 to 1, defining how opaque
#' the map polygon fill color or underlying map tiles are. 0 removes the tile layer,
#' but retains the polygon borders if any.
#' @param map_palette Character or function. Must be hexadecimal colors or valid
#' css colors, or use [set_heatmap()] if specifying color breakpoints.
#' @param hovertext Function. Use [set_hovertext()].
#' @param border_width Integer. The width of polygon borders.
#' @param legend Function. Use [set_legend()].
#' @param data_contour Logical. Experimental. If TRUE, [robomap()] will produce
#' a contour-like representation of the data, which does not conform to the boundaries
#' of the polygons. This provides a smoother transition and helps in visualizing
#' general trends across regions. Default is FALSE.
#' @param tile_style Character string specifying the map style to use. Options include:
#' \describe{
#'   \item{"normal"}{A standard, detailed map suitable for most general-purpose use.}
#'   \item{"minimalist"}{A clean, modern map with minimal design, focused on clarity and smooth rendering.}
#'   \item{"dark"}{A dark-themed map, ideal for nighttime or high-contrast visualizations.}
#'   \item{"topo"}{An outdoor-focused map, highlighting features relevant for activities like hiking and exploration.}
#'   \item{"bw"}{A high-contrast, black-and-white map, useful for bold, minimalist visuals.}
#'   \item{"street"}{A detailed street map, with emphasis on urban and road networks for navigation and infrastructure insights.}
#'   \item{"satellite"}{High-resolution satellite imagery, offering detailed aerial views for geographic analysis.}
#'   \item{"grayscale"}{A minimalist grayscale map, often used for background or overlay purposes in more complex visualizations.}
#' }
#' @param wrap Logical. Whether the map should wrap around the globe. Default is TRUE.
#' @param zoom Logical. Whether the map is zoomable or not.
#' @param markers Logical. Experimental. Whether markers will be added on the map
#' based on the columns "lat" and "lon". Default is FALSE.
#' @param height,width Numeric. Height and width of the plot. Default width is NA.
#' for responsive plots.
#' @param ... Placeholder for other parameters.
#' @returns A list of classes leaflet, htmlwidget and roboplot.robomap
#' @importFrom htmltools HTML tags
#' @importFrom leaflet addControl addEasyButton addLegend colorFactor easyButton leaflet labelFormat leafletOptions
#' @importFrom purrr map
#' @importFrom stringr str_glue str_remove
#' @importFrom utils head tail
#' @export
#' @examples
#' # You can use `robomap()` to create interactive maps. Note that very large
#' # number of map polygons makes for slow rendering maps.
#' vaesto_postinumeroittain |>
#'   robomap(Postinumeroalue, title = "V\u00e4kiluku postinumeroalueittain", caption = "Tilastokeskus")
#'
#' # Default polygon colors are picked from trace colors set with
#' # `set_roboplot_options()` based on luminosity. Control polygon colors with
#' # `map_palette`. `robomap()` expands upon this as necessary.
#'
#' vaesto_postinumeroittain |>
#'   dplyr::filter(Alue == "Espoo") |>
#'   robomap(
#'     Postinumeroalue,
#'     title = "V\u00e4kiluku Espoossa",
#'     subtitle = "Otanta",
#'     caption = "Tilastokeskus",
#'     map_palette = c("lightgreen", "darkred")
#'   )
#' # You might want to disallow zooming for some reason. The map will be draggable,
#' # but zoom by buttons or scrolling is disabled.
#'
#' vaesto_postinumeroittain |>
#'   dplyr::filter(Alue == "Espoo") |>
#'   robomap(
#'     Postinumeroalue,
#'     title = "V\u00e4est\u00f6 Espoossa",
#'     subtitle = "Otanta",
#'     caption = "Tilastokeskus",
#'     zoom = FALSE
#'   )
#'
#' # Set polygon border width with `border_width", polygon opacity with `map_opacity`,
#' # and the opacity of the underlying map tiles with `tile_opacity`. Use `set_legend()`
#' # to control legend specifics.
#'
#' vaesto_postinumeroittain |>
#'   dplyr::filter(Alue == "Espoo") |>
#'   robomap(
#'     Postinumeroalue,
#'     title = "V\u00e4est\u00f6 Espoossa",
#'     caption = "Tilastokeskus",
#'     border_width = 2,
#'     tile_opacity = 0.2,
#'     map_opacity = 0.5,
#'     legend = set_legend(title = "V\u00e4est\u00f6")
#'   )
#'
#' # Control the story you want to tell by using `set_heatmap()` with `map_palette`,
#' # setting the colors and breakpoints. Use `legend` with `set_legend(breaks)`
#' # to control how many entries the legend is split to. Heatmap maps cannot have a
#' # gradient legend. `robomap()` will detect it, but you can also set it manually
#' # to avoid unnecessary messages.
#' vaesto_postinumeroittain |>
#'   dplyr::filter(Alue == "Espoo") |>
#'   robomap(
#'     Postinumeroalue,
#'     title = "V\u00e4est\u00f6 Espoossa",
#'     caption = "Tilastokeskus",
#'     map_palette = set_heatmap(
#'       midvalue = 6000,
#'       midcolor = "yellow",
#'       maxcolor = "red"
#'     ),
#'     legend = set_legend(breaks = 3, gradient = FALSE),
#'     map_opacity = 1,
#'     border_width = 0
#'   )
#'
#' # Or just give the legend breaks
#' vaesto_postinumeroittain |>
#'   dplyr::filter(Alue == "Espoo") |>
#'   robomap(
#'     Postinumeroalue,
#'     title = "V\u00e4est\u00f6 Espoossa",
#'     caption = "Tilastokeskus",
#'     map_palette = set_heatmap(midvalue = 6000, midcolor = "yellow", maxcolor = "red"),
#'     legend = set_legend(breaks = c(3000, 6000, 9000), gradient = FALSE),
#'     map_opacity = 1,
#'     border_width = 0
#'   )
#'
#' # Use labformat to format your labels to your liking. You probably want to be
#' # quite specific with the breaks in this case, or create the labformat function
#' # dynamically.
#' vaesto_postinumeroittain |>
#'   dplyr::filter(stringr::str_detect(Postinumero, "^009")) |>
#'   robomap(Postinumeroalue,
#'           "V\u00e4kiluku Itä-Helsingissä",
#'           tile_opacity = 1,
#'           map_opacity = 1,
#'           rounding = 0,
#'           legend = set_legend(
#'             title = "Suuruusluokka",
#'             breaks = 2,
#'             labformat = function(x) {
#'               dplyr::case_when(x > 16000 ~ "Suuri", x > 6000 ~ "Keskiverto", TRUE ~ "Pieni")
#'             }
#'           )
#'   )
#'
#' # You might have categorial data instead.
#' vaesto_postinumeroittain |>
#'   dplyr::filter(stringr::str_detect(Postinumero, "^009")) |>
#'   dplyr::mutate(
#'     value = dplyr::case_when(
#'       value >= quantile(vaesto_postinumeroittain$value)["75%"] ~ "Suuri",
#'       value <= quantile(vaesto_postinumeroittain$value)["25%"] ~ "Pieni",
#'       TRUE ~ "Normaali"
#'     ) |>
#'       forcats::fct_relevel("Pieni", "Normaali", "Suuri")
#'   ) |>
#'   robomap(
#'     Postinumeroalue,
#'     "V\u00e4kiluku Itä-Helsingissä",
#'     "Suuruusluokittain",
#'     legend = set_legend(title = "Suuruusluokka")
#'   )
#'
#' # Control map style with `tile_style`. Some options are "dark", "minimalist",
#' # "satellite" etc. See `robomap()` documentation for full list.
#' vaesto_postinumeroittain |>
#'   dplyr::filter(stringr::str_detect(Postinumero, "^009")) |>
#'   robomap(Postinumeroalue,
#'           "V\u00e4kiluku It\u00e4-Helsingiss\u00e4",
#'           tile_opacity = 1,
#'           map_opacity = 1,
#'           tile_style = "dark"
#'   )
robomap <-
  function(d,
           area,
           title = NULL,
           subtitle = "",
           caption = NULL,
           hovertext = set_hovertext(),
           map_opacity = 0.9,
           tile_opacity = 0.7,
           wrap = TRUE,
           tile_style = NULL,
           map_palette = NULL,
           border_width = getOption("roboplot.trace.border")$width,
           height = getOption("roboplot.height"),
           width = getOption("roboplot.width"),
           legend = set_legend(),
           data_contour = FALSE,
           markers = FALSE,
           zoom = TRUE,
           ...) {
    roboplotr_ns_alert("sf", "usage of `robomap()`")

    roboplotr_typecheck(d, "sf", allow_null = F, size = NULL)
    if(sf::st_crs(d)$epsg != "4326") {
      roboplotr_alert("The projection of the data is not WGS 84 (EPSG:4326)!\nThis may cause issues with the map projection.")
      d <- sf::st_transform(d, 4326)
    }

    roboplotr_typecheck(d$value, c("factor","numeric"), allow_na = T, size = NULL)
    legend_labs <- NULL
    if(is.factor(d$value)) {
      legend_labs <- levels(d$value)
    }
    d <- d |> mutate(robomap.value = as.numeric(.data$value))

    title <- roboplotr_set_title(title, d, "in `robomap()`")

    roboplotr_typecheck(legend,
                        c("numeric", "set_legend"),
                        allow_null = F,
                        extra = "in robomap()")

    if (!is.list(legend)) {
      legend <- set_legend(breaks = legend)
    }

    legend$gradient <- legend$gradient %||% (class(d$value) != "factor")

    if(!is.null(map_palette)) {
      if("roboplotr.set_heatmap" %in% class(map_palette) & legend$gradient) {
        roboplotr_alert("Heatmap color palette is not compatible with gradient legend!\nGradient legend unset. Alternatively, use a color vector as map_palette, but you will lose specific value-color matching control.")
        legend$gradient <- FALSE

      }
    }
    if(legend$gradient) {
      if(!roboplotr_is_equally_spaced(legend$breaks)) {
        roboplotr_alert("Legend breaks must be equal in size when using a gradient legend!\nIgnoring the specific breaks")
        legend$breaks <- length(legend$breaks)
      }
    }

    roboplotr_typecheck(zoom, "logical", allow_null = F, extra = "in robomap()")

    roboplotr_typecheck(wrap, "logical", allow_null = F, extra = "in robomap()")

    roboplotr_typecheck(hovertext, "set_hovertext")

    rounding <- hovertext$rounding

    caption <- roboplotr_set_caption(caption, d, "in `robomap()`")

    roboplotr_typecheck(width, "numeric", allow_na = T)
    roboplotr_typecheck(height, "numeric", allow_na = T)
    if (!is.null(width)) {
      if (is.na(width)) {
        width <- NULL
      }
    }
    if (!is.null(height)) {
      if (is.na(height)) {
        height <- NULL
      }
    }

    roboplotr_typecheck(map_palette,
                        c("character", "set_heatmap"),
                        size = NULL,
                        extra = "in robomap()")

    if (!is.null(map_palette)) {
      if (all(is.character(map_palette))) {
        roboplotr_valid_colors(map_palette)
      }
    } else {
      roboplotr_colors <- getOption("roboplot.colors.traces")
      roboplotr.luminance <- roboplotr_get_luminance(roboplotr_colors)
      map_palette <- c(roboplotr_colors[which(roboplotr.luminance == max(roboplotr.luminance))], roboplotr_colors[which(roboplotr.luminance == min(roboplotr.luminance))])
    }


    if(!is.null(hovertext$format)) {
      .unitformat <- hovertext$format
    } else {
      .unitformat <- function(x) {
        str_glue("{roboplotr_format_robotable_numeric(x, rounding)} {hovertext$unit}")
      }
    }

    if(is.factor(d$value)) {
      d <- d |>
        mutate(
          leafletlabel = ifelse(as.character({{area}}) == as.character(.data$value), as.character(.data$value), str_c({{area}},"<br>",as.character(.data$value))),
          leafletlabel = map(.data$leafletlabel, HTML)
        )
    } else {
      d <- d |>
        mutate(
          leafletlabel = str_c({{area}},"<br>",.unitformat(.data$robomap.value)),
          leafletlabel = map(.data$leafletlabel, HTML)
        )
    }

    get_bins <- function(legend_breaks, rounding = hovertext$rounding) {
      if (length(legend_breaks) == 1) {
        bins <- rev(seq(
          min(d$robomap.value, na.rm = T),
          max(d$robomap.value, na.rm = T),
          length.out = min(round(length(
            d$robomap.value |> unique()
          )), legend_breaks + 1)
        ))
      } else {
        check_breaks <- function(legend_breaks) {
          for(breaks in legend_breaks) {
            roboplotr_is_between(breaks, "set_legend()", lims = range(d$robomap.value, na.rm = T))
          }
        }
        check_breaks(legend_breaks)
        bins <- c(max(d$robomap.value, na.rm = T), rev(sort(legend_breaks)))
        if(min(d$robomap.value, na.rm = T) < min(bins)) {
          bins <- c(bins,min(d$robomap.value, na.rm = T))
        }
      }

      if (length(bins) == 1) {
        bins <- roboplotr_round_magnitude(bins, rounding, round)
      } else {
        .first <- roboplotr_round_magnitude(bins[1], rounding, ceiling)
        .last <- roboplotr_round_magnitude(last(bins), rounding, .fun = floor)
        if (.first < max(bins)) {
          .first <- max(bins)
        }
        bins <- roboplotr_round_magnitude(bins, rounding, round)
        bins[1] <- .first
        bins[length(bins)] <- .last

      }

      bins |> unique()
    }

    bins <- get_bins(legend$breaks)

    if (is.list(map_palette)) {
      robomap_palette <- roboplotr_tbl_heatmap_colorfun(
        tibble(d),
        hmin = map_palette$min,
        hmid = map_palette$mid,
        hmax = map_palette$max,
        cols = "robomap.value"
      )
      legend_palette <- robomap_palette
    } else  {
      robomap_palette <- roboplotr_get_map_palette(bins, map_palette, legend$gradient, legend$gradient)
      legend_palette <- roboplotr_get_map_palette(bins, map_palette, legend$gradient, rev = !legend$gradient)
    }

    robomap_id <- str_c("robomap-", str_remove(runif(1), "\\."))

    map_pal <- robomap_palette(d$robomap.value)

    this_map <- leaflet(
      d,
      height = height,
      width = width,
      # elementId = robomap_id,
      options = leafletOptions(
        scrollWheelZoom = zoom,
        doubleClickZoom = zoom,
        touchZoom = zoom,
        boxZoom = zoom,
        zoomControl = FALSE,
        preferCanvas = FALSE
      )
    ) |>
      roboplotr_map_tilelayer(tile_opacity, wrap, tile_style) |>
      roboplotr_map_rasterlayer(d, data_contour, map_opacity, robomap_palette) |>
      roboplotr_map_polygonlayer(data_contour, map_opacity, map_pal, border_width) |>
      roboplotr_map_markerlayer(d, markers)

    if(str_length(caption) > 0) {
      caption <- tags$span(
        style = str_glue(
          'opacity: 1; font-size: {getOption("roboplot.font.caption")$size}px;'
        ),
        HTML(caption)
      )
    } else {
      caption <- NULL
    }

    get_map_title <- function() {
      titlefun <-
        if (getOption("roboplot.font.title")$bold == T) {
          title <- tags$b(HTML(title$title))
        } else {
          title <- HTML(title$title)
        }

      ifelse(
        subtitle == "",
        str_glue("{title}"),
        str_glue(
          "{title}<br><span style = 'font-size: 75%'>{subtitle}</span>"
        )
      )
    }

    map_title <- get_map_title()

    this_map <- roboplotr_robomap_modebar(this_map, title$title, zoom)

    robotable_logo_height <- ifelse(is.null(height), "30px", str_glue("{round(height/20)}px"))
    this_map <- this_map |>
      addControl(
        html = HTML(map_title),
        position = "topleft",
        className = str_glue("robomap-title")
      ) |>
      addControl(
        roboplotr_get_robomap_css(robomap_id, legend, robotable_logo_height),
        position = "topleft",
        className = str_glue("{robomap_id}-info-control")
      ) |>
      addControl(
        html = robotable_logo(robotable_logo_height),
        position = "bottomright",
        className = str_glue("map-info robomap-logo")
      )

    if (!is.null(caption)) {
      this_map <- this_map |>
        addControl(
          html = caption,
          position = "bottomleft",
          className = str_glue("map-info")
        )
    }

    this_map <- this_map |>
      onRender(
        str_glue(
          "function(el, x) {
    var leafletContainer = $(el).closest('.leaflet-container');
    leafletContainer.attr('id', '{<robomap_id}_leaflet-container');
      var map = this;
      map.initialCenter = map.getCenter();
      map.initialZoom = map.getZoom();
  }",
          .open = "{<"
        )
      )

    if (legend$position != "none") {

      if(!legend$gradient) {
        if(!is.null(legend$labformat)) {
          .formatfun <- function(x) legend$labformat(x)
        } else {
          if(is.null(legend_labs)) {
            .formatfun <- function(x) roboplotr_format_robotable_numeric(x, rounding = max(rounding - 1, 0))
          } else {
            .formatfun <- function(x) legend_labs[x]
          }
        }
        this_map <- this_map |> addLegend(
          className = str_glue("map-info legend"),
          position = legend$position,
          opacity = map_opacity,
          labels = .formatfun(bins),
          colors = robomap_palette(bins),
          na.label = "",
          title = legend$title %||% ""
        )
      } else {
        if(!is.null(legend$labformat)) {
          .labformatfun <- function(type, cuts, p) {
            legend$labformat(rev(cuts))
          }
        } else {
          .labformatfun <- function(type, cuts, p) {
            roboplotr_format_robotable_numeric(rev(cuts), rounding = max(rounding - 1, 0))
          }
        }
        this_map <- this_map |>
          addLegend(
            className = str_glue("map-info legend"),
            position = legend$position,
            opacity = map_opacity,
            pal = legend_palette,
            labFormat = .labformatfun,
            values = bins,
            bins = legend$breaks,
            na.label = "",
            title = legend$title %||% ""
          )
      }

    }

    this_map <- structure(this_map, class = c(class(this_map), "roboplotr", "roboplotr.robomap"))

    this_map
  }

roboplotr_get_robomap_css <- function(robomap_id, legend, logo_height) {


  mainfontsize <- getOption("roboplot.font.main")$size

  control_style <- tagList(tags$style(
    str_glue(
      roboplotr_set_specific_css(
        "#{<robomap_id}_leaflet-container .robomap-title",
        "width" = "fit-content",
        "background" = getOption("roboplot.colors.background"),
        "opacity" = as.character(legend$opacity),
        "font-size" = '{<getOption("roboplot.font.title")$size}px',
        "font-family" = getOption("roboplot.font.title")$family,
        "color" = getOption("roboplot.font.title")$color,
        "padding" =
          '{<round(mainfontsize/3)}px {<mainfontsize}px {<round(mainfontsize/3)}px {<round(mainfontsize/2)}px',
        "border-radius" = "5px"
      ),
      roboplotr_set_specific_css(
        "#{<robomap_id}_leaflet-container .map-info",
        "width" = "fit-content",
        "background" = getOption("roboplot.colors.background"),
        "opacity" = as.character(legend$opacity),
        ## mikä on sopiva..?
        "font-size" = '{<getOption("roboplot.font.main")$size}px',
        "font-family" = getOption("roboplot.font.main")$family,
        "color" = getOption("roboplot.font.main")$color,
        "padding" =
          '{<round(mainfontsize/3)}px {<mainfontsize}px {<round(mainfontsize/3)}px {<round(mainfontsize/2)}px',
        "border-radius" = "5px"
      ),
      roboplotr_set_specific_css(".{<robomap_id}-info-control", "display" = "none"),
      roboplotr_set_specific_css(
        "#{<robomap_id}_leaflet-container",
        "background" = getOption("roboplot.colors.background")
      ),
      roboplotr_set_specific_css(
        "#{<robomap_id}_leaflet-container .leaflet-control-attribution, #{<robomap_id}_leaflet-container .leaflet-control-attribution a",
        "background" = getOption("roboplot.colors.background"),
        "font-size" = str_glue('{getOption("roboplot.font.caption")$size}px'),
        "font-family" = getOption("roboplot.font.caption")$family,
        "color" = getOption("roboplot.font.caption")$color,
        "opacity" = 0.5
      ),
      # modebar
      roboplotr_set_specific_css(
        "#{<robomap_id}_leaflet-container .leaflet-top.leaflet-right",
        "display" = "flex",
        "flex-direction" = "row",
        "align-items" = "flex-end",
        "margin-top" = "9px",
        "margin-right" = "5px",
        "height" = "20px",
        "width" = "fit-content",
        "background" = "transparent",
        "opacity" = as.character(legend$opacity),
        ## mikä on sopiva..?
        "font-size" = '{<getOption("roboplot.font.main")$size}px',
        "font-family" = getOption("roboplot.font.main")$family,
        "color" = getOption("roboplot.font.main")$color,
        "padding" =
          '{<round(mainfontsize/3)}px {<mainfontsize}px {<round(mainfontsize/3)}px {<round(mainfontsize/2)}px',
        "border-radius" = "5px"
      ),
      roboplotr_set_specific_css(
        "#{<robomap_id}_leaflet-container:hover .leaflet-top.leaflet-right",
        "background" = getOption("roboplot.colors.background"),
        "transition" = "opacity 0.3s ease-in-out, width 0.3s ease-in-out"
      ),
      #modebar buttons
      roboplotr_set_specific_css(
        "#{<robomap_id}_leaflet-container .easy-button-container, #{<robomap_id}_leaflet-container .easy-button-button",
        "background-color" = "transparent",
        "height" = "20px",
        "border" = "none",
        "width" = "18px",
        "padding-left" = " 0px",
        "padding-top" = "0px",
        "margin" = "5px 2px 4px 3px",
        "outline" = "none",
        "box-shadow" = "none",
        "cursor" = "pointer",
        "font-size" = "16px"
      ),
      roboplotr_set_specific_css(
        "#{<robomap_id}_leaflet-container .easy-button-button:hover",
        "background-color" = "transparent"
      ),
      roboplotr_set_specific_css(
        "#{<robomap_id}_leaflet-container .easy-button-button:active",
        "background-color" = "transparent"
      ),
      # modebar icons
      roboplotr_set_specific_css(
        '#{<robomap_id}_leaflet-container .leaflet-top.leaflet-right .fa, #{<robomap_id}_leaflet-container .leaflet-top.leaflet-right svg path',
        'fill' = 'none',
        'color' = 'transparent',
        'transition' =  'opacity 0.3s ease-in-out width 0.3s ease-in-out'
      ),
      roboplotr_set_specific_css(
        "#{<robomap_id}_leaflet-container:hover .leaflet-top.leaflet-right .fa, #{<robomap_id}_leaflet-container:hover .leaflet-top.leaflet-right svg path",
        "fill" = "rgba(68, 68, 68, 0.3)",
        "color" = "rgba(68, 68, 68, 0.3)",
        "transition" = "opacity 0.3s ease-in-out, width 0.3s ease-in-out"
      ),
      roboplotr_set_specific_css(
        "#{<robomap_id}_leaflet-container .leaflet-top.leaflet-right .easy-button-button:hover .fa, #{<robomap_id}_leaflet-container .leaflet-top.leaflet-right .easy-button-button:hover svg path",
        "fill" = "rgba(68, 68, 68, 0.7)",
        "color" = "rgba(68, 68, 68, 0.7)",
        "transition" = "opacity 0.3s ease-in-out, width 0.3s ease-in-out"
      ),
      roboplotr_set_specific_css(
        "#{<robomap_id}_leaflet-container:hover #robonomist-link svg path, #{<robomap_id}_leaflet-container.easy-button-button:hover #robonomist-link svg path",
        "fill" = "rgba(68, 68, 68, 1)",
        "transition" = "opacity 0.3s ease-in-out, width 0.3s ease-in-out"
      ),
      roboplotr_set_specific_css(
        "#{<robomap_id}_leaflet-container .robomap-logo",
        "margin-bottom" = "2px",
        "height" = if(getOption("roboplot.shinyapp")) { NULL } else { logo_height }
      ),
      # roboplotr_set_specific_css(
      #   "#{<robomap_id}_leaflet-container .map-info.legend g line",
      #   "display" = "none"
      # ),
      .open = "{<"
    )
  ))

  control_style
}
