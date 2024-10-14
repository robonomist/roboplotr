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

#' @importFrom leaflet addTiles tileOptions
roboplotr_map_tilelayer <- function(map, tile_opacity, wrap = F) {
  if (tile_opacity > 0) {
    map |>
      addTiles(options = tileOptions(opacity = tile_opacity, noWrap = wrap))
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
#' @param hovertext List. Use a list with named characters flag and unit.
#' @param border_width Integer. The width of polygon borders.
#' @param legend Function. Use [set_legend()].
#' @param data_contour Logical. Experimental. If TRUE, [robomap()] will produce
#' a contour-like representation of the data, which does not conform to the boundaries
#' of the polygons. This provides a smoother transition and helps in visualizing
#' general trends across regions. Default is FALSE.
#' @param zoom Logical. Whether the map is zoomable or not.
#' @param rounding Numeric. How [robomap()] rounds numeric values.
#' @param markers Logical. Experimental. Whether markers will be added on the map
#' based on the columns "lat" and "lon". Default is FALSE.
#' @param height,width Numeric. Height and width of the plot. Default width is NA.
#' for responsive plots.
#' @param ... Placeholder for other parameters.
#' @returns A list of classes leaflet, htmlwidget and roboplot.robomap
#' @importFrom htmltools HTML tags
#' @importFrom leaflet addControl addEasyButton addLegend easyButton leaflet labelFormat leafletOptions
#' @importFrom purrr map
#' @importFrom stringr str_glue str_remove
#' @importFrom utils head tail
#' @export
#' @examples
#' # You can use `robomap()` to create interactive maps. Note that very large
#' # number of map polygons makes for slow rendering maps.
#' vaesto_postinumeroittain |>
#'   dplyr::filter(Alue == "Espoo") |>
#'   robomap(Postinumeroalue, title = "V\u00e4kiluku Espoossa", caption = "Tilastokeskus")
#' 
#' # Default polygon colors are picked from trace colors set with
#' # `set_roboplot_options()` based on luminosity. Control polygon colors with
#' # `map_palette`. `robomap()` expands upon this as necessary.
#' 
#' vaesto_postinumeroittain |>
#'   robomap(
#'     Postinumeroalue,
#'     title = "V\u00e4kiluku postinumeroittain",
#'     subtitle = "Otanta",
#'     caption = "Tilastokeskus",
#'     map_palette = c("lightgreen", "darkred")
#'   )
#' # You might want to disallow zooming for some reason. The map will be draggable,
#' # but zoom by buttons or scrolling is disabled.
#' 
#' vaesto_postinumeroittain |>
#'   robomap(
#'     Postinumeroalue,
#'     title = "V\u00e4est\u00f6 postinumeroittain",
#'     subtitle = "Otanta",
#'     caption = "Tilastokeskus",
#'     zoom = FALSE
#'   )
#' 
#' # Other controls you might need.
#' 
#' vaesto_postinumeroittain |>
#'   robomap(
#'     Postinumeroalue,
#'     title = "V\u00e4est\u00f6 postinumeroittain",
#'     caption = "Tilastokeskus",
#'     map_palette = c("lightgreen", "darkred"),
#'     border_width = 2,
#'     tile_opacity = 0.2,
#'     map_opacity = 0.5
#'   )
#' 
#' # Control the story you want to tell by using `set_heatmap()` with `map_palette`,
#' # setting the colors and breakpoints. Use `legend` with `set_legend(breaks)`
#' # to control how many entries the legend is split to.
#' vaesto_postinumeroittain |>
#'   robomap(
#'     Postinumeroalue,
#'     title = "V\u00e4est\u00f6 postinumeroittain",
#'     caption = "Tilastokeskus",
#'     map_palette = set_heatmap(
#'       midvalue = 6000,
#'       midcolor = "yellow",
#'       maxcolor = "red"
#'     ),
#'     legend = set_legend(breaks = 3),
#'     map_opacity = 1,
#'     border_width = 0
#'   )
#' # Or just give the legend breaks
#' vaesto_postinumeroittain |>
#'   robomap(
#'     Postinumeroalue,
#'     title = "V\u00e4est\u00f6 postinumeroittain",
#'     caption = "Tilastokeskus",
#'     map_palette = set_heatmap(midvalue = 6000, midcolor = "yellow", maxcolor = "red"),
#'     legend = set_legend(breaks = c(3000, 6000, 9000)),
#'     map_opacity = 1,
#'     border_width = 0
#'   )
#' 
robomap <-
  function(d,
           area,
           title = NULL,
           subtitle = "",
           caption = NULL,
           hovertext = list(flag = "", unit = ""),
           map_opacity = 0.9,
           tile_opacity = 0.7,
           map_palette = NULL,
           border_width = getOption("roboplot.trace.border")$width,
           height = getOption("roboplot.height"),
           width = getOption("roboplot.width"),
           legend = set_legend(),
           data_contour = FALSE,
           markers = FALSE,
           rounding = round(getOption("roboplot.rounding")),
           zoom = TRUE,
           ...) {
    roboplotr_ns_alert("sf", "usage of `robomap()`")
    
    title <- roboplotr_set_title(title, d, "in `robomap()`")
    
    roboplotr_typecheck(legend,
                        c("numeric", "set_legend"),
                        allow_null = F,
                        extra = "in robomap()")
    
    if (!is.list(legend)) {
      legend <- set_legend(breaks = legend)
    }
    
    roboplotr_typecheck(zoom, "logical", allow_null = F, extra = "in robomap()")
    
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
    
    d <- d |>
      mutate(
        leafletlabel = str_c({
          {
            area
          }
        }, "<br>", roboplotr_format_robotable_numeric(.data$value, rounding, flag = hovertext$flag), " ", {
          hovertext$unit
        }),
        leafletlabel = map(.data$leafletlabel, HTML)
      )

    get_bins <- function(legend_breaks) {
      if (length(legend_breaks) == 1) {
        bins <- rev(seq(
          min(d$value, na.rm = T),
          max(d$value, na.rm = T),
          length.out = min(round(length(
            d$value |> unique()
          )), legend_breaks + 1)
        ))
      } else {
        bins <- c(max(d$value, na.rm = T), rev(sort(legend_breaks)), min(d$value, na.rm = T))
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
    # bins <- c(10000, 2203, 0)
    d <- d |> mutate(robomap.value = .data$value)
    
    if (is.list(map_palette)) {
      # käytetään robotable heatmappia
      robomap_palette <- roboplotr_tbl_heatmap_colorfun(
        tibble(d),
        hmin = map_palette$min,
        hmid = map_palette$mid,
        hmax = map_palette$max,
        cols = "robomap.value"
      )
    } else {
      robomap_palette <- roboplotr_get_map_palette(d, map_palette, data_contour, bins)
    }
    
    robomap_id <- str_c("robomap-", str_remove(runif(1), "\\."))
    
    map_pal <- robomap_palette(d$robomap.value)
    
    this_map <- leaflet(
      d,
      height = height,
      width = width,
      elementId = robomap_id,
      options = leafletOptions(
        scrollWheelZoom = zoom,
        doubleClickZoom = zoom,
        touchZoom = zoom,
        boxZoom = zoom,
        zoomControl = FALSE,
        preferCanvas = TRUE
      )
    ) |>
      roboplotr_map_tilelayer(tile_opacity) |>
      roboplotr_map_rasterlayer(d, data_contour, map_opacity, robomap_palette) |>
      roboplotr_map_polygonlayer(data_contour, map_opacity, map_pal, border_width) |>
      roboplotr_map_markerlayer(d, markers)
    
    caption <- tags$span(
      style = str_glue(
        'opacity: 1; font-size: {getOption("roboplot.font.caption")$size}px;'
      ),
      htmltools::HTML(caption)
    )
    
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
          "margin-bottom" = 0,
          "height" = str_glue("{round(height / 20)}px")
        ),
        roboplotr_set_specific_css(
          "#{<robomap_id}_leaflet-container .map-info.legend span",
          "transform" = "rotate(180deg)"
        ),
        roboplotr_set_specific_css(
          "#{<robomap_id}_leaflet-container .map-info.legend g",
          "transform" = str_glue(
            "translate(0, {round(getOption('roboplot.font.main')$size*0.75)}px)"
          )
        ),
        roboplotr_set_specific_css(
          "#{<robomap_id}_leaflet-container .map-info.legend g line",
          "display" = "none"
        ),
        # roboplotr_set_specific_css(
        #   "#{<robomap_id}_leaflet-container .map-info.legend svg",
        #   "margin-top" = str_glue("{round(getOption('roboplot.font.main')$size)*0}px")
        # ),
        .open = "{<"
      )
    ))
    
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
    
    this_map <- this_map |>
      addEasyButton(
        easyButton(
          id = "reset-view",
          icon = "fa-home",
          title = "Kohdista",
          position = "topright",
          onClick = JS(
            "function(btn, map) {
          // Reset to the initial view
          map.setView(map.initialCenter, map.initialZoom);
        }"
          )
        )
      ) |>
      addEasyButton(
        easyButton(
          id = "download-map",
          icon = fa("file-image", vertical_align = "0em"),
          title = "Lataa kartta",
          position = "topright",
          onClick = JS(
            str_c(
              "function(btn, map) {
          // Check if html2canvas is already loaded
          if (typeof html2canvas === 'undefined') {
            // Load html2canvas script if it's not already included
            var scriptHtml2Canvas = document.createElement('script');
            scriptHtml2Canvas.src = 'https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js';
            document.head.appendChild(scriptHtml2Canvas);

            // Wait for the script to load before proceeding
            scriptHtml2Canvas.onload = function() {
              captureMap();
            };
          } else {
            captureMap();
          }

          // Function to capture the map and download as PNG
          function captureMap() {
            // Hide navigation controls before capture
            var controls = document.querySelectorAll('.leaflet-top.leaflet-right');
            controls.forEach(function(control) {
              control.style.display = 'none';
            });

            // Set the map container style to avoid shifting during capture
            var mapContainer = document.querySelector('.leaflet-container');
            var originalPosition = mapContainer.style.position;
            var originalTop = mapContainer.style.top;
            var originalLeft = mapContainer.style.left;
            mapContainer.style.position = 'absolute';
            mapContainer.style.top = '0px';
            mapContainer.style.left = '0px';

            // Use html2canvas to capture the map
            html2canvas(mapContainer, {
              useCORS: true,  // Enable CORS to capture tiles from other domains
              allowTaint: true
            }).then(function(canvas) {
              // Create a download link
              var link = document.createElement('a');
              link.href = canvas.toDataURL('image/png');
              link.download = '",
              roboplotr_string2filename(str_remove_all(title$title,"<[^>]*>")),
              ".png';  // Filename for the download
              link.click();  // Trigger the download
            }).finally(function() {
              // Restore visibility of navigation controls after capture
              controls.forEach(function(control) {
                control.style.display = 'flex';
              });
              // Restore the original position style of the map container
              mapContainer.style.position = originalPosition;
              mapContainer.style.top = originalTop;
              mapContainer.style.left = originalLeft;
            });
          }
        }
      "
            )
          )
        )
      )
    
    if (zoom) {
      this_map <- this_map |>
        addEasyButton(
          easyButton(
            # Add zoom controls manually
            id = "map-plus",
            icon = "fa-plus",
            title = "Lähennä",
            onClick = JS("function(btn, map){ map.zoomIn(); }"),
            position = "topright"
          )
        ) |>
        addEasyButton(
          easyButton(
            id = "map-minus",
            icon = "fa-minus",
            title = "Loitonna",
            onClick = JS("function(btn, map){ map.zoomOut(); }"),
            position = "topright"
          )
        )
    }
    
    this_map <-  this_map |>
      addEasyButton(
        easyButton(
          id = "robonomist-link",
          icon = '<svg version="1.1" viewBox="0 0 71.447 32" style = "width: 12pt;padding-bottom: 2px" xmlns="http://www.w3.org/2000/svg"><path transform="scale(.31159)"  d="M 229.3 53.2 L 174.3 90.1 L 174.3 69.1 L 199.5 53.2 L 174.3 37.3 L 174.3 16.3 M112 0c14.2 0 23.3 1.8 30.7 7 6.3 4.4 10.3 10.8 10.3 20.5 0 11.3-6.4 22.8-22.3 26.5l18.4 32.5c5 8.7 7.7 9.7 12.5 9.7v6.5h-27.3l-23.7-45.8h-7v27.6c0 10.5 0.7 11.7 9.9 11.7v6.5h-43.2v-6.7c10.3 0 11.3-1.6 11.3-11.9v-65.7c0-10.2-1-11.7-11.3-11.7v-6.7zm-4.8 7.9c-3.3 0-3.6 1.5-3.6 8.6v32.3h6.4c15.8 0 20.2-8.7 20.2-21.3 0-6.3-1.7-11.5-5-15-2.9-3-7-4.6-13-4.6z M 0 53.2 L 55 16.3 L 55 37.3 L 29.8 53.2 L 55 69.1 L 55 90.1"/></svg>',
          title = "Robonomist",
          onClick = JS(
            "function(btn, map) {window.open(\"https://robonomist.com\")  }"
          ),
          position = "topright"
        )
      ) |>
      addControl(
        html = HTML(map_title),
        position = "topleft",
        className = str_glue("robomap-title")
      ) |>
      addControl(
        control_style,
        position = "topleft",
        className = str_glue("{robomap_id}-info-control")
      ) |>
      addControl(
        html = robotable_logo(),
        position = "bottomright",
        className = str_glue("{robomap_id}-info robomap-logo")
      ) |>
      addControl(
        html = caption,
        position = "bottomleft",
        className = str_glue("map-info")
      )
    
    if (legend$position != "none") {
      this_map <- this_map |>
        addLegend(
          className = str_glue("map-info legend"),
          opacity = map_opacity,
          position = legend$position,
          pal = robomap_palette,
          labFormat = function(type, cuts, p) {
            # Custom format function
            roboplotr_format_robotable_numeric(bins, rounding = max(rounding -
                                                                           1, 0))  # Simply return the cuts without any formatting
          },
          values = ~ robomap.value,
          na.label = "",
          title = ""
        )
      
    }
    
    this_map <- structure(this_map, class = c(class(this_map), "roboplotr", "roboplotr.robomap"))
    
    this_map |>
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
  }
