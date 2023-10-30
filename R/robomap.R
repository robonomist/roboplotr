#' Raster layer for data contour maps
#'
#' @importFrom gstat idw
#' @importFrom leaflet addRasterImage
#' @importFrom progress progress_bar
#' @importFrom raster crs crs<- extent focal mask raster rasterize resample setExtent
#' @importFrom scales rescale
#' @importFrom sf st_area st_bbox st_crs st_sample st_sf st_union
#' @importFrom sp "coordinates<-" "gridded<-" proj4string "proj4string<-"
roboplotr_map_rasterlayer <- function(map, d, data_contour = F, opacity, robomap_palette) {
  if(data_contour == T) {

    d <- d %>% mutate(area = as.numeric(st_area(.)))

    d <- d %>% mutate(area = area / sum(area),
                      num_points = round(rescale(area, c(1,sqrt(sqrt(sqrt(max(d$area))))))))

    show.pb <- getOption("roboplot.verbose") == "All"

    if(show.pb) {
      bpb <- progress::progress_bar$new(total = (2*nrow(d))+3, format = ":info for contour map.. [:bar]")
    }

    interior_df <- d %>%
      mutate(sampled_points_interior = map2(geom, num_points, ~ {
        if(show.pb) { bpb$tick(token = list(info = "Determining boundaries")) }
        st_sample(.x, size = .y)
      })) %>%
      unnest(sampled_points_interior) %>%
      st_sf()

    boundary_df <- d %>%
      mutate(sampled_points_boundary = map2(geom, num_points, ~ {
        if(show.pb) { bpb$tick(token = list(info = "Determining boundaries")) }
        st_sample(.x, size = max(ceiling(.y * 0.2),1), type="regular")
        })) %>%
      unnest(sampled_points_boundary) %>%
      st_sf()

    sampled_df <- bind_rows(interior_df, boundary_df)

    sampled_sp <- as(sampled_df, "Spatial")

    proj4string(sampled_sp) <- st_crs(d)$proj4string

    bb <- st_bbox(d$geom)
    grd <- expand.grid(lon = seq(bb['xmin'], bb['xmax'], by = 0.1),
                       lat = seq(bb['ymin'], bb['ymax'], by = 0.1))
    coordinates(grd) <- ~lon+lat
    gridded(grd) <- TRUE
    proj4string(grd) <- proj4string(sampled_sp)
    if(show.pb) { bpb$tick(token = list(info = "Creating raster layer")) }
    capture.output(idw_model <- idw(formula = robomap.value ~ 1, locations = sampled_sp, newdata = grd))
    idw_raster <- raster(idw_model)

    if(show.pb) { bpb$tick(token = list(info = "Masking raster layer")) }

    muni_mask <- as(st_union(d),"Spatial")

    idw_masked <- mask(idw_raster, muni_mask)

    template_raster <- raster(extent(idw_masked), ncol=idw_masked@ncols*7, nrow=idw_masked@nrows*7)
    crs(template_raster) <- crs(idw_masked)
    higher_res <- resample(idw_masked, template_raster, method="bilinear")

    smoothed_raster <- focal(higher_res, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE)
    smoothed_clipped <- mask(smoothed_raster, muni_mask)

    map <- map %>%
      addRasterImage(smoothed_clipped, colors = robomap_palette, opacity = opacity)
    if(show.pb) { bpb$terminate() }
    map
  }
  else {
    map
  }
}

#' @importFrom leaflet addCircleMarkers
#' @importFrom scales rescale
roboplotr_map_markerlayer <- function(map, d, markers, size_scale = c(1,12)) {
  if (!markers) {
    map
  } else {
    if(!all(c("lon","lat") %in% names(d))) {
      stop("Currently robomap needs longitude and latitude as columns named \"lon\" and \"lat\"!", call. = F)
    }
    size_scale <- function(value) {
      rescale(value, to = size_scale)
    }
    map |>
      addCircleMarkers(
        lng = ~ lon,
        lat =  ~ lat,
        stroke = T,
        color = unique(getOption("roboplot.grid")[c("xcolor","ycolor")]) %>% first(),
        fillColor = getOption("roboplot.colors.background"),
        fillOpacity = 1,
        radius = ~size_scale(robomap.value),
        weight = unique(getOption("roboplot.grid")[c("xwidth","ywidth")]) %>% unlist() %>% max(),
        label = ~ leafletlabel
      )
  }
}

#' @importFrom leaflet addPolygons labelOptions
roboplotr_map_polygonlayer <- function(map, data_contour, map_opacity, robomap_palette, border_width) {

  map |>
    addPolygons(
      color = getOption("roboplot.trace.border")$color,
      weight = border_width,
      fillColor = ~ robomap_palette(robomap.value),#~ if (data_contour) { NULL } else { robomap.value },
      fillOpacity = ifelse(data_contour, 0, map_opacity),
      smoothFactor = 0.5,
      label = ~ leafletlabel,
      labelOptions = leaflet::labelOptions(
        style = list(
          "background" = getOption("roboplot.colors.background"),
          "font-size" = str_glue('{getOption("roboplot.font.main")$size}px'),
          "font-family" = getOption("roboplot.font.main")$family,
          "color" = roboplotr_text_color_picker(getOption("roboplot.colors.background")),
          "border" = str_glue('{max(getOption("roboplot.border")[c("xwidth","ywidth")] %>% unlist())}pt solid {first(unique(getOption("roboplot.border")[c("xcolor","ycolor")]))}')
        )
      )
      # weight = getOption("roboplot.trace.border")$width
    )
}

roboplotr_map_tilelayer <- function(map, tile_opacity, wrap = F) {
  if (tile_opacity > 0) {
    map |>
      addTiles(options = tileOptions(opacity = tile_opacity, noWrap = wrap))
  } else {
    map
  }
}

#' Automated leaflet maps.
#'
#' Wrapper for [leaflet::leaflet] for shorthand declaration of many map layout arguments.
#' Automates many formatting options.
#'
#' @param d Data frame. Data to be created a table from.
#' @param area Symbol, string, or function resulting in symbol or string. Variable from argument 'd' to use to identify the areas described by the map. This must be of class sfc_MULTIPOLYGON.
#' @param title,subtitle Characters. Labels for plot elements. Optionally, use [set_title()] for the title if you want to omit the title from the displayed plot, but include it for any downloads through the modebar.
#' @param caption Function or character. Use a string, or [set_caption()].
#' @param opacity,tile_opacity Double. Value from 0 to 1, defining how opaque the map polygon fill color or underlying map tiles are. 0 removes the tile layer, but retains the polygon borders if any.
#' @param map_palette Character. Colors used for heatmap color range. Must be a hexadecimal color strings or a valid css color strings.
#' @param hovertext List. Use a list with named items flag and unit.
#' @param tile_opacity Double. Use a value between 0 and 1 to set the opacity of the map tiles under the map polygons.
#' @param border_width Integer. The width of polygon borders. Default is the trace border width set with [set_roboplot_options()].
#' @param legend_cap Integer. The intended legend length. The actual length might vary.
#' @param data_contour Logical. Experimental. If TRUE, [robomap()] will produce a contour-like representation of the data, which does not conform to the boundaries of the polygons.
#' This provides a smoother transition and helps in visualizing general trends across regions. Default is FALSE.
#' @param markers Logical. Experimental. Whether markers will be added on the map based on the columns "lat" and "lon". Default is FALSE.
#' @return A list of classes "leaflet" and "htmlwidget"
#' @importFrom htmltools HTML tags
#' @importFrom leaflet addControl addLegend addPolygons addTiles colorBin colorNumeric colorQuantile leaflet tileOptions
#' @importFrom purrr map
#' @importFrom stringr str_glue str_remove
#' @export
#' @examples
#' # You can use roboplotr::robomap() to create html maps. Note that very large
#' # number of map polygons makes for slow rendering maps.
#'
#' vaesto_postinumeroittain |> robomap(Postinumeroalue)
#'
#' # Currently robomap() only supports very little customization.
#' vaesto_postinumeroittain |>
#'   dplyr::filter(Alue == "Espoo") |>
#'   robomap(Postinumeroalue, title = "Väestö Espoossa", caption = "Tilastokeskus")
#'
#' # Default polygon colors are picked from trace colors set with set_roboplot_options, based on luminosity.
#' # Control polygon colors with map_palette. robomap() expands upon this as necessary.
#'
#' vaesto_postinumeroittain |>
#'   robomap(
#'     Postinumeroalue,
#'     title = "Väestö postinumeroittain",
#'     subtitle = "Otanta",
#'     caption = "Tilastokeskus",
#'     map_palette = c("lightgreen", "darkred")
#'   )
#'
#' # robomap() automatically scales the values to differentiate large differences in maximum and minimum values.
#' # Control this with log_colors
#'
#' vaesto_postinumeroittain |>
#'   robomap(
#'     Postinumeroalue,
#'     title = "Väestö postinumeroittain",
#'     caption = "Tilastokeskus",
#'     map_palette = c("lightgreen", "darkred"),
#'     log_colors = F
#'   )
#'
#' # Control background tile opacity with 'tile_opacity', polygon opacity with 'map_opacity' and borders with 'border_width'.
#'
#' vaesto_postinumeroittain |>
#'   robomap(
#'     Postinumeroalue,
#'     title = "Väestö postinumeroittain",
#'     caption = "Tilastokeskus",
#'     map_palette = c("lightgreen", "darkred"),
#'     border_width = 2,
#'     tile_opacity = 0.2,
#'     map_opacity = 0.5
#'   )
#'
#' # Control the story you want to tell by giving an uneven palette.
#' biased_palette <- colorRampPalette(c("lightgreen", "darkred"))(12)[c(1,3,5,7:12)]
#' # This is how this biased palette looks like, more red colors than green.
#' scales::show_col(biased_palette)
#' # use the biased palette
#' vaesto_postinumeroittain |>
#'   robomap(
#'     Postinumeroalue,
#'     title = "Väestö postinumeroittain",
#'     caption = "Tilastokeskus",
#'     map_palette = biased_palette,
#'     map_opacity = 1,
#'     data_contour = T,
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
           legend_cap = 5,
           data_contour = F,
           markers = F,
           log_colors = NULL,
           rounding = 1
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

    roboplotr_check_param(
      map_palette,
      c("character"),
      size = NULL,
      extra = "robomap()"
    )

    if(!is.null(map_palette)) {
      if(all(is.character(map_palette))) {
        roboplotr_valid_colors(map_palette)
      }
    } else {
      roboplotr_colors <- getOption("roboplot.colors.traces")
      roboplotr.luminance <- getOption("roboplot.colors.traces") %>% roboplotr_get_luminance()
      map_palette <- c(
        roboplotr_colors[which(roboplotr.luminance == max(roboplotr.luminance))],
                         roboplotr_colors[which(roboplotr.luminance == min(roboplotr.luminance))]
      )
    }

    d <- d |>
      mutate(
        leafletlabel = str_c({
          {
            area
          }
        }, "<br>", roboplotr_format_robotable_numeric(value, rounding, flag = hovertext$flag), " ", {
          hovertext$unit
        }),
        leafletlabel = map(leafletlabel, HTML)
      )

    log_colors <- (function() {
      if(is.null(log_colors) & all(d$value >= 0)) {
        maxval <- round(max(d$value,na.rm = T)*1000)
        minval <- round(min(d$value,na.rm = T)*1000)
        magnitude_range <- nchar(maxval) - nchar(minval)
        magnitude_range > 3
      } else if(log_colors == T & any(d$value < 0)) {
        roboplotr_message("Some values are less than 0, unable to use 'log_colors == TRUE'.")
        F
      } else {
        log_colors
      }

    })()

    if(log_colors == T) {
      if(min(d$value) == 0) {
        d <- d |> mutate(robomap.value = log(value+1))
      } else {
        d <- d |> mutate(robomap.value = log(value))
      }
    } else {
      d <- d |> mutate(robomap.value = value)
    }

    get_bins <- function() {
      bins <-rev(seq(
        min(d$robomap.value),
        max(d$robomap.value),
        length.out = min(round(length(
          d$value |> unique()
        )), legend_cap)
      ))
      if(length(bins) == 1) {
        bins <- ceiling(bins)
      } else {
        .first <- ceiling(bins[1])
        .last <- floor(last(bins))
        bins <- round(bins)
        bins[1] <- .first
        bins[length(bins)] <- .last

      }

      bins
    }


    bins <- get_bins()

    robomap_palette <- roboplotr_get_map_palette(d, map_palette, data_contour, bins)

    robomap_id <- str_c("robomap-", str_remove(runif(1), "\\."))

    this_map <- leaflet(d,elementId = robomap_id) |>
      roboplotr_map_tilelayer(tile_opacity) |>
      roboplotr_map_rasterlayer(d, data_contour, map_opacity, robomap_palette) |>
      roboplotr_map_polygonlayer(data_contour, map_opacity, robomap_palette, border_width) |>
      roboplotr_map_markerlayer(d, markers)

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
          "width" = "fit-content",
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
      ),
      tags$style(
        roboplotr_set_specific_css(
          str_glue("#{robomap_id}_leaflet-container .leaflet-control-attribution, #{robomap_id}_leaflet-container .leaflet-control-attribution a"),
          "background" = getOption("roboplot.colors.background"),
          "font-size" = str_glue('{getOption("roboplot.font.caption")$size}px'),
          "font-family" = getOption("roboplot.font.caption")$family,
          "color" = getOption("roboplot.font.caption")$color
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


    round_magnitude <- function(vals, rounding) {

      map(vals, function(val) {
        num_digits <- nchar(abs(round(val,0)))
        if(num_digits == 1 | all(val < 1, val > -1)) {
          round(val, rounding)
        } else {
          .round_magnitude <- case_when(
            num_digits %in% c(1,2) ~ 1,
            num_digits == 3 ~ 10,
            num_digits == 4 ~ 100,
            num_digits == 5 ~ 1000,
            num_digits == 6 ~ 10000,
            TRUE ~ 100000
          )
          ceiling(val / .round_magnitude) * .round_magnitude
        }
      }) %>% unlist()

    }

    legend_title <-
      ifelse(
        subtitle == "",
        str_glue("{title$title}"),
        str_glue(
          "{title$title}<br><span style = 'font-size: 75%'>{subtitle}</span>"
        )
      )

    if (data_contour == T) {
      this_map <- this_map |>
        addLegend(
        className = str_glue("{robomap_id}-info legend"),
        na.label = "",
        pal = robomap_palette,
        opacity = 0.9,
        position = "bottomright",
        bins = legend_cap,
        values = ~ robomap.value,
        labFormat = function(type,cuts) {
          if(log_colors) {
            if(min(d$value) == 0) {
              cuts <- exp(cuts)-1
            } else {
              cuts <- exp(cuts)
            }
          }
          cuts <- round_magnitude(cuts, rounding)
          .mag <- round(max(cuts)) |> nchar()
          .rounding <- ifelse(.mag > 2, max(rounding-.mag, 0), rounding)
          labs <- roboplotr_format_robotable_numeric(cuts, .rounding)
          # print(labs)
          # labs <- labs[!duplicated(labs,fromLast = T)]
          labs
        },
        title = legend_title
      )
    } else if (length(bins) == 1) {
      this_map <- this_map |>
        addLegend(
          className = str_glue("{robomap_id}-info legend"),
          na.label = "",
          opacity = 0.9,
          position = "bottomright",
          labels = roboplotr_format_robotable_numeric(unique(d$value), rounding),
          colors = robomap_palette(d$value),
          values = ~ value,
          title = legend_title
        )
    } else {
      this_map <- this_map |>
        addLegend(
          className = str_glue("{robomap_id}-info legend"),
          na.label = "",
          opacity = 0.9,
          labFormat = function(type, cuts) {
            if(log_colors) {
              if(min(d$value) == 0) {
                cuts <- exp(cuts)-1
              } else {
                cuts <- exp(cuts)
              }
            }
            lo_end <- (tail(cuts,-1) - c(
              rep(-1, length(cuts) - 2), 0
            )) |> round_magnitude(rounding) |> roboplotr_format_robotable_numeric(rounding)
            hi_end <- head(cuts,-1) |> round_magnitude(rounding) |> roboplotr_format_robotable_numeric(rounding)
            str_glue("{lo_end} – {hi_end}") |>
              map(~ tags$span(.x, style = "white-space: nowrap;") |> as.character() |> HTML()) |>
              reduce(c)
          },
          position = "bottomright",
          pal = robomap_palette,
          values = ~ robomap.value,
          title = legend_title
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
