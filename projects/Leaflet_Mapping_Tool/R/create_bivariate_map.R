#
# Bivariate map: Income × Green area per 1000 people (greenspace within walkable buffer). 3 classes each (±1 SD), 9 colours.
# Income = median total income of household (2020 $); classified Low/Med/High by study-area mean ± 1 SD.
# Requires: income GeoPackage (e.g. data/ses_index.gpkg from build_ses_index.R or data/income.gpkg) with 'population' and 'income_raw'; process_bbox is called automatically if needed.
# Usage: source("R/create_bivariate_map.R"); create_bivariate_map(city = "Halifax, NS", income_gpkg = "data/ses_index.gpkg")
#
# Popup: Income (median household $), Income Category, Greenspace (area, population, per 1000, category), Description.
#

if (dir.exists("R")) {
  source("R/map_config.R")
  source("R/process_bbox.R")
  source("R/process_green_da.R")
} else {
  source("map_config.R")
  source("process_bbox.R")
  source("process_green_da.R")
}

library(tidyverse)
library(sf)
library(leaflet)
library(htmltools)
library(htmlwidgets)

# 3x3 bivariate palette: Income × Green area (user-specified)
BIVAR_COLOURS <- c(
  "#F3F3F3", "#C2F1CE", "#8BE2AF",   # Income Low  × Green Low, Med, High
  "#EAC5DD", "#9EC6D3", "#7FC6B1",   # Income Med  × Green Low, Med, High
  "#E6A3D0", "#BC9FCE", "#7B8EAF"     # Income High × Green Low, Med, High
)
names(BIVAR_COLOURS) <- c(
  "Low-Low", "Low-Med", "Low-High",
  "Med-Low", "Med-Med", "Med-High",
  "High-Low", "High-Med", "High-High"
)

# Bivariate category descriptions (row = Income, col = Greenspace)
BIVAR_DESCRIPTIONS <- c(
  "Low-Low"  = "Low income with limited greenspace access.",
  "Low-Med"  = "Low income with moderate greenspace access.",
  "Low-High" = "Low income with high greenspace access.",
  "Med-Low"  = "Moderate income with low greenspace access.",
  "Med-Med"  = "Moderate income with moderate greenspace access.",
  "Med-High" = "Moderate income with high greenspace access.",
  "High-Low" = "High income with low greenspace access.",
  "High-Med" = "High income with moderate greenspace access.",
  "High-High"= "High income with high greenspace access."
)

# Convert hex colour to rgba with given alpha (for legend transparency)
hex_to_rgba <- function(hex, alpha = 0.6) {
  r <- strtoi(substr(hex, 2, 3), 16L)
  g <- strtoi(substr(hex, 4, 5), 16L)
  b <- strtoi(substr(hex, 6, 7), 16L)
  sprintf("rgba(%d,%d,%d,%.2f)", r, g, b, alpha)
}

# Build HTML for 3x3 legend: y-axis = Greenspace (dependent), x-axis = Income (independent).
# Grid: rows = Green High/Med/Low, cols = Income Low/Med/High. Colour at (green_row, income_col) = same (income, green) combo.
bivariate_legend_html <- function(fill_alpha = 1) {
  # BIVAR_COLOURS indexed as (income_row, green_col); income_row 1=Low,2=Med,3=High; green_col 1=Low,2=Med,3=High.
  # Legend rows = Green (1=High, 2=Med, 3=Low), cols = Income (1=Low, 2=Med, 3=High). So colour idx = (income_col - 1)*3 + (4 - green_row).
  cell <- function(green_row, income_col) {
    i <- (income_col - 1L) * 3L + (4L - green_row)
    col_rgba <- hex_to_rgba(BIVAR_COLOURS[i], fill_alpha)
    sprintf('<div style="width:26px;height:26px;background:%s;border:1px solid #555;"></div>', col_rgba)
  }
  y_label_rotated <- '<div style="writing-mode:vertical-rl;transform:rotate(-180deg);font-size:9px;height:78px;line-height:1.35;text-align:center;">Green area per<br>1,000 people</div>'
  rows <- c(
    paste0('<tr><td rowspan="3" style="vertical-align:middle;width:1px;padding:0 4px 0 0;">', y_label_rotated, '</td>',
           '<td style="text-align:right;padding-right:4px;font-size:9px;vertical-align:middle;">High</td>',
           '<td>', cell(1, 1), '</td><td>', cell(1, 2), '</td><td>', cell(1, 3), '</td></tr>'),
    paste0('<tr><td style="text-align:right;padding-right:4px;font-size:9px;vertical-align:middle;">Med</td>',
           '<td>', cell(2, 1), '</td><td>', cell(2, 2), '</td><td>', cell(2, 3), '</td></tr>'),
    paste0('<tr><td style="text-align:right;padding-right:4px;font-size:9px;vertical-align:middle;">Low</td>',
           '<td>', cell(3, 1), '</td><td>', cell(3, 2), '</td><td>', cell(3, 3), '</td></tr>'),
    '<tr><td></td><td></td><td style="text-align:center;font-size:9px;padding-top:2px;">Low</td><td style="text-align:center;font-size:9px;padding-top:2px;">Med</td><td style="text-align:center;font-size:9px;padding-top:2px;">High</td></tr>',
    '<tr><td></td><td></td><td colspan="3" style="text-align:center;font-size:9px;padding-top:4px;line-height:1.35;">Median Household<br>Income</td></tr>'
  )
  paste0(
    '<div class="leaflet-bivariate-legend" style="z-index:9999;position:relative;background:white;padding:10px;border-radius:4px;box-shadow:0 1px 5px rgba(0,0,0,0.3);max-width:260px;">',
    '<div style="font-weight:bold;font-size:11px;margin-bottom:6px;">Exploring how median household income influences greenspace access by dissemination area, 2020</div>',
    '<div style="font-size:9px;margin-bottom:8px;line-height:1.35;"><b>Interpretation:</b><br>',
    '<b>Blues:</b> Equal greenspace access to median household income.<br>',
    '<b>Greens:</b> Equitable greenspace access for DAs with low median household income.<br>',
    '<b>Pinks:</b> Low greenspace access for DAs with high median household income.</div>',
    '<table style="border-collapse:collapse;"><tbody>', paste(rows, collapse = ""), '</tbody></table>',
    '</div>'
  )
}

# For testing: write the bivariate legend to a standalone HTML file and optionally open it.
# Usage: source("R/create_bivariate_map.R"); preview_legend()
# Edit bivariate_legend_html() or BIVAR_COLOURS, then run preview_legend() again to refresh.
preview_legend <- function(open = TRUE, file = "output/maps/legend_preview.html") {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  html <- paste0(
    "<!DOCTYPE html><html><head><meta charset='UTF-8'></head><body style='margin:20px;'>",
    bivariate_legend_html(),
    "</body></html>"
  )
  writeLines(html, file)
  message("Legend written to ", normalizePath(file, mustWork = FALSE))
  if (open) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      rstudioapi::viewer(file)
    } else {
      utils::browseURL(file)
    }
  }
  invisible(file)
}

create_bivariate_map <- function(city, income_gpkg, amalgamation_cities_list = NULL, city_pr = NULL,
                                 custom_bbox = NULL, custom_distance_km = 0, green_buffer_m = 500,
                                 show_green_layers = FALSE) {
  # green_buffer_m: for each DA, greenspace is summed inside the DA plus within this distance (m) of the DA. Zone = DA + buffer (default 500 m).

  message("Step 1 of 5: Processing bounding box and clipping income layer...")
  expanded_bbox <- process_bbox(
    study_area = city,
    income_gpkg = income_gpkg,
    amalgamation_cities_list = amalgamation_cities_list,
    city_pr = city_pr,
    custom_bbox = custom_bbox,
    custom_distance_km = custom_distance_km
  )

  modified_text <- str_replace_all(str_extract(city, "^[^,]+"), " ", "_")
  income_dir <- file.path(dirname(income_gpkg), "clipped_income")
  city_income_path <- file.path(income_dir, paste0(modified_text, ".gpkg"))
  if (!file.exists(city_income_path)) stop("Clipped income layer not found: ", city_income_path, " Run process_bbox first (e.g. by running this function once with the same city).")

  city_shp <- st_read(city_income_path, quiet = TRUE)
  if (nrow(city_shp) == 0) stop("No polygons in study area.")
  if (!"population" %in% names(city_shp)) stop("Clipped layer missing 'population'. Ensure income layer includes CHARACTERISTIC_ID 1 for total population.")

  message("Step 2 of 5: Computing green area per DA (OSM, buffer ", green_buffer_m, " m)...")
  city_shp <- process_green_da(city_shp, expanded_bbox, buffer_m = green_buffer_m, city = city)
  n_na_green <- sum(is.na(city_shp$green_area_per_1000))
  if (n_na_green == nrow(city_shp) && nrow(city_shp) > 0) {
    warning(
      "Green area per 1,000 people is NA for all DAs. Likely cause: 'population' is missing or zero in the census. ",
      "Ensure your income layer includes CHARACTERISTIC_ID 1 (Total population), then process_bbox and this map.",
      call. = FALSE
    )
  }

  # Prefer income_raw (median household $); fallback to ses_idx or index if present (e.g. from build_ses_index.R)
  idx_col <- intersect(c("ses_idx", "income_raw", "index"), names(city_shp))[1]
  if (is.na(idx_col)) idx_col <- names(city_shp)[sapply(city_shp, is.numeric)][1]
  city_shp$plot_income <- city_shp[[idx_col]]
  if ("income_raw" %in% names(city_shp)) {
    city_shp$plot_income[is.na(city_shp$income_raw)] <- NA_real_
  }
  green_var <- "green_area_per_1000"

  # 3 classes each: ±1 SD (Low < mean - sd, High > mean + sd, else Medium)
  mean_income   <- mean(city_shp$plot_income, na.rm = TRUE)
  sd_income     <- sd(city_shp$plot_income, na.rm = TRUE)
  mean_green <- mean(city_shp[[green_var]], na.rm = TRUE)
  sd_green   <- sd(city_shp[[green_var]], na.rm = TRUE)
  if (is.na(sd_income) || sd_income == 0) sd_income <- 1e-9
  if (is.na(sd_green) || sd_green == 0) sd_green <- 1e-9

  city_shp <- city_shp %>%
    mutate(
      class_income   = case_when(
        is.na(plot_income) ~ NA_character_,
        plot_income < mean_income - sd_income ~ "Low",
        plot_income > mean_income + sd_income ~ "High",
        TRUE ~ "Med"
      ),
      class_green = case_when(
        .data[[green_var]] < mean_green - sd_green ~ "Low",
        .data[[green_var]] > mean_green + sd_green ~ "High",
        TRUE ~ "Med"
      )
    ) %>%
    mutate(
      class_green = replace_na(class_green, "Low"),
      bivar = if_else(is.na(class_income) | is.na(class_green), NA_character_,
                      paste(class_income, class_green, sep = "-"))
    )
  city_shp$bivar_colour <- BIVAR_COLOURS[city_shp$bivar]
  city_shp$bivar_colour[is.na(city_shp$bivar_colour)] <- "#CBCBCB"
  city_shp$bivar_opacity <- if_else(is.na(city_shp$bivar), 0, 0.6)
  # Normal stroke for non-extreme DAs; inner stroke only for extreme (corner) DAs
  BIVAR_EXTREMES <- c("Low-Low", "High-High", "High-Low", "Low-High")
  city_shp$bivar_edge_color <- if_else(
    is.na(city_shp$bivar), "transparent",
    if_else(city_shp$bivar %in% BIVAR_EXTREMES, "#FFBB00", "white")
  )
  city_shp$bivar_edge_weight <- if_else(is.na(city_shp$bivar), 0L, if_else(city_shp$bivar %in% BIVAR_EXTREMES, 1.5, 0.5))
  city_shp$polygon_stroke_color <- if_else(
    is.na(city_shp$bivar), "transparent",
    if_else(city_shp$bivar %in% BIVAR_EXTREMES, "transparent", "white")
  )
  city_shp$polygon_stroke_weight <- if_else(is.na(city_shp$bivar), 0L, if_else(city_shp$bivar %in% BIVAR_EXTREMES, 0L, 0.5))

  bbox_polygon <- st_as_sfc(st_bbox(c(
    xmin = expanded_bbox["x", "min"], ymin = expanded_bbox["y", "min"],
    xmax = expanded_bbox["x", "max"], ymax = expanded_bbox["y", "max"]
  ), crs = st_crs(4326)))

  message("Step 3 of 5: Building Income × Greenspace map...")
  green_na <- is.na(city_shp[[green_var]])

  fmt_km2 <- function(x) format(round(x / 1e6, 1), big.mark = ",", nsmall = 1, trim = TRUE)
  fmt_pop <- function(x) if_else(is.na(x) | !is.finite(x), "\u2014", format(as.integer(x), big.mark = ",", trim = TRUE))

  green_km2_str <- fmt_km2(city_shp$green_area_m2)
  green_per_1000_km2 <- city_shp[[green_var]] / 1e6
  green_per_1000_str <- if_else(
    green_na,
    if_else(!is.finite(city_shp$population) | city_shp$population <= 0,
            "No data (missing or zero population)", "No data"),
    paste0(format(round(green_per_1000_km2, 1), big.mark = ",", nsmall = 1, trim = TRUE), " km\u00b2")
  )

  fmt_income <- function(x) if_else(is.na(x) | !is.finite(x), "\u2014", paste0("$", format(round(x, 0), big.mark = ",", trim = TRUE)))
  has_income_raw <- "income_raw" %in% names(city_shp)

  bivar_desc <- BIVAR_DESCRIPTIONS[city_shp$bivar]
  bivar_desc[is.na(bivar_desc)] <- ""

  city_shp <- city_shp %>%
    mutate(
      popup_text = paste0(
        "<b style='font-size:13px;'>Income</b><br>",
        "<b>Median household income:</b> ", if (has_income_raw) fmt_income(income_raw) else fmt_income(plot_income), "<br>",
        "<b>Income Category:</b> ", if_else(is.na(class_income), "\u2014", class_income),
        "<br><br>",
        "<b style='font-size:13px;'>Greenspace</b><br>",
        "<b>Green area (in buffer):</b> ", green_km2_str, " km\u00b2<br>",
        "<b>Population:</b> ", fmt_pop(population), "<br>",
        "<b>Green area per 1,000 people:</b> ", green_per_1000_str, "<br>",
        "<b>Green area Category:</b> ", if_else(is.na(class_green), "\u2014", class_green),
        "<br><br>",
        "<b>Description:</b> ", bivar_desc
      )
    )

  # Inner-boundary stroke so outline is drawn inward and always visible (no overlap at shared edges)
  utm_zone <- floor((expanded_bbox["x", "min"] + expanded_bbox["x", "max"]) / 2 + 180) %/% 6 + 1
  crs_planar <- if (expanded_bbox["y", "min"] + expanded_bbox["y", "max"] > 0) {
    st_crs(paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs"))
  } else {
    st_crs(paste0("+proj=utm +zone=", utm_zone, " +south +datum=WGS84 +units=m +no_defs"))
  }
  inner_offset_m <- 3
  city_utm_geom <- st_transform(st_geometry(city_shp), crs_planar)
  inner_buf <- st_buffer(city_utm_geom, -inner_offset_m)
  inner_boundary <- st_boundary(inner_buf)
  empty_idx <- st_is_empty(inner_boundary)
  if (any(empty_idx)) {
    orig_boundary <- st_boundary(city_utm_geom)
    inner_boundary[empty_idx] <- orig_boundary[empty_idx]
  }
  inner_boundary_wgs84 <- st_transform(inner_boundary, 4326)
  city_shp_inner <- st_sf(
    bivar_edge_color = city_shp$bivar_edge_color,
    bivar_edge_weight = city_shp$bivar_edge_weight,
    geometry = inner_boundary_wgs84,
    crs = st_crs(4326)
  )
  extreme_idx <- city_shp$bivar %in% BIVAR_EXTREMES
  city_shp_inner_extreme <- city_shp_inner[extreme_idx, ]

  map_leaflet <- leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
    fitBounds(
      lng1 = expanded_bbox["x", "min"], lat1 = expanded_bbox["y", "min"],
      lng2 = expanded_bbox["x", "max"], lat2 = expanded_bbox["y", "max"]
    ) %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light") %>%
    addTiles(group = "Street") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addPolygons(
      data = bbox_polygon,
      color = "black", weight = 2, fill = FALSE, label = "Study area",
      group = "Study area"
    ) %>%
    addPolygons(
      data = city_shp,
      fillColor = ~bivar_colour,
      color = ~polygon_stroke_color,
      weight = ~polygon_stroke_weight,
      fillOpacity = ~bivar_opacity,
      popup = ~popup_text,
      group = "Income × Green"
    )
  if (nrow(city_shp_inner_extreme) > 0) {
    map_leaflet <- map_leaflet %>%
      addPolylines(
        data = city_shp_inner_extreme,
        color = ~bivar_edge_color,
        weight = ~bivar_edge_weight,
        opacity = 1,
        group = "Income × Green"
      )
  }

  # Merged greenspace (one lightweight unioned layer for display)
  message("Fetching merged greenspace layer (OSM) for display...")
  merged_green <- fetch_greenspace_merged(expanded_bbox, city = city)
  if (is.null(merged_green) || nrow(merged_green) == 0) {
    message("  No merged greenspace layer returned (OSM query may have failed or been rate limited). Map will show DAs only.")
  }
  if (!is.null(merged_green) && nrow(merged_green) > 0) {
    map_leaflet <- map_leaflet %>%
      addPolygons(
        data = merged_green,
        fillColor = "#228B22", fillOpacity = 0.4, color = "#006400", weight = 1,
        group = "Greenspace (merged)"
      )
  }

  # Per-type greenspace layers (optional; disabled by default because thousands of polygons
  # make the HTML huge and can prevent the layer control from rendering)
  green_group_names <- character(0)
  if (show_green_layers) {
    message("Fetching greenspace layers by OSM type (show_green_layers = TRUE)...")
    green_layers <- fetch_greenspace_layers(expanded_bbox)
    for (i in seq_along(green_layers)) {
      grp <- green_layers[[i]]$group
      sf_layer <- green_layers[[i]]$sf
      map_leaflet <- map_leaflet %>%
        addPolygons(
          data = sf_layer,
          fillColor = "#228B22", fillOpacity = 0.35, color = "#006400", weight = 1,
          group = grp
        )
      green_group_names <- c(green_group_names, grp)
    }
  }

  overlay_grps <- c("Income × Green", "Study area", if (!is.null(merged_green) && nrow(merged_green) > 0) "Greenspace (merged)", green_group_names)

  opacity_slider_html <- paste0(
    '<div class="leaflet-opacity-control" style="background:white;padding:8px 10px;border-radius:4px;box-shadow:0 1px 5px rgba(0,0,0,0.3);min-width:140px;">',
    '<label style="display:block;font-size:11px;font-weight:bold;margin-bottom:4px;">Overlay opacity</label>',
    '<input type="range" min="0" max="100" value="60" data-overlay-opacity-slider style="width:100%;cursor:pointer;">',
    '</div>'
  )

  map_leaflet <- map_leaflet %>%
    addLayersControl(
      position = "topright",
      baseGroups = c("Light", "Street", "Satellite"),
      overlayGroups = overlay_grps,
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addControl(
      html = HTML(bivariate_legend_html()),
      position = "bottomleft"
    ) %>%
    addControl(
      html = HTML(opacity_slider_html),
      position = "bottomright"
    ) %>%
    htmlwidgets::onRender(
      "function(el, x) {
        var w = HTMLWidgets.find('#' + el.id);
        if (!w || typeof w.getMap !== 'function') return;
        var map = w.getMap();
        if (!map) return;
        var lm = map.layerManager;
        if (!lm || typeof lm.getLayerGroup !== 'function') return;
        var overlayGroups = ['Study area', 'Income × Green', 'Greenspace (merged)'];
        function applyOpacityToGroup(groupName, v) {
          var group = lm.getLayerGroup(groupName, false);
          if (group && group.eachLayer) {
            group.eachLayer(function(l) {
              if (l.setStyle) l.setStyle({ fillOpacity: v, opacity: v });
            });
          }
        }
        function setOverlayOpacity(pct) {
          var v = Math.max(0, Math.min(1, pct / 100));
          overlayGroups.forEach(function(name) { applyOpacityToGroup(name, v); });
        }
        function init() {
          function hideGreenspace() {
            if (typeof map.hideGroup === 'function') {
              map.hideGroup('Greenspace (merged)');
            } else {
              var g = lm.getLayerGroup('Greenspace (merged)', false);
              if (g && map.hasLayer && map.hasLayer(g)) map.removeLayer(g);
            }
          }
          hideGreenspace();
          setTimeout(hideGreenspace, 100);
          var slider = el.querySelector('input[data-overlay-opacity-slider]');
          if (slider) {
            setOverlayOpacity(parseInt(slider.value, 10));
            slider.addEventListener('input', function() {
              setOverlayOpacity(parseInt(slider.value, 10));
            });
            var control = slider.closest('.leaflet-opacity-control') || slider.parentElement;
            if (control) {
              ['mousedown', 'touchstart', 'click', 'dblclick'].forEach(function(ev) {
                control.addEventListener(ev, function(e) { e.stopPropagation(); }, true);
              });
            }
          }
        }
        if (map.whenReady) {
          map.whenReady(init);
        } else {
          setTimeout(init, 150);
        }
      }"
    )

  message("Step 4 of 5: Saving map...")
  out_dir <- "output/maps"
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(normalizePath(out_dir, mustWork = FALSE), paste0(modified_text, "_bivariate.html"))
  saveWidget(map_leaflet, file = out_file, selfcontained = FALSE)
  message("Step 5 of 5: Done. Map saved to ", out_file)
  message("  Open the HTML in a browser; keep the _files/ folder next to it.")
  invisible(map_leaflet)
}
