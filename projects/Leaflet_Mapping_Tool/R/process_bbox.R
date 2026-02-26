# Process bounding box and clip income layer to study area.
# Uses income_gpkg (path to income GeoPackage, e.g. data/ses_index.gpkg or data/income.gpkg), writes to data/clipped_income/.
# Caches clipped layer by bbox UID in data/cache.gpkg (layer clipped_income) when available.

library(sf)
library(geosphere)
library(osmdata)
library(stringr)
if (dir.exists("R")) source("R/cache_utils.R") else source("cache_utils.R")

process_bbox <- function(study_area, income_gpkg, amalgamation_cities_list = NULL,
                         city_pr = NULL, custom_bbox = NULL, custom_distance_km = 0) {

  income_sf <- st_read(income_gpkg, quiet = TRUE)
  # Fix invalid polygons (e.g. duplicate vertices) so st_intersection doesn't fail
  if (!all(st_is_valid(income_sf))) {
    income_sf <- st_make_valid(income_sf)
  }

  expand_bbox <- function(bbox, distance_km) {
    lat_expansion <- distance_km / 111.32
    centroid_lat <- (bbox["y", "min"] + bbox["y", "max"]) / 2
    lon_expansion <- distance_km / (111.32 * cos(centroid_lat * pi / 180))
    matrix(
      c(
        bbox["x", "min"] - lon_expansion,
        bbox["y", "min"] - lat_expansion,
        bbox["x", "max"] + lon_expansion,
        bbox["y", "max"] + lat_expansion
      ),
      nrow = 2,
      dimnames = list(c("x", "y"), c("min", "max"))
    )
  }

  if (!is.null(custom_bbox)) {
    if (length(custom_bbox) != 4) stop("custom_bbox must be c(xmin, ymin, xmax, ymax)")
    bbox <- matrix(
      c(custom_bbox[1], custom_bbox[2], custom_bbox[3], custom_bbox[4]),
      nrow = 2,
      dimnames = list(c("x", "y"), c("min", "max"))
    )
  } else if (!is.null(city_pr)) {
    stop("Processing multiple cities (city_pr) is not supported with automatic bbox saving.")
  } else if (!is.null(amalgamation_cities_list)) {
    bboxes <- lapply(amalgamation_cities_list, getbb)
    bbox <- matrix(
      c(
        min(sapply(bboxes, function(bb) bb["x", "min"])),
        min(sapply(bboxes, function(bb) bb["y", "min"])),
        max(sapply(bboxes, function(bb) bb["x", "max"])),
        max(sapply(bboxes, function(bb) bb["y", "max"]))
      ),
      nrow = 2,
      dimnames = list(c("x", "y"), c("min", "max"))
    )
  } else {
    bbox <- getbb(study_area)
  }

  if (custom_distance_km > 0) bbox <- expand_bbox(bbox, custom_distance_km)

  assign("expanded_bbox", bbox, envir = .GlobalEnv)

  modified_text <- if (!is.null(study_area)) {
    str_replace_all(str_extract(study_area, "^[^,]+"), " ", "_")
  } else {
    "custom_bbox"
  }

  out_dir <- file.path(dirname(income_gpkg), "clipped_income")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, paste0(modified_text, ".gpkg"))

  # Use cached clipped layer for this bbox when available
  cached_clip <- get_cached_clipped_income(bbox)
  if (!is.null(cached_clip) && nrow(cached_clip) > 0) {
    st_write(cached_clip, out_path, delete_dsn = TRUE, quiet = TRUE)
    message("Using cached clipped income for this bbox.")
    return(bbox)
  }

  bbox_polygon <- st_as_sfc(st_bbox(
    c(xmin = bbox["x", "min"], ymin = bbox["y", "min"], xmax = bbox["x", "max"], ymax = bbox["y", "max"]),
    crs = st_crs(4326)
  ))

  clipped <- tryCatch(
    st_intersection(income_sf, bbox_polygon),
    error = function(e) {
      # s2 can fail on some boundary geometries; retry using planar (GEOS) intersection
      message("Retrying clip in planar CRS (s2 failed: ", conditionMessage(e), ")")
      utm_zone <- floor((bbox["x", "min"] + bbox["x", "max"]) / 2 + 180) %/% 6 + 1
      if (bbox["y", "min"] + bbox["y", "max"] > 0) {
        crs_planar <- st_crs(paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs"))
      } else {
        crs_planar <- st_crs(paste0("+proj=utm +zone=", utm_zone, " +south +datum=WGS84 +units=m +no_defs"))
      }
      income_planar <- st_transform(income_sf, crs_planar)
      bbox_planar <- st_transform(bbox_polygon, crs_planar)
      out <- st_intersection(income_planar, bbox_planar)
      st_transform(out, st_crs(4326))
    }
  )
  if (nrow(clipped) == 0) warning("Bbox does not intersect any income-layer polygons; clipped layer is empty. Use a larger area or different city.", call. = FALSE)

  st_write(clipped, out_path, delete_dsn = TRUE, quiet = TRUE)
  set_cached_clipped_income(bbox, clipped)

  return(bbox)
}
