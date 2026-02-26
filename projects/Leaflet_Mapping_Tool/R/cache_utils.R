#
# GeoPackage cache: clipped income (by bbox), greenspace (by bbox), buffered DAs (by bbox + buffer_m).
# Cache path: data/cache.gpkg (layers: clipped_income, greenspace, da_buffers).
# UID is derived only from bbox coordinates (normalized); city is optional metadata.
#

library(sf)
library(dplyr)

# Default cache path (under project data/)
CACHE_GPKG <- "data/cache.gpkg"
CACHE_PRECISION <- 5L  # decimal places for bbox normalization

#' Generate a stable short UID from a bbox (matrix with x/y min/max or equivalent).
#' Same bbox always yields the same UID; city/label is not used.
#' @param bbox 2x2 matrix with rownames "x","y" and colnames "min","max", or length-4 vector c(xmin, ymin, xmax, ymax)
#' @return character UID
bbox_to_uid <- function(bbox) {
  if (is.matrix(bbox) && all(c("x", "y") %in% rownames(bbox)) && all(c("min", "max") %in% colnames(bbox))) {
    vec <- c(bbox["x", "min"], bbox["y", "min"], bbox["x", "max"], bbox["y", "max"])
  } else if (is.vector(bbox) && length(bbox) >= 4) {
    vec <- bbox[1:4]
  } else {
    stop("bbox must be a 2x2 matrix (x/y, min/max) or length-4 vector c(xmin, ymin, xmax, ymax)")
  }
  vec <- round(as.numeric(vec), CACHE_PRECISION)
  key <- paste(vec, collapse = "_")
  if (requireNamespace("digest", quietly = TRUE)) {
    return(substring(digest::digest(key, algo = "sha256"), 1L, 16L))
  }
  key <- gsub("[^0-9a-zA-Z._-]", "", key)
  if (nchar(key) > 64) key <- substring(key, 1L, 64L)
  key
}

#' Ensure cache file and layers exist (creates empty layers if missing).
ensure_cache <- function(path = CACHE_GPKG) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  wgs <- st_crs(4326)
  if (!file.exists(path)) {
    empty_geom <- st_sfc(crs = wgs)
    empty_green <- st_sf(
      bbox_uid = character(0),
      xmin = numeric(0), ymin = numeric(0), xmax = numeric(0), ymax = numeric(0),
      city = character(0), fetched_at = character(0),
      geometry = empty_geom
    )
    st_write(empty_green, path, layer = "greenspace", quiet = TRUE)
    empty_buf <- st_sf(
      bbox_uid = character(0), buffer_m = integer(0), city = character(0),
      fetched_at = character(0), da_row = integer(0),
      geometry = empty_geom
    )
    st_write(empty_buf, path, layer = "da_buffers", quiet = TRUE)
  }
  invisible(path)
}

#' Read cached merged greenspace for a bbox. Returns sf (one row, WGS84) or NULL if miss.
get_cached_greenspace <- function(bbox, path = CACHE_GPKG) {
  ensure_cache(path)
  uid <- bbox_to_uid(bbox)
  if (!"greenspace" %in% st_layers(path)$name) return(NULL)
  cached <- tryCatch(
    st_read(path, layer = "greenspace", quiet = TRUE),
    error = function(e) NULL
  )
  if (is.null(cached) || nrow(cached) == 0) return(NULL)
  hit <- cached[cached$bbox_uid == uid, ]
  if (nrow(hit) == 0) return(NULL)
  # Geometry column name can be "geom" when read from GeoPackage; use st_geometry to avoid select(geometry) failing
  st_sf(geometry = st_geometry(hit), crs = st_crs(cached))
}

#' Write merged greenspace to cache (one row per bbox). merged_sf: single geometry, WGS84 preferred.
set_cached_greenspace <- function(bbox, merged_sf, city = NULL, path = CACHE_GPKG) {
  ensure_cache(path)
  uid <- bbox_to_uid(bbox)
  if (is.matrix(bbox) && all(c("x", "y") %in% rownames(bbox)))
    coords <- c(bbox["x", "min"], bbox["y", "min"], bbox["x", "max"], bbox["y", "max"])
  else
    coords <- bbox[1:4]
  merged_sf <- st_transform(merged_sf, 4326)
  row <- st_sf(
    bbox_uid = uid,
    xmin = coords[1], ymin = coords[2], xmax = coords[3], ymax = coords[4],
    city = as.character(city %||% NA_character_),
    fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    geometry = st_geometry(merged_sf),
    crs = st_crs(4326)
  )
  existing <- tryCatch(st_read(path, layer = "greenspace", quiet = TRUE), error = function(e) NULL)
  if (!is.null(existing) && nrow(existing) > 0) {
    existing <- existing[existing$bbox_uid != uid, ]
    if (nrow(existing) > 0) {
      geom_col <- attr(existing, "sf_column")
      if (geom_col != "geometry") {
        names(existing)[names(existing) == geom_col] <- "geometry"
        attr(existing, "sf_column") <- "geometry"
      }
      existing <- existing[, names(row), drop = FALSE]
      row <- rbind(existing, row)
    }
  }
  st_write(row, path, layer = "greenspace", quiet = TRUE, append = FALSE)
  invisible(NULL)
}

#' Read cached buffered DAs for (bbox, buffer_m). Returns sf with da_row and geometry (WGS84) or NULL.
get_cached_buffers <- function(bbox, buffer_m, path = CACHE_GPKG) {
  ensure_cache(path)
  uid <- bbox_to_uid(bbox)
  if (!"da_buffers" %in% st_layers(path)$name) return(NULL)
  cached <- tryCatch(
    st_read(path, layer = "da_buffers", quiet = TRUE),
    error = function(e) NULL
  )
  if (is.null(cached) || nrow(cached) == 0) return(NULL)
  hit <- cached[cached$bbox_uid == uid & as.integer(cached$buffer_m) == as.integer(buffer_m), ]
  if (nrow(hit) == 0) return(NULL)
  # Geometry column name can be "geom" when read from GeoPackage
  st_sf(da_row = hit$da_row, geometry = st_geometry(hit), crs = st_crs(cached))
}

#' Write buffered DAs to cache. buffers_sf must have ._da_row and geometry (any CRS; stored as WGS84).
set_cached_buffers <- function(bbox, buffer_m, buffers_sf, city = NULL, path = CACHE_GPKG) {
  if (!"._da_row" %in% names(buffers_sf)) stop("buffers_sf must have ._da_row")
  ensure_cache(path)
  uid <- bbox_to_uid(bbox)
  buf <- buffers_sf %>% select(._da_row)
  buf$bbox_uid <- uid
  buf$buffer_m <- as.integer(buffer_m)
  buf$city <- as.character(city %||% NA_character_)
  buf$fetched_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  buf <- buf %>% rename(da_row = ._da_row)
  buf <- st_transform(buf, 4326)
  if (attr(buf, "sf_column") != "geometry") {
    names(buf)[names(buf) == attr(buf, "sf_column")] <- "geometry"
    attr(buf, "sf_column") <- "geometry"
  }
  existing <- tryCatch(st_read(path, layer = "da_buffers", quiet = TRUE), error = function(e) NULL)
  if (!is.null(existing) && nrow(existing) > 0) {
    existing <- existing[!(existing$bbox_uid == uid & as.integer(existing$buffer_m) == as.integer(buffer_m)), ]
    if (nrow(existing) > 0) {
      geom_col <- attr(existing, "sf_column")
      if (geom_col != "geometry") {
        names(existing)[names(existing) == geom_col] <- "geometry"
        attr(existing, "sf_column") <- "geometry"
      }
      common <- intersect(names(existing), names(buf))
      existing <- existing[, common, drop = FALSE]
      buf <- buf[, common, drop = FALSE]
      buf <- rbind(existing, buf)
    }
  }
  st_write(buf, path, layer = "da_buffers", quiet = TRUE, append = FALSE)
  invisible(NULL)
}

#' Read cached clipped income layer for a bbox. Returns sf (WGS84) or NULL if miss.
get_cached_clipped_income <- function(bbox, path = CACHE_GPKG) {
  ensure_cache(path)
  uid <- bbox_to_uid(bbox)
  if (!"clipped_income" %in% st_layers(path)$name) return(NULL)
  cached <- tryCatch(
    st_read(path, layer = "clipped_income", quiet = TRUE),
    error = function(e) NULL
  )
  if (is.null(cached) || nrow(cached) == 0) return(NULL)
  hit <- cached[cached$bbox_uid == uid, ]
  if (nrow(hit) == 0) return(NULL)
  hit$bbox_uid <- NULL
  hit
}

#' Write clipped income layer to cache (keyed by bbox UID). clipped_sf: output of st_intersection with bbox.
set_cached_clipped_income <- function(bbox, clipped_sf, path = CACHE_GPKG) {
  ensure_cache(path)
  uid <- bbox_to_uid(bbox)
  clipped_sf$bbox_uid <- uid
  existing <- NULL
  if ("clipped_income" %in% st_layers(path)$name) {
    existing <- tryCatch(
      st_read(path, layer = "clipped_income", quiet = TRUE),
      error = function(e) NULL
    )
  }
  if (!is.null(existing) && nrow(existing) > 0) {
    existing <- existing[existing$bbox_uid != uid, ]
    if (nrow(existing) > 0) {
      geom_col <- attr(existing, "sf_column")
      target_geom <- attr(clipped_sf, "sf_column")
      if (geom_col != target_geom) {
        names(existing)[names(existing) == geom_col] <- target_geom
        attr(existing, "sf_column") <- target_geom
      }
      existing <- existing[, names(clipped_sf), drop = FALSE]
      clipped_sf <- rbind(existing, clipped_sf)
    }
  }
  st_write(clipped_sf, path, layer = "clipped_income", quiet = TRUE, append = FALSE)
  invisible(NULL)
}

# Simple %||% for optional default
`%||%` <- function(x, y) if (is.null(x)) y else x
