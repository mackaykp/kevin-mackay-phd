#
# Green area per DA (and per 1000 people). Uses multiple OSM greenspace types and optional
# walkable buffer around each DA.
# Input: da_sf (clipped DAs with 'population'), bbox (matrix x/y min/max, WGS84).
# Optional: buffer_m = walkable distance in metres (e.g. 500 m).
#
# For each DA, greenspace area is summed over: (1) the DA itself, plus (2) the area
# within buffer_m of the DA boundary. So the zone is "inside the DA + within X m of
# the DA" (st_buffer(DA, buffer_m) gives exactly that). This reflects access to
# greenspace both inside and just outside the DA (reduces MAUP; helps edge DAs).
# OSM greenspace is fetched for bbox only; use a larger bbox (e.g. custom_distance_km)
# if you want full counts where the buffer extends beyond the study area.
#
# Returns: da_sf with green_area_m2 and green_area_per_1000 added.
#

library(sf)
library(dplyr)
library(osmdata)
if (dir.exists("R")) source("R/cache_utils.R") else source("cache_utils.R")

# Check Overpass/OSM error message for rate limiting or known API errors; warn and return TRUE if so
warn_if_overpass_error <- function(e) {
  msg <- conditionMessage(e)
  if (!is.character(msg) || length(msg) == 0) return(FALSE)
  rate_limit <- grepl("429|503|rate limit|rate-limited|too many requests|service unavailable|timeout|timed out|overpass", msg, ignore.case = TRUE)
  if (rate_limit) {
    warning(
      "OSM/Overpass API may be rate limiting or overloaded. ",
      "Error: ", msg, " ",
      "Wait a few minutes and re-run, or use set_overpass_url() in osmdata to try another server.",
      call. = FALSE
    )
    return(TRUE)
  }
  FALSE
}

# OSM key/value pairs for public greenspace (parks, gardens, forests, meadows, etc.)
# OSM key/value pairs merged into the greenspace layer (polygons + multipolygons)
OSM_GREEN_FEATURES <- list(
  list(key = "leisure", value = "dog_park"),
  list(key = "leisure", value = "garden"),
  list(key = "leisure", value = "park"),
  list(key = "leisure", value = "pitch"),
  list(key = "landcover", value = "grass"),
  list(key = "landuse", value = "meadow"),
  list(key = "landuse", value = "recreation_ground"),
  list(key = "landuse", value = "village_green"),
  list(key = "landuse", value = "grass"),
  list(key = "landuse", value = "greenery"),
  list(key = "landuse", value = "allotments"),
  list(key = "natural", value = "heath"),
  list(key = "natural", value = "scrub"),
  list(key = "natural", value = "wetland"),
  list(key = "natural", value = "grassland"),
  list(key = "natural", value = "shrubbery"),
  list(key = "natural", value = "tundra"),
  list(key = "natural", value = "wood")
)

process_green_da <- function(da_sf, bbox, buffer_m = 2000, city = NULL) {
  if (is.null(da_sf) || nrow(da_sf) == 0 || !"population" %in% names(da_sf)) {
    da_sf$green_area_m2 <- NA_real_
    da_sf$green_area_per_1000 <- NA_real_
    return(da_sf)
  }
  # Coerce population to numeric (GPKG/read_sf can sometimes return character)
  da_sf$population <- suppressWarnings(as.numeric(da_sf$population))
  bbox_vec <- c(bbox["x", "min"], bbox["y", "min"], bbox["x", "max"], bbox["y", "max"])

  # Planar CRS for area (m²) and buffer
  utm_zone <- floor((bbox["x", "min"] + bbox["x", "max"]) / 2 + 180) %/% 6 + 1
  if (bbox["y", "min"] + bbox["y", "max"] > 0) {
    crs_planar <- st_crs(paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs"))
  } else {
    crs_planar <- st_crs(paste0("+proj=utm +zone=", utm_zone, " +south +datum=WGS84 +units=m +no_defs"))
  }

  da_sf$._da_row <- seq_len(nrow(da_sf))
  da_utm <- st_transform(da_sf %>% select(._da_row), crs_planar)

  # --- Greenspace: try cache (by bbox UID), else fetch OSM and merge ---
  greens_union <- NULL
  greens_cached <- get_cached_greenspace(bbox)
  if (!is.null(greens_cached) && nrow(greens_cached) > 0) {
    greens_utm <- st_transform(greens_cached, crs_planar)
    if (!all(st_is_valid(greens_utm))) greens_utm <- st_make_valid(greens_utm)
    greens_union <- st_union(st_geometry(greens_utm))
    if (length(greens_union) > 1) greens_union <- st_union(greens_union)
    message("Using cached greenspace for this bbox. Green area uses buffer ", buffer_m, " m.")
  }

  if (is.null(greens_union)) {
    # Fetch all greenspace polygon types and combine (polygons + multipolygons so e.g. natural=wood is included)
    collect_polygons <- function(key, value) {
      tryCatch({
        od <- opq(bbox_vec) %>%
          add_osm_feature(key = key, value = value) %>%
          osmdata_sf()
        polys <- od$osm_polygons
        multi <- od$osm_multipolygons
        if (!is.null(multi) && nrow(multi) > 0) {
          multi <- st_cast(multi %>% select(geometry), "POLYGON", warn = FALSE)
          if (is.null(polys) || nrow(polys) == 0) polys <- multi
          else polys <- rbind(polys %>% select(geometry), multi)
        }
        if (is.null(polys) || nrow(polys) == 0) NULL else polys
      }, error = function(e) {
        warn_if_overpass_error(e)
        NULL
      })
    }
    poly_list <- lapply(OSM_GREEN_FEATURES, function(f) collect_polygons(f$key, f$value))
    poly_list <- poly_list[!sapply(poly_list, is.null)]
    poly_list <- poly_list[sapply(poly_list, nrow) > 0]

    if (length(poly_list) == 0) {
      warning(
        "No OSM greenspace polygons returned for this bbox. ",
        "If you ran this repeatedly, Overpass may be rate limiting; wait a few minutes and try again.",
        call. = FALSE
      )
      da_sf$green_area_m2 <- 0
      da_sf$green_area_per_1000 <- 0
      da_sf$._da_row <- NULL
      return(da_sf)
    }

    greens_sf <- do.call(rbind, lapply(poly_list, function(x) x %>% select(geometry)))
    if (!all(st_is_valid(greens_sf))) greens_sf <- st_make_valid(greens_sf)
    greens_utm <- st_transform(greens_sf, crs_planar)
    if (!all(st_is_valid(greens_utm))) greens_utm <- st_make_valid(greens_utm)
    greens_geom <- st_geometry(greens_utm)

    n_poly <- nrow(greens_utm)
    greens_union <- tryCatch(
      st_union(greens_geom),
      error = function(e) {
        message("st_union failed (", conditionMessage(e), "); trying buffer(0) to fix topology...")
        fixed <- tryCatch(st_buffer(greens_utm, 0), error = function(e2) greens_utm)
        if (!all(st_is_valid(fixed))) fixed <- st_make_valid(fixed)
        tryCatch(
          st_union(st_geometry(fixed)),
          error = function(e2) {
            message("buffer(0) union failed; trying chunked union...")
            chunk <- 200L
            idx <- seq_len(nrow(fixed))
            chunks <- split(idx, ceiling(idx / chunk))
            parts <- lapply(chunks, function(i) st_union(st_geometry(fixed[i, ])))
            Reduce(st_union, parts)
          }
        )
      }
    )
    if (length(greens_union) > 1) greens_union <- st_union(greens_union)
    message("Merged ", n_poly, " greenspace polygons into one layer (overlaps counted once). Green area uses buffer ", buffer_m, " m.")
    # Cache merged greenspace (WGS84)
    greens_sf_planar <- st_sf(geometry = st_sfc(greens_union, crs = crs_planar))
    set_cached_greenspace(bbox, st_transform(greens_sf_planar, 4326), city = city)
  }

  # --- Buffered DAs: try cache (by bbox UID + buffer_m), else compute ---
  cached_buf <- get_cached_buffers(bbox, buffer_m)
  use_cached_buf <- FALSE
  if (!is.null(cached_buf) && nrow(cached_buf) > 0 &&
      all(da_sf$._da_row %in% cached_buf$da_row) && nrow(cached_buf) == nrow(da_sf)) {
    geoms <- st_geometry(st_transform(cached_buf, crs_planar))
    da_utm <- st_sf(
      ._da_row = da_sf$._da_row,
      geometry = geoms[match(da_sf$._da_row, cached_buf$da_row)],
      crs = crs_planar
    )
    use_cached_buf <- TRUE
    message("Using cached buffered DAs for this bbox and buffer ", buffer_m, " m.")
  }

  # Zone for summing greenspace = DA plus buffer (so greenspace inside DA + within buffer_m counts)
  if (!use_cached_buf && buffer_m > 0) {
    da_utm <- st_buffer(da_utm, buffer_m)
    set_cached_buffers(bbox, buffer_m, da_utm, city = city)
  }

  int <- tryCatch({
    st_intersection(da_utm, greens_union)
  }, error = function(e) NULL)

  if (is.null(int) || nrow(int) == 0) {
    da_sf$green_area_m2 <- 0
    da_sf$green_area_per_1000 <- 0
    da_sf$._da_row <- NULL
    return(da_sf)
  }

  int$area_m2 <- as.numeric(st_area(int))
  agg <- int %>%
    st_drop_geometry() %>%
    group_by(._da_row) %>%
    summarise(green_area_m2 = sum(area_m2, na.rm = TRUE), .groups = "drop")
  da_sf <- da_sf %>%
    left_join(agg, by = "._da_row") %>%
    mutate(green_area_m2 = replace_na(green_area_m2, 0))
  da_sf$._da_row <- NULL

  # Green area per 1000 people: (m²) * 1000 / population
  pop <- da_sf$population
  pop[!is.finite(pop) | pop <= 0] <- NA
  da_sf$green_area_per_1000 <- (da_sf$green_area_m2 * 1000) / pop
  da_sf$green_area_per_1000[!is.finite(da_sf$green_area_per_1000)] <- NA_real_

  da_sf
}

# Fetch greenspace polygons by OSM type for toggleable map layers (WGS84).
# Includes both osm_polygons and osm_multipolygons (e.g. natural=wood) so nothing is missed.
fetch_greenspace_layers <- function(bbox) {
  bbox_vec <- c(bbox["x", "min"], bbox["y", "min"], bbox["x", "max"], bbox["y", "max"])
  out <- list()
  for (f in OSM_GREEN_FEATURES) {
    label <- paste0(f$key, ": ", f$value)
    poly <- tryCatch({
      od <- opq(bbox_vec) %>% add_osm_feature(key = f$key, value = f$value) %>% osmdata_sf()
      polys <- od$osm_polygons
      multi <- od$osm_multipolygons
      if (!is.null(multi) && nrow(multi) > 0) {
        multi <- st_cast(multi %>% select(geometry), "POLYGON", warn = FALSE)
        if (is.null(polys) || nrow(polys) == 0) polys <- multi else polys <- rbind(polys %>% select(geometry), multi)
      }
      if (is.null(polys) || nrow(polys) == 0) NULL else (polys %>% select(geometry))
    }, error = function(e) {
      warn_if_overpass_error(e)
      NULL
    })
    if (!is.null(poly) && nrow(poly) > 0) {
      if (!all(st_is_valid(poly))) poly <- st_make_valid(poly)
      out[[label]] <- list(group = label, sf = poly)
    }
  }
  out
}

# Fetch all greenspace, merge (union) into one layer so overlaps are single polygons (WGS84).
# Use for display so the map shows one merged layer with no double-drawn overlap.
# Uses GeoPackage cache by bbox UID when available; otherwise fetches OSM and caches result.
fetch_greenspace_merged <- function(bbox, city = NULL) {
  cached <- get_cached_greenspace(bbox)
  if (!is.null(cached) && nrow(cached) > 0) {
    message("Using cached merged greenspace for this bbox.")
    return(cached)
  }
  layers <- fetch_greenspace_layers(bbox)
  if (length(layers) == 0) return(NULL)
  all_sf <- do.call(rbind, lapply(layers, function(x) x$sf))
  if (nrow(all_sf) == 0) return(NULL)
  if (!all(st_is_valid(all_sf))) all_sf <- st_make_valid(all_sf)
  utm_zone <- floor((bbox["x", "min"] + bbox["x", "max"]) / 2 + 180) %/% 6 + 1
  if (bbox["y", "min"] + bbox["y", "max"] > 0) {
    crs_planar <- st_crs(paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs"))
  } else {
    crs_planar <- st_crs(paste0("+proj=utm +zone=", utm_zone, " +south +datum=WGS84 +units=m +no_defs"))
  }
  planar <- st_transform(all_sf, crs_planar)
  if (!all(st_is_valid(planar))) planar <- st_make_valid(planar)
  geom <- st_geometry(planar)
  merged <- tryCatch(
    st_union(geom),
    error = function(e) {
      message("fetch_greenspace_merged: st_union failed (", conditionMessage(e), "); trying buffer(0)...")
      fixed <- tryCatch(st_buffer(planar, 0), error = function(e2) planar)
      if (!all(st_is_valid(fixed))) fixed <- st_make_valid(fixed)
      tryCatch(
        st_union(st_geometry(fixed)),
        error = function(e2) {
          message("fetch_greenspace_merged: trying chunked union...")
          n <- nrow(fixed)
          chunk <- 200L
          idx <- seq_len(n)
          chunks <- split(idx, ceiling(idx / chunk))
          parts <- lapply(chunks, function(i) st_union(st_geometry(fixed[i, ])))
          Reduce(st_union, parts)
        }
      )
    }
  )
  if (length(merged) > 1) merged <- st_union(merged)
  merged_sf <- st_sf(geometry = merged, crs = crs_planar)
  merged_wgs84 <- st_transform(merged_sf, st_crs(all_sf))
  result <- st_sf(geometry = st_geometry(merged_wgs84), crs = st_crs(all_sf))
  set_cached_greenspace(bbox, result, city = city)
  result
}
