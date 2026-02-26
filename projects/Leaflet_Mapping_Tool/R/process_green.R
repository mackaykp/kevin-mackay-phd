#
# OSM green space data and study-area green access status (parks, playgrounds, nature reserves).
# Input: bbox (matrix with rownames "x","y" and colnames "min","max").
# Returns: list with green_status (Limited/Good/Great), parks_sf, playgrounds_sf, etc.
# Note: create_bivariate_map uses process_green_da.R and fetch_greenspace_* only; this script is for other use (e.g. green-status summaries or marker layers).
#

library(sf)
library(dplyr)
library(osmdata)

process_green <- function(bbox) {
  if (is.null(bbox) || !identical(rownames(bbox), c("x", "y"))) {
    return(list(green_status = NA_character_, parks_sf = NULL, playgrounds_sf = NULL))
  }
  bbox_vec <- c(bbox["x", "min"], bbox["y", "min"], bbox["x", "max"], bbox["y", "max"])

  # Parks (polygons → centroids for markers)
  parks <- tryCatch({
    opq(bbox_vec) %>%
      add_osm_feature(key = "leisure", value = "park") %>%
      osmdata_sf() %>%
      .$osm_polygons
  }, error = function(e) NULL)
  parks_pts <- if (!is.null(parks) && nrow(parks) > 0) st_centroid(parks) else NULL

  # Playgrounds
  playgrounds <- tryCatch({
    opq(bbox_vec) %>%
      add_osm_feature(key = "leisure", value = "playground") %>%
      osmdata_sf()
  }, error = function(e) NULL)
  play_poly <- if (!is.null(playgrounds$osm_polygons) && nrow(playgrounds$osm_polygons) > 0) playgrounds$osm_polygons else NULL
  play_pts  <- if (!is.null(playgrounds$osm_points) && nrow(playgrounds$osm_points) > 0) playgrounds$osm_points else NULL
  playgrounds_sf <- if (!is.null(play_poly)) st_centroid(play_poly) else play_pts

  # Nature reserves (polygons → centroids)
  nature <- tryCatch({
    opq(bbox_vec) %>%
      add_osm_feature(key = "leisure", value = "nature_reserve") %>%
      osmdata_sf() %>%
      .$osm_polygons
  }, error = function(e) NULL)
  nature_pts <- if (!is.null(nature) && nrow(nature) > 0) st_centroid(nature) else NULL

  # Green access status: count of park + playground + nature features
  n_parks <- if (is.null(parks_pts)) 0 else nrow(parks_pts)
  n_play  <- if (is.null(playgrounds_sf)) 0 else nrow(playgrounds_sf)
  n_nat   <- if (is.null(nature_pts)) 0 else nrow(nature_pts)
  total_green <- n_parks + n_play + n_nat

  # Thresholds from map_config (or defaults if not loaded)
  th <- if (exists("map_config")) map_config$green$thresholds else c(great = 20L, good = 5L, limited = 1L)
  green_status <- dplyr::case_when(
    total_green >= th["great"]   ~ "Great",
    total_green >= th["good"]   ~ "Good",
    total_green >= th["limited"] ~ "Limited",
    TRUE                         ~ "None"
  )

  list(
    green_status = green_status,
    parks_sf = parks_pts,
    playgrounds_sf = playgrounds_sf,
    nature_reserves_sf = nature_pts,
    n_parks = n_parks,
    n_playgrounds = n_play,
    n_nature = n_nat
  )
}
