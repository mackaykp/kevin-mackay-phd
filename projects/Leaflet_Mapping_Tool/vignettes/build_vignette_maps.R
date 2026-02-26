#
# Build all maps used by vignettes.qmd and copy them to vignettes/maps/ with
# the filenames expected by the iframes. Run from project root.
# Usage: source("vignettes/build_vignette_maps.R")
#
# If Overpass rate-limits, wait a few minutes or run in sections (comment out
# some create_bivariate_map calls).
#

if (!dir.exists("R")) stop("Run from project root (where R/ and data/ live).")
if (!file.exists("data/ses_index.gpkg")) stop("Build income data first: source('R/build_ses_index.R')")

source("R/create_bivariate_map.R")
out_dir <- "output/maps"
vignette_maps <- "vignettes/maps"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(vignette_maps, recursive = TRUE, showWarnings = FALSE)

# Copy output/maps/{base}_bivariate.html (and _files) to vignettes/maps/{target}.html
copy_to_vignette <- function(base_name, target_name = NULL) {
  if (is.null(target_name)) target_name <- base_name
  src <- file.path(out_dir, paste0(base_name, "_bivariate.html"))
  if (!file.exists(src)) {
    warning("Not found: ", src)
    return(invisible(NULL))
  }
  dst <- file.path(vignette_maps, paste0(target_name, "_bivariate.html"))
  file.copy(src, dst, overwrite = TRUE)
  message("Copied -> ", dst)
  src_files <- file.path(out_dir, paste0(base_name, "_bivariate_files"))
  if (dir.exists(src_files)) {
    dst_files <- file.path(vignette_maps, paste0(target_name, "_bivariate_files"))
    if (dir.exists(dst_files)) unlink(dst_files, recursive = TRUE)
    file.copy(src_files, vignette_maps, recursive = TRUE)
  }
  invisible(NULL)
}

message("=== Hamilton (default) ===")
create_bivariate_map("Hamilton, ON", "data/ses_index.gpkg")
copy_to_vignette("Hamilton")

message("=== Hamilton (expanded 5 km) ===")
create_bivariate_map("Hamilton, ON", "data/ses_index.gpkg", custom_distance_km = 5)
copy_to_vignette("Hamilton", "Hamilton_expanded")

message("=== Hamilton (1 km buffer + green layers) ===")
create_bivariate_map("Hamilton, ON", "data/ses_index.gpkg", green_buffer_m = 1000, show_green_layers = TRUE)
copy_to_vignette("Hamilton", "Hamilton_1km_green_layers")

message("=== Custom bbox (Drumheller badlands, AB) ===")
create_bivariate_map("Drumheller, AB", "data/ses_index.gpkg", custom_bbox = c(-112.82, 51.42, -112.55, 51.52))
copy_to_vignette("Drumheller", "Custom_bbox")

message("=== Halifax ===")
create_bivariate_map("Halifax, NS", "data/ses_index.gpkg")
copy_to_vignette("Halifax")

message("=== Charlottetown ===")
create_bivariate_map("Charlottetown, PE", "data/ses_index.gpkg")
copy_to_vignette("Charlottetown")

message("=== Brandon (expanded 3 km) ===")
create_bivariate_map("Brandon, MB", "data/ses_index.gpkg", custom_distance_km = 3)
copy_to_vignette("Brandon")

message("=== Yellowknife ===")
create_bivariate_map("Yellowknife, NT", "data/ses_index.gpkg")
copy_to_vignette("Yellowknife")

message("Done. Maps in ", normalizePath(vignette_maps, mustWork = FALSE))
message("Re-render the site with: quarto render")
