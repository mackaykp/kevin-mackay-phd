# Vignette maps

The [Vignettes](vignettes.qmd) page embeds **pre-generated** Leaflet maps so the webpage loads quickly and does not hit OpenStreetMap/Overpass rate limits when visitors view it.

## One-time setup

1. From the **project root**, ensure you have built the income GeoPackage:
   ```r
   source("R/build_ses_index.R")
   ```
2. Run the map-generation script (also from project root):
   ```r
   source("vignettes/build_vignette_maps.R")
   ```
   This will:
   - Create each example map and write it to `output/maps/`
   - Copy each file into `vignettes/maps/` with the filenames expected by the vignette iframes

Run the script when you want to refresh the embedded maps (e.g. after changing buffer or bbox). If you see Overpass rate-limit messages, wait a few minutes between runs or run the script in stages.

## Expected files in `vignettes/maps/`

- `Hamilton_bivariate.html`
- `Hamilton_expanded_bivariate.html` (from run with `custom_distance_km = 5`)
- `Hamilton_1km_green_layers_bivariate.html` (from run with `green_buffer_m = 1000`, `show_green_layers = TRUE`)
- `Custom_bbox_bivariate.html` (e.g. Drumheller badlands from build script)
- `Halifax_bivariate.html`
- `Charlottetown_bivariate.html`
- `Brandon_bivariate.html`
- `Yellowknife_bivariate.html`
- `International_placeholder.html` (included in repo; replace with a real map when you have non-Canadian data)

## Publishing the site

After `quarto render`, the site is in `_site/`. The iframes reference `maps/...`, which resolves to `_site/vignettes/maps/` when viewing the vignette page. Ensure `vignettes/maps/*.html` (and their `_files/` folders if not self-contained) are present before rendering, or Quarto may not copy them. If maps are missing, add `vignettes/maps` to your project resources or copy the folder into `_site/vignettes/` after rendering.
