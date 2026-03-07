# Run once to install R dependencies for the Offline Geocoding Tool
pkgs <- c("tidyverse", "stringdist", "fuzzyjoin", "readr")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
}
message("Done. Load with: source('R/utils_geocode.R'); source('R/geocode_ref.R')")
