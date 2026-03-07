# =============================================================================
# Create a small test Excel file with addresses that exist in the reference
# data (e.g. NAR; Ajax, ON) so you can verify the geocoding tool before real data.
# =============================================================================
# Option A - Use the pre-made CSV (no R needed for this step):
#   The file data/ungeocoded_files/test_addresses.csv already exists. The tool
#   accepts CSV; run geocode_excel on it (see Option B).
#
# Option B - Run this script in R to create test_addresses.xlsx:
#   setwd(".../Offline_Geocoding_Tool"); source("create_test_addresses.R")
#
# Then test the geocoding tool (in R):
#   source("geocode_excel_file.R")
#   geocode_excel("data/ungeocoded_files/test_addresses.csv", address_cols = c("Street", "City", "Province", "Postal Code"))
#   # Or use test_addresses.xlsx if you ran create_test_addresses.R
# =============================================================================

# Install writexl if needed (to create .xlsx)
if (!requireNamespace("writexl", quietly = TRUE)) {
  install.packages("writexl", repos = "https://cloud.r-project.org")
}
library(writexl)

# Test addresses (e.g. Ajax, Ontario) - these should match the reference table
test_addresses <- data.frame(
  Street = c(
    "734 Audley Road South",
    "726 Audley Road South",
    "320 Audley Road North",
    "462 Kingston Road East",
    "448 Bayly Street East",
    "750 Rossland Road East",
    "1363 Salem Road North",
    "678 Audley Road South",
    "291 Williamson Drive West",
    "125 Rushworth Drive"
  ),
  City = rep("Ajax", 10),
  Province = rep("Ontario", 10),
  `Postal Code` = rep("L1S 1A1", 10),  # Ajax area
  stringsAsFactors = FALSE,
  check.names = FALSE
)

out_dir <- "data/ungeocoded_files"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_path <- file.path(out_dir, "test_addresses.xlsx")

write_xlsx(list(Addresses = test_addresses), out_path)
message("Created: ", normalizePath(out_path, mustWork = FALSE))
message("Columns: ", paste(names(test_addresses), collapse = ", "))
message("\nTo test the geocoding tool, run:")
message('  source("geocode_excel_file.R")')
message('  geocode_excel("data/ungeocoded_files/test_addresses.xlsx", address_cols = c("Street", "City", "Province", "Postal Code"))')
