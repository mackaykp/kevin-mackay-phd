# =============================================================================
# TEST: Run geocoding on the test address file (single hierarchy, no phases)
# Hierarchy: exact → exact_street → relaxed → fuzzy (narrow) → FSA centroid → city centroid.
# =============================================================================
# In R: setwd(".../Offline_Geocoding_Tool"); source("run_test_geocode.R")
# Or: Rscript run_test_geocode.R
# =============================================================================

proj_root <- "C:/Users/macka/OneDrive/Documents/Cursor-Projects/Offline_Geocoding_Tool"
if (dir.exists(proj_root)) setwd(proj_root)

# Optional: remove previous run for a fresh test
geocoded_path <- file.path(proj_root, "output", "test_addresses_geocoded.csv")
if (file.exists(geocoded_path)) file.remove(geocoded_path)

source("geocode_excel_file.R")

test_file <- file.path(proj_root, "data", "ungeocoded_files", "test_addresses.csv")
if (!file.exists(test_file)) test_file <- file.path(proj_root, "data", "ungeocoded_files", "test_addresses.xlsx")
if (!file.exists(test_file)) stop("Run create_test_addresses.R first, or add data/ungeocoded_files/test_addresses.csv")
nar_db <- file.path(proj_root, "data", "nar.duckdb")
if (!file.exists(nar_db)) stop("NAR DuckDB not found. Run run_build_nar_duckdb.R first.")

message("[TEST] Geocoding: ", basename(test_file), "  |  Output: output/test_addresses_geocoded.csv")
result <- geocode_excel(
  test_file,
  address_cols    = c("Street", "City", "Province", "Postal Code"),
  nar_duckdb_path = nar_db
)

message("\n[PREVIEW] Result (match_type, geocode_step, coords):")
cols_show <- c("ugc_Street", "ugc_City", "ref_Latitude", "ref_Longitude", "match_type", "geocode_step", "match_quality")
print(result[, names(result) %in% cols_show, drop = FALSE])
