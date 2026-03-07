# =============================================================================
# PRODUCTION: Run geocoding on the SIF school data (or change paths below)
# =============================================================================
# Single hierarchy: exact → relaxed → fuzzy (narrow) → FSA centroid → city centroid.
# Progress saved every 100 rows; re-run to resume.
# In R: setwd(".../Offline_Geocoding_Tool"); source("run_geocode.R")
# Or: Rscript run_geocode.R
# =============================================================================

# Use project root as working directory (required for relative paths)
proj_root <- "C:/Users/macka/OneDrive/Documents/Cursor-Projects/Offline_Geocoding_Tool"
if (dir.exists(proj_root)) {
  setwd(proj_root)
} else {
  proj_root <- getwd()
}

source("geocode_excel_file.R")

xlsx_path <- file.path(proj_root, "data", "ungeocoded_files", "sif_data_table_2022_2023_en.xlsx")
nar_db    <- file.path(proj_root, "data", "nar.duckdb")

if (!file.exists(xlsx_path)) stop("Excel file not found: ", xlsx_path)
if (!file.exists(nar_db)) stop("NAR DuckDB not found: ", nar_db, ". Run run_build_nar_duckdb.R first.")

# Output goes to output/sif_data_table_2022_2023_en_geocoded.csv by default.
# To start fresh, delete that file before running.

message("[PRODUCTION] Geocoding SIF data (NAR DuckDB)")
result <- geocode_excel(
  xlsx_path,
  address_cols     = c("Street", "City", "Province", "Postal Code"),
  nar_duckdb_path  = nar_db
)

message("\n[PREVIEW] First 10 rows (address + match_type + coords):")
cols_show <- c("ugc_Street", "ugc_City", "ugc_Province", "ref_Latitude", "ref_Longitude", "match_type", "geocode_step", "composite_score")
print(head(result[, names(result) %in% cols_show, drop = FALSE], 10))
