# =============================================================================
# One-time: Build DuckDB from NAR (National Address Register)
# Run once (or when NAR data changes). Geocoding then uses data/nar.duckdb only.
# =============================================================================
# In R: setwd(".../Offline_Geocoding_Tool"); source("run_build_nar_duckdb.R")
# Or: Rscript run_build_nar_duckdb.R
# =============================================================================

proj_root <- "C:/Users/macka/OneDrive/Documents/Cursor-Projects/Offline_Geocoding_Tool"
if (dir.exists(proj_root)) setwd(proj_root)

if (!requireNamespace("duckdb", quietly = TRUE)) {
  message("Installing duckdb ...")
  install.packages("duckdb", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr", repos = "https://cloud.r-project.org")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr", repos = "https://cloud.r-project.org")
library(dplyr)
library(stringr)

source("R/utils_geocode.R")
source("R/build_nar_duckdb.R")

nar_dir  <- file.path(proj_root, "data", "NAR2025")
db_path  <- file.path(proj_root, "data", "nar.duckdb")

if (!dir.exists(nar_dir)) {
  stop("NAR directory not found: ", nar_dir, "\nPlace NAR Addresses/ and Locations/ under data/NAR2025/")
}

message("[NAR] Building reference DB: data/nar.duckdb")
build_nar_duckdb(nar_dir, db_path, address_chunk_size = 50000L)
message("[NAR] Done. Run run_geocode.R or run_test_geocode.R to geocode.")
