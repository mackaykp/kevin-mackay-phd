# =============================================================================
# Geocode an Excel file of addresses (reference table in DuckDB, e.g. NAR)
# =============================================================================
# Usage:
#   1. Set working directory to this project: setwd(".../Offline_Geocoding_Tool")
#   2. Source this file: source("geocode_excel_file.R")
#   3. Call the function: geocode_excel("path/to/your_file.xlsx")
#   From shell: Rscript geocode_excel_file.R "path/to/your_file.xlsx"
# =============================================================================

# Install and load required packages (geocoding uses dplyr, readxl, stringdist, readr, stringr, tibble)
repos <- "https://cloud.r-project.org"
for (pkg in c("readxl", "dplyr", "stringdist", "readr", "stringr", "tibble")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, repos = repos)
}
library(readxl)
library(dplyr)
library(stringr)
library(readr)
library(tibble)

# Load the geocoding tool (working directory must be project root)
if (!file.exists("R/utils_geocode.R")) {
  stop("Working directory must be the Offline_Geocoding_Tool project root.\n",
       "Run: setwd('C:/Users/macka/OneDrive/Documents/Cursor-Projects/Offline_Geocoding_Tool')")
}
source("R/utils_geocode.R")
source("R/geocode_ref.R")

#' Geocode addresses from an Excel file (single hierarchy: exact → relaxed → fuzzy → centroids)
#'
#' Processes in chunks of 100 and saves progress after each chunk. If the run
#' fails or is interrupted, re-run with the same arguments to resume from the
#' last saved output (skips already-geocoded rows).
#'
#' Match hierarchy: exact (full address) → exact (street only) → relaxed (same province+city+FSA) →
#' fuzzy (narrow pool, strict threshold) → FSA centroid → city centroid. No cross-FSA or cross-city point matches.
#'
#' @param xlsx_path Path to the Excel/CSV file with address columns.
#' @param address_cols Character vector of column names to amalgamate into one
#'   address (in order). Default c("Street", "City", "Province", "Postal Code").
#' @param out_path Path for the geocoded output CSV. If NULL, writes to
#'   \code{output/<input_basename>_geocoded.csv}. Same path is used for
#'   partial progress; if it exists, geocoding resumes from the next row.
#' @param sheet Sheet to read (number or name). Default 1.
#' @param nar_duckdb_path Path to the NAR DuckDB file (table "ref"). Default "data/nar.duckdb". Build with \code{run_build_nar_duckdb.R}.
#' @param ... Passed to \code{geocode_ref} (e.g. \code{max_candidate_pool}, \code{fuzzy_threshold}, \code{use_city_in_narrow_pool}). Set \code{use_city_in_narrow_pool = FALSE} to omit city from the narrow pool when full 6-digit postal is present (helps when city names often don't match).
#' @return The geocoded tibble (invisibly), with \code{match_type} and \code{geocode_step} indicating how each row was matched.
geocode_excel <- function(xlsx_path,
                          address_cols = c("Street", "City", "Province", "Postal Code"),
                          out_path = NULL,
                          sheet = 1,
                          nar_duckdb_path = "data/nar.duckdb",
                          ...) {
  if (missing(xlsx_path) || is.null(xlsx_path) || length(xlsx_path) == 0 || !nzchar(xlsx_path)) {
    stop("Please provide xlsx_path: the path to your Excel file, e.g. geocode_excel('data/ungeocoded_files/my_data.xlsx')")
  }
  xlsx_path <- path.expand(xlsx_path)
  if (!file.exists(xlsx_path)) {
    abs_path <- normalizePath(xlsx_path, mustWork = FALSE)
    stop("File not found: ", xlsx_path,
         "\n  (resolved to: ", abs_path, ")\n  Use full path if the file is outside the project, e.g. 'C:/Users/.../file.xlsx'")
  }

  file_type <- if (grepl("\\.csv$", xlsx_path, ignore.case = TRUE)) "CSV" else "Excel"
  message("[INPUT] Reading ", file_type, ": ", basename(xlsx_path))
  if (grepl("\\.csv$", xlsx_path, ignore.case = TRUE)) {
    data <- readr::read_csv(xlsx_path, show_col_types = FALSE)
  } else {
    data <- read_excel(xlsx_path, sheet = sheet)
  }
  message("[INPUT] Rows: ", nrow(data), "  |  Columns: ", ncol(data), "  |  Address cols: ", paste(address_cols, collapse = ", "))
  # Drop rows with no usable address (all address columns NA or blank) to avoid duplicate/NA rows in output
  has_any_addr <- rep(FALSE, nrow(data))
  for (ac in address_cols) {
    if (ac %in% names(data)) {
      v <- data[[ac]]
      has_any_addr <- has_any_addr | !(is.na(v) | (is.character(v) & !nzchar(trimws(as.character(v)))))
    }
  }
  if (!all(has_any_addr)) {
    n_drop <- sum(!has_any_addr)
    data <- data[has_any_addr, , drop = FALSE]
    message("[INPUT] Dropped ", n_drop, " row(s) with no address; ", nrow(data), " rows to geocode.")
  }

  missing_cols <- setdiff(address_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("These address columns were not found: ", paste(missing_cols, collapse = ", "),
         "\nYour columns: ", paste(names(data), collapse = ", "))
  }

  # Reference DuckDB only (e.g. NAR; build with run_build_nar_duckdb.R)
  nar_db <- path.expand(nar_duckdb_path)
  if (!file.exists(nar_db))
    stop("Reference DuckDB not found: ", nar_db, ". Run run_build_nar_duckdb.R once to create it.")
  if (!requireNamespace("duckdb", quietly = TRUE)) stop("Install duckdb: install.packages(\"duckdb\")")
  ref_duckdb_conn <- duckdb::dbConnect(duckdb::duckdb(), nar_db, read_only = TRUE)
  on.exit(if (!is.null(ref_duckdb_conn)) duckdb::dbDisconnect(ref_duckdb_conn, shutdown = TRUE), add = TRUE)
  if (!exists("query_ref_exact", mode = "function")) source("R/query_ref_duckdb.R")
  message("[REF] DuckDB: ", basename(nar_db), "  |  Match order: exact → relaxed → fuzzy → postal → FSA → city centroid")

  if (is.null(out_path) || !nzchar(out_path)) {
    base_name <- sub("\\.(xlsx?|csv)$", "", basename(xlsx_path), ignore.case = TRUE)
    out_dir <- "output"
    if (!dir.exists(out_dir)) dir.create(out_dir, showWarnings = FALSE)
    out_path <- file.path(out_dir, paste0(base_name, "_geocoded.csv"))
  }
  out_path <- path.expand(out_path)

  # Check for existing partial or complete output so we can resume
  chunk_size <- 100L
  total_rows <- nrow(data)
  accumulated <- NULL
  rows_done <- 0L
  message("[OUTPUT] ", out_path)
  if (file.exists(out_path)) {
    accumulated <- readr::read_csv(out_path, show_col_types = FALSE)
    n_done <- nrow(accumulated)
    if (n_done >= total_rows) {
      message("[DONE] File already complete (", n_done, " rows). Returning existing output.")
      if ("match_type" %in% names(accumulated)) {
        .geocode_descriptive_stats(accumulated, total_rows = nrow(accumulated), elapsed_sec = NA)
      }
      return(invisible(accumulated))
    }
    message("[RESUME] ", n_done, " rows done  |  ", total_rows - n_done, " remaining")
    rows_done <- n_done
  } else {
    message("[START] Geocoding ", total_rows, " rows  |  Progress saved every ", chunk_size, " rows")
  }

  if (rows_done >= total_rows) {
    return(invisible(accumulated))
  }

  # Output: ugc_* (ungeocoded input), match info, ref_* (reference table).
  match_info_cols <- c("match_type", "geocode_step", "match_quality", "same_city", "civic_number_exact", "text_score", "number_score", "geo_score", "composite_score")
  ref_output_cols <- c("matched_address", "Latitude", "Longitude", "Civic_Number", "City", "Postal_Code", "Processed_City", "Census_Subdivision_Name", "Standardized_Street_Name", "Province_or_Territory_Unique_Identifier")
  ref_cols_prefixed <- paste0("ref_", ref_output_cols)
  geocode_cols <- c(match_info_cols, ref_cols_prefixed)
  input_cols <- names(data)
  out_cols <- c(paste0("ugc_", input_cols), match_info_cols, ref_cols_prefixed)

  geocode_col_na <- function(cn, n) {
    switch(cn,
           match_type = rep(NA_character_, n),
           geocode_step = rep(NA_character_, n),
           match_quality = rep(NA_character_, n),
           same_city = rep(NA, n),
           civic_number_exact = rep(NA, n),
           text_score = rep(NA_real_, n),
           number_score = rep(NA_real_, n),
           geo_score = rep(NA_real_, n),
           composite_score = rep(NA_real_, n),
           ref_matched_address = rep(NA_character_, n),
           ref_Latitude = rep(NA_real_, n),
           ref_Longitude = rep(NA_real_, n),
           ref_Civic_Number = rep(NA_real_, n),
           ref_City = rep(NA_character_, n),
           ref_Postal_Code = rep(NA_character_, n),
           ref_Processed_City = rep(NA_character_, n),
           ref_Census_Subdivision_Name = rep(NA_character_, n),
           ref_Standardized_Street_Name = rep(NA_character_, n),
           ref_Province_or_Territory_Unique_Identifier = rep(NA_character_, n),
           rep(NA, n))
  }
  # Merge geocode result into a chunk; ref columns are written as ref_* to avoid clashing with input columns
  merge_geocode_into_chunk <- function(chunk_df, res) {
    if (is.null(res) || nrow(res) == 0) return(chunk_df)
    res_geo <- res %>%
      dplyr::select(.row_id, dplyr::any_of(c("match_type", "match_quality", "same_city", "civic_number_exact", "text_score", "number_score", "geo_score", "composite_score", "Full_Address", "Latitude", "Longitude", "Civic_Number", "City", "Postal_Code", "Processed_City", "Census_Subdivision_Name", "Standardized_Street_Name", "Province_or_Territory_Unique_Identifier")))
    if ("Full_Address" %in% names(res_geo)) res_geo <- res_geo %>% dplyr::rename(matched_address = Full_Address)
    if (!"matched_address" %in% names(res_geo)) res_geo$matched_address <- NA_character_
    n <- nrow(chunk_df)
    # Map result columns to chunk columns: match info as-is, ref columns -> ref_*
    res_to_chunk <- setdiff(names(res_geo), ".row_id")
    chunk_update_cols <- ifelse(res_to_chunk %in% ref_output_cols, paste0("ref_", res_to_chunk), res_to_chunk)
    for (j in seq_along(res_to_chunk)) {
      cn <- chunk_update_cols[j]
      if (!cn %in% names(chunk_df)) chunk_df[[cn]] <- geocode_col_na(cn, n)
    }
    chunk_df_row_id <- chunk_df$.row_id
    for (i in seq_len(nrow(res_geo))) {
      rid <- res_geo$.row_id[i]
      idx <- which(chunk_df_row_id == rid)
      if (length(idx) > 0L) {
        for (j in seq_along(res_to_chunk)) chunk_df[[chunk_update_cols[j]]][idx] <- res_geo[[res_to_chunk[j]]][i]
      }
    }
    chunk_df
  }

  default_max_pool <- 20000L

  time_start <- Sys.time()

  # Process in chunks by row index so we always stop after exactly total_rows (avoids infinite loop from stale "remaining")
  chunk_num <- 0L
  while (rows_done < total_rows) {
    chunk_num <- chunk_num + 1L
    n_take <- min(chunk_size, total_rows - rows_done)
    chunk <- data[seq(rows_done + 1L, rows_done + n_take), , drop = FALSE]
    start_row <- rows_done + 1L
    chunk$.row_id <- seq(start_row, length.out = nrow(chunk))

    message("[CHUNK ", chunk_num, "] Rows ", start_row, "-", start_row + nrow(chunk) - 1L, " (", nrow(chunk), " addresses)")

    # Initialize geocode columns with correct types (so rows_update doesn't mix logical with character/numeric)
    nchunk <- nrow(chunk)
    for (c in geocode_cols) if (!c %in% names(chunk)) chunk[[c]] <- geocode_col_na(c, nchunk)
    full_chunk <- chunk

    message("         Running match pipeline ...")
    res1 <- geocode_ref(chunk, ref_duckdb = ref_duckdb_conn, address_cols = address_cols, auto_select = TRUE, fuzzy_threshold = 0.75, max_candidate_pool = list(...)$max_candidate_pool %||% default_max_pool, ...)
    full_chunk <- merge_geocode_into_chunk(full_chunk, res1)

    # Set geocode_step from match_type for readable output
    full_chunk$geocode_step <- dplyr::case_when(
      full_chunk$match_type == "exact" ~ "Exact (full address)",
      full_chunk$match_type == "exact_street" ~ "Exact (street only)",
      full_chunk$match_type == "relaxed" ~ "Relaxed (same geography)",
      full_chunk$match_type == "fuzzy" ~ "Fuzzy (narrow pool)",
      full_chunk$match_type == "postal_centroid" ~ "Postal code (6-digit) centroid fallback",
      full_chunk$match_type == "fsa_centroid" ~ "FSA centroid fallback",
      full_chunk$match_type == "city_centroid" ~ "City centroid fallback",
      TRUE ~ NA_character_
    )
    # Select output columns (input + match info + ref_*); .row_id is internal only
    to_write <- full_chunk %>% dplyr::select(dplyr::any_of(c(input_cols, match_info_cols, ref_cols_prefixed)))
    # Prefix ungeocoded input columns for troubleshooting (ref_* already set in chunk)
    for (col in input_cols) if (col %in% names(to_write)) names(to_write)[names(to_write) == col] <- paste0("ugc_", col)
    append_mode <- (chunk_num > 1 || rows_done > 0)
    if (!append_mode) {
      readr::write_csv(to_write, out_path)
    } else {
      suppressWarnings(readr::write_csv(to_write, out_path, append = TRUE, col_names = FALSE))
    }
    rows_done <- rows_done + n_take
    message("         Saved ", rows_done, " / ", total_rows, " rows")
  }

  time_end <- Sys.time()
  elapsed_sec <- as.numeric(difftime(time_end, time_start, units = "secs"))

  message("")
  message("[DONE] Geocoding complete  |  Output: ", out_path)
  # Re-read output for return and summary (streaming avoids holding full result in memory during run)
  out_result <- readr::read_csv(out_path, show_col_types = FALSE)
  if ("match_type" %in% names(out_result)) {
    .geocode_descriptive_stats(out_result, total_rows = nrow(out_result), elapsed_sec = elapsed_sec)
  }
  invisible(out_result %>% dplyr::select(dplyr::any_of(out_cols)))
}

#' Print elapsed time and descriptive statistics for a geocoded result.
#' @param result Data frame with match_type (and optionally ref_Latitude) columns.
#' @param total_rows Total number of rows.
#' @param elapsed_sec Elapsed time in seconds (NA to skip timing).
.geocode_descriptive_stats <- function(result, total_rows, elapsed_sec = NA) {
  message("")
  message("---------- GEOCODING SUMMARY ----------")
  if (!is.na(elapsed_sec) && elapsed_sec >= 0) {
    mins <- elapsed_sec / 60
    message("  Time:    ", round(elapsed_sec, 1), " sec (", round(mins, 2), " min)")
    if (total_rows > 0 && elapsed_sec > 0)
      message("  Rate:    ", round(total_rows / mins, 0), " addresses/min")
  }
  n <- total_rows
  matched <- sum(!is.na(result$match_type) & nzchar(trimws(as.character(result$match_type %||% ""))))
  if ("ref_Latitude" %in% names(result))
    has_coord <- sum(!is.na(result$ref_Latitude) & is.finite(result$ref_Latitude))
  else
    has_coord <- matched
  unmatched <- n - matched
  message("  Total:   ", n, " rows")
  message("  Matched: ", matched, " (", round(100 * matched / n, 1), "%)")
  message("  Coords:  ", has_coord, " (", round(100 * has_coord / n, 1), "%)")
  message("  Unmatched: ", unmatched, " (", round(100 * unmatched / n, 1), "%)")
  if ("match_type" %in% names(result)) {
    tbl <- table(result$match_type, useNA = "ifany")
    message("  By match_type:")
    for (i in seq_along(tbl)) {
      nm <- names(tbl)[i]
      if (is.na(nm)) nm <- "<NA>"
      pct <- round(100 * tbl[i] / n, 1)
      message("    ", nm, ": ", tbl[i], " (", pct, "%)")
    }
  }
  message("---------------------------------------")
  message("")
}

# When run via Rscript with a file path argument, run geocode_excel with it
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1 && nzchar(args[1])) {
  geocode_excel(args[1])
} else {
  message("[GEOCODE] Helper loaded. Call: geocode_excel(\"data/ungeocoded_files/your_file.xlsx\")")
}
