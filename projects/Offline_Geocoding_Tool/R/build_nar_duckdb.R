# =============================================================================
# Build DuckDB from NAR (National Address Register) — single reference DB for geocoding
# Joins Address + Location on LOC_GUID; outputs table "ref" with schema expected by geocode_ref
# =============================================================================
# Usage: source("R/build_nar_duckdb.R"); build_nar_duckdb("data/NAR2025", "data/nar.duckdb")
# =============================================================================

# NAR province code (numeric) to 2-letter (StatCan SGC)
NAR_PROV_TO_2LETTER <- c(
  "10" = "NL", "11" = "PE", "12" = "NS", "13" = "NB", "24" = "QC",
  "35" = "ON", "46" = "MB", "47" = "SK", "48" = "AB", "59" = "BC",
  "60" = "YT", "61" = "NT", "62" = "NU"
)

#' Convert NA and literal "NA" to empty string so they don't appear in address strings
.blank_na <- function(x) {
  x <- trimws(as.character(x %||% ""))
  x[is.na(x) | toupper(x) == "NA" | !nzchar(x)] <- ""
  x
}

#' Build full address string(s) from NAR Address columns (vectorized).
#' NA and literal "NA" in any component are treated as blank so they don't break matching.
.build_nar_full_address_vec <- function(civic_no, apt, street_name, street_type, street_dir, csd_name, prov_abvn, postal) {
  civic_no   <- .blank_na(civic_no)
  apt        <- .blank_na(apt)
  street_name <- .blank_na(street_name)
  street_type <- .blank_na(street_type)
  street_dir <- .blank_na(street_dir)
  csd_name   <- .blank_na(csd_name)
  prov_abvn  <- .blank_na(prov_abvn)
  postal     <- .blank_na(postal)
  part1 <- ifelse(nzchar(apt), paste0(apt, "-", civic_no), civic_no)
  out <- paste(part1, street_name, street_type, street_dir, csd_name, prov_abvn, postal, sep = " ")
  out <- trimws(gsub("\\s+", " ", out))
  # Remove any stray "NA" token that might remain (e.g. from CSV literal "NA")
  out <- gsub("\\bNA\\b", "", out, ignore.case = TRUE)
  trimws(gsub("\\s+", " ", out))
}


#' Build DuckDB from NAR Address + Location CSVs. Table "ref" has schema expected by geocode_ref.
#' @param nar_dir Root directory containing Addresses/ and Locations/ (e.g. "data/NAR2025")
#' @param db_path Path to .duckdb file (e.g. "data/nar.duckdb")
#' @param address_chunk_size Rows per chunk when reading Address CSVs (default 50000)
#' @return Path to created DB (invisibly)
#' @export
build_nar_duckdb <- function(nar_dir, db_path = NULL, address_chunk_size = 50000L) {
  if (!requireNamespace("duckdb", quietly = TRUE)) stop("Install duckdb: install.packages(\"duckdb\")")
  nar_dir <- path.expand(nar_dir)
  if (!dir.exists(nar_dir)) stop("NAR directory not found: ", nar_dir)
  addr_dir <- file.path(nar_dir, "Addresses")
  loc_dir <- file.path(nar_dir, "Locations")
  if (!dir.exists(addr_dir)) stop("NAR Addresses folder not found: ", addr_dir)
  if (!dir.exists(loc_dir)) stop("NAR Locations folder not found: ", loc_dir)
  if (is.null(db_path) || !nzchar(db_path)) db_path <- file.path(nar_dir, "nar.duckdb")
  db_path <- path.expand(db_path)

  message("[NAR] Creating DB: ", basename(db_path))
  con <- duckdb::dbConnect(duckdb::duckdb(), db_path)
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # 1) Load all Location files into table "loc"
  loc_files <- list.files(loc_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(loc_files) == 0) stop("No CSV files in ", loc_dir)
  message("[NAR] Loading Locations (", length(loc_files), " files) -> table 'loc'")
  loc_created <- FALSE
  for (f in loc_files) {
    d <- suppressMessages(readr::read_csv(f, show_col_types = FALSE))
    d <- d[, c("LOC_GUID", "BG_LATITUDE", "BG_LONGITUDE"), drop = FALSE]
    duckdb::dbWriteTable(con, "loc", d, append = loc_created, overwrite = !loc_created)
    loc_created <- TRUE
    rm(d); gc()
  }
  n_loc <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM loc")$n
  message("[NAR]   loc: ", format(n_loc, big.mark = ","), " rows")

  # 2) Stream Address files in chunks; join with loc; append to ref
  addr_files <- list.files(addr_dir, pattern = "^Address_.*\\.csv$", full.names = TRUE)
  if (length(addr_files) == 0) stop("No Address_*.csv files in ", addr_dir)
  message("[NAR] Processing Addresses (", length(addr_files), " files), chunk size ", format(address_chunk_size, big.mark = ","))
  ref_created <- FALSE
  for (f in addr_files) {
    message("[NAR]   ", basename(f))
    std_names <- NULL
    skip <- 0L
    repeat {
      chunk <- if (skip == 0L) {
        suppressMessages(readr::read_csv(f, n_max = address_chunk_size, show_col_types = FALSE))
      } else {
        suppressMessages(readr::read_csv(f, skip = skip, n_max = address_chunk_size, show_col_types = FALSE, col_names = std_names, skip_empty_rows = TRUE))
      }
      if (nrow(chunk) == 0) break
      if (is.null(std_names)) std_names <- names(chunk)
      # Next chunk: skip header (1) + rows already read
      skip <- skip + nrow(chunk) + if (skip == 0L) 1L else 0L
      guids <- unique(chunk$LOC_GUID)
      guids <- guids[!is.na(guids) & nzchar(trimws(as.character(guids)))]
      if (length(guids) == 0) { next }
      # Query lat/lon for these GUIDs (batch in SQL)
      escaped <- gsub("'", "''", as.character(guids), fixed = TRUE)
      in_list <- paste0("'", escaped, "'", collapse = ", ")
      q <- paste0("SELECT LOC_GUID, BG_LATITUDE, BG_LONGITUDE FROM loc WHERE LOC_GUID IN (", in_list, ")")
      loc_sub <- DBI::dbGetQuery(con, q)
      chunk <- dplyr::left_join(chunk, loc_sub, by = "LOC_GUID")
      # Drop rows with no coordinates (no matching location)
      chunk <- chunk[!is.na(chunk$BG_LATITUDE) & !is.na(chunk$BG_LONGITUDE), , drop = FALSE]
      if (nrow(chunk) == 0) { next }
      # Build ref-shaped rows (use .blank_na so NA / "NA" in source don't become literal "NA" in addresses)
      prov_2letter <- NAR_PROV_TO_2LETTER[as.character(chunk$PROV_CODE)]
      prov_2letter[is.na(prov_2letter)] <- ""
      csd <- .blank_na(chunk$CSD_ENG_NAME)
      city_norm <- normalize_city_for_match(csd)
      postal <- .blank_na(chunk$MAIL_POSTAL_CODE)
      fsa <- toupper(stringr::str_replace_all(substr(postal, 1, 3), "[^A-Za-z0-9]", ""))
      fsa[!nzchar(fsa)] <- NA_character_
      full_addr <- .build_nar_full_address_vec(
        chunk$CIVIC_NO, chunk$APT_NO_LABEL,
        chunk$OFFICIAL_STREET_NAME, chunk$OFFICIAL_STREET_TYPE, chunk$OFFICIAL_STREET_DIR,
        chunk$CSD_ENG_NAME, chunk$MAIL_PROV_ABVN, chunk$MAIL_POSTAL_CODE
      )
      address_norm <- normalize_string(full_addr)
      civic_num <- suppressWarnings(as.numeric(stringr::str_extract(.blank_na(chunk$CIVIC_NO), "^[0-9]+")))
      # Street-only line (number + street name + type + direction) for exact_street matching
      civic_char  <- .blank_na(chunk$CIVIC_NO)
      street_name <- .blank_na(chunk$OFFICIAL_STREET_NAME)
      street_type <- .blank_na(chunk$OFFICIAL_STREET_TYPE)
      street_dir  <- .blank_na(chunk$OFFICIAL_STREET_DIR)
      street_line <- paste(civic_char, street_name, street_type, street_dir, sep = " ")
      street_line <- gsub("\\bNA\\b", "", street_line, ignore.case = TRUE)
      street_line <- trimws(gsub("\\s+", " ", street_line))
      street_only_norm <- normalize_string(street_line)
      ref_chunk <- tibble::tibble(
        Latitude = as.numeric(chunk$BG_LATITUDE),
        Longitude = as.numeric(chunk$BG_LONGITUDE),
        Full_Address = full_addr,
        Civic_Number = civic_num,
        Street_Name = street_name,
        Standardized_Street_Name = street_name,
        Street_Type = street_type,
        Street_Direction = street_dir,
        Census_Subdivision_Name = csd,
        City = csd,
        Processed_City = csd,
        Postal_Code = postal,
        Province_or_Territory_Unique_Identifier = as.character(chunk$PROV_CODE),
        province_2letter = prov_2letter,
        city_norm = city_norm,
        fsa = fsa,
        address_norm = address_norm,
        street_only_norm = street_only_norm
      )
      duckdb::dbWriteTable(con, "ref", ref_chunk, append = ref_created, overwrite = !ref_created)
      ref_created <- TRUE
      rm(chunk, loc_sub, ref_chunk); gc()
    }
  }
  DBI::dbExecute(con, "DROP TABLE IF EXISTS loc")
  n_ref <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM ref")$n
  message("[NAR] Done. Table 'ref': ", format(n_ref, big.mark = ","), " rows")
  invisible(db_path)
}
