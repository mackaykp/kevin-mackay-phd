# =============================================================================
# Query reference table from DuckDB (NAR geocoding)
# Narrow pool: province + city + (full 6-digit postal when available, else FSA).
# Centroid fallbacks: postal (6-digit) -> FSA (3-digit) -> city.
# =============================================================================

#' Get one row from the ref table to use as template for empty tibbles (0-row same structure)
#' @param conn DuckDB connection
#' @return Tibble with 0 rows and same columns as ref table
get_ref_empty_template <- function(conn) {
  q <- "SELECT * FROM ref LIMIT 0"
  out <- DBI::dbGetQuery(conn, q)
  if (ncol(out) > 0) return(tibble::as_tibble(out))
  q1 <- "SELECT * FROM ref LIMIT 1"
  one <- DBI::dbGetQuery(conn, q1)
  if (nrow(one) == 0) return(tibble::tibble())
  tibble::as_tibble(one[0, ])
}

#' Query reference rows for exact match: address_norm IN (vector of normalized addresses).
#' Returns one row per address_norm (first occurrence) for join.
#' @param conn DuckDB connection
#' @param address_norms Character vector of normalized addresses to match
#' @return Tibble with columns including address_norm, Latitude, Longitude, Full_Address, etc.
query_ref_exact <- function(conn, address_norms) {
  if (length(address_norms) == 0) return(get_ref_empty_template(conn))
  address_norms <- unique(as.character(address_norms))
  address_norms <- address_norms[!is.na(address_norms) & nzchar(address_norms)]
  if (length(address_norms) == 0) return(get_ref_empty_template(conn))
  escaped <- gsub("'", "''", address_norms, fixed = TRUE)
  in_list <- paste0("'", escaped, "'", collapse = ", ")
  q <- paste0("SELECT * FROM ref WHERE address_norm IN (", in_list, ")")
  out <- DBI::dbGetQuery(conn, q)
  out <- tibble::as_tibble(out)
  if (nrow(out) == 0) return(out)
  out <- out %>% dplyr::group_by(address_norm) %>% dplyr::slice(1) %>% dplyr::ungroup()
  out
}

#' Query reference rows for exact street-only match: street_only_norm IN (vector).
#' Used when ref table has street_only_norm column (e.g. NAR build). Returns one row per street_only_norm.
#' @param conn DuckDB connection
#' @param street_only_norms Character vector of normalized "number + street" strings
#' @return Tibble with ref rows (or empty if no street_only_norm column or no matches)
query_ref_exact_street <- function(conn, street_only_norms) {
  if (length(street_only_norms) == 0) return(get_ref_empty_template(conn))
  street_only_norms <- unique(as.character(street_only_norms))
  street_only_norms <- street_only_norms[!is.na(street_only_norms) & nzchar(street_only_norms)]
  if (length(street_only_norms) == 0) return(get_ref_empty_template(conn))
  # Check for street_only_norm column (optional; NAR build adds it)
  cols <- DBI::dbGetQuery(conn, "SELECT column_name FROM (DESCRIBE ref) WHERE column_name = 'street_only_norm'")
  if (nrow(cols) == 0) return(get_ref_empty_template(conn))
  escaped <- gsub("'", "''", street_only_norms, fixed = TRUE)
  in_list <- paste0("'", escaped, "'", collapse = ", ")
  q <- paste0("SELECT * FROM ref WHERE street_only_norm IN (", in_list, ")")
  out <- DBI::dbGetQuery(conn, q)
  out <- tibble::as_tibble(out)
  if (nrow(out) == 0) return(out)
  out <- out %>% dplyr::group_by(street_only_norm) %>% dplyr::slice(1) %>% dplyr::ungroup()
  out
}

#' Exact street-only match within the same narrow geography (province + city + postal/FSA).
#' Ensures we never return a ref point from another city/FSA for the same street name.
#' @param conn DuckDB connection
#' @param street_only_norms Character vector of normalized "number + street" strings
#' @param province_2letter Two-letter province code
#' @param city_norm Normalized city (from normalize_city_for_match)
#' @param fsa First 3 chars of postal (used when full_postal not provided)
#' @param full_postal Optional 6-char normalized postal. When set, filters by full postal instead of FSA.
#' @param use_city If FALSE and full_postal is set, omit city filter (same as narrow pool logic).
#' @return Tibble of ref rows (one per street_only_norm in this geography), or empty
query_ref_exact_street_in_geography <- function(conn, street_only_norms, province_2letter, city_norm, fsa, full_postal = NULL, use_city = TRUE) {
  if (length(street_only_norms) == 0) return(get_ref_empty_template(conn))
  street_only_norms <- unique(as.character(street_only_norms))
  street_only_norms <- street_only_norms[!is.na(street_only_norms) & nzchar(street_only_norms)]
  if (length(street_only_norms) == 0) return(get_ref_empty_template(conn))
  cols <- DBI::dbGetQuery(conn, "SELECT column_name FROM (DESCRIBE ref) WHERE column_name = 'street_only_norm'")
  if (nrow(cols) == 0) return(get_ref_empty_template(conn))
  prov <- as.character(province_2letter)[1]
  city <- as.character(city_norm)[1]
  if (is.na(prov) || !nzchar(trimws(prov))) return(get_ref_empty_template(conn))
  prov_s <- .sql_esc(toupper(trimws(prov)))
  city_s <- .sql_esc(tolower(trimws(city)))
  escaped_street <- gsub("'", "''", street_only_norms, fixed = TRUE)
  in_list <- paste0("'", escaped_street, "'", collapse = ", ")
  where <- paste0("WHERE street_only_norm IN (", in_list, ") AND province_2letter = '", prov_s, "'")
  if (use_city && nzchar(trimws(city))) where <- paste0(where, " AND city_norm = '", city_s, "'")
  full_postal_val <- if (length(full_postal) && !is.na(full_postal[1]) && nchar(trimws(full_postal[1])) == 6)
    .sql_esc(toupper(trimws(full_postal[1]))) else NA_character_
  if (!is.na(full_postal_val)) {
    where <- paste0(where, " AND UPPER(REPLACE(TRIM(COALESCE(Postal_Code,'')), ' ', '')) = '", full_postal_val, "'")
  } else {
    fsa_val <- if (length(fsa) && !is.na(fsa[1]) && nzchar(trimws(fsa[1]))) .sql_esc(toupper(trimws(fsa[1]))) else NA_character_
    if (!is.na(fsa_val)) where <- paste0(where, " AND fsa = '", fsa_val, "'")
  }
  q <- paste0("SELECT * FROM ref ", where)
  out <- DBI::dbGetQuery(conn, q)
  out <- tibble::as_tibble(out)
  if (nrow(out) == 0) return(out)
  out <- out %>% dplyr::group_by(street_only_norm) %>% dplyr::slice(1) %>% dplyr::ungroup()
  out
}

#' Escape string for SQL single-quote
.sql_esc <- function(x) gsub("'", "''", as.character(x), fixed = TRUE)

#' Query reference candidates for fuzzy matching in narrow geography only.
#' When full_postal (6-char) is provided, filters by province + (optionally city) + full postal.
#' Otherwise filters by province + city + FSA (first 3 chars). use_city=FALSE only applies when full_postal is set.
#' @param conn DuckDB connection
#' @param province_2letter Two-letter province code (e.g. "ON")
#' @param city_norm Normalized city from normalize_city_for_match
#' @param fsa First 3 chars of postal (used when full_postal is not provided)
#' @param full_postal Optional 6-char normalized postal (from normalize_postal). When set, overrides fsa filter.
#' @param use_city If FALSE and full_postal is set, omit city filter (province + postal only). When only FSA, city is always used.
#' @param limit Max rows to return (default 50000)
#' @param street_prefix Optional first 2+ chars of street name to filter in SQL
#' @return Tibble of reference rows
query_ref_candidates_narrow <- function(conn, province_2letter, city_norm, fsa, full_postal = NULL, use_city = TRUE, limit = 50000L, street_prefix = NULL) {
  prov <- as.character(province_2letter)[1]
  city <- as.character(city_norm)[1]
  if (is.na(prov) || !nzchar(trimws(prov))) prov <- ""
  if (is.na(city) || !nzchar(trimws(city))) city <- ""
  prov_s <- .sql_esc(toupper(trimws(prov)))
  city_s <- .sql_esc(tolower(trimws(city)))
  where <- paste0("WHERE province_2letter = '", prov_s, "'")
  if (use_city && nzchar(city)) where <- paste0(where, " AND city_norm = '", city_s, "'")
  full_postal_val <- if (length(full_postal) && !is.na(full_postal[1]) && nchar(trimws(full_postal[1])) == 6)
    .sql_esc(toupper(trimws(full_postal[1]))) else NA_character_
  if (!is.na(full_postal_val)) {
    where <- paste0(where, " AND UPPER(REPLACE(TRIM(COALESCE(Postal_Code,'')), ' ', '')) = '", full_postal_val, "'")
  } else {
    fsa_val <- if (length(fsa) && !is.na(fsa[1]) && nzchar(trimws(fsa[1]))) as.character(fsa[1]) else NA_character_
    fsa_s <- if (!is.na(fsa_val) && nzchar(fsa_val)) .sql_esc(toupper(trimws(fsa_val))) else NA_character_
    if (!is.na(fsa_s)) where <- paste0(where, " AND fsa = '", fsa_s, "'")
  }
  if (length(street_prefix) > 0 && !is.na(street_prefix[1]) && nchar(trimws(street_prefix[1])) >= 2) {
    pre <- .sql_esc(tolower(substr(trimws(street_prefix[1]), 1, 10)))
    where <- paste0(where, " AND LOWER(COALESCE(Standardized_Street_Name, Street_Name, '')) LIKE LOWER('", pre, "') || '%'")
  }
  q <- paste0("SELECT * FROM ref ", where, " LIMIT ", max(1L, as.integer(limit)))
  out <- DBI::dbGetQuery(conn, q)
  tibble::as_tibble(out)
}

#' Backward-compatible alias: same as query_ref_candidates_narrow (phase, full_postal, use_city ignored).
query_ref_candidates <- function(conn, province_2letter, city_norm, fsa, phase = 1L, limit = 50000L, street_prefix = NULL) {
  query_ref_candidates_narrow(conn, province_2letter, city_norm, fsa, full_postal = NULL, use_city = TRUE, limit, street_prefix)
}

#' Postal code (6-digit) centroid: one point per province + full postal code.
#' Higher resolution than FSA centroid. Used as fallback before FSA when input has full postal.
#' @param conn DuckDB connection
#' @param province_2letter Two-letter province code
#' @param postal_code Full 6-char postal (normalized via normalize_postal), e.g. "K1A0B1"
#' @return Tibble with one row (Latitude, Longitude = centroid; representative columns)
query_ref_postal_centroid <- function(conn, province_2letter, postal_code) {
  if (is.na(province_2letter) || !nzchar(trimws(province_2letter)) || is.na(postal_code) || nchar(trimws(postal_code)) != 6)
    return(get_ref_empty_template(conn))
  prov_s <- .sql_esc(toupper(trimws(province_2letter)[1]))
  pc_norm <- toupper(gsub("[^A-Za-z0-9]", "", trimws(as.character(postal_code)[1])))
  if (nchar(pc_norm) != 6) return(get_ref_empty_template(conn))
  pc_s <- .sql_esc(pc_norm)
  q <- paste0(
    "SELECT AVG(Latitude) AS Latitude, AVG(Longitude) AS Longitude, ",
    "MIN(Full_Address) AS Full_Address, MIN(Civic_Number) AS Civic_Number, MIN(Census_Subdivision_Name) AS City, ",
    "MIN(Postal_Code) AS Postal_Code, MIN(Processed_City) AS Processed_City, MIN(Census_Subdivision_Name) AS Census_Subdivision_Name, ",
    "MIN(Standardized_Street_Name) AS Standardized_Street_Name, MIN(Province_or_Territory_Unique_Identifier) AS Province_or_Territory_Unique_Identifier ",
    "FROM ref WHERE province_2letter = '", prov_s, "' AND UPPER(REPLACE(TRIM(COALESCE(Postal_Code,'')), ' ', '')) = '", pc_s, "'"
  )
  out <- DBI::dbGetQuery(conn, q)
  if (nrow(out) == 0 || (is.na(out$Latitude) && is.na(out$Longitude))) return(get_ref_empty_template(conn))
  tibble::as_tibble(out)
}

#' FSA (forward sortation area) centroid: one point per province + FSA (3-digit).
#' Used as fallback when no address-level or street-level match in that geography.
#' @param conn DuckDB connection
#' @param province_2letter Two-letter province code
#' @param fsa First 3 chars of postal (e.g. "K1A")
#' @return Tibble with one row: Latitude, Longitude (centroid), plus representative Full_Address, City, etc.
query_ref_fsa_centroid <- function(conn, province_2letter, fsa) {
  if (is.na(province_2letter) || !nzchar(trimws(province_2letter)) || is.na(fsa) || !nzchar(trimws(fsa)))
    return(get_ref_empty_template(conn))
  prov_s <- .sql_esc(toupper(trimws(province_2letter)[1]))
  fsa_s <- .sql_esc(toupper(trimws(fsa)[1]))
  q <- paste0(
    "SELECT AVG(Latitude) AS Latitude, AVG(Longitude) AS Longitude, ",
    "MIN(Full_Address) AS Full_Address, MIN(Civic_Number) AS Civic_Number, MIN(Census_Subdivision_Name) AS City, ",
    "MIN(Postal_Code) AS Postal_Code, MIN(Processed_City) AS Processed_City, MIN(Census_Subdivision_Name) AS Census_Subdivision_Name, ",
    "MIN(Standardized_Street_Name) AS Standardized_Street_Name, MIN(Province_or_Territory_Unique_Identifier) AS Province_or_Territory_Unique_Identifier ",
    "FROM ref WHERE province_2letter = '", prov_s, "' AND fsa = '", fsa_s, "'"
  )
  out <- DBI::dbGetQuery(conn, q)
  if (nrow(out) == 0 || (is.na(out$Latitude) && is.na(out$Longitude))) return(get_ref_empty_template(conn))
  tibble::as_tibble(out)
}

#' City (CSD) centroid: one point per province + city_norm.
#' Used as fallback when no address/street/fuzzy match and no FSA centroid.
#' @param conn DuckDB connection
#' @param province_2letter Two-letter province code
#' @param city_norm Normalized city from normalize_city_for_match
#' @return Tibble with one row: Latitude, Longitude (centroid), plus representative columns
query_ref_city_centroid <- function(conn, province_2letter, city_norm) {
  if (is.na(province_2letter) || !nzchar(trimws(province_2letter)) || is.na(city_norm) || !nzchar(trimws(city_norm)))
    return(get_ref_empty_template(conn))
  prov_s <- .sql_esc(toupper(trimws(province_2letter)[1]))
  city_s <- .sql_esc(tolower(trimws(city_norm)[1]))
  q <- paste0(
    "SELECT AVG(Latitude) AS Latitude, AVG(Longitude) AS Longitude, ",
    "MIN(Full_Address) AS Full_Address, MIN(Civic_Number) AS Civic_Number, MIN(Census_Subdivision_Name) AS City, ",
    "MIN(Postal_Code) AS Postal_Code, MIN(Processed_City) AS Processed_City, MIN(Census_Subdivision_Name) AS Census_Subdivision_Name, ",
    "MIN(Standardized_Street_Name) AS Standardized_Street_Name, MIN(Province_or_Territory_Unique_Identifier) AS Province_or_Territory_Unique_Identifier ",
    "FROM ref WHERE province_2letter = '", prov_s, "' AND city_norm = '", city_s, "'"
  )
  out <- DBI::dbGetQuery(conn, q)
  if (nrow(out) == 0 || (is.na(out$Latitude) && is.na(out$Longitude))) return(get_ref_empty_template(conn))
  tibble::as_tibble(out)
}
