# =============================================================================
# Offline Geocoding — single hierarchy (literature-based)
# Exact → exact_street → relaxed (same geography) → fuzzy (narrow pool only) →
# FSA centroid → city centroid. No cross-FSA or cross-city point matches.
# =============================================================================

#' Build a normalized full-address field for reference table (for exact matching)
ref_normalized_address <- function(ref) {
  if ("Full_Address" %in% names(ref)) {
    return(normalize_string(ref$Full_Address))
  }
  num <- if ("Civic_Number" %in% names(ref)) as.character(ref$Civic_Number) else ""
  street <- if ("Standardized_Street_Name" %in% names(ref)) ref$Standardized_Street_Name else (ref[["Street_Name"]] %||% "")
  type <- if ("Standardized_Street_Type" %in% names(ref)) ref$Standardized_Street_Type else (ref[["Street_Type"]] %||% "")
  dir_ <- if ("Standardized_Street_Direction" %in% names(ref)) ref$Standardized_Street_Direction else (ref[["Street_Direction"]] %||% "")
  full <- stringr::str_trim(paste(num, street, type, dir_))
  normalize_string(full)
}

#' %||% helper
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

first_char_civic <- function(x) {
  if (is.na(x) || (is.character(x) && !nzchar(trimws(x)))) return(NA_character_)
  s <- trimws(as.character(x))
  d <- stringr::str_extract(s, "^[0-9]")
  if (!is.na(d)) d else NA_character_
}
first_char_alpha <- function(x) {
  if (is.na(x) || (is.character(x) && !nzchar(trimws(x)))) return(NA_character_)
  s <- tolower(trimws(as.character(x)))
  a <- stringr::str_extract(s, "[a-z]")
  if (!is.na(a)) a else NA_character_
}

get_available_memory_bytes <- function() {
  tryCatch({
    if (.Platform$OS.type == "windows") {
      out <- tryCatch(
        system2("wmic", args = c("OS", "get", "FreePhysicalMemory", "/value"), stdout = TRUE, stderr = NULL),
        error = function(e) character(0)
      )
      if (length(out) > 0) {
        line <- grep("^FreePhysicalMemory=", out, value = TRUE, ignore.case = TRUE)
        if (length(line) > 0) {
          kb <- as.numeric(sub("^FreePhysicalMemory=", "", line[1], ignore.case = TRUE))
          if (!is.na(kb) && kb > 0) return(kb * 1024)
        }
      }
      if (exists("memory.limit", mode = "function")) {
        lim_mb <- memory.limit()
        if (is.numeric(lim_mb) && lim_mb > 0) return(lim_mb * 1024 * 1024 * 0.5)
      }
      return(NA_real_)
    }
    if (file.exists("/proc/meminfo")) {
      x <- readLines("/proc/meminfo", n = 20)
      avail <- grep("^MemAvailable:", x, value = TRUE)
      if (length(avail) == 0) avail <- grep("^MemFree:", x, value = TRUE)
      if (length(avail) == 0) return(NA_real_)
      kb <- as.numeric(strsplit(avail[1], "\\s+")[[1]][2])
      if (is.na(kb)) return(NA_real_)
      return(kb * 1024)
    }
    out <- system2("vm_stat", stdout = TRUE, stderr = NULL)
    if (length(out) == 0) return(NA_real_)
    free <- grep("Pages free", out, value = TRUE)
    if (length(free) == 0) return(NA_real_)
    pages <- as.numeric(gsub("[^0-9]", "", free))
    if (is.na(pages)) return(NA_real_)
    return(pages * 4096)
  }, error = function(e) NA_real_)
}

max_candidate_pool_from_memory <- function(bytes_per_row = 2500, memory_fraction = 0.25, min_pool = 50000L, max_pool = 400000L) {
  avail <- get_available_memory_bytes()
  if (is.na(avail) || avail <= 0) return(300000L)
  n <- as.integer((avail * memory_fraction) / bytes_per_row)
  n <- max(min_pool, min(max_pool, n))
  n
}

#' Geocode addresses using reference table in DuckDB (single hierarchy, no phases).
#'
#' Match order: exact (full) → exact (street only) → relaxed (attribute relaxation in same
#' province+city+FSA) → fuzzy (narrow pool, strict threshold) → FSA centroid → city centroid.
#' Unmatched rows get no coordinates (no wrong-geography point matches).
#'
#' @param addresses Data frame with address columns, or character vector of addresses.
#' @param ref_duckdb Path to .duckdb file or duckdb_connection. Table "ref" required.
#' @param address_col Column name when addresses is a data frame. Default "address".
#' @param address_cols Optional. e.g. c("Street", "City", "Province", "Postal Code").
#' @param auto_select If TRUE, return single best match per address.
#' @param fuzzy_threshold Minimum composite score for fuzzy match (default 0.75).
#' @param score_weights Weights for composite: c(text, number, geo). Default c(0.5, 0.25, 0.25).
#' @param max_fuzzy_candidates Max candidates to score per address (default 50).
#' @param max_candidate_pool Max ref rows per narrow group (default from memory).
#' @param progress_interval Report progress every N fuzzy addresses (0 = off).
#' @param use_first_char_filter Restrict pool by first char of civic/street/city/postal.
#' @param use_city_in_narrow_pool If TRUE (default), narrow pool requires province + city + postal/FSA. If FALSE, when full 6-digit postal is present the pool uses province + postal only (city omitted to avoid city-name mismatches). When only FSA is available, city is always used.
#' @return Tibble with match_type (exact, exact_street, relaxed, fuzzy, postal_centroid, fsa_centroid, city_centroid), scores, ref columns.
geocode_ref <- function(addresses,
                        ref_duckdb,
                        address_col = "address",
                        address_cols = NULL,
                        auto_select = TRUE,
                        fuzzy_threshold = 0.75,
                        score_weights = c(text = 0.5, number = 0.25, geo = 0.25),
                        max_fuzzy_candidates = 50,
                        max_candidate_pool = NULL,
                        progress_interval = 100L,
                        use_first_char_filter = TRUE,
                        use_city_in_narrow_pool = TRUE) {

  if (is.null(ref_duckdb)) stop("ref_duckdb is required (path or duckdb_connection).")
  conn <- NULL
  conn_owned <- FALSE
  if (is.character(ref_duckdb) && length(ref_duckdb) == 1 && nzchar(ref_duckdb)) {
    if (!requireNamespace("duckdb", quietly = TRUE)) stop("Install duckdb: install.packages(\"duckdb\")")
    if (!file.exists(ref_duckdb)) stop("DuckDB file not found: ", ref_duckdb)
    conn <- duckdb::dbConnect(duckdb::duckdb(), ref_duckdb, read_only = TRUE)
    conn_owned <- TRUE
    on.exit(if (conn_owned && !is.null(conn)) { duckdb::dbDisconnect(conn, shutdown = TRUE); conn_owned <- FALSE }, add = TRUE)
  } else if (inherits(ref_duckdb, "duckdb_connection")) {
    conn <- ref_duckdb
  } else {
    stop("ref_duckdb must be a path (character) or duckdb_connection.")
  }

  if (is.data.frame(addresses)) {
    if (!is.null(address_cols) && length(address_cols) > 0) {
      missing <- setdiff(address_cols, names(addresses))
      if (length(missing)) stop("address_cols not found: ", paste(missing, collapse = ", "))
      parts <- lapply(address_cols, function(col) {
        x <- as.character(addresses[[col]])
        x[is.na(x) | trimws(x) == ""] <- NA_character_
        trimws(x)
      })
      addr_vec <- vapply(seq_len(nrow(addresses)), function(i) {
        p <- vapply(parts, function(col) col[i], character(1))
        p <- p[!is.na(p) & nzchar(p)]
        paste(p, collapse = ", ")
      }, character(1))
    } else {
      addr_vec <- addresses[[address_col]]
      if (is.null(addr_vec)) stop("Column '", address_col, "' not found.")
    }
    input_df <- addresses
    input_df$.addr_input <- addr_vec
  } else {
    addr_vec <- as.character(addresses)
    input_df <- tibble::tibble(.addr_input = addr_vec)
  }
  if (!".row_id" %in% names(input_df)) input_df$.row_id <- seq_len(nrow(input_df))

  if (!exists("get_ref_empty_template", mode = "function"))
    source("R/query_ref_duckdb.R")
  empty_ref <- get_ref_empty_template(conn)
  if (ncol(empty_ref) > 0) {
    empty_ref$.ref_norm <- character(0)
    empty_ref$.ref_fsa <- character(0)
    empty_ref$.ref_idx <- integer(0)
  }

  n_total <- length(addr_vec)
  message("[GEO] N = ", n_total, "  |  Order: exact → exact_street → relaxed → fuzzy → postal → FSA → city centroid")
  parsed <- parse_address_components(addr_vec)
  parsed$.row_id <- input_df$.row_id
  parsed$street_only_norm <- normalize_string(
    paste(ifelse(is.na(parsed$civic_number), "", as.character(parsed$civic_number)), parsed$street_part)
  )

  # ---- Step 1: Exact full address ----
  message("[GEO] Step 1: Exact (full address)")
  exact_df <- query_ref_exact(conn, unique(parsed$address_normalized))
  ref_keys <- if (nrow(exact_df) > 0) unique(exact_df$address_norm) else character(0)
  ref_one_per_norm <- if (nrow(exact_df) > 0) {
    exact_df$.ref_norm <- exact_df$address_norm
    exact_df$.ref_idx <- seq_len(nrow(exact_df))
    exact_df %>%
      dplyr::group_by(.ref_norm) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(.ref_idx, .ref_norm, dplyr::any_of(c("Latitude", "Longitude", "Full_Address", "Civic_Number", "City", "Postal_Code", "Processed_City", "Census_Subdivision_Name", "Standardized_Street_Name", "Province_or_Territory_Unique_Identifier")))
  } else {
    empty_ref %>% dplyr::select(dplyr::any_of(c(".ref_idx", ".ref_norm", "Latitude", "Longitude", "Full_Address", "Civic_Number", "City", "Postal_Code", "Processed_City", "Census_Subdivision_Name", "Standardized_Street_Name", "Province_or_Territory_Unique_Identifier")))
  }
  out_exact <- input_df %>%
    dplyr::inner_join(parsed %>% dplyr::select(.row_id, address_normalized), by = ".row_id") %>%
    dplyr::filter(address_normalized %in% ref_keys) %>%
    dplyr::left_join(ref_one_per_norm, by = c("address_normalized" = ".ref_norm")) %>%
    dplyr::mutate(
      match_type = "exact",
      text_score = 1, number_score = 1, geo_score = 1, composite_score = 1,
      same_city = TRUE, civic_number_exact = TRUE, match_quality = "high"
    )
  n_1 <- nrow(out_exact)
  message("[GEO]   Step 1: ", n_1, " exact")

  # ---- Step 2: Exact street only (within same geography: province + city + postal/FSA) ----
  still_unmatched <- input_df %>% dplyr::filter(!.row_id %in% out_exact$.row_id)
  parsed_street <- parsed %>% dplyr::filter(.row_id %in% still_unmatched$.row_id)
  if (nrow(parsed_street) > 0) {
    # Attach geography for batching (same logic as fuzzy step)
    ps_join <- parsed_street %>%
      dplyr::left_join(still_unmatched %>% dplyr::select(.row_id, dplyr::any_of(c("City", "Municipality"))), by = ".row_id")
    city_for_norm <- dplyr::coalesce(ps_join$City, ps_join$Municipality, ps_join$city)
    parsed_street$city_norm <- normalize_city_for_match(trimws(as.character(city_for_norm)))
    parsed_street$prov_2letter <- REF_PROVINCE_SGC[as.character(parsed_street$province)]
    na_prov <- is.na(parsed_street$prov_2letter) & !is.na(parsed_street$province) & nchar(trimws(parsed_street$province)) == 2
    parsed_street$prov_2letter[na_prov] <- toupper(trimws(parsed_street$province[na_prov]))
    idx_na <- which(is.na(parsed_street$prov_2letter) & nzchar(trimws(as.character(parsed_street$province))))
    if (length(idx_na) > 0 && exists("PROVINCE_NAME_TO_2LETTER")) {
      pl <- tolower(trimws(as.character(parsed_street$province[idx_na])))
      parsed_street$prov_2letter[idx_na[pl %in% names(PROVINCE_NAME_TO_2LETTER)]] <- PROVINCE_NAME_TO_2LETTER[pl[pl %in% names(PROVINCE_NAME_TO_2LETTER)]]
    }
    parsed_street$prov_2letter[is.na(parsed_street$prov_2letter)] <- ""
    parsed_street$input_fsa <- NA_character_
    for (k in seq_along(parsed_street$postal_code)) {
      pc <- parsed_street$postal_code[k]
      if (!is.na(pc) && nzchar(trimws(as.character(pc))))
        parsed_street$input_fsa[k] <- toupper(stringr::str_replace_all(substr(trimws(as.character(pc)), 1, 3), "[^A-Za-z0-9]", ""))
    }
    parsed_street$input_fsa[!nzchar(parsed_street$input_fsa)] <- NA_character_
    parsed_street$input_postal_norm <- normalize_postal(parsed_street$postal_code)
    grp_postal_or_fsa <- ifelse(nchar(parsed_street$input_postal_norm) == 6, parsed_street$input_postal_norm, parsed_street$input_fsa)
    grp_postal_or_fsa[is.na(grp_postal_or_fsa)] <- ""
    parsed_street$grp_key <- paste(parsed_street$prov_2letter, parsed_street$city_norm, grp_postal_or_fsa, sep = "\r")
    grps <- unique(parsed_street$grp_key)
    street_matches_list <- vector("list", nrow(parsed_street))
    for (gk in grps) {
      idx <- which(parsed_street$grp_key == gk)
      if (length(idx) == 0) next
      prov_2letter <- parsed_street$prov_2letter[idx[1]]
      city_norm <- parsed_street$city_norm[idx[1]]
      input_fsa <- parsed_street$input_fsa[idx[1]]
      full_postal <- if (nchar(parsed_street$input_postal_norm[idx[1]]) == 6) parsed_street$input_postal_norm[idx[1]] else NULL
      use_city <- is.null(full_postal) || nchar(trimws(full_postal %||% "")) != 6 || use_city_in_narrow_pool
      street_norms <- unique(parsed_street$street_only_norm[idx])
      street_norms <- street_norms[!is.na(street_norms) & nzchar(street_norms)]
      if (length(street_norms) == 0) next
      street_df <- query_ref_exact_street_in_geography(conn, street_norms, prov_2letter, city_norm, input_fsa, full_postal, use_city)
      if (nrow(street_df) == 0) next
      street_ref_keys <- street_df$street_only_norm
      street_ref_one <- street_df %>%
        dplyr::select(.ref_norm = street_only_norm, dplyr::any_of(c("Latitude", "Longitude", "Full_Address", "Civic_Number", "City", "Postal_Code", "Processed_City", "Census_Subdivision_Name", "Standardized_Street_Name", "Province_or_Territory_Unique_Identifier")))
      for (i in idx) {
        sn <- parsed_street$street_only_norm[i]
        if (!sn %in% street_ref_keys) next
        row_id <- parsed_street$.row_id[i]
        best_row <- street_ref_one[street_ref_one$.ref_norm == sn, ][1, ]
        street_matches_list[[i]] <- tibble::tibble(
          .row_id = row_id,
          Latitude = best_row$Latitude, Longitude = best_row$Longitude,
          Full_Address = best_row$Full_Address, Civic_Number = best_row$Civic_Number,
          City = best_row$City, Postal_Code = best_row$Postal_Code, Processed_City = best_row$Processed_City,
          Census_Subdivision_Name = best_row$Census_Subdivision_Name, Standardized_Street_Name = best_row$Standardized_Street_Name,
          Province_or_Territory_Unique_Identifier = best_row$Province_or_Territory_Unique_Identifier
        )
      }
    }
    out_exact_street_df <- dplyr::bind_rows(street_matches_list[!sapply(street_matches_list, is.null)])
    if (nrow(out_exact_street_df) > 0) {
      out_exact_street <- still_unmatched %>%
        dplyr::inner_join(out_exact_street_df, by = ".row_id") %>%
        dplyr::mutate(
          match_type = "exact_street",
          text_score = 1, number_score = 1, geo_score = 1, composite_score = 1,
          same_city = TRUE, civic_number_exact = TRUE, match_quality = "high"
        )
      out_exact <- dplyr::bind_rows(out_exact, out_exact_street)
      message("[GEO]   Step 2: ", nrow(out_exact_street), " exact_street")
    }
  }

  to_fuzzy <- input_df %>% dplyr::filter(!.row_id %in% out_exact$.row_id)
  n_fuzzy <- nrow(to_fuzzy)
  if (n_fuzzy == 0) {
    message("[GEO] All matched in steps 1–2. Total: ", nrow(out_exact))
    return(out_exact %>%
             dplyr::select(-dplyr::any_of(c("address_normalized", "street_only_norm", ".ref_idx"))) %>%
             dplyr::rename(address = .addr_input) %>%
             dplyr::arrange(.row_id))
  }

  parsed_fuzzy <- parsed %>% dplyr::filter(.row_id %in% to_fuzzy$.row_id)
  city_for_norm <- if ("City" %in% names(to_fuzzy)) to_fuzzy$City[match(parsed_fuzzy$.row_id, to_fuzzy$.row_id)] else parsed_fuzzy$city
  city_for_norm[is.na(city_for_norm)] <- parsed_fuzzy$city[is.na(city_for_norm)]
  if ("Municipality" %in% names(to_fuzzy)) {
    miss <- is.na(city_for_norm) | !nzchar(trimws(as.character(city_for_norm)))
    if (any(miss)) city_for_norm[miss] <- to_fuzzy$Municipality[match(parsed_fuzzy$.row_id[miss], to_fuzzy$.row_id)]
  }
  parsed_fuzzy$city_norm <- normalize_city_for_match(trimws(as.character(city_for_norm)))
  parsed_fuzzy$prov_2letter <- REF_PROVINCE_SGC[as.character(parsed_fuzzy$province)]
  na_prov <- is.na(parsed_fuzzy$prov_2letter) & !is.na(parsed_fuzzy$province) & nchar(trimws(parsed_fuzzy$province)) == 2
  parsed_fuzzy$prov_2letter[na_prov] <- toupper(trimws(parsed_fuzzy$province[na_prov]))
  idx_na <- which(is.na(parsed_fuzzy$prov_2letter) & nzchar(trimws(as.character(parsed_fuzzy$province))))
  if (length(idx_na) > 0 && exists("PROVINCE_NAME_TO_2LETTER")) {
    pl <- tolower(trimws(as.character(parsed_fuzzy$province[idx_na])))
    parsed_fuzzy$prov_2letter[idx_na[pl %in% names(PROVINCE_NAME_TO_2LETTER)]] <- PROVINCE_NAME_TO_2LETTER[pl[pl %in% names(PROVINCE_NAME_TO_2LETTER)]]
  }
  parsed_fuzzy$prov_2letter[is.na(parsed_fuzzy$prov_2letter)] <- ""
  parsed_fuzzy$input_fsa <- NA_character_
  for (k in seq_along(parsed_fuzzy$postal_code)) {
    pc <- parsed_fuzzy$postal_code[k]
    if (!is.na(pc) && nzchar(trimws(as.character(pc))))
      parsed_fuzzy$input_fsa[k] <- toupper(stringr::str_replace_all(substr(trimws(as.character(pc)), 1, 3), "[^A-Za-z0-9]", ""))
  }
  parsed_fuzzy$input_fsa[!nzchar(parsed_fuzzy$input_fsa)] <- NA_character_
  parsed_fuzzy$input_postal_norm <- normalize_postal(parsed_fuzzy$postal_code)

  if (is.null(max_candidate_pool)) {
    max_candidate_pool <- max_candidate_pool_from_memory()
    message("[GEO] Pool limit: ", format(max_candidate_pool, big.mark = ","), " per (prov, city, postal/FSA)")
  }
  pool_limit <- max_candidate_pool %||% 50000L
  use_dt <- requireNamespace("data.table", quietly = TRUE)

  # ---- Step 3: Relaxed (attribute relaxation in narrow geography) ----
  message("[GEO] Step 3: Relaxed")
  relaxed_list <- vector("list", n_fuzzy)
  grp_postal_or_fsa <- ifelse(nchar(parsed_fuzzy$input_postal_norm) == 6, parsed_fuzzy$input_postal_norm, parsed_fuzzy$input_fsa)
  grp_postal_or_fsa[is.na(grp_postal_or_fsa)] <- ""
  grp_key <- paste(parsed_fuzzy$prov_2letter, parsed_fuzzy$city_norm, grp_postal_or_fsa, sep = "\r")
  grps <- unique(grp_key)
  relaxed_norms <- build_relaxed_address_norms(parsed_fuzzy$address_normalized)

  for (gi in seq_along(grps)) {
    gk <- grps[gi]
    idx_in <- which(grp_key == gk)
    prov_2letter <- parsed_fuzzy$prov_2letter[idx_in[1]]
    city_norm <- parsed_fuzzy$city_norm[idx_in[1]]
    input_fsa <- parsed_fuzzy$input_fsa[idx_in[1]]
    full_postal <- if (nchar(parsed_fuzzy$input_postal_norm[idx_in[1]]) == 6) parsed_fuzzy$input_postal_norm[idx_in[1]] else NULL
    use_city <- use_city_in_narrow_pool || is.null(full_postal) || nchar(trimws(full_postal %||% "")) != 6
    cand <- query_ref_candidates_narrow(conn, prov_2letter, city_norm, input_fsa, full_postal = full_postal, use_city = use_city, limit = pool_limit)
    if (nrow(cand) == 0) next
    ref_norms <- cand$address_norm
    for (ii in idx_in) {
      row_id <- parsed_fuzzy$.row_id[ii]
      variants <- relaxed_norms[[ii]]
      if (length(variants) == 0) next
      hit <- ref_norms %in% variants
      if (!any(hit)) next
      best_row <- cand[which(hit)[1], ]
      relaxed_list[[ii]] <- tibble::tibble(
        .row_id = row_id, .addr_input = to_fuzzy$.addr_input[to_fuzzy$.row_id == row_id][1],
        Latitude = best_row[["Latitude"]], Longitude = best_row[["Longitude"]],
        Full_Address = best_row[["Full_Address"]], Civic_Number = best_row[["Civic_Number"]],
        City = best_row[["Census_Subdivision_Name"]] %||% best_row[["City"]], Postal_Code = best_row[["Postal_Code"]],
        Processed_City = best_row[["Processed_City"]], Standardized_Street_Name = best_row[["Standardized_Street_Name"]],
        Province_or_Territory_Unique_Identifier = best_row[["Province_or_Territory_Unique_Identifier"]],
        match_type = "relaxed", text_score = 1, number_score = 1, geo_score = 1, composite_score = 1,
        same_city = TRUE, civic_number_exact = TRUE, match_quality = "medium"
      )
    }
    rm(cand); gc()
  }
  out_relaxed <- dplyr::bind_rows(relaxed_list[!sapply(relaxed_list, is.null)])
  n_relaxed <- nrow(out_relaxed)
  if (n_relaxed > 0) message("[GEO]   Step 3: ", n_relaxed, " relaxed")
  out_so_far <- dplyr::bind_rows(out_exact, out_relaxed)
  to_fuzzy <- to_fuzzy %>% dplyr::filter(!.row_id %in% out_so_far$.row_id)
  parsed_fuzzy <- parsed_fuzzy %>% dplyr::filter(.row_id %in% to_fuzzy$.row_id)
  n_fuzzy <- nrow(to_fuzzy)
  if (n_fuzzy == 0) {
    message("[GEO] Done. Total matched: ", nrow(out_so_far))
    return(out_so_far %>%
             dplyr::select(-dplyr::any_of(c("address_normalized", "street_only_norm", ".ref_idx"))) %>%
             dplyr::rename(address = .addr_input) %>%
             dplyr::arrange(.row_id))
  }

  # ---- Step 4: Fuzzy (narrow pool only, strict threshold) ----
  message("[GEO] Step 4: Fuzzy (threshold ", fuzzy_threshold, ")")
  grp_postal_or_fsa <- ifelse(nchar(parsed_fuzzy$input_postal_norm) == 6, parsed_fuzzy$input_postal_norm, parsed_fuzzy$input_fsa)
  grp_postal_or_fsa[is.na(grp_postal_or_fsa)] <- ""
  grp_key <- paste(parsed_fuzzy$prov_2letter, parsed_fuzzy$city_norm, grp_postal_or_fsa, sep = "\r")
  grps <- unique(grp_key)
  fuzzy_list <- vector("list", nrow(parsed_fuzzy))
  min_street_text_score <- 0.5

  for (gi in seq_along(grps)) {
    gk <- grps[gi]
    idx_in_fuzzy <- which(grp_key == gk)
    if (progress_interval > 0 && (gi %% max(1L, length(grps) %/% 10) == 0 || gi == 1 || gi == length(grps)))
      message("[GEO]   Fuzzy batch ", gi, "/", length(grps), " (", length(idx_in_fuzzy), " addr)")
    first_i <- idx_in_fuzzy[1]
    full_postal <- if (nchar(parsed_fuzzy$input_postal_norm[first_i]) == 6) parsed_fuzzy$input_postal_norm[first_i] else NULL
    use_city <- use_city_in_narrow_pool || is.null(full_postal) || nchar(trimws(full_postal %||% "")) != 6
    cand <- query_ref_candidates_narrow(conn, parsed_fuzzy$prov_2letter[first_i], parsed_fuzzy$city_norm[first_i], parsed_fuzzy$input_fsa[first_i], full_postal = full_postal, use_city = use_city, limit = pool_limit)
    if (nrow(cand) > 0) {
      cand$.ref_norm <- cand$address_norm
      cand$.ref_fsa <- cand$fsa
      cand$.ref_idx <- seq_len(nrow(cand))
    } else {
      cand <- empty_ref
      if (nrow(empty_ref) == 0 && ncol(empty_ref) > 0) { cand$.ref_norm <- character(0); cand$.ref_fsa <- character(0); cand$.ref_idx <- integer(0) }
    }
    if (nrow(cand) == 0) { rm(cand); gc(); next }
    if (nrow(cand) > pool_limit) cand <- dplyr::slice(cand, sample.int(nrow(cand), pool_limit))

    for (ii in seq_along(idx_in_fuzzy)) {
      i <- idx_in_fuzzy[ii]
      row_id <- parsed_fuzzy$.row_id[i]
      addr_norm <- parsed_fuzzy$address_normalized[i]
      civic_in <- parsed_fuzzy$civic_number[i]
      street_in <- parsed_fuzzy$street_part[i]
      direction_in <- parsed_fuzzy$street_direction[i]
      city_in <- city_for_norm[i]
      prov_in <- parsed_fuzzy$province[i]
      postal_in <- parsed_fuzzy$postal_code[i]
      city_present <- !is.na(city_in) && nzchar(trimws(as.character(city_in)))

      cand_i <- cand
      if (use_first_char_filter && nrow(cand_i) > 0) {
        fc_civic <- first_char_civic(civic_in); fc_street <- first_char_alpha(street_in); fc_city <- first_char_alpha(city_in); fc_postal <- first_char_alpha(postal_in)
        ref_civic_str <- as.character(cand_i[["Civic_Number"]] %||% "")
        ref_first_civic <- stringr::str_extract(ref_civic_str, "^[0-9]")
        ref_street_col <- dplyr::coalesce(cand_i[["Standardized_Street_Name"]], cand_i[["Street_Name"]], "")
        ref_first_street <- stringr::str_extract(tolower(trimws(as.character(ref_street_col))), "[a-z]")
        ref_city_csd <- stringr::str_extract(tolower(trimws(as.character(cand_i[["Census_Subdivision_Name"]] %||% ""))), "[a-z]")
        ref_city_pc <- stringr::str_extract(tolower(trimws(as.character(cand_i[["Processed_City"]] %||% ""))), "[a-z]")
        ref_city_alt <- stringr::str_extract(tolower(trimws(as.character(cand_i[["City"]] %||% ""))), "[a-z]")
        keep_city <- if (!is.na(fc_city)) (ref_city_csd == fc_city | ref_city_pc == fc_city | ref_city_alt == fc_city) else rep(TRUE, nrow(cand_i))
        ref_first_postal <- stringr::str_extract(tolower(trimws(as.character(cand_i[["Postal_Code"]] %||% ""))), "[a-z]")
        keep <- rep(TRUE, nrow(cand_i))
        if (!is.na(fc_civic)) keep <- keep & (ref_first_civic == fc_civic)
        if (!is.na(fc_street)) keep <- keep & (ref_first_street == fc_street)
        if (!is.na(fc_city)) keep <- keep & keep_city
        if (!is.na(fc_postal)) keep <- keep & (ref_first_postal == fc_postal)
        keep[is.na(keep)] <- FALSE
        if (any(keep)) cand_i <- cand_i[keep, ]
      }
      if (nrow(cand_i) > 10000 && nchar(street_in) >= 2) {
        street_pre <- substr(street_in, 1, 2)
        ref_street_col <- dplyr::coalesce(cand_i[["Standardized_Street_Name"]], cand_i[["Street_Name"]], "")
        idx <- which(tolower(substr(ref_street_col, 1, 2)) == street_pre)
        if (length(idx) > 0) cand_i <- dplyr::slice(cand_i, idx)
      }
      if (nrow(cand_i) > max_fuzzy_candidates * 5) cand_i <- dplyr::slice(cand_i, sample.int(nrow(cand_i), max_fuzzy_candidates * 5))
      if (nrow(cand_i) == 0) next
      if (!is.na(direction_in) && nzchar(direction_in) && nrow(cand_i) > 0) {
        ref_dir_raw <- dplyr::coalesce(cand_i[["Standardized_Street_Direction"]], cand_i[["Street_Direction"]], "")
        cand_i$.ref_dir <- normalize_ref_direction(ref_dir_raw)
        no_dir <- is.na(cand_i$.ref_dir) | !cand_i$.ref_dir %in% c("n", "s", "e", "w")
        if (any(no_dir)) {
          end_dir <- stringr::str_trim(stringr::str_extract(cand_i$.ref_norm, "\\b(n|s|e|w)\\s*$"))
          end_dir[!end_dir %in% c("n", "s", "e", "w")] <- NA_character_
          cand_i$.ref_dir[no_dir] <- end_dir[no_dir]
        }
        cand_i <- cand_i %>% dplyr::filter(.ref_dir == direction_in) %>% dplyr::select(-.ref_dir)
      }
      if (nrow(cand_i) == 0) next

      ref_street <- dplyr::coalesce(cand_i[["Standardized_Street_Name"]], cand_i[["Street_Name"]], "")
      ref_full <- dplyr::coalesce(cand_i[["Full_Address"]], cand_i$.ref_norm, "")
      text_score <- pmax(score_text_similarity(addr_norm, ref_full), score_text_similarity(street_in, ref_street))
      street_score <- score_text_similarity(street_in, ref_street)
      ref_civic <- if ("Civic_Number" %in% names(cand_i)) cand_i$Civic_Number else rep(NA_real_, nrow(cand_i))
      num_score <- score_numeric_nearness(civic_in, ref_civic)
      ref_city_geo <- if ("Census_Subdivision_Name" %in% names(cand_i)) cand_i[["Census_Subdivision_Name"]] else dplyr::coalesce(cand_i[["Processed_City"]], cand_i[["City"]], "")
      geo_score <- score_geographic(prov_in, dplyr::coalesce(cand_i[["Province_or_Territory_Unique_Identifier"]], ""), city_in, ref_city_geo, postal_in, dplyr::coalesce(cand_i[["Postal_Code"]], ""))
      comp <- composite_score(text_score, num_score, geo_score, score_weights)
      cand_i$text_score <- text_score; cand_i$street_score <- street_score; cand_i$number_score <- num_score; cand_i$geo_score <- geo_score; cand_i$composite_score <- comp
      cand_i <- cand_i %>% dplyr::arrange(dplyr::desc(.[["composite_score"]])) %>% dplyr::slice_head(n = max_fuzzy_candidates)
      best <- if (use_dt) tibble::as_tibble(data.table::as.data.table(cand_i)[1]) else cand_i %>% dplyr::slice(1)
      accept <- nrow(best) > 0 && (best$composite_score[1] %||% 0) >= fuzzy_threshold && (best$street_score[1] %||% 0) >= min_street_text_score && (!city_present || (best$geo_score[1] %||% 0) > 0)
      if (isTRUE(accept)) {
        same_city_flag <- best$geo_score[1] >= 0.2
        civic_exact_flag <- best$number_score[1] == 1
        qual <- dplyr::case_when(same_city_flag && civic_exact_flag && best$text_score[1] >= 0.8 ~ "high", same_city_flag && (civic_exact_flag || best$text_score[1] >= 0.6) ~ "medium", TRUE ~ "low")
        fuzzy_list[[i]] <- tibble::tibble(
          .row_id = row_id, .addr_input = to_fuzzy$.addr_input[to_fuzzy$.row_id == row_id],
          Latitude = best[["Latitude"]] %||% NA_real_, Longitude = best[["Longitude"]] %||% NA_real_,
          Full_Address = best[["Full_Address"]] %||% NA_character_, Civic_Number = best[["Civic_Number"]] %||% NA_real_,
          City = best[["Census_Subdivision_Name"]] %||% best[["City"]] %||% NA_character_, Postal_Code = best[["Postal_Code"]] %||% NA_character_,
          Processed_City = best[["Processed_City"]] %||% NA_character_, Standardized_Street_Name = best[["Standardized_Street_Name"]] %||% NA_character_,
          Province_or_Territory_Unique_Identifier = best[["Province_or_Territory_Unique_Identifier"]] %||% NA_character_,
          match_type = "fuzzy", text_score = best$text_score, number_score = best$number_score, geo_score = best$geo_score, composite_score = best$composite_score,
          same_city = same_city_flag, civic_number_exact = civic_exact_flag, match_quality = qual
        )
      }
    }
    rm(cand); gc()
  }

  out_fuzzy <- dplyr::bind_rows(fuzzy_list[!sapply(fuzzy_list, is.null)])
  message("[GEO]   Step 4: ", nrow(out_fuzzy), " fuzzy")
  out_so_far <- dplyr::bind_rows(out_so_far, out_fuzzy)
  to_fuzzy <- to_fuzzy %>% dplyr::filter(!.row_id %in% out_so_far$.row_id)
  parsed_fuzzy <- parsed_fuzzy %>% dplyr::filter(.row_id %in% to_fuzzy$.row_id)
  n_remaining <- nrow(to_fuzzy)
  if (n_remaining == 0) {
    message("[GEO] Done. Total matched: ", nrow(out_so_far))
    return(out_so_far %>%
             dplyr::select(-dplyr::any_of(c("address_normalized", "street_only_norm", ".ref_idx"))) %>%
             dplyr::rename(address = .addr_input) %>%
             dplyr::arrange(.row_id))
  }

  # ---- Step 5: Postal code (6-digit) centroid ----
  message("[GEO] Step 5: Postal centroid (", n_remaining, " left)")
  postal_centroid_list <- vector("list", n_remaining)
  for (j in seq_len(n_remaining)) {
    row_id <- to_fuzzy$.row_id[j]
    prov_2letter <- parsed_fuzzy$prov_2letter[parsed_fuzzy$.row_id == row_id][1]
    input_postal_norm <- parsed_fuzzy$input_postal_norm[parsed_fuzzy$.row_id == row_id][1]
    if (is.na(prov_2letter) || !nzchar(prov_2letter) || is.na(input_postal_norm) || nchar(input_postal_norm) != 6) next
    cen <- query_ref_postal_centroid(conn, prov_2letter, input_postal_norm)
    if (nrow(cen) == 0) next
    postal_centroid_list[[j]] <- tibble::tibble(
      .row_id = row_id, .addr_input = to_fuzzy$.addr_input[j],
      Latitude = cen$Latitude[1], Longitude = cen$Longitude[1],
      Full_Address = cen$Full_Address[1], Civic_Number = cen$Civic_Number[1],
      City = cen$City[1], Postal_Code = cen$Postal_Code[1], Processed_City = cen$Processed_City[1],
      Census_Subdivision_Name = cen$Census_Subdivision_Name[1], Standardized_Street_Name = cen$Standardized_Street_Name[1],
      Province_or_Territory_Unique_Identifier = cen$Province_or_Territory_Unique_Identifier[1],
      match_type = "postal_centroid", text_score = NA_real_, number_score = NA_real_, geo_score = NA_real_, composite_score = NA_real_,
      same_city = NA, civic_number_exact = NA, match_quality = "low"
    )
  }
  out_postal <- dplyr::bind_rows(postal_centroid_list[!sapply(postal_centroid_list, is.null)])
  if (nrow(out_postal) > 0) message("[GEO]   Step 5: ", nrow(out_postal), " postal centroid")
  out_so_far <- dplyr::bind_rows(out_so_far, out_postal)
  to_fuzzy <- to_fuzzy %>% dplyr::filter(!.row_id %in% out_so_far$.row_id)
  parsed_fuzzy <- parsed_fuzzy %>% dplyr::filter(.row_id %in% to_fuzzy$.row_id)
  n_remaining <- nrow(to_fuzzy)
  if (n_remaining == 0) {
    message("[GEO] Done. Total matched: ", nrow(out_so_far))
    return(out_so_far %>%
             dplyr::select(-dplyr::any_of(c("address_normalized", "street_only_norm", ".ref_idx"))) %>%
             dplyr::rename(address = .addr_input) %>%
             dplyr::arrange(.row_id))
  }

  # ---- Step 6: FSA (3-digit) centroid ----
  message("[GEO] Step 6: FSA centroid (", n_remaining, " left)")
  fsa_centroid_list <- vector("list", n_remaining)
  for (j in seq_len(n_remaining)) {
    row_id <- to_fuzzy$.row_id[j]
    prov_2letter <- parsed_fuzzy$prov_2letter[parsed_fuzzy$.row_id == row_id][1]
    input_fsa <- parsed_fuzzy$input_fsa[parsed_fuzzy$.row_id == row_id][1]
    if (is.na(prov_2letter) || !nzchar(prov_2letter) || is.na(input_fsa) || !nzchar(input_fsa)) next
    cen <- query_ref_fsa_centroid(conn, prov_2letter, input_fsa)
    if (nrow(cen) == 0) next
    fsa_centroid_list[[j]] <- tibble::tibble(
      .row_id = row_id, .addr_input = to_fuzzy$.addr_input[j],
      Latitude = cen$Latitude[1], Longitude = cen$Longitude[1],
      Full_Address = cen$Full_Address[1], Civic_Number = cen$Civic_Number[1],
      City = cen$City[1], Postal_Code = cen$Postal_Code[1], Processed_City = cen$Processed_City[1],
      Census_Subdivision_Name = cen$Census_Subdivision_Name[1], Standardized_Street_Name = cen$Standardized_Street_Name[1],
      Province_or_Territory_Unique_Identifier = cen$Province_or_Territory_Unique_Identifier[1],
      match_type = "fsa_centroid", text_score = NA_real_, number_score = NA_real_, geo_score = NA_real_, composite_score = NA_real_,
      same_city = NA, civic_number_exact = NA, match_quality = "low"
    )
  }
  out_fsa <- dplyr::bind_rows(fsa_centroid_list[!sapply(fsa_centroid_list, is.null)])
  if (nrow(out_fsa) > 0) message("[GEO]   Step 6: ", nrow(out_fsa), " FSA centroid")
  out_so_far <- dplyr::bind_rows(out_so_far, out_fsa)
  to_fuzzy <- to_fuzzy %>% dplyr::filter(!.row_id %in% out_so_far$.row_id)
  parsed_fuzzy <- parsed_fuzzy %>% dplyr::filter(.row_id %in% to_fuzzy$.row_id)
  n_remaining <- nrow(to_fuzzy)
  if (n_remaining == 0) {
    message("[GEO] Done. Total matched: ", nrow(out_so_far))
    return(out_so_far %>%
             dplyr::select(-dplyr::any_of(c("address_normalized", "street_only_norm", ".ref_idx"))) %>%
             dplyr::rename(address = .addr_input) %>%
             dplyr::arrange(.row_id))
  }

  # ---- Step 7: City centroid ----
  message("[GEO] Step 7: City centroid (", n_remaining, " left)")
  city_centroid_list <- vector("list", n_remaining)
  for (j in seq_len(n_remaining)) {
    row_id <- to_fuzzy$.row_id[j]
    prov_2letter <- parsed_fuzzy$prov_2letter[parsed_fuzzy$.row_id == row_id][1]
    city_norm <- parsed_fuzzy$city_norm[parsed_fuzzy$.row_id == row_id][1]
    if (is.na(prov_2letter) || !nzchar(prov_2letter) || is.na(city_norm) || !nzchar(city_norm)) next
    cen <- query_ref_city_centroid(conn, prov_2letter, city_norm)
    if (nrow(cen) == 0) next
    city_centroid_list[[j]] <- tibble::tibble(
      .row_id = row_id, .addr_input = to_fuzzy$.addr_input[j],
      Latitude = cen$Latitude[1], Longitude = cen$Longitude[1],
      Full_Address = cen$Full_Address[1], Civic_Number = cen$Civic_Number[1],
      City = cen$City[1], Postal_Code = cen$Postal_Code[1], Processed_City = cen$Processed_City[1],
      Census_Subdivision_Name = cen$Census_Subdivision_Name[1], Standardized_Street_Name = cen$Standardized_Street_Name[1],
      Province_or_Territory_Unique_Identifier = cen$Province_or_Territory_Unique_Identifier[1],
      match_type = "city_centroid", text_score = NA_real_, number_score = NA_real_, geo_score = NA_real_, composite_score = NA_real_,
      same_city = NA, civic_number_exact = NA, match_quality = "low"
    )
  }
  out_city <- dplyr::bind_rows(city_centroid_list[!sapply(city_centroid_list, is.null)])
  if (nrow(out_city) > 0) message("[GEO]   Step 7: ", nrow(out_city), " city centroid")
  out_so_far <- dplyr::bind_rows(out_so_far, out_city)
  n_unmatched <- n_total - nrow(out_so_far)
  message("[GEO] Done. exact: ", n_1, " + exact_street: ", nrow(out_exact) - n_1, " + relaxed: ", n_relaxed, " + fuzzy: ", nrow(out_fuzzy), " + postal: ", nrow(out_postal), " + FSA: ", nrow(out_fsa), " + city: ", nrow(out_city), "  |  Unmatched: ", n_unmatched)

  if (is.data.frame(addresses) && ncol(addresses) > 1) {
    join_cols <- setdiff(names(input_df), c(".addr_input", ".row_id"))
    if (length(join_cols) > 0)
      out_so_far <- out_so_far %>% dplyr::left_join(input_df %>% dplyr::select(.row_id, dplyr::any_of(join_cols)), by = ".row_id")
  }

  out_so_far %>%
    dplyr::select(-dplyr::any_of(c("address_normalized", "street_only_norm", ".ref_idx"))) %>%
    dplyr::rename(address = .addr_input) %>%
    dplyr::arrange(.row_id)
}
