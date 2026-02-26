#
# Build SES (socioeconomic status) index from Statistics Canada 2021 Census
# and DA boundaries. Reads data/raw/ census CSV and data/raw/da_boundaries/,
# joins, computes index, writes data/ses_index.gpkg.
#
# Requires: tidyverse, sf. Run after scripts/fetch_statcan_census.py.
#
# Test mode (faster for debugging): set env var SES_INDEX_TEST=true before sourcing,
#   or in R: Sys.setenv(SES_INDEX_TEST = "true"); source("R/build_ses_index.R")
#   Uses 1 region only, limits census rows to 200k, output: ses_index_test.gpkg
#

library(tidyverse)
library(sf)
if (dir.exists("R")) source("R/map_config.R") else source("map_config.R")

# Test mode: 1 region, limit rows, write to ses_index_test.gpkg
TEST_MODE <- tolower(Sys.getenv("SES_INDEX_TEST", "")) %in% c("true", "1", "yes")
N_MAX_CENSUS <- if (TEST_MODE) 200000L else Inf

# Paths (relative to project root when run from project root)
PROJECT_ROOT <- if (dir.exists("data")) "." else if (dir.exists("../data")) ".." else stop("Run from project root")
RAW_DIR     <- file.path(PROJECT_ROOT, "data", "raw")
BOUNDARY_DIR <- file.path(RAW_DIR, "da_boundaries")
OUT_GPKG    <- file.path(PROJECT_ROOT, "data", if (TEST_MODE) "ses_index_test.gpkg" else "ses_index.gpkg")

# Find boundary shapefile: prefer cartographic (lda_000b*, land-only, no water) over digital (lda_000a*)
boundary_files <- list.files(BOUNDARY_DIR, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
if (length(boundary_files) == 0) stop("No .shp found in ", BOUNDARY_DIR, ". Run fetch_statcan_census.py --boundaries first.")
carto <- boundary_files[grepl("000b21|cartographic", basename(boundary_files), ignore.case = TRUE)]
boundary_path <- if (length(carto) > 0) carto[1] else boundary_files[1]
if (length(carto) == 0) message("Using digital boundaries. For land-only (no water) use cartographic: python scripts/fetch_statcan_census.py --boundaries (downloads lda_000b21a_e).")

# Find census CSV: prefer census_profile_*.csv (after Python extract), or *CSV_data*.csv in raw/ subdirs
census_files <- list.files(RAW_DIR, pattern = "census_profile.*\\.csv$", full.names = TRUE)
# Skip files that are actually ZIPs (StatCan returns zip with .csv name)
census_files <- census_files[vapply(census_files, function(f) {
  h <- readBin(f, "raw", n = 2)
  !identical(h, charToRaw("PK"))
}, logical(1))]
if (length(census_files) == 0) {
  census_files <- list.files(RAW_DIR, pattern = ".*CSV_data.*\\.csv$", full.names = TRUE, recursive = TRUE)
  census_files <- census_files[vapply(census_files, function(f) {
    h <- readBin(f, "raw", n = 2)
    !identical(h, charToRaw("PK"))
  }, logical(1))]
}
if (length(census_files) == 0) {
  stop("No valid census CSV in ", RAW_DIR, ". StatCan returns ZIP: re-run fetch_statcan_census.py to download and extract, or extract the ZIP and place the inner CSV in data/raw/.")
}
if (TEST_MODE) {
  census_files <- census_files[1]
  message("TEST MODE: using 1 region only, n_max = ", N_MAX_CENSUS, ", output: ", basename(OUT_GPKG))
}
message("Using census file(s): ", paste(basename(census_files), collapse = ", "))

message("Reading boundaries from: ", boundary_path)
boundaries <- st_read(boundary_path, quiet = TRUE) %>%
  st_transform(4326)

id_col <- intersect(c("DGUID", "DAUID", "dguid", "dauid"), names(boundaries))[1]
if (is.na(id_col)) id_col <- names(boundaries)[1]
message("Using geography ID column: ", id_col)

# StatCan 2021 Census Profile: 1 = Total population; 243 = Median total income of household in 2020 ($), value in C1_COUNT_TOTAL
# If your CSV uses a different ID, set INCOME_CHAR_ID: Sys.setenv(INCOME_CHAR_ID = "2151"); source("R/build_ses_index.R")
INCOME_CHAR_ID <- as.character(Sys.getenv("INCOME_CHAR_ID", "243"))

read_census_safe <- function(path, n_max = Inf) {
  suppressMessages({
    d <- read_csv(path, show_col_types = FALSE, guess_max = 10000, n_max = n_max)
  })
  d
}

census_list <- lapply(census_files, function(f) read_census_safe(f, n_max = N_MAX_CENSUS))
census_raw  <- bind_rows(census_list)

# Detect long vs wide format
is_long <- all(c("GEO_LEVEL", "CHARACTERISTIC_ID", "DGUID") %in% names(census_raw)) &&
  ("C10_RATE_TOTAL" %in% names(census_raw) || "C1_COUNT_TOTAL" %in% names(census_raw))

if (is_long) {
  message("Detected long-format Census Profile; filtering to DAs, population and median household income.")
  has_count <- "C1_COUNT_TOTAL" %in% names(census_raw)
  has_rate  <- "C10_RATE_TOTAL" %in% names(census_raw)
  # Median household income may be in C1_COUNT_TOTAL (dollar value) or a rate column depending on product
  alt_geo_col <- intersect(c("ALT_GEO_CODE", "ALT GEO CODE"), names(census_raw))[1]
  if (is.na(alt_geo_col)) alt_geo_col <- names(census_raw)[grep("ALT.*GEO|GEO_CODE", names(census_raw), ignore.case = TRUE)][1]
  char_ids <- c("1", INCOME_CHAR_ID)
  census_da <- census_raw %>%
    filter(GEO_LEVEL == "Dissemination area", as.character(CHARACTERISTIC_ID) %in% char_ids) %>%
    mutate(
      dguid_join = as.character(trimws(DGUID)),
      dauid_join = if (!is.na(alt_geo_col)) as.character(trimws(.data[[alt_geo_col]])) else NA_character_,
      value_num = suppressWarnings(
        if (has_count && has_rate) {
          if_else(as.character(CHARACTERISTIC_ID) == "1", as.numeric(.data[["C1_COUNT_TOTAL"]]), as.numeric(.data[["C1_COUNT_TOTAL"]]))
        } else if (has_count) {
          as.numeric(.data[["C1_COUNT_TOTAL"]])
        } else {
          as.numeric(.data[["C10_RATE_TOTAL"]])
        }
      )
    ) %>%
    filter(!is.na(dguid_join), dguid_join != "")
  census_slim <- census_da %>%
    group_by(dguid_join) %>%
    mutate(dauid_join = first(dauid_join)) %>%
    ungroup() %>%
    select(dguid_join, dauid_join, CHARACTERISTIC_ID, value_num) %>%
    tidyr::pivot_wider(names_from = CHARACTERISTIC_ID, values_from = value_num, names_prefix = "char_") %>%
    mutate(
      population = if ("char_1" %in% names(.)) suppressWarnings(as.numeric(.data[["char_1"]])) else NA_real_,
      income_raw = if (paste0("char_", INCOME_CHAR_ID) %in% names(.)) suppressWarnings(as.numeric(.data[[paste0("char_", INCOME_CHAR_ID)]])) else NA_real_
    ) %>%
    select(dguid_join, dauid_join, population, income_raw)
  if (all(is.na(census_slim$income_raw))) {
    stop("Median household income (CHARACTERISTIC_ID ", INCOME_CHAR_ID, ") not found or all NA. Confirm CHARACTERISTIC_ID in your Census Profile (e.g. 243); set INCOME_CHAR_ID if different.")
  }
  if (all(is.na(census_slim$dauid_join)) || all(trimws(census_slim$dauid_join) == "", na.rm = TRUE)) {
    census_slim <- census_slim %>% mutate(dauid_join = str_sub(dguid_join, -8, -1))
  }
  census_slim <- census_slim %>%
    mutate(dauid_join_raw = trimws(replace_na(dauid_join, "")),
           dauid_join = str_pad(dauid_join_raw, width = 8, side = "left", pad = "0"))
} else {
  census_id_col <- intersect(c("DGUID", "GEO_CODE", "dguid"), names(census_raw))[1]
  if (is.na(census_id_col)) census_id_col <- names(census_raw)[1]
  if ("GEO_LEVEL" %in% names(census_raw)) {
    census_da <- census_raw %>% filter(GEO_LEVEL == 4 | GEO_LEVEL == 5 | GEO_LEVEL == "Dissemination area")
  } else {
    census_da <- census_raw
  }
  find_col <- function(df, patterns) {
    nms <- names(df)
    for (p in patterns) {
      idx <- grep(p, nms, ignore.case = TRUE)
      if (length(idx) > 0) return(nms[idx[1]])
    }
    NA_character_
  }
  col_income <- find_col(census_da, c("Median total income of household", "Median household income", "Median total income", "Median income"))
  cols_keep <- unique(c(census_id_col, col_income))
  cols_keep <- intersect(cols_keep, names(census_da))
  if (length(cols_keep) < 2) cols_keep <- names(census_da)[seq_len(min(3, ncol(census_da)))]
  census_slim <- census_da %>%
    select(any_of(cols_keep)) %>%
    mutate(dguid_join = as.character(.data[[census_id_col]]))
  for (c in setdiff(cols_keep, census_id_col)) {
    if (c %in% names(census_slim)) census_slim[[c]] <- suppressWarnings(as.numeric(census_slim[[c]]))
  }
  census_slim <- census_slim %>% filter(!is.na(dguid_join) & dguid_join != "")
  inc_col <- if (col_income %in% names(census_slim)) col_income else names(census_slim)[sapply(census_slim, is.numeric)][1]
  census_slim <- census_slim %>%
    mutate(
      dauid_join = NA_character_,
      dauid_join_raw = NA_character_,
      population = NA_real_,
      income_raw = if (!is.na(inc_col)) .data[[inc_col]] else NA_real_
    ) %>%
    select(dguid_join, dauid_join, dauid_join_raw, population, income_raw)
}

# Income: use raw median household income ($). Classify by study-area mean ± SD (same logic as before).
# Keep income_raw as-is; bivariate map uses it for ±1 SD Low/Med/High. idx_st is the 5-category status (Lowest/.../Highest) from map_config for reference.
ref <- census_slim %>%
  summarise(
    m_inc = mean(income_raw, na.rm = TRUE),
    s_inc = sd(income_raw, na.rm = TRUE)
  )
ref <- ref %>% mutate(s_inc = replace_na(s_inc, 0))
if (ref$s_inc == 0) ref$s_inc <- 1e-9

lab <- map_config$ses$labels
m   <- map_config$ses$sd_multipliers
census_slim <- census_slim %>%
  mutate(
    ses_idx = income_raw,
    idx_st = case_when(
      is.na(income_raw) ~ NA_character_,
      income_raw < ref$m_inc + m[1] * ref$s_inc ~ lab[1],
      income_raw < ref$m_inc + m[2] * ref$s_inc ~ lab[2],
      income_raw <= ref$m_inc + m[3] * ref$s_inc ~ lab[3],
      income_raw <= ref$m_inc + m[4] * ref$s_inc ~ lab[4],
      TRUE ~ lab[5]
    )
  )

# Join to boundaries. Per StatCan DGUID definition (92F0138M): for DA, DGUID = VVVV+T+SSSS+DAUID
# (9 fixed chars + 8-char DAUID). Shapefile .dbf often truncates DGUID to 10 chars, so full-DGUID
# join fails; use last 8 of census DGUID = boundary DAUID as the canonical fallback.
# https://www150.statcan.gc.ca/n1/pub/92f0138m/92f0138m2019001-eng.htm
boundary_has_dguid <- "DGUID" %in% names(boundaries)
boundary_has_dauid <- "DAUID" %in% names(boundaries)
census_slim$dguid_suffix8 <- str_sub(census_slim$dguid_join, -8, -1)  # last 8 = DAUID per StatCan

ses_sf <- NULL
if (boundary_has_dguid) {
  ses_sf <- boundaries %>%
    mutate(dguid_join = as.character(DGUID)) %>%
    inner_join(census_slim %>% select(dguid_join, population, income_raw, ses_idx, idx_st),
               by = "dguid_join")
}
if (is.null(ses_sf) || nrow(ses_sf) == 0 && boundary_has_dauid) {
  message("DGUID join produced 0 rows; trying last 8 of DGUID = DAUID (per StatCan 92F0138M).")
  bbox_join <- boundaries %>%
    mutate(dauid_8 = str_pad(trimws(as.character(DAUID)), width = 8, side = "left", pad = "0"))
  ses_sf <- bbox_join %>%
    inner_join(census_slim %>% select(dauid_8 = dguid_suffix8, population, income_raw, ses_idx, idx_st),
               by = "dauid_8") %>%
    select(-dauid_8)
}
if (is.null(ses_sf) || nrow(ses_sf) == 0 && boundary_has_dauid && "dauid_join" %in% names(census_slim)) {
  message("DAUID-from-DGUID join had 0 rows; trying census ALT_GEO_CODE as DAUID.")
  bbox_join <- boundaries %>%
    mutate(dauid_join = str_pad(trimws(as.character(DAUID)), width = 8, side = "left", pad = "0"))
  ses_sf <- bbox_join %>%
    inner_join(census_slim %>% select(dauid_join, population, income_raw, ses_idx, idx_st),
               by = "dauid_join")
  if (nrow(ses_sf) == 0 && "dauid_join_raw" %in% names(census_slim)) {
    message("Padded DAUID join had 0 rows; trying raw DAUID (no zero-pad).")
    bbox_join2 <- boundaries %>% mutate(dauid_join = trimws(as.character(DAUID)))
    census_join2 <- census_slim %>% filter(dauid_join_raw != "")
    if (nrow(census_join2) > 0) {
      ses_sf <- bbox_join2 %>%
        inner_join(census_join2 %>% select(dauid_join = dauid_join_raw, population, income_raw, ses_idx, idx_st),
                   by = "dauid_join")
    }
  }
}

if (is.null(ses_sf) || nrow(ses_sf) == 0) {
  # One more try: match last 8 chars of census DGUID to any boundary ID-like column (e.g. DAUID with different name)
  id_like <- names(boundaries)[grepl("UID|GUID|ID", names(boundaries), ignore.case = TRUE)]
  for (bcol in id_like) {
    if (!bcol %in% names(boundaries)) next
    bvals <- trimws(as.character(boundaries[[bcol]]))
    if (!all(nchar(bvals) >= 6, na.rm = TRUE)) next
    try_join <- boundaries %>%
      mutate(join_try = .data[[bcol]]) %>%
      mutate(join_try = trimws(as.character(join_try))) %>%
      inner_join(census_slim %>% select(join_try = dguid_suffix8, population, income_raw, ses_idx, idx_st), by = "join_try")
    if (nrow(try_join) > 0) {
      ses_sf <- try_join %>% select(-join_try)
      message("Join succeeded using census DGUID (last 8 chars) = boundary '", bcol, "'.")
      break
    }
  }
}

if (is.null(ses_sf) || nrow(ses_sf) == 0) {
  # Detect ADA vs DA: census is DA (DGUID schema 0512); boundaries may be ADA (schema 0516, has ADAUID, no DAUID)
  boundary_dguid_sample <- if ("DGUID" %in% names(boundaries)) head(trimws(as.character(boundaries$DGUID)), 1) else ""
  census_dguid_sample   <- head(census_slim$dguid_join, 1)
  boundary_is_ada <- ("ADAUID" %in% names(boundaries) && !("DAUID" %in% names(boundaries))) ||
    (nchar(boundary_dguid_sample) >= 9 && grepl("0516", substr(boundary_dguid_sample, 1, 9)))
  census_is_da   <- nchar(census_dguid_sample) >= 9 && grepl("0512", substr(census_dguid_sample, 1, 9))
  if (boundary_is_ada && census_is_da) {
    stop(
      "Boundary file is Aggregate Dissemination Area (ADA), but census data is Dissemination Area (DA). ",
      "You need DA boundaries. Options: (1) Re-download boundaries: python scripts/fetch_statcan_census.py --boundaries ",
      "(then replace contents of data/raw/da_boundaries/ with the new DA shapefile), or ",
      "(2) Download the Dissemination Area boundary file (lda_000b21a_e.zip cartographic recommended) from StatCan catalogue 92-169-X2021001 ",
      "and extract it into data/raw/da_boundaries/."
    )
  }
  message("=== Join failed. Diagnostics ===")
  message("Boundary columns: ", paste(names(boundaries), collapse = ", "))
  id_like_diag <- names(boundaries)[grepl("UID|GUID|ID", names(boundaries), ignore.case = TRUE)]
  for (c in id_like_diag) {
    message("  Boundary ", c, " (first 3): ", paste(head(unique(as.character(boundaries[[c]])), 3), collapse = " | "))
  }
  message("Census dguid_join (first 3): ", paste(head(unique(census_slim$dguid_join), 3), collapse = " | "))
  message("Census dauid_join (first 3): ", paste(head(unique(census_slim$dauid_join), 3), collapse = " | "))
  if ("dauid_join_raw" %in% names(census_slim)) message("Census dauid_join_raw (first 3): ", paste(head(unique(census_slim$dauid_join_raw), 3), collapse = " | "))
  message("Census dguid_suffix8 (first 3): ", paste(head(unique(census_slim$dguid_suffix8), 3), collapse = " | "))
  stop("Join produced 0 rows. Census and boundary IDs do not match. See diagnostics above.")
}

message("Writing ", nrow(ses_sf), " DAs to ", OUT_GPKG)
dir.create(dirname(OUT_GPKG), recursive = TRUE, showWarnings = FALSE)
st_write(ses_sf, OUT_GPKG, delete_dsn = TRUE, quiet = TRUE)
message("Done: ", OUT_GPKG)
if (TEST_MODE) message("(Test mode. For full Canada, unset SES_INDEX_TEST and re-run.)")
