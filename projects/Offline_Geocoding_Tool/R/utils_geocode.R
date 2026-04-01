# =============================================================================
# Offline Geocoding Tool - utilities and scoring
# =============================================================================

# Street type map: full words and abbreviations -> canonical (Canada Post / StatCan)
if (file.exists("R/street_type_map.R")) {
  source("R/street_type_map.R")
} else if (file.exists("street_type_map.R")) {
  source("street_type_map.R")
}

#' Full province/territory name (lowercase) to 2-letter code - for input data that uses "Ontario" instead of "ON"
#' @export
PROVINCE_NAME_TO_2LETTER <- c(
  "newfoundland and labrador" = "NL", "newfoundland" = "NL",
  "prince edward island" = "PE", "pei" = "PE",
  "nova scotia" = "NS",
  "new brunswick" = "NB",
  "quebec" = "QC", "québec" = "QC",
  "ontario" = "ON",
  "manitoba" = "MB",
  "saskatchewan" = "SK",
  "alberta" = "AB",
  "british columbia" = "BC", "bc" = "BC",
  "yukon" = "YT", "yukon territory" = "YT",
  "northwest territories" = "NT", "nwt" = "NT",
  "nunavut" = "NU"
)

#' Reference table province SGC (numeric) to 2-letter code (NAR/StatCan use SGC in province column)
REF_PROVINCE_SGC <- c(
  "10" = "NL", "11" = "PE", "12" = "NS", "13" = "NB", "24" = "QC",
  "35" = "ON", "46" = "MB", "47" = "SK", "48" = "AB", "59" = "BC",
  "60" = "YT", "61" = "NT", "62" = "NU"
)

#' Normalize cardinal directions to a single abbreviated form for matching.
#' E.g. "North" / "N." -> "N", "Street West" / "St W" -> "st w".
#' Uses word boundaries so "Northern Ave" is unchanged.
#' @param x Character vector (typically already lowercased)
#' @return Character vector with directions normalized
normalize_cardinal_directions <- function(x) {
  x %>%
    stringr::str_replace_all("\\bnorth\\b", "n") %>%
    stringr::str_replace_all("\\bsouth\\b", "s") %>%
    stringr::str_replace_all("\\beast\\b", "e") %>%
    stringr::str_replace_all("\\bwest\\b", "w") %>%
    stringr::str_replace_all("\\bn\\.?", "n") %>%
    stringr::str_replace_all("\\bs\\.?", "s") %>%
    stringr::str_replace_all("\\be\\.?", "e") %>%
    stringr::str_replace_all("\\bw\\.?", "w") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()
}

#' Normalize street-type words in a string to canonical abbreviations (STREET_TYPE_MAP).
#' E.g. "74 avenue" -> "74 ave", "rue main" -> "rue main" (rue canonical).
#' Enables consistent matching across English/French and full/abbreviated input.
#' @param x Character vector (single string or vector of strings)
#' @return Character vector of same length
normalize_street_types_in_string <- function(x) {
  if (!exists("STREET_TYPE_MAP", mode = "character")) return(x)
  x <- as.character(x)
  x[is.na(x)] <- ""
  out <- x
  for (i in seq_along(x)) {
    if (!nzchar(trimws(x[i]))) next
    w <- strsplit(tolower(trimws(x[i])), "\\s+")[[1]]
    idx <- w %in% names(STREET_TYPE_MAP)
    if (any(idx)) w[idx] <- STREET_TYPE_MAP[w[idx]]
    out[i] <- paste(w, collapse = " ")
  }
  out
}

#' Normalize a string for matching (lowercase, trim, collapse whitespace, cardinal directions, street types)
#' @param x Character vector
#' @return Normalized character vector
normalize_string <- function(x) {
  x %>%
    tolower() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\s+", " ") %>%
    normalize_cardinal_directions() %>%
    normalize_street_types_in_string()
}

#' Normalize Canadian postal code to 6-character form (uppercase, no space) for matching.
#' e.g. "K1A 0B1" or "k1a0b1" -> "K1A0B1". Returns "" if not 6 alphanumeric chars.
#' @param x Character vector of postal codes
#' @return Character vector of length(x)
normalize_postal <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  out <- toupper(gsub("[^A-Za-z0-9]", "", trimws(x)))
  out[nchar(out) != 6] <- ""
  out
}

#' Normalize city/CSD name for matching (lowercase, strip punctuation, collapse spaces).
#' Strips StatCan Census Subdivision suffixes like " (CY)", " (C)", " (T)" so "Hamilton (CY)" matches "Hamilton".
#' Strips leading "City of ", "Town of ", etc. so "City of Sault Ste. Marie" matches "Sault Ste Marie".
#' Used for reference Census_Subdivision_Name (CSD name) and input city.
#' @param x Character vector of city/CSD names
#' @return Character vector of normalized names
normalize_city_for_match <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x <- tolower(trimws(as.character(x)))
  x[is.na(x) | !nzchar(x)] <- ""
  x <- gsub("\\s*\\([a-z]+\\)\\s*$", "", x)
  x <- gsub("[[:punct:]]", " ", x)
  x <- trimws(gsub("\\s+", " ", x))
  # Strip leading "city of ", "town of ", etc. (NAR/StatCan style)
  for (prefix in c("city of ", "town of ", "township of ", "municipality of ", "village of ", "rural municipality of ")) {
    x <- sub(paste0("^", prefix), "", x)
  }
  # Strip trailing ", city of", ", town of", etc. (SIF/Municipality style e.g. "Sault Ste. Marie, City of")
  for (suffix in c(", city of$", ", town of$", ", township of$", ", municipality of$", ", village of$")) {
    x <- gsub(suffix, "", x)
  }
  trimws(gsub("\\s+", " ", x))
}

#' Check which reference rows match an input city (normalized) on *any* of CSD, Processed_City, or City.
#' Reference table may have multiple city-like columns that can give better match chance.
#' @param input_city_norm Single normalized city string (from normalize_city_for_match).
#' @param ref_df Data frame with columns Census_Subdivision_Name, Processed_City, City (any can be missing).
#' @return Logical vector, length nrow(ref_df): TRUE where any of the three columns (normalized) equals input_city_norm.
#' @export
ref_city_match_any <- function(input_city_norm, ref_df) {
  if (length(input_city_norm) != 1 || !nzchar(trimws(input_city_norm))) return(rep(TRUE, nrow(ref_df)))
  n <- nrow(ref_df)
  csd  <- if ("Census_Subdivision_Name" %in% names(ref_df)) normalize_city_for_match(ref_df[["Census_Subdivision_Name"]] %||% rep("", n)) else rep("", n)
  proc <- if ("Processed_City" %in% names(ref_df)) normalize_city_for_match(ref_df[["Processed_City"]] %||% rep("", n)) else rep("", n)
  city <- if ("City" %in% names(ref_df)) normalize_city_for_match(ref_df[["City"]] %||% rep("", n)) else rep("", n)
  (csd == input_city_norm) | (proc == input_city_norm) | (city == input_city_norm)
}

#' First letter of every word in a string (for Phase 3 city filter).
#' E.g. "Town of Sudbury" -> c("t","o","s") so "Sudbury" (S) can match "Town of Sudbury" (S in set).
#' @param x Single string (scalar)
#' @return Character vector of unique first letters (alpha only) of each word
first_letters_of_words <- function(x) {
  if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(as.character(x)))) return(character(0))
  words <- strsplit(trimws(tolower(as.character(x))), "\\s+")[[1]]
  words <- words[nzchar(words)]
  if (length(words) == 0) return(character(0))
  fl <- substr(words, 1, 1)
  unique(fl[grepl("[a-z]", fl)])
}

#' Phase-3 city filter: keep reference row if input city matches by (1) any word first letter, or (2) substring.
#' Used only in Phase 3 so e.g. "Sudbury" matches "Town of Sudbury", "Rockland" matches "Clarence-Rockland".
#' @param input_city Single input city string (e.g. "Sudbury")
#' @param ref_df Candidate reference data frame with Census_Subdivision_Name, Processed_City, City
#' @return Logical vector, length nrow(ref_df)
ref_city_match_phase3 <- function(input_city, ref_df) {
  n <- nrow(ref_df)
  if (n == 0) return(logical(0))
  input_trim <- trimws(as.character(input_city))
  if (length(input_trim) != 1 || is.na(input_trim) || !nzchar(input_trim)) return(rep(TRUE, n))
  input_norm <- normalize_city_for_match(input_trim)
  input_letters <- first_letters_of_words(input_trim)
  csd  <- if ("Census_Subdivision_Name" %in% names(ref_df)) trimws(as.character(ref_df[["Census_Subdivision_Name"]] %||% rep("", n))) else rep("", n)
  proc <- if ("Processed_City" %in% names(ref_df)) trimws(as.character(ref_df[["Processed_City"]] %||% rep("", n))) else rep("", n)
  city <- if ("City" %in% names(ref_df)) trimws(as.character(ref_df[["City"]] %||% rep("", n))) else rep("", n)
  csd_norm <- normalize_city_for_match(csd)
  proc_norm <- normalize_city_for_match(proc)
  city_norm <- normalize_city_for_match(city)
  ref_letters <- lapply(seq_len(n), function(j) {
    unique(c(
      first_letters_of_words(csd[j]),
      first_letters_of_words(proc[j]),
      first_letters_of_words(city[j])
    ))
  })
  word_match <- vapply(seq_len(n), function(j) any(input_letters %in% ref_letters[[j]]), logical(1))
  substring_match <- (nzchar(input_norm) & (
    (nzchar(csd_norm)  & (stringr::str_detect(csd_norm,  stringr::fixed(input_norm))  | stringr::str_detect(input_norm, stringr::fixed(csd_norm)))) |
    (nzchar(proc_norm) & (stringr::str_detect(proc_norm, stringr::fixed(input_norm))  | stringr::str_detect(input_norm, stringr::fixed(proc_norm)))) |
    (nzchar(city_norm) & (stringr::str_detect(city_norm, stringr::fixed(input_norm))  | stringr::str_detect(input_norm, stringr::fixed(city_norm))))
  ))
  word_match | substring_match
}

#' Build relaxed normalized address variants for attribute-relaxation matching (TAMU-style).
#' Used only within narrow geography (province+city+FSA) so matches stay in same area.
#' Drops trailing direction (n/s/e/w), then trailing street type (from STREET_TYPE_MAP), one at a time.
#' @param address_normalized Character vector of normalized full addresses (e.g. from parse_address_components)
#' @return List of character vectors: each element is the set of variants for one input (first is full, rest relaxed)
build_relaxed_address_norms <- function(address_normalized) {
  x <- as.character(address_normalized)
  x[is.na(x)] <- ""
  # Trailing street-type pattern: all variants and canonicals from map (longest first for regex)
  street_types_pattern <- if (exists("STREET_TYPE_MAP", mode = "character")) {
    types <- unique(c(names(STREET_TYPE_MAP), STREET_TYPE_MAP))
    types_esc <- gsub("([.*+?^${}()|[\\]\\\\])", "\\\\\\1", types, perl = TRUE)
    types_esc <- types_esc[order(-nchar(types_esc))]
    paste0("\\s+(", paste(types_esc, collapse = "|"), ")\\s*$")
  } else {
    "\\s+(st|street|ave|avenue|blvd|boulevard|dr|drive|rd|road|lane|ln|ct|court|way|pl|place|cres|circle|cir|trl|trail|rue|chemin|sentier|promenade|route)\\s*$"
  }
  out <- lapply(x, function(addr) {
    if (!nzchar(trimws(addr))) return(character(0))
    variants <- character(0)
    variants[1] <- trimws(addr)
    # Drop trailing cardinal direction (word boundary)
    no_dir <- stringr::str_replace(addr, "\\s+(n|s|e|w)\\s*$", "")
    no_dir <- trimws(no_dir)
    if (nzchar(no_dir) && !no_dir %in% variants) variants <- c(variants, no_dir)
    # Drop trailing street type (full list: English + French)
    no_type <- stringr::str_replace(no_dir, street_types_pattern, "")
    no_type <- trimws(no_type)
    if (nzchar(no_type) && !no_type %in% variants) variants <- c(variants, no_type)
    unique(variants)
  })
  out
}

#' Extract leading numeric civic number from a string
#' @param x Character vector (e.g. "500 Main St" or "500")
#' @return Numeric vector (NA where no number found)
extract_civic_number <- function(x) {
  out <- suppressWarnings(
    as.numeric(stringr::str_extract(x, "^\\s*\\d+"))
  )
  na_idx <- is.na(out) & !is.na(x) & x != ""
  if (any(na_idx)) {
    first_num <- stringr::str_extract(x[na_idx], "\\d+")
    out[na_idx] <- suppressWarnings(as.numeric(first_num))
  }
  out
}

#' Parse address components from a single full-address string
#' Attempts to get number and street; city/province/postal if present.
#' @param address Character vector of address strings
#' @return Tibble with columns: address_normalized, civic_number, street_part, city, province, postal_code
parse_address_components <- function(address) {
  address <- as.character(address)
  normalized <- normalize_string(address)

  civic_number <- extract_civic_number(normalized)

  postal_code <- stringr::str_extract(normalized, "[a-z]\\d[a-z]\\s*\\d[a-z]\\d") %>%
    stringr::str_replace_all("\\s+", "") %>%
    toupper()

  prov_codes <- "ab|bc|mb|nb|nl|ns|nt|nu|on|pe|qc|sk|yt"
  province <- stringr::str_extract(normalized, paste0("\\b(", prov_codes, ")\\b"))
  prov_names <- c(
    "newfoundland and labrador" = "nl", "newfoundland" = "nl", "labrador" = "nl",
    "prince edward island" = "pe", "pei" = "pe",
    "nova scotia" = "ns", "ns" = "ns",
    "new brunswick" = "nb", "nb" = "nb",
    "quebec" = "qc", "québec" = "qc",
    "ontario" = "on", "on" = "on",
    "manitoba" = "mb", "mb" = "mb",
    "saskatchewan" = "sk", "sk" = "sk",
    "alberta" = "ab", "ab" = "ab",
    "british columbia" = "bc", "bc" = "bc",
    "yukon" = "yt", "yt" = "yt", "yukon territory" = "yt",
    "northwest territories" = "nt", "nt" = "nt", "nwt" = "nt",
    "nunavut" = "nu", "nu" = "nu"
  )
  need_prov <- is.na(province) | !nzchar(trimws(province))
  prov_names_long <- prov_names[nchar(names(prov_names)) > 2L]
  prov_names_long <- prov_names_long[order(-nchar(names(prov_names_long)))]
  for (nm in names(prov_names_long)) {
    if (!any(need_prov)) break
    code <- unname(prov_names_long[nm])
    pat <- paste0("\\b", gsub("([.*+?^${}()|[\\]\\\\])", "\\\\\\1", nm, perl = TRUE), "\\b")
    hit <- need_prov & grepl(pat, normalized, ignore.case = TRUE)
    province[hit] <- code
    need_prov <- need_prov & !hit
  }

  street_part_raw <- normalized %>%
    stringr::str_remove("^\\s*\\d+\\s*") %>%
    stringr::str_trim()
  has_comma <- stringr::str_detect(street_part_raw, ",")
  street_part <- ifelse(has_comma,
    stringr::str_trim(stringr::str_split_fixed(street_part_raw, ",", 2L)[, 1L]),
    street_part_raw
  )
  street_direction <- stringr::str_extract(street_part, "\\b(n|s|e|w)\\s*$") %>%
    stringr::str_trim()
  street_direction[!street_direction %in% c("n", "s", "e", "w")] <- NA_character_

  city <- vapply(seq_along(normalized), function(i) {
    parts <- stringr::str_split(normalized[i], ",\\s*")[[1]]
    if (length(parts) < 2) return(NA_character_)
    last <- parts[length(parts)]
    if (!grepl("^[a-z]{2}$", last) && !grepl("[a-z]\\d[a-z]\\s*\\d[a-z]\\d", last))
      return(last)
    if (length(parts) >= 3) return(parts[length(parts) - 1])
    NA_character_
  }, character(1))

  tibble::tibble(
    address_normalized = normalized,
    civic_number = civic_number,
    street_part = street_part,
    street_direction = street_direction,
    city = city,
    province = province,
    postal_code = postal_code
  )
}

#' Text-based similarity score between two strings (0 = different, 1 = identical)
score_text_similarity <- function(a, b, method = "jw") {
  a[is.na(a)] <- ""
  b[is.na(b)] <- ""
  d <- stringdist::stringdist(a, b, method = method)
  max_d <- switch(method,
    "jw" = 1,
    "osa" = , "lv" = pmax(nchar(a), nchar(b)),
    "dl" = pmax(nchar(a), nchar(b)),
    1
  )
  if (method %in% c("osa", "lv", "dl")) {
    out <- 1 - d / pmax(max_d, 1)
  } else {
    out <- 1 - d
  }
  pmax(0, pmin(1, out))
}

#' Numeric nearness score: 1 when same, decays as numbers differ
score_numeric_nearness <- function(x, y, scale = 50) {
  both_na <- is.na(x) & is.na(y)
  either_na <- is.na(x) | is.na(y)
  diff <- abs(as.numeric(x) - as.numeric(y))
  out <- 1 / (1 + diff / max(scale, 1))
  out[either_na & !both_na] <- 0
  out[both_na] <- 1
  pmin(1, pmax(0, out))
}

#' Geographic proximity score (same province > same city > same postal code)
score_geographic <- function(province_a, province_b,
                             city_a = NA, city_b = NA,
                             postal_a = NA, postal_b = NA,
                             weights = list(province = 0.5, city = 0.3, postal = 0.2)) {
  province_a <- normalize_string(as.character(province_a))
  province_b <- normalize_string(as.character(province_b))
  city_a <- normalize_string(as.character(city_a))
  city_b <- normalize_string(as.character(city_b))
  postal_a <- normalize_string(as.character(postal_a))
  postal_b <- normalize_string(as.character(postal_b))

  same_province <- (province_a == province_b) & (province_a != "" & province_b != "")
  same_city <- (city_a == city_b) & (city_a != "" & city_b != "")
  same_postal <- (postal_a == postal_b) & (postal_a != "" & postal_b != "")

  w <- weights
  out <- (as.numeric(same_province) * w$province +
    as.numeric(same_city) * w$city +
    as.numeric(same_postal) * w$postal) / (w$province + w$city + w$postal)
  out[is.nan(out)] <- 0
  out
}

#' Normalize reference direction field (full word or abbreviation) to single letter n/s/e/w
#' @param x Character vector (e.g. "North", "N", "N.")
#' @return Character vector of "n", "s", "e", "w", or NA
normalize_ref_direction <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x == ""] <- NA_character_
  out <- dplyr::case_when(
    x %in% c("n", "n.", "north") ~ "n",
    x %in% c("s", "s.", "south") ~ "s",
    x %in% c("e", "e.", "east") ~ "e",
    x %in% c("w", "w.", "west") ~ "w",
    TRUE ~ NA_character_
  )
  out
}

#' Composite match score from text, numeric, and geographic components
composite_score <- function(text_score, number_score, geo_score,
                            weights = c(text = 0.5, number = 0.25, geo = 0.25)) {
  w <- weights
  (text_score * w["text"] + number_score * w["number"] + geo_score * w["geo"]) / sum(w)
}
