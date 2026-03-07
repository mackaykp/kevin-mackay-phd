# Offline Geocoding Tool (R + NAR)

Offline geocoding for Canadian addresses using [Statistics Canada's National Address Register (NAR)](https://www.statcan.gc.ca/en/lode/databases/nar). All reference data is stored in a single DuckDB database (table `ref`). Built with **tidyverse**-style R and compatible packages.

## Features

- **NAR-based**: Uses the National Address Register (Address + Location CSVs). One DuckDB holds all reference data.
- **Flexible input**: Geocode Excel/CSV with address columns (Street, City, Province, Postal Code).
- **Single match hierarchy** (literature-based): Exact (full address) → exact (street only) → relaxed → fuzzy (narrow pool: province+city+**full 6-digit postal** when available, else FSA) → **postal code (6-digit) centroid** → FSA centroid → city centroid. Geography from smallest to largest; no cross-FSA or cross-city point matches.
- **Composite scoring**: Text similarity (Jaro–Winkler), numeric nearness (civic numbers), geographic (province, city, postal).
- **Low memory**: Single DuckDB connection; batched fuzzy queries (20k pool); streamed CSV output.

## Setup

### 1. Install R dependencies

```r
install.packages(c("tidyverse", "stringdist", "readr", "duckdb"))
```

### 2. Get NAR data

- Obtain the [National Address Register (NAR)](https://www.statcan.gc.ca/en/lode/databases/nar) from Statistics Canada.
- Place the **Addresses** and **Locations** folders under `data/NAR2025/`, e.g.:
  - `data/NAR2025/Addresses/Address_35_part_1.csv`, ...
  - `data/NAR2025/Locations/Location_35_part_1.csv`, ...

### 3. Build the NAR DuckDB (once)

From the project root in R:

```r
source("run_build_nar_duckdb.R")
```

Or from shell:

```r
Rscript run_build_nar_duckdb.R
```

This joins Address + Location on `LOC_GUID`, normalizes fields, and writes `data/nar.duckdb` (table `ref`). Re-run when NAR data or the build script changes.

### 4. Put input files in `data/ungeocoded_files/`

- Production: e.g. `sif_data_table_2022_2023_en.xlsx`
- Test: e.g. `test_addresses.csv` (create with `create_test_addresses.R` if needed)

## How to run

**Test run** (small file):

```r
setwd("C:/Users/macka/OneDrive/Documents/Cursor-Projects/Offline_Geocoding_Tool")
source("run_test_geocode.R")
```

Or: `Rscript run_test_geocode.R`

**Production run** (SIF school data or your file):

```r
source("run_geocode.R")
```

Or: `Rscript run_geocode.R`

**Geocode a custom file:**

```r
source("geocode_excel_file.R")
geocode_excel("data/ungeocoded_files/your_file.xlsx",
              address_cols = c("Street", "City", "Province", "Postal Code"),
              nar_duckdb_path = "data/nar.duckdb")
```

Progress is saved every 100 rows; re-run to resume. Output is written to **`output/<input_basename>_geocoded.csv`**.

## Usage

### Geocode an Excel/CSV file (recommended)

```r
source("geocode_excel_file.R")
result <- geocode_excel("data/ungeocoded_files/your_file.xlsx",
                        nar_duckdb_path = "data/nar.duckdb")
```

### Tuning (via `geocode_excel(..., ...)` or `geocode_ref`)

- **Fuzzy threshold**: default 0.75; pass `fuzzy_threshold = 0.7` in `...` to loosen.
- **Candidate pool**: 20k per (province, city, FSA) by default; override with `max_candidate_pool` in `...`.

## Project layout

**Folders**

- **`data/`** – NAR source: `data/NAR2025/` (Addresses + Locations), `data/nar.duckdb` (built once), `data/ungeocoded_files/` (input Excel/CSV only).
- **`output/`** – Geocoded CSV results (e.g. `output/sif_data_table_2022_2023_en_geocoded.csv`).
- **`ARCHIVE/`** – Old scripts and obsolete outputs (not used at runtime).

**Scripts**

- **R/utils_geocode.R** – Normalization, parsing, scoring, relaxed-address variants.
- **R/build_nar_duckdb.R** – Build NAR DuckDB: join Address + Location, normalize; table `ref`.
- **R/query_ref_duckdb.R** – Query helpers: exact match, narrow candidates (province+city+FSA), FSA/city centroids.
- **R/geocode_ref.R** – Core `geocode_ref()`: single hierarchy (exact → relaxed → fuzzy → centroids).
- **geocode_excel_file.R** – `geocode_excel()`: NAR DuckDB; streamed output; resume support; writes to `output/` by default.
- **run_build_nar_duckdb.R** – One-time build of `data/nar.duckdb` from NAR CSVs.
- **run_test_geocode.R** – Test run on `data/ungeocoded_files/test_addresses.csv` → `output/test_addresses_geocoded.csv`.
- **run_geocode.R** – Production run on SIF (or your) data → `output/`.

## Reference table (NAR → DuckDB "ref")

The NAR build produces a single DuckDB table **`ref`** with columns expected by the geocoder: `Latitude`, `Longitude`, `Full_Address`, `Civic_Number`, `Street_Name`, `Standardized_Street_Name`, `Census_Subdivision_Name`, `City`, `Processed_City`, `Postal_Code`, `Province_or_Territory_Unique_Identifier`, plus `province_2letter`, `city_norm`, `fsa`, `address_norm`. City matching uses **Census Subdivision (CSD) name** (`city_norm`).

### String matching and place-name help

- **In this tool:** Normalization is done with `normalize_city_for_match()` (lowercase, strip punctuation, collapse spaces, remove StatCan suffixes like " (CY)"). City matching uses the CSD name only; fuzzy address scoring uses **stringdist** (e.g. Jaro–Winkler) for street/address text.
- **R packages that can help** with harder name matching (sound, visual, or similarity):
  - **stringdist** (already used): Levenshtein, Jaro–Winkler, q-grams, etc. Good for typos and small edits.
  - **phonics**: Soundex, Metaphone, NYSIIS, Caverphone, etc. Good for “sounds like” matching (e.g. similar-sounding place names).
  - **fuzzyjoin**: Fuzzy joins on string columns (e.g. join input cities to a reference list by similarity).
- **External lists:** Statistics Canada’s [Census Subdivision (CSD) list](https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/page.cfm?Lang=E&DGUIDlist=2021A000235&GENDERlist=1&STATISTIClist=1&HEADERlist=0) and similar geography products give official names and codes. You can build a small “input city → CSD name/code” lookup to normalize names (e.g. “Rockland” → “Clarence-Rockland”) before or during geocoding.

### Data folder and 2021 Geographic Attribute File

The **2021 Geographic Attribute File** (`data/2021_Geographic_Attribute_File.csv`) is a StatCan product that lists geographic attributes at the dissemination block level. Key columns for city/CSD work:

- **CSDNAME_SDRNOM** – Official Census Subdivision (CSD) name (e.g. "St. John's", "Clarence-Rockland").
- **CSDUID_SDRIDU** – CSD unique identifier.
- **CSDTYPE_SDRGENRE** – CSD type (CY, T, TP, MU, etc.).
- **PRNAME_PRNOM** – Province (bilingual).

This file gives you the **authoritative list of CSD names and codes** by province. It does *not* contain alternate names (e.g. township names or "Rockland" as an alias for "Clarence-Rockland"). You can use it to: (1) get the full set of valid CSD names for validation or autocomplete; (2) build a lookup from CSDUID to CSDNAME for use when you have a separate township→CSD mapping. A true "township → CSD" bridge would be a different table (e.g. place-name correspondence); the Geographic Attribute File is the reference list of official CSDs that NAR and other StatCan products use.

## License

MIT.
