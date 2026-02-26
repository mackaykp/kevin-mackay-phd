"""
Fetch Statistics Canada 2021 Census Profile (DA-level) and DA boundary files.
Saves census CSV and boundary shapefile/GeoJSON to data/raw/ for R build_ses_index.R.

Census Profile: https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger.cfm?Lang=E
Boundaries: https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/
"""

import os
import sys
import zipfile
import requests
from pathlib import Path

# Default: project root = parent of 'scripts'
SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent
RAW_DIR = PROJECT_ROOT / "data" / "raw"
RAW_DIR.mkdir(parents=True, exist_ok=True)

# Census Profile comprehensive download (DA-level by region)
# GEONO options: 006_Atlantic_Atlantique, 006_Quebec, 006_Ontario, 006_Prairies, 006_BC_CB, 006_Territories_Territoires
BASE_CENSUS = "https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger/comp/GetFile.cfm"
CENSUS_REGIONS = {
    "Atlantic": "006_Atlantic_Atlantique",
    "Quebec": "006_Quebec",
    "Ontario": "006_Ontario",
    "Prairies": "006_Prairies",
    "BC": "006_BC_CB",
    "Territories": "006_Territories_Territoires",
}

# 2021 DA cartographic boundaries (English, full extent). Cartographic = land only (no water), so DAs don't extend into lakes/ocean.
# Digital (lda_000a21a_e) extends into water and inflates greenspace. Use cartographic (b) instead.
# Naming per StatCan 92-160-G: l=Lambert, da_=dissemination area, 000=Canada, b=cartographic, 21=2021, a=shapefile, _e=English
# Catalogue 92-169-X2021001 - Dissemination area boundary files
BOUNDARY_ZIP_URL = "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lda_000b21a_e.zip"


def download_census_region(region_key: str) -> Path:
    """Download one Census Profile (DA-level) by region. StatCan returns a ZIP; we extract the CSV."""
    if region_key not in CENSUS_REGIONS:
        raise ValueError(f"Unknown region. Choose from: {list(CENSUS_REGIONS.keys())}")
    geono = CENSUS_REGIONS[region_key]
    url = f"{BASE_CENSUS}?Lang=E&FILETYPE=CSV&GEONO={geono}"
    out_path = RAW_DIR / f"census_profile_2021_DA_{region_key}.csv"
    print(f"Downloading Census Profile ({region_key})...")
    r = requests.get(url, timeout=600, stream=True)
    r.raise_for_status()
    content = r.content
    # StatCan returns a ZIP for large files; check first two bytes
    if content[:2] == b"PK":
        print("  Response is ZIP; extracting CSV...")
        temp_zip = RAW_DIR / f"_temp_{region_key}.zip"
        temp_zip.write_bytes(content)
        with zipfile.ZipFile(temp_zip, "r") as z:
            # Find the *_CSV_data_*.csv or *data*Ontario.csv style name
            csv_names = [n for n in z.namelist() if "CSV_data" in n and n.endswith(".csv")]
            if not csv_names:
                csv_names = [n for n in z.namelist() if n.endswith(".csv") and "data" in n]
            if not csv_names:
                raise RuntimeError(f"No CSV found in zip. Names: {z.namelist()}")
            inner_name = csv_names[0]
            z.extract(inner_name, path=RAW_DIR)
            extracted = RAW_DIR / inner_name
            if extracted != out_path:
                if out_path.exists():
                    out_path.unlink()
                extracted.rename(out_path)
        temp_zip.unlink(missing_ok=True)
        print(f"  Extracted and saved: {out_path}")
    else:
        with open(out_path, "wb") as f:
            f.write(content)
        print(f"Saved: {out_path}")
    return out_path


def download_all_census_regions() -> list[Path]:
    """Download Census Profile CSV for all regions. Large total size."""
    paths = []
    for region in CENSUS_REGIONS:
        try:
            p = download_census_region(region)
            paths.append(p)
        except Exception as e:
            print(f"Warning: failed to download {region}: {e}")
    return paths


def download_boundaries() -> Path:
    """Download 2021 DA boundary zip and extract to data/raw/da_boundaries/.
    Removes old boundary files (e.g. lada* ADA files) so only DA boundaries remain.
    Avoids rmtree so we don't hit Windows 'file in use' errors."""
    out_zip = RAW_DIR / "lda_000b21a_e.zip"
    out_dir = RAW_DIR / "da_boundaries"
    out_dir.mkdir(parents=True, exist_ok=True)
    # Remove old boundary files (lada* = ADA; lda_* = previous DA) so extract is clean.
    # We only delete files, not the folder, to avoid Windows "file in use" on rmtree.
    for p in list(out_dir.rglob("*")):
        if p.is_file() and (p.name.startswith("lada") or p.name.startswith("lda_")):
            try:
                p.unlink()
            except OSError as e:
                print(f"Could not remove {p}: {e}")
                print("Close any program using data/raw/da_boundaries (e.g. R, File Explorer, QGIS) and try again.")
                raise SystemExit(1) from e
    print(f"Downloading DA boundaries to {out_zip}...")
    r = requests.get(BOUNDARY_ZIP_URL, timeout=120, stream=True)
    r.raise_for_status()
    with open(out_zip, "wb") as f:
        for chunk in r.iter_content(chunk_size=8192):
            f.write(chunk)
    print("Extracting zip...")
    with zipfile.ZipFile(out_zip, "r") as z:
        z.extractall(out_dir)
    print(f"Extracted to: {out_dir}")
    return out_dir


def main():
    import argparse
    p = argparse.ArgumentParser(description="Fetch StatCan 2021 Census Profile and DA boundaries")
    p.add_argument("--census", action="store_true", help="Download Census Profile CSV (DA-level)")
    p.add_argument("--region", choices=list(CENSUS_REGIONS.keys()), default="Atlantic",
                   help="Census region (default: Atlantic for a smaller test run)")
    p.add_argument("--all-regions", action="store_true", help="Download all census regions (large)")
    p.add_argument("--boundaries", action="store_true", help="Download 2021 DA boundary files")
    p.add_argument("--all", action="store_true", help="Download boundaries + one census region (Atlantic)")
    args = p.parse_args()

    if args.all:
        download_boundaries()
        download_census_region("Atlantic")
        return

    if args.boundaries:
        download_boundaries()
    if args.census:
        download_census_region(args.region)
    if args.all_regions:
        download_all_census_regions()

    if not (args.boundaries or args.census or args.all_regions or args.all):
        p.print_help()
        print("\nExample: python fetch_statcan_census.py --all")
        print("         python fetch_statcan_census.py --boundaries --census --region Ontario")


if __name__ == "__main__":
    main()
