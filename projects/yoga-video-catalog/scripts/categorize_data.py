"""
Add category columns to yoga_videos_enriched.csv:
  - duration_bucket: round to nearest 5 min (0-5 min rounds to 5, then normal rounding)
  - upload_year: year only
  - views_category: one of Under 500k, 500k–1M, 1M–2.5M, 2.5M–10M, 10M+
  - views_category_order: 1–5 for sorting

Run after enrich_videos.py. Overwrites the CSV with added columns (keeps all existing columns).
"""

from pathlib import Path

import numpy as np
import pandas as pd

DATA_DIR = Path(__file__).parent / "data"
INPUT_CSV = DATA_DIR / "yoga_videos_enriched.csv"
OUTPUT_CSV = DATA_DIR / "yoga_videos_enriched.csv"
OUTPUT_XLSX = DATA_DIR / "yoga_videos_enriched.xlsx"

VIEWS_BREAKS = [0, 500_000, 1_000_000, 2_500_000, 10_000_000]
VIEWS_LABELS = ["Under 500k", "500k–1M", "1M–2.5M", "2.5M–10M", "10M+"]


def duration_to_mins(s):
    """Parse 'M:SS' or 'H:MM:SS'. For 3 parts: if value as H:MM:SS would be > 1:59:59 (120 min), assume MM:SS:xx (e.g. 2:00:00 = 2 min); else H:MM:SS."""
    if pd.isna(s) or str(s).strip() == "":
        return None
    parts = str(s).strip().split(":")
    parts = [float(x) for x in parts if x.replace(".", "").isdigit()]
    if not parts:
        return None
    if len(parts) == 2:
        return parts[0] + parts[1] / 60
    if len(parts) == 3:
        as_hms = parts[0] * 60 + parts[1] + parts[2] / 60
        if as_hms >= 120:
            return parts[0] + parts[1] / 60  # MM:SS:xx (e.g. 25:42:00 or 2:00:00)
        return as_hms  # H:MM:SS (1:59:59 or less)
    return None


def main():
    if not INPUT_CSV.exists():
        print(f"Not found: {INPUT_CSV}")
        return

    df = pd.read_csv(INPUT_CSV)

    # Duration: round to nearest 5 min (but 0-5 min rounds to 5, not 0)
    if "duration_seconds" in df.columns:
        sec = pd.to_numeric(df["duration_seconds"], errors="coerce")
        df["duration_mins"] = sec / 60
    else:
        df["duration_mins"] = df["duration"].map(duration_to_mins)
    df["duration_mins"] = df["duration_mins"].fillna(0)
    # Round to nearest 5, but if result is 0 and original > 0, set to 5; cap at 120 so bad/very long durations don't show as 999
    df["duration_bucket"] = (np.round(df["duration_mins"] / 5) * 5).clip(upper=120).astype(int)
    df.loc[(df["duration_bucket"] == 0) & (df["duration_mins"] > 0), "duration_bucket"] = 5
    df["duration_category"] = np.where(
        df["duration_bucket"] == 0, "—",
        np.where(df["duration_bucket"] == 120,
                 np.where(df["duration_mins"] > 120, "120+ min", "120 min"),
                 df["duration_bucket"].astype(str) + " min")
    )

    # Upload date -> year
    df["upload_year"] = pd.to_datetime(df["upload_date"], errors="coerce").dt.year
    df["upload_year"] = df["upload_year"].fillna(0).astype(int)
    df["upload_year_display"] = df["upload_year"].replace(0, "").astype(str).replace("", "—")

    # Views: 5 categories
    views = pd.to_numeric(df["view_count"], errors="coerce").fillna(-1)
    order = np.searchsorted(VIEWS_BREAKS[1:] + [np.inf], views, side="left") + 1
    order[views < 0] = 0
    order = np.clip(order, 0, 5)
    df["views_category_order"] = order.astype(int)
    df["views_category"] = [VIEWS_LABELS[i - 1] if 1 <= i <= 5 else "—" for i in df["views_category_order"]]
    df.loc[df["view_count"].isna(), "views_category"] = "—"

    df.to_csv(OUTPUT_CSV, index=False, encoding="utf-8")
    print(f"Updated {OUTPUT_CSV} with duration_category, upload_year, views_category.")
    try:
        df.to_excel(OUTPUT_XLSX, index=False, engine="openpyxl")
        print(f"Updated {OUTPUT_XLSX}.")
    except Exception as e:
        print(f"Note: Could not update Excel: {e}")


if __name__ == "__main__":
    main()
