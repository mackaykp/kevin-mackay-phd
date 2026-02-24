"""
Enrich a yoga videos CSV with per-video metadata using yt-dlp (no YouTube API).
Fetches: title, description, duration, upload_date, view_count, like_count, etc.
Adds a focus column by parsing title + description with focus keywords.

Usage:
  python enrich_videos.py                           # process data/yoga_videos.csv -> data/yoga_videos_enriched.csv
  python enrich_videos.py --input data/foo.csv --output data/foo_enriched.csv --channel "My Channel"
  python enrich_videos.py --input data/new.csv --append-to data/yoga_videos_enriched.csv --channel "New Channel"
  python enrich_videos.py --limit 20 --delay 2
"""

import argparse
import json
import re
import time
from pathlib import Path

import pandas as pd
import yt_dlp

DATA_DIR = Path(__file__).parent / "data"
CONFIG_DIR = Path(__file__).parent / "config"
DEFAULT_INPUT = DATA_DIR / "yoga_videos.csv"
DEFAULT_OUTPUT = DATA_DIR / "yoga_videos_enriched.csv"
DEFAULT_XLSX = DATA_DIR / "yoga_videos_enriched.xlsx"

with open(CONFIG_DIR / "focus_map.json", encoding="utf-8") as f:
    FOCUS_MAP = json.load(f)

FOCUS_KEYWORDS = [(canonical, variations) for canonical, variations in FOCUS_MAP.items()]

_SEPARATOR_RE = re.compile(
    r"^[\s]*"
    r"([-]{3,}|[—]{3,}|[-\s]{5,}|[*]{3,}|[_]{3,}|[=]{3,}|[═]{3,}|[─]{3,})"
    r"[\s]*$",
    re.MULTILINE,
)


def truncate_at_separator(desc: str) -> str:
    """Return description text above the first visual separator line."""
    if not desc:
        return ""
    m = _SEPARATOR_RE.search(desc)
    return desc[: m.start()].strip() if m else desc


def extract_focus(text: str) -> list[str]:
    """Return list of canonical focus names found in text (lowercase)."""
    if not text or not isinstance(text, str):
        return []
    text_lower = text.lower()
    found = []
    # Check in order (more specific first due to FOCUS_MAP ordering)
    for canonical, variations in FOCUS_KEYWORDS:
        if canonical in found:
            continue
        for variation in variations:
            if variation in text_lower:
                found.append(canonical)
                break
    return found


def fetch_video_metadata(url: str, ydl_opts: dict) -> dict | None:
    """Return a flat dict of fields we want, or None on failure."""
    try:
        with yt_dlp.YoutubeDL(ydl_opts) as ydl:
            info = ydl.extract_info(url, download=False)
            if not info:
                return None
            # Normalize to dict and take only the keys we need
            if hasattr(ydl, "sanitize_info"):
                info = ydl.sanitize_info(info)
            elif hasattr(info, "get"):
                info = dict(info)
            else:
                info = dict(info) if info else {}
    except Exception as e:
        print(f"  Error: {e}")
        return None

    title = info.get("title") or ""
    description = (info.get("description") or "")[:10000]  # cap size
    duration = info.get("duration")
    upload_date = info.get("upload_date")  # YYYYMMDD
    view_count = info.get("view_count")
    like_count = info.get("like_count")
    categories = info.get("categories")
    tags = info.get("tags")

    # Format duration as M:SS or H:MM:SS
    duration_str = ""
    if duration is not None and duration >= 0:
        h = int(duration) // 3600
        m = (int(duration) % 3600) // 60
        s = int(duration) % 60
        if h > 0:
            duration_str = f"{h}:{m:02d}:{s:02d}"
        else:
            duration_str = f"{m}:{s:02d}"

    # Format upload_date as YYYY-MM-DD
    upload_date_str = ""
    if upload_date and len(str(upload_date)) >= 8:
        u = str(upload_date)[:8]
        upload_date_str = f"{u[:4]}-{u[4:6]}-{u[6:8]}"

    focus_list = extract_focus(title + " " + truncate_at_separator(description))
    focus_str = " | ".join(focus_list) if focus_list else ""

    return {
        "title": title,
        "description": description,
        "duration_seconds": duration if duration is not None else "",
        "duration": duration_str or "",
        "upload_date": upload_date_str,
        "view_count": view_count if view_count is not None else "",
        "like_count": like_count if like_count is not None else "",
        "categories": " | ".join(categories) if isinstance(categories, list) else (categories or ""),
        "tags": " | ".join(tags) if isinstance(tags, list) else (tags or ""),
        "focus": focus_str,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description="Enrich yoga videos CSV with yt-dlp metadata")
    parser.add_argument("--input", type=Path, default=DEFAULT_INPUT, help="Input CSV path")
    parser.add_argument("--output", type=Path, default=None, help="Output CSV path (default: data/yoga_videos_enriched.csv or same as --append-to)")
    parser.add_argument("--append-to", type=Path, default=None, help="Append enriched rows to this CSV (and update same-name .xlsx). Use with --input and --channel to add a new channel.")
    parser.add_argument("--channel", type=str, default="", help="Channel display name (added as 'channel' column; required when using --append-to for new channel)")
    parser.add_argument("--limit", type=int, default=None, help="Process only first N rows (for testing)")
    parser.add_argument("--delay", type=float, default=1.5, help="Seconds between requests (default 1.5)")
    parser.add_argument("--no-excel", action="store_true", help="Skip writing .xlsx file")
    args = parser.parse_args()

    input_csv = Path(args.input)
    append_to = Path(args.append_to) if args.append_to else None
    channel_name = (args.channel or "").strip()

    if append_to:
        output_csv = append_to
        output_xlsx = append_to.with_suffix(".xlsx")
    else:
        output_csv = Path(args.output) if args.output else DEFAULT_OUTPUT
        output_xlsx = output_csv.with_suffix(".xlsx") if not args.no_excel else None

    if not input_csv.exists():
        print(f"Input not found: {input_csv}")
        return

    df = pd.read_csv(input_csv)
    if "url" not in df.columns:
        print("CSV must have a 'url' column")
        return

    if append_to and not channel_name:
        print("When using --append-to you must set --channel so rows are labeled.")
        return

    urls = df["url"].astype(str).tolist()
    if args.limit:
        urls = urls[: args.limit]
        df = df.iloc[: len(urls)].copy()

    if channel_name and "channel" not in df.columns:
        df.insert(0, "channel", channel_name)

    ydl_opts = {
        "quiet": True,
        "no_warnings": True,
        "extract_flat": False,
        "skip_download": True,
    }

    enriched_rows = []
    for i, url in enumerate(urls):
        if not url or not url.startswith("http"):
            enriched_rows.append({k: "" for k in ("title", "description", "duration_seconds", "duration", "upload_date", "view_count", "like_count", "categories", "tags", "focus")})
            continue
        print(f"[{i + 1}/{len(urls)}] {url[:60]}...")
        row = fetch_video_metadata(url, ydl_opts)
        if row:
            enriched_rows.append(row)
        else:
            enriched_rows.append({k: "" for k in ("title", "description", "duration_seconds", "duration", "upload_date", "view_count", "like_count", "categories", "tags", "focus")})
        time.sleep(args.delay)

    enrich_df = pd.DataFrame(enriched_rows)
    out = df.copy()
    for col in enrich_df.columns:
        out[col] = enrich_df[col].values
    original_cols = [c for c in df.columns]
    new_cols = [c for c in enrich_df.columns if c not in df.columns]
    out = out[original_cols + new_cols]

    if append_to and append_to.exists():
        existing = pd.read_csv(append_to)
        if "channel" not in existing.columns:
            existing.insert(0, "channel", "Yoga With Adriene")
        out = pd.concat([existing, out], ignore_index=True)

    DATA_DIR.mkdir(parents=True, exist_ok=True)
    out.to_csv(output_csv, index=False, encoding="utf-8")
    print(f"Saved {output_csv}")

    if not args.no_excel and output_xlsx:
        out.to_excel(output_xlsx, index=False, engine="openpyxl")
        print(f"Saved {output_xlsx}")

    print("Done.")


if __name__ == "__main__":
    main()
