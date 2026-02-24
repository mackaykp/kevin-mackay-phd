"""
Update the catalog with only NEW videos from each configured channel.

- Reads data/channel_urls.json for channel name -> videos page URL.
- Reads data/yoga_videos_enriched.csv to get existing video_ids.
- For each channel: scrapes the channel page, keeps only video_ids not already
  in the spreadsheet, enriches those new videos, and appends them.
- Then runs categorize_data.py and update_focus.py on the full file (categories + focus).

Run without re-pulling or re-processing videos you already have.

Usage:
  python update_from_channels.py
  python update_from_channels.py --headless
  python update_from_channels.py --no-categorize   # skip categorize and body-parts steps
"""

import argparse
import csv
import json
import subprocess
import sys
from pathlib import Path

import pandas as pd

from scrape_channel import scrape_channel_new_only

DATA_DIR = Path(__file__).parent / "data"
CHANNEL_URLS_JSON = DATA_DIR / "channel_urls.json"
ENRICHED_CSV = DATA_DIR / "yoga_videos_enriched.csv"
ENRICHED_XLSX = DATA_DIR / "yoga_videos_enriched.xlsx"


def load_channel_config() -> dict[str, str]:
    """Load channel display name -> videos page URL."""
    path = CHANNEL_URLS_JSON.resolve()
    if not path.exists():
        print(f"Config not found: {path}")
        print("Create it with JSON: {\"Channel Name\": \"https://www.youtube.com/@handle/videos\", ...}")
        return {}
    with open(path, encoding="utf-8") as f:
        out = json.load(f)
    if not isinstance(out, dict):
        print(f"Invalid config: {path} should be a JSON object.")
        return {}
    return out


def load_existing_video_ids() -> set[str]:
    """Return set of video_ids already in the enriched CSV."""
    if not ENRICHED_CSV.exists():
        return set()
    df = pd.read_csv(ENRICHED_CSV)
    if "video_id" not in df.columns:
        return set()
    return set(df["video_id"].astype(str).dropna().unique())


def main() -> None:
    parser = argparse.ArgumentParser(description="Update catalog with new videos only from configured channels")
    parser.add_argument("--headless", action="store_true", help="Run browser headless when scraping")
    parser.add_argument("--no-categorize", action="store_true", help="Skip categorize_data and update_focus")
    args = parser.parse_args()

    print("Loading channel config...")
    print(f"  Config path: {CHANNEL_URLS_JSON.resolve()}")
    channels = load_channel_config()
    if not channels:
        return
    channel_list = list(channels.items())
    print(f"  Found {len(channel_list)} channel(s) to check.")
    for idx, (name, _) in enumerate(channel_list, 1):
        print(f"    {idx}. {name}")

    print("\nLoading existing catalog...")
    existing_ids = load_existing_video_ids()
    print(f"  Existing videos in catalog: {len(existing_ids)}")

    script_dir = Path(__file__).resolve().parent
    python_exe = sys.executable
    any_new = False

    for idx, (channel_name, channel_url) in enumerate(channel_list, 1):
        print(f"\n--- Channel {idx}/{len(channel_list)}: {channel_name} ---")
        print(f"  URL: {channel_url}")
        try:
            new_videos = scrape_channel_new_only(
                existing_ids,
                channel_url,
                channel_name=channel_name,
                headless=args.headless,
                log=print,
            )
        except Exception as e:
            print(f"  Scrape failed: {e}")
            import traceback
            traceback.print_exc()
            continue

        print(f"  New videos to add: {len(new_videos)}")

        if not new_videos:
            print("  Nothing to add for this channel.")
            continue

        any_new = True
        temp_csv = DATA_DIR / f"_new_{channel_name.replace(' ', '_')}.csv"
        fieldnames = ["channel", "video_id", "title", "duration", "metadata_line", "url"]
        print(f"  Writing {len(new_videos)} rows to temp file...")
        with open(temp_csv, "w", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
            writer.writeheader()
            writer.writerows(new_videos)

        print(f"  Enriching {len(new_videos)} new videos (yt-dlp)...")
        subprocess.run(
            [
                python_exe,
                str(script_dir / "enrich_videos.py"),
                "--input", str(temp_csv),
                "--append-to", str(ENRICHED_CSV),
                "--channel", channel_name,
            ],
            check=True,
            cwd=str(script_dir),
        )
        temp_csv.unlink(missing_ok=True)
        existing_ids.update(v["video_id"] for v in new_videos)
        print(f"  Appended to {ENRICHED_CSV}.")

    if not any_new:
        print("\nNo new videos to add. Catalog is up to date.")
        return

    if args.no_categorize:
        print("\nSkipping categorize and body-parts (--no-categorize).")
        print("Done.")
        return

    print("\nUpdating categories on full file (categorize_data.py)...")
    subprocess.run([python_exe, str(script_dir / "categorize_data.py")], check=True, cwd=str(script_dir))
    print("Updating focus (update_focus.py)...")
    subprocess.run([python_exe, str(script_dir / "update_focus.py")], check=True, cwd=str(script_dir))
    print("\nDone. Re-run quarto render yoga_catalog.qmd to refresh the catalog page.")


if __name__ == "__main__":
    main()
