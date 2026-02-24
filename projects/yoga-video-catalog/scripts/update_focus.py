"""
Update focus column in yoga_videos_enriched.csv using the focus keyword list
(same as enrich_videos.py).

Run this to update existing enriched data without re-fetching from YouTube.
The CSV must have a column named **focus** (see INSTRUCTIONS.md if yours says body_part_focus).
"""

import json
import re
from pathlib import Path

import pandas as pd

DATA_DIR = Path(__file__).parent / "data"
CONFIG_DIR = Path(__file__).parent / "config"
CSV_FILE = DATA_DIR / "yoga_videos_enriched.csv"

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
    for canonical, variations in FOCUS_KEYWORDS:
        if canonical in found:
            continue
        for variation in variations:
            if variation in text_lower:
                found.append(canonical)
                break
    return found


def main():
    if not CSV_FILE.exists():
        print(f"Not found: {CSV_FILE}")
        return

    df = pd.read_csv(CSV_FILE, na_values=["", "NA"])

    # CSV must use "focus" column (rename body_part_focus in the CSV header if needed — see INSTRUCTIONS.md)
    if "focus" not in df.columns and "body_part_focus" in df.columns:
        print("The CSV has 'body_part_focus' but this script expects 'focus'.")
        print("Rename the column: open data/yoga_videos_enriched.csv, change the header in the first row from body_part_focus to focus, save, then run this again.")
        return
    if "body_part_focus" in df.columns:
        df = df.drop(columns=["body_part_focus"])

    titles = df["title"].fillna("")
    descs = df["description"].fillna("").apply(truncate_at_separator)
    title_desc = (titles + " " + descs).str[:10000]

    # Extract focus from keywords
    focus_lists = title_desc.apply(extract_focus)
    df["focus"] = focus_lists.apply(lambda x: " | ".join(x) if x else "")

    df.to_csv(CSV_FILE, index=False, encoding="utf-8")
    print(f"Updated focus in {CSV_FILE}")
    print(f"Videos with focus tags: {(df['focus'] != '').sum()} / {len(df)}")


if __name__ == "__main__":
    main()
