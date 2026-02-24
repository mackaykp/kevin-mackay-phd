"""
Scrape a YouTube channel's videos page using Playwright.
Run with: python scrape_channel.py [--url URL] [--output FILE] [--channel-name NAME]
Browser runs visible (headless=False) so you can watch it work.
"""

import argparse
import csv
import re
import time
from pathlib import Path

from playwright.sync_api import sync_playwright

DATA_DIR = Path(__file__).parent / "data"
DEFAULT_URL = "https://www.youtube.com/@yogawithadriene/videos"
DEFAULT_OUTPUT = DATA_DIR / "yoga_videos.csv"
# How long to wait for the grid to load and between scrolls
PAGE_LOAD_WAIT_MS = 4000
SCROLL_PAUSE_SEC = 1.2
# Stop after this many scrolls with no new videos
MAX_EMPTY_SCROLLS = 5


def scroll_to_load_all(page) -> None:
    """Scroll the page until no new videos appear."""
    last_count = 0
    empty_scrolls = 0

    while empty_scrolls < MAX_EMPTY_SCROLLS:
        # Scroll the main content (YouTube uses #content or the scrollable container)
        page.evaluate("window.scrollBy(0, window.innerHeight)")
        time.sleep(SCROLL_PAUSE_SEC)

        # Count current video links (links to /watch?v=)
        count = page.locator('a[href^="/watch?v="]').count()
        if count > last_count:
            last_count = count
            empty_scrolls = 0
        else:
            empty_scrolls += 1

    # One more scroll and wait to catch any final items
    page.evaluate("window.scrollTo(0, document.body.scrollHeight)")
    time.sleep(SCROLL_PAUSE_SEC)


def scroll_once(page) -> None:
    """Scroll down one viewport height to load more content."""
    page.evaluate("window.scrollBy(0, window.innerHeight)")
    time.sleep(SCROLL_PAUSE_SEC)


def parse_duration(text: str) -> str:
    """Return duration as-is (e.g. '12:34' or '1:23:45')."""
    if not text or not text.strip():
        return ""
    return text.strip()


def parse_views(text: str) -> str:
    """Return view count string as-is (e.g. '1.2M views')."""
    if not text or not text.strip():
        return ""
    return text.strip()


def extract_videos(page) -> list[dict]:
    """Extract video entries from the current page."""
    videos = []
    # Video links in the grid (exclude sidebar/other links by using the grid context)
    # YouTube shows each video in a rich-item-renderer with a link containing /watch?v=
    links = page.locator('a[href^="/watch?v="]').all()

    seen_ids = set()

    for link in links:
        try:
            href = link.get_attribute("href") or ""
            if "&" in href:
                video_id = href.split("&")[0].replace("/watch?v=", "")
            else:
                video_id = href.replace("/watch?v=", "").strip()
            if not video_id or video_id in seen_ids:
                continue
            seen_ids.add(video_id)

            url = f"https://www.youtube.com{href}" if href.startswith("/") else href

            # Title: often in the link's title attribute or in an inner text element
            title = link.get_attribute("title") or ""
            if not title:
                title_el = link.locator("yt-formatted-string#video-title").first
                if title_el.count():
                    title = title_el.text_content() or ""

            # Duration: in a span that usually has time or duration (overlay on thumbnail)
            duration = ""
            container = link.locator("xpath=ancestor::ytd-rich-item-renderer").first
            if container.count():
                duration_el = container.locator(
                    "span.ytd-thumbnail-overlay-time-status-renderer, "
                    "#text.ytd-thumbnail-overlay-time-status-renderer, "
                    "ytd-thumbnail-overlay-time-status-renderer span"
                ).first
                if duration_el.count():
                    duration = parse_duration(duration_el.text_content() or "")

            # Views and date: in metadata line (e.g. "1.2M views · 2 years ago")
            metadata = ""
            if container.count():
                meta_el = container.locator(
                    "ytd-video-meta-block #metadata-line span, "
                    "span.ytd-video-meta-block"
                ).first
                if meta_el.count():
                    metadata = meta_el.text_content() or ""

            videos.append({
                "video_id": video_id,
                "title": title.strip(),
                "duration": duration,
                "metadata_line": metadata.strip(),
                "url": url,
            })
        except Exception as e:
            print(f"Skip one entry: {e}")
            continue

    return videos


def slug_from_url(url: str) -> str:
    """Return a short slug for the channel URL (e.g. @yogawithadriene -> yogawithadriene)."""
    if not url:
        return "channel"
    u = url.rstrip("/").replace("https://www.youtube.com/", "").replace("https://youtube.com/", "")
    return u.replace("@", "").replace("/videos", "").replace("/", "_") or "channel"


def scrape_channel_to_list(
    url: str,
    channel_name: str = "",
    headless: bool = False,
) -> list[dict]:
    """
    Scrape a channel's videos page and return a list of video dicts.
    Each dict has: video_id, title, duration, metadata_line, url; plus "channel" if channel_name is set.
    """
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=headless)
        context = browser.new_context(
            viewport={"width": 1280, "height": 720},
            user_agent=(
                "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
                "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
            ),
        )
        page = context.new_page()
        try:
            page.goto(url, wait_until="domcontentloaded", timeout=60000)
            page.wait_for_timeout(PAGE_LOAD_WAIT_MS)
            scroll_to_load_all(page)
            videos = extract_videos(page)
            by_id = {}
            for v in videos:
                if v["video_id"] not in by_id:
                    by_id[v["video_id"]] = v
            videos = list(by_id.values())
            if channel_name:
                for v in videos:
                    v["channel"] = channel_name
            return videos
        finally:
            browser.close()


def scrape_channel_new_only(
    existing_ids: set[str],
    url: str,
    channel_name: str = "",
    headless: bool = False,
    log=None,
) -> list[dict]:
    """
    Scrape a channel's videos page (newest first) and return only videos that are
    not in existing_ids. Stops loading as soon as the first already-cataloged video
    is found, since everything below it is older and already in the spreadsheet.
    """
    if log is None:
        log = print

    with sync_playwright() as p:
        browser = p.chromium.launch(headless=headless)
        context = browser.new_context(
            viewport={"width": 1280, "height": 720},
            user_agent=(
                "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
                "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
            ),
        )
        page = context.new_page()
        try:
            log("  Opening channel page...")
            page.goto(url, wait_until="domcontentloaded", timeout=60000)
            page.wait_for_timeout(PAGE_LOAD_WAIT_MS)

            prev_count = 0
            empty_scrolls = 0

            while empty_scrolls < MAX_EMPTY_SCROLLS:
                log("  Reading video list from page...")
                videos = extract_videos(page)
                count = len(videos)
                log(f"  Loaded {count} videos so far (newest at top).")

                # Walk top-to-bottom (newest first); stop at first video already in catalog
                for i, v in enumerate(videos):
                    if v["video_id"] in existing_ids:
                        log(f"  Found first cataloged video at position {i + 1} — stopping (all above are new).")
                        new_list = videos[:i]
                        if channel_name:
                            for u in new_list:
                                u["channel"] = channel_name
                        return new_list

                if count == prev_count and prev_count > 0:
                    empty_scrolls += 1
                else:
                    empty_scrolls = 0
                prev_count = count

                log("  No match yet — scrolling to load more...")
                scroll_once(page)

            # Reached end of channel (no more videos load); all we have are new
            log("  Reached end of channel list; treating all as new.")
            if channel_name:
                for v in videos:
                    v["channel"] = channel_name
            return videos
        finally:
            browser.close()


def main() -> None:
    parser = argparse.ArgumentParser(description="Scrape a YouTube channel's videos page")
    parser.add_argument("--url", default=DEFAULT_URL, help=f"Channel videos URL (default: {DEFAULT_URL})")
    parser.add_argument("--output", type=Path, default=None, help="Output CSV path (default: data/yoga_videos_<slug>.csv for custom URL, else data/yoga_videos.csv)")
    parser.add_argument("--channel-name", type=str, default="", help="Channel display name (stored in 'channel' column if provided)")
    args = parser.parse_args()

    url = args.url
    channel_name = (args.channel_name or "").strip()
    if args.output is not None:
        output_csv = Path(args.output)
    else:
        if url == DEFAULT_URL:
            output_csv = DEFAULT_OUTPUT
        else:
            slug = slug_from_url(url)
            output_csv = DATA_DIR / f"yoga_videos_{slug}.csv"

    output_csv.parent.mkdir(parents=True, exist_ok=True)

    print(f"Opening {url} ...")
    videos = scrape_channel_to_list(url, channel_name=channel_name, headless=False)
    print(f"Found {len(videos)} videos.")

    if not videos:
        print("No videos extracted. Check the page structure or selectors.")
        return

    fieldnames = ["video_id", "title", "duration", "metadata_line", "url"]
    if channel_name:
        fieldnames = ["channel"] + fieldnames

    with open(output_csv, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(videos)

    print(f"Saved to {output_csv}")


if __name__ == "__main__":
    main()
