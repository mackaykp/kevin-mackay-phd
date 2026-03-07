# Geocoding Tool – Test Result Analysis (Expert Review)

## Implementation status (P0–P4)

- **P0** – Fixed: when `address_cols` includes a `"City"` column, that value is used for `geo_score` (and city filter) instead of the parser’s segment.
- **P1** – Implemented: after province filter, candidates are restricted to same city (normalized Processed_City == input City).
- **P2** – Implemented: Phase 1b exact match on street-only (number + street segment) against reference `full_addr` before fuzzy.
- **P3** – Implemented: fuzzy match accepted only if `street_score >= 0.5` and, when input has city, `geo_score > 0`.
- **P4** – Implemented: output includes `same_city`, `civic_number_exact`, and `match_quality` (high/medium/low).

Re-run the test with: `source("run_test_geocode.R")` and inspect `test_addresses_geocoded.csv`.

---

## Original analysis (pre–P0–P4)

**Test file:** `test_addresses_geocoded.csv` (10 ideal addresses, Ajax ON)  
**Reference data:** Ontario only (e.g. NAR or single-province build).

---

## 1. What Worked Well

- **Tool runs end-to-end** – No crashes; reads input, loads reference data, runs exact then fuzzy, writes output with all expected columns (Latitude, Longitude, match_type, scores, matched_address).
- **Province filtering is on** – Only Ontario candidates are used (province parsing and column names are correct). So the candidate pool is province-restricted.
- **Fuzzy matching fires** – 9/10 rows got a match; only 320 Audley Road North got NA. So the pipeline is capable of returning a best candidate for most inputs.
- **Scores are exposed** – `text_score`, `number_score`, `geo_score`, `composite_score` are in the output, so we can diagnose quality and tune later.
- **Civic number is used** – `number_score` is often high when the chosen reference candidate has a close street number (e.g. 734 vs 733), which shows numeric nearness is in the loop.
- **Chunking and CSV support** – Progress saving and CSV input work as intended for production use.

---

## 2. What Didn’t Work Well

### 2.1 Geographic score is always zero

- **Observation:** Every row has `geo_score = 0`, including clear same-city cases (e.g. 734 Audley Road South, Ajax).
- **Cause:** City used in scoring is taken from the **parser** on the concatenated address string. For `"734 Audley Road South, Ajax, Ontario, L1S 1A1"` the comma segments are `[street, city, province, postal]`. The parser’s heuristic “last segment before postal” is **Ontario** (the province), so it sets `city = "Ontario"` instead of `"Ajax"`. Then `same_city` is (Ontario vs AJAX) → false → `geo_score = 0`.
- **Impact:** We get no credit for same city and no penalty for wrong city. The matcher can prefer a candidate in another city (e.g. Hamilton) if text + number score are slightly better.

### 2.2 Wrong matches accepted

- **734 Audley Road South** → matched to **733** Audley Road South (wrong civic number).
- **726 Audley Road South** → **707** Audley Road South (wrong number).
- **448 Bayly Street East, Ajax** → **448 Barton Street East** (wrong street name, likely different city, e.g. Hamilton).
- **462 Kingston Road East, Ajax** → **463 King Street East** (wrong street: “King Street” ≠ “Kingston Road”).
- **750 Rossland Road East** → **451** Rossland Road East (wrong number).
- **1363 Salem Road North** → **412 Salem Ave N** (wrong number and street type).
- **125 Rushworth Drive** → **138 Rush Rd** (different street form and number).

So we are often returning a **different address** (wrong number and/or wrong street and/or wrong city), not the correct reference record.

### 2.3 One complete failure

- **320 Audley Road North** → no match (all geocode columns NA). Possible reasons: no candidate passed the composite threshold, or direction filter (North) removed all candidates, or that specific address is missing from the reference data.

### 2.4 Exact match never used

- **Observation:** All matches are `match_type = fuzzy`; no exact matches.
- **Cause:** Input is built as full string: `"734 Audley Road South, Ajax, Ontario, L1S 1A1"`. Reference `full_addr` is street only, e.g. `"734 AUDLEY ROAD SOUTH"`. So normalized strings never match exactly.
- **Impact:** We never get “perfect” matches even when the street line is identical; we rely entirely on fuzzy logic and scores.

---

## 3. Root Causes (Team View)

1. **City for scoring is wrong when using address_cols**  
   We use parser-derived “city” from the concatenated string instead of the **actual City column**. So geo_score is broken for the main use case (Street, City, Province, Postal Code).

2. **No requirement for same city**  
   Because geo_score is 0, we don’t down-rank or exclude candidates in other cities. So “448 Bayly Street East, Ajax” can match “448 Barton Street East” elsewhere.

3. **Exact match is defined on full string**  
   With multi-component addresses, exact match is effectively unused. We could add a second pass: try exact match on **street-only** (e.g. first segment or normalized street line) against reference `full_addr`.

4. **Fuzzy threshold and weights**  
   Default composite threshold (0.3) lets in weak matches. With geo_score stuck at 0, the composite is driven only by text + number, so wrong-street or wrong-city candidates can still win.

5. **Street name similarity too permissive**  
   “Bayly” vs “Barton” and “Kingston Road” vs “King Street” still get moderate text scores. We might need a **minimum street-name similarity** or a **same-city (or same-FSA) requirement** before accepting a candidate.

---

## 4. What We’d Improve (Prioritized)

### P0 – Fix city for scoring and filtering

- When `address_cols` includes a column named `"City"` (or similar), use that column’s value for:
  - `geo_score` (same_city),
  - and optionally for **candidate filtering** (e.g. restrict to reference rows with same Processed_City, or fuzzy-match city first).
- Stop relying on the parser’s “city” when it’s known to be wrong for “Street, City, Province, Postal” concatenation.

### P1 – Use city (and optionally FSA) to restrict candidates

- After province filter, optionally filter by **city** (exact or fuzzy match on Processed_City) so we only consider same-city (or same region) reference rows. That would have prevented Ajax addresses matching Hamilton (Barton) or other cities.

### P2 – Optional exact match on street-only

- Build a “street line” from number + street (first segment when comma-separated).
- Try exact match of this normalized street line against reference `full_addr` before fuzzy. That would give true exact matches when the street line is identical.

### P3 – Stricter fuzzy rules

- Require **minimum text_score** (e.g. 0.6) on the **street** component so “Bayly” vs “Barton” is rejected.
- Or require **geo_score > 0** (same city) when input city is present, so we never return a different city unless we explicitly allow “no city match”.

### P4 – Quality flags in output

- Add a column such as `match_quality` or flags: e.g. “same_city”, “same_street_name”, “civic_number_exact”, so users can filter or review low-quality matches.

### P5 – Direction and missing match

- For 320 Audley Road North: log or report when no candidate passes the threshold; optionally relax direction filter or lower threshold for “no match” cases so we can at least return a same-street candidate with a low score and a flag.

---

## 5. Pros and Cons of Current Design

| Aspect | Pro | Con |
|--------|-----|-----|
| Province filter | Shrinks pool; avoids cross-province errors | Alone it’s not enough; we need city |
| Fuzzy only (no street exact) | Still gives a “best” candidate | Wrong candidate often; no true exact match |
| Composite = text + number + geo | Theoretically balances components | geo is 0 in practice; weights don’t help yet |
| No city filter | Maximizes recall | Lets wrong-city matches win (e.g. Barton vs Bayly) |
| Exposing scores | Good for auditing and tuning | No automatic “reject low quality” yet |

---

## 6. Summary

- **Working:** Pipeline, province filter, fuzzy matching, scoring formula, output structure, chunking, CSV support.
- **Broken:** City used in scoring (parser bug for address_cols) → geo_score = 0 → wrong-city and wrong-address matches accepted.
- **Next steps:** Fix city source for scoring (and optionally filtering), then add same-city or same-region constraint and optional street-only exact match. After that, tighten fuzzy rules and add quality flags so the tool is reliable on ideal data before scaling to real data.
