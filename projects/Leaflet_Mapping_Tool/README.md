# Green Space & SES Explorer (Leaflet Mapping Tool)

Explore how **income** (median household income by dissemination area) relates to **access to green space** in Canadian cities. The tool produces a bivariate map: Income × green area per 1,000 people. Data can come from the SES index build (Statistics Canada Census) or another income GeoPackage; parks and greenspace are from OpenStreetMap.

## Requirements

- **R** (tidyverse, sf, leaflet, osmdata, leaflet.extras, htmlwidgets, classInt, geosphere, glue, readr)
- **Python 3** (requests, pandas) for data fetch
- **Quarto** (optional) to render the project as an HTML website

Install R packages (run once in R):

```r
install.packages(c("tidyverse", "sf", "leaflet", "leaflet.extras", "htmlwidgets", "classInt", "osmdata", "geosphere", "glue", "readr"))
```

## Quick start

### 1. Fetch data (Python)

```bash
pip install -r requirements.txt
python scripts/fetch_statcan_census.py --all
```

This downloads 2021 DA boundaries and one Census Profile region (Atlantic) into `data/raw/`. For all of Canada use `--boundaries --all-regions`.

### 2. Build SES index (R)

From the project root in R:

```r
source("R/build_ses_index.R")
```

Writes `data/ses_index.gpkg` (use this path as `income_gpkg` in step 3).

### 3. Create a map (R)

The mapping function builds a **bivariate map** (Income × green area per 1000 people). Requires **population** in the income layer (e.g. from `build_ses_index.R`). Greenspace (parks, forest, meadow, etc.) from OSM within a walkable buffer; 3×3 classes (±1 SD). Use the **layer control** (top right) to switch basemaps (Light, Street, Satellite) and toggle overlay layers (Income × Green, Study area, Greenspace). The **legend** (bottom left) explains the 3×3 bivariate colours.

```r
source("R/create_bivariate_map.R")
create_bivariate_map(city = "Halifax, NS", income_gpkg = "data/ses_index.gpkg")
```

Output: `output/maps/Halifax_bivariate.html`.

Optional arguments: `amalgamation_cities_list`, `city_pr`, `custom_bbox`, `custom_distance_km`, `green_buffer_m`, `show_green_layers`.

**Popup fields** (click a DA):

| Field | Meaning |
|-------|--------|
| **Median household income** | Income (e.g. 2020 $); raw value from census. |
| **Income Category** | Low / Med / High (±1 SD from the study-area mean). |
| **Green area (in buffer)** | Total greenspace (m²) from OSM within the walkable buffer of the DA. |
| **Green area per 1000 people** | (Green area m² × 1000) / population; "No data" if population is missing or zero. |
| **Green category** | Low / Med / High for green access (±1 SD in the study area). |
| **Description** | Combined class (e.g. High–Med = High income, Medium green access). |

### 4. Render Quarto site (optional)

```bash
quarto render
```

Open `_site/index.html` or publish `_site/` to your web host.

## Project structure

- **R/** – R scripts: `map_config.R`, `build_ses_index.R` (adds population for park per capita), `process_bbox.R`, `process_green.R`, `process_green_da.R` (park area per DA), `create_bivariate_map.R`, `mapping_functions.R`
- **scripts/** – Python: `fetch_statcan_census.py`
- **data/raw/** – Census CSV and DA boundaries (from Python)
- **data/ses_index.gpkg** – GeoPackage from build_ses_index.R (income + population); pass as `income_gpkg` to create_bivariate_map
- **data/clipped_income/** – Income layer clipped to each study area
- **output/maps/** – Generated Leaflet HTML maps
- **vignettes/** – Example city pages (Halifax, Vancouver, Hamilton)
- **guide.qmd** – Full guide; **index.qmd** – Landing page

## Data pipeline and map

**build_ses_index.R** builds a GeoPackage from 2021 Census (education, income, employment at DA level) and writes `data/ses_index.gpkg`. The **bivariate map** uses that file as the income source: it displays **median household income** × green area per 1,000 people (Low/Med/High by study-area ±1 SD). To change index categories or green-space thresholds used by the build script, edit **`R/map_config.R`**. See `R/build_ses_index.R` and [Guide](guide.qmd).

## Data sources

- **Income/Census:** Statistics Canada, [2021 Census Profile](https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/index.cfm?Lang=E) (DA-level).
- **Boundaries:** Statistics Canada, [2021 DA boundary files](https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/).
- **Green space:** [OpenStreetMap](https://www.openstreetmap.org/copyright) (parks, playgrounds, nature reserves).

## Licence

- Census data: Statistics Canada [Open Licence](https://www.statcan.gc.ca/en/reference/licence).
- OpenStreetMap: © [OpenStreetMap contributors](https://www.openstreetmap.org/copyright).
