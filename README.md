# Portfolio website (Quarto + GitHub Pages)

A professional portfolio site built with **R** and **Quarto**, designed for GitHub Pages. It includes:

- **Navbar:** Home, About, Resume, Projects (with sidebar navigation)
- **About:** Background, interests, expertise, contact
- **Resume:** Experience, education, skills (placeholder content)
- **Projects:** Multi-category sidebar — Statistics & Data, Mapping & GIS, Automation, Other
- **Leaflet demo:** Example R + tidyverse + leaflet map in the Mapping section
- **Responsive layout:** Bootstrap-based; navbar collapses to a menu on smaller screens

## Build locally

1. Install [Quarto](https://quarto.org/docs/get-started/) and ensure R is installed with:
   - `tidyverse`
   - `leaflet`
2. In the project directory, run:

   ```bash
   quarto render
   ```

3. Preview the site:

   ```bash
   quarto preview
   ```

   Output is in `_site/`. Open `_site/index.html` in a browser if you prefer not to use preview.

## Deploy to GitHub Pages

1. Create a repository (e.g. `Github_Pages_Website` or `username.github.io`).
2. Push this project to the repo.
3. In GitHub: **Settings → Pages → Build and deployment**:
   - Source: **GitHub Actions** (recommended), or **Deploy from a branch**.
4. If using **GitHub Actions**, add the Quarto workflow (e.g. from [quarto-dev/quarto-actions](https://github.com/quarto-dev/quarto-actions)) so the site is built from the repo and published to Pages.
5. If using **branch**: set branch to `main` (or your default) and folder to `/_site` (or `/docs` if you set `output-dir: docs` in `_quarto.yml`), then run `quarto render` and push `_site` (or `docs`).

## Customize

- **_quarto.yml:** Site title, `site-url`, `repo-url`, navbar links (e.g. GitHub, LinkedIn), footer.
- **index.qmd, about.qmd, resume.qmd:** Replace placeholder text with your content.
- **projects/:** Add `.qmd` files under `projects/stats`, `projects/mapping`, `projects/automation`, `projects/others`; they will appear in the Projects sidebar automatically.
- **custom.scss:** Adjust colors, spacing, and typography.

Replace "Your Name" and "yourusername" (and any placeholder URLs) with your details before publishing.
