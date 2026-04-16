# Projects folder

## New interactive project page (embedded HTML + tab navigation)

Copy [`_project-interactive-page-template.qmd`](_project-interactive-page-template.qmd) into a new folder under `projects/<slug>/index.qmd`. Files whose names start with `_` are not published as standalone pages by Quarto; the template is reference-only.

Tab order is fixed: **Final product** → **Project Overview** → **Project Approach** → **Scripts & Code** → **Impact & Outcome**. Replace placeholder panels and keep the same element ids (`portfolio-final-product`, `portfolio-overview`, `portfolio-approach`, `portfolio-scripts`, `portfolio-impact`) and button classes so styling and tab JavaScript keep working.

## New PDF / slides project page

Copy [`_project-pdf-slides-page-template.qmd`](_project-pdf-slides-page-template.qmd) and add your `slides.pdf` plus intro and **Project Approach** copy.

## Styling

Interactive tab buttons use `.portfolio-view-toggle` (see `custom.scss`). The layout uses flex wrap so five buttons stay responsive on narrow viewports.

For stronger visual rhythm, include:

- one `.portfolio-page-intro` block under the tab toggle
- a small `.portfolio-meta-row` with 2-4 `.portfolio-meta-chip` labels
- `.portfolio-panel-surface` on each tab panel container

## Table of contents

[`_metadata.yml`](_metadata.yml) in this folder turns off Quarto’s right-hand section TOC (`toc: false`) for all pages under `projects/`. Interactive and PDF-style project pages also hide the margin column via `custom.scss` where `.portfolio-project-page-root` or `.project-pdf-slides-root` is present.

## Project Approach (voice)

Treat **Project Approach** as a **pre-build planning frame**: past tense is fine for **problem definition** (what the situation was); use **present / future / “planned”** for scope, design, and workflow. The templates include a short note on this pattern.
