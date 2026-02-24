# Step-by-step: Push this website to GitHub Pages

Your Quarto site is already configured for GitHub Pages. Your repository is:

**https://github.com/mackaykp/kevin-mackay-phd**

Follow these steps to push this local project to that repo and publish the site.

---

## 1. Initialize Git and make the first commit

In your project folder, open **Git Bash** or **PowerShell** (use Git Bash if PowerShell says `git` is not recognized—e.g. right after installing Git).

```bash
cd "c:\Users\macka\OneDrive\Documents\Cursor-Projects\Github_Pages_Website"

git init
git add .
git status
git commit -m "Initial commit: Quarto portfolio site for GitHub Pages"
```

---

## 2. Connect to GitHub and push

Add your repo as the remote and push. Your repo is **mackaykp/kevin-mackay-phd**, so use:

```bash
git branch -M main
git remote add origin https://github.com/mackaykp/kevin-mackay-phd.git
git push -u origin main
```

When prompted, sign in with your GitHub account (browser or token).

**Note:** The repo [mackaykp/kevin-mackay-phd](https://github.com/mackaykp/kevin-mackay-phd) already has some files (e.g. `.Rhistory`, `styles.css`, `website.Rproj`). If this is a fresh push of your full local site, you may need to force-push once (`git push -u origin main --force`) to replace the repo contents with your local project. Only do that if you’re sure the local version is the one you want to keep.

---

## 3. Turn on GitHub Pages

1. Open **https://github.com/mackaykp/kevin-mackay-phd**
2. Go to **Settings** → **Pages** (left sidebar).
3. Under **Build and deployment**:
   - **Source:** choose **GitHub Actions**.
4. Save. You don’t need to create a workflow — this project already has `.github/workflows/publish.yml`.

---

## 4. Wait for the first build

1. Open the **Actions** tab: **https://github.com/mackaykp/kevin-mackay-phd/actions**
2. You should see a workflow run (e.g. “Publish site to GitHub Pages”). Wait until it finishes (green check).
3. Your site will be live at:

   **https://mackaykp.github.io/kevin-mackay-phd/**

---

## 5. `_quarto.yml` is already correct

Your `_quarto.yml` already has the right URLs for this repo:

- `site-url: "https://mackaykp.github.io/kevin-mackay-phd"`
- `repo-url: "https://github.com/mackaykp/kevin-mackay-phd"`

No changes needed unless you rename the repo or your username.

---

## Summary

| Step | Action |
|------|--------|
| 1 | `git init`, `git add .`, `git commit -m "Initial commit: ..."` |
| 2 | `git remote add origin https://github.com/mackaykp/kevin-mackay-phd.git`, `git push -u origin main` |
| 3 | In repo **Settings → Pages** → Source: **GitHub Actions** |
| 4 | Check **Actions** tab; site at **https://mackaykp.github.io/kevin-mackay-phd/** |

After this, every push to `main` will rebuild and update the site automatically.

---

## Test the site build locally (before pushing)

To avoid long CI cycles, you can confirm the site renders on your machine first:

1. **Render the site** (same as CI):
   ```bash
   quarto render
   ```
   If this fails (e.g. missing R package), fix it locally; CI will then succeed.

2. **Preview** (optional):
   ```bash
   quarto preview
   ```
   Open the URL it prints and check the site.

3. **Run the full workflow locally with act** (optional, needs [Docker](https://www.docker.com/) and [act](https://github.com/nektos/act)):
   ```bash
   act push
   ```
   This runs the GitHub Actions workflow in Docker. The first run is slow (installs R + tidyverse); later runs are faster with cache.
