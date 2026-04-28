# GitHub Reproducibility Release Checklist

Use this checklist to publish the project as a public reproducibility package while excluding raw `.sur` surface files.

## Repository Target

Recommended repository name:

```text
optical-tactile-surface-benchmark
```

Final manuscript placeholder to replace:

```text
https://github.com/dawidkucharski/optical-tactile-surface-benchmark
```

## Include

- R and Python analysis scripts.
- `config/params.yaml` and configuration documentation.
- Manuscript and supplement LaTeX sources in `paper/`.
- Bibliography files used by the manuscript.
- Generated manuscript tables in `paper/tables/`.
- Generated figures in `plots/`.
- Derived CSV and text outputs in `outputs/` needed to reproduce manuscript claims.
- `paper/reproducibility_manifest.md`.
- `README.md` and this release checklist.

## Exclude

- Raw `.sur` / `.SUR` surface files.
- Local Python/R environments and cache files.
- LaTeX build intermediates such as `.aux`, `.log`, `.out`, `.bbl`, `.blg`, `.fls`, and `.fdb_latexmk`.
- Operating-system metadata such as `.DS_Store`.

The `.gitignore` file already contains these rules.

## Pre-Publication Check

Run from the project root:

```sh
find . -iname '*.sur' -o -iname '*.SUR'
git status --ignored
```

The first command should return no files staged for publication. If `.sur` files exist locally, they should appear as ignored in `git status --ignored` after `git init`.

## Suggested Git Commands

Run only when ready to publish:

```sh
git init
git add .
git status
git commit -m "Prepare reproducibility package"
git branch -M main
git remote add origin https://github.com/dawidkucharski/optical-tactile-surface-benchmark.git
git push -u origin main
```

After the repository is public, verify that the manuscript URL opens correctly. For stronger journal reproducibility, connect the GitHub repository to Zenodo and reserve a DOI for the submitted version.