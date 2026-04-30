# GitHub Reproducibility Release Checklist

Use this checklist to publish the project as a public reproducibility package while excluding raw `.sur` surface files.

## Repository Target

Recommended repository name:

```text
optical-tactile-surface-benchmark
```

Final manuscript repository URL:

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

## Versioned Release

After pushing `main`, create and push the submitted-version tag:

```sh
git tag -a v1.0.0 -m "Submission reproducibility package"
git push origin v1.0.0
```

Then create a GitHub Release from tag `v1.0.0` and use `RELEASE_NOTES_v1.0.0.md` as the release description.

For Zenodo DOI archiving, keep this repository enabled in Zenodo's GitHub integration before publishing the release. The current DOI-ready release is `v1.0.4` and uses `RELEASE_NOTES_v1.0.4.md`.

```sh
git tag -a v1.0.4 -m "Zenodo DOI metadata consolidation"
git push origin v1.0.4
```

Create a GitHub Release from tag `v1.0.4`; Zenodo should archive that release under the existing concept DOI.

After the repository is public, verify that the manuscript URL opens correctly. For stronger journal reproducibility, connect the GitHub repository to Zenodo and reserve a DOI for the submitted version.