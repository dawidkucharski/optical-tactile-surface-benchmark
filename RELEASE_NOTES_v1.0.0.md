# v1.0.0 - Submission Reproducibility Package

Repository: https://github.com/dawidkucharski/optical-tactile-surface-benchmark

This release is the manuscript-facing reproducibility package for the retrospective optical-vs-tactile surface roughness benchmark.

## Included

- R and Python scripts used to generate manuscript-facing summaries.
- Configuration files, including `config/params.yaml`.
- Manuscript and supplement LaTeX sources.
- Bibliography files used by the manuscript.
- Generated manuscript tables and supplementary tables.
- Generated figures and derived CSV/text outputs supporting manuscript claims.
- `paper/reproducibility_manifest.md` with rebuild commands.
- `GITHUB_RELEASE_CHECKLIST.md` with publication and archive checks.

## Excluded

- Raw `.sur` / `.SUR` surface files are intentionally excluded from the public repository.
- Local Python/R environments, operating-system metadata, Office lock files, and LaTeX build intermediates are excluded by `.gitignore`.

## Main Rebuild Commands

Run from the project root:

```sh
Rscript korelacje_tactile_optics.R
Rscript make_results_decision_tables.R
Rscript ml_influence.R
python3 paper/summarize_results_for_manuscript.py
cd paper && python3 generate_supplement.py
```

Build the manuscript from `paper/`:

```sh
pdflatex -interaction=nonstopmode manuscript.tex
bibtex manuscript
pdflatex -interaction=nonstopmode manuscript.tex
pdflatex -interaction=nonstopmode manuscript.tex
```

## Validation Status

- No `.sur` / `.SUR` files were present in the staged public release snapshot.
- No oversized files above GitHub's normal file-size limit were detected.
- `paper/manuscript.pdf` compiled successfully after the final Data Availability update.
- The release commit is intended for DOI archiving through GitHub Releases and Zenodo.