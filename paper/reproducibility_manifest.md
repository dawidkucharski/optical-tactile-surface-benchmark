# Manuscript Reproducibility Manifest

This manifest records the commands and generated files needed to rebuild the manuscript-facing evidence package for the retrospective optical-vs-tactile benchmark. The public GitHub reproducibility package is intended to contain all scripts, configuration files, manuscript sources, derived CSV outputs, generated tables, and generated figures, except raw `.sur` surface files.

Public repository:

```text
https://github.com/dawidkucharski/optical-tactile-surface-benchmark
```

DOI-ready release tag:

```text
v1.0.2
```

Zenodo DOI:

```text
https://doi.org/10.5281/zenodo.19844491
```

## Inputs

- Tactile exports: `tactile/` (`.sur` files excluded from public GitHub release if present)
- Optical exports: `optics/` (`.sur` files excluded from public GitHub release if present)
- Parameter and optics-column configuration: `config/params.yaml`
- Existing derived analysis products: `outputs/`

Raw measurement exports are required for a complete recalculation from original instrument files. The public GitHub package will exclude `.sur` raw surface files, but will include derived CSV outputs, generated manuscript tables, figures, and the scripts used to rebuild the reported manuscript-facing summaries.

## Main Recalculation Commands

Run from the project root:

```sh
Rscript korelacje_tactile_optics.R
Rscript make_results_decision_tables.R
Rscript ml_influence.R
python3 paper/summarize_results_for_manuscript.py
```

The decision-summary generator writes:

- `outputs/results_decision_best_optical_by_group.csv`
- `outputs/results_config_medians_by_group.csv`
- `outputs/results_fixed_workflow_group_medians.csv`
- `outputs/results_fixed_workflow_sensitivity.csv`
- `outputs/results_best_discrepancy_heatmap_by_process_param.csv`
- `paper/tables/results_decision_by_param.tex`
- `paper/tables/results_decision_by_process.tex`
- `paper/tables/results_decision_by_material.tex`
- `paper/tables/results_fixed_workflow_sensitivity.tex`
- `plots/global_best_discrepancy_heatmap.pdf`

## Manuscript Build

Run from the project root:

```sh
cd paper
pdflatex -interaction=nonstopmode manuscript.tex
bibtex manuscript
pdflatex -interaction=nonstopmode manuscript.tex
pdflatex -interaction=nonstopmode manuscript.tex
```

Expected output: `paper/manuscript.pdf`.

## Supplement Build

Run from the project root:

```sh
cd paper
python3 generate_supplement.py
pdflatex -interaction=nonstopmode supplement.tex
pdflatex -interaction=nonstopmode supplement.tex
```

Expected output: `paper/supplement.pdf`.

## Submission Package Recommendation

For journal submission, deposit the project in a public GitHub repository using the `.gitignore` rules in the project root. The repository should include `config/params.yaml`, R/Python scripts, manuscript and supplement sources, generated CSV files, manuscript table `.tex` files, generated figure PDFs, and this manifest. Raw `.sur` surface files should be excluded from GitHub; if reviewers require them, provide them through controlled author request or a journal-approved restricted data channel. After Zenodo archives the DOI-ready release, add the minted DOI to the manuscript Data Availability statement and cite the versioned repository snapshot.