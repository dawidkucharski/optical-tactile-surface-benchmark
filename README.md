# Optical-Tactile Surface Benchmark

Public reproducibility package for the manuscript-facing retrospective benchmark of optical surface-roughness workflows against an internal tactile baseline.

Repository:

```text
https://github.com/dawidkucharski/optical-tactile-surface-benchmark
```

Current DOI-ready release: `v1.0.4`

Zenodo DOI:

```text
https://doi.org/10.5281/zenodo.19844490
```

## Scope

The repository contains the scripts, configuration files, manuscript sources, derived CSV outputs, generated tables, and generated figures needed to rebuild the evidence package for the submitted manuscript.

Raw `.sur` / `.SUR` surface files are intentionally excluded from the public GitHub-Zenodo package. The public package includes derived outputs and rebuild scripts; raw surface files can be requested from the authors subject to data-transfer constraints.

The study is framed as a retrospective workflow-level benchmark. It does not claim formal optical-tactile equivalence, metrological interchangeability, or universal optical substitution.

## Project Structure

- `korelacje_tactile_optics.R`: parameter-level optical-vs-tactile discrepancy calculations using `config/params.yaml`.
- `make_results_decision_tables.R`: manuscript decision tables, fixed-workflow sensitivity outputs, workflow-transfer checks, `Rsm` diagnostics, bootstrap summaries, and the global process-parameter heatmap.
- `ml_influence.R`: optional machine-learning influence analyses, including cross-validation outputs.
- `paper/`: manuscript, supplement, bibliography, supplement generator, submission files, and reproducibility manifest.
- `paper/tables/`: generated manuscript tables.
- `plots/`: generated manuscript figures.
- `outputs/`: derived CSV/text outputs supporting manuscript claims.
- `config/params.yaml`: roughness-parameter and optics-column configuration.
- `GITHUB_RELEASE_CHECKLIST.md`: publication and archive checklist for the public repository.

Legacy project-report files are retained for provenance, but the manuscript reproducibility package is controlled by `paper/reproducibility_manifest.md`.

## Manuscript Package

The main reproducibility control file is:

```text
paper/reproducibility_manifest.md
```

It records the inputs, rebuild commands, expected generated files, environment snapshot, and public-release boundary. Use it as the authoritative guide for journal review and DOI archiving.

Key manuscript-facing generated outputs include:

- `paper/tables/results_fixed_workflow_sensitivity.tex`: sensitivity of conclusions to a single fixed optical workflow.
- `paper/tables/results_workflow_transfer_summary.tex`: leave-one-surface workflow-transfer summary.
- `paper/tables/results_rsm_diagnostic.tex`: diagnostic segmentation of `Rsm` discrepancies by optical system, material, and process.
- `paper/tables/results_rsm_unit_scale_sensitivity.tex`: sensitivity of `Rsm` to the retained optical export unit scale.
- `paper/tables/results_bootstrap_ci_aggregate_medians.tex`: bootstrap percentile intervals for aggregate retrospective lowest-discrepancy summaries.
- `plots/global_best_discrepancy_heatmap.pdf`: process-parameter heatmap for retrospective lowest optical-tactile discrepancy.
- `plots/workflow_selection_penalty_overview.pdf`: overview of workflow-selection penalty.
- `outputs/results_fixed_workflow_sensitivity.csv`, `outputs/results_workflow_transfer_by_group.csv`, `outputs/results_workflow_transfer_summary_by_param.csv`, `outputs/results_best_discrepancy_heatmap_by_process_param.csv`, `outputs/results_rsm_diagnostic_by_stratum.csv`, `outputs/results_rsm_unit_scale_sensitivity.csv`, and `outputs/results_bootstrap_ci_aggregate_medians.csv`: source data for tables and figures.

## Requirements

- R and `Rscript`.
- Python 3.
- Pandoc for R Markdown / DOCX conversion workflows.
- LaTeX / pdfLaTeX for manuscript and supplement builds.
- Optional: Docker for containerised reproduction.

On macOS with Homebrew, a minimal local setup can start with:

```sh
brew install pandoc
/usr/local/bin/Rscript -e "install.packages('tinytex', repos='https://cloud.r-project.org'); tinytex::install_tinytex()"
```

Check Pandoc availability with:

```sh
which pandoc
/usr/local/bin/Rscript -e "rmarkdown::pandoc_version()"
```

## Main Recalculation

Run from the project root:

```sh
Rscript korelacje_tactile_optics.R
Rscript make_results_decision_tables.R
Rscript ml_influence.R
python3 paper/summarize_results_for_manuscript.py
```

Minimal recalculation for the current manuscript tables and figures:

```sh
Rscript make_results_decision_tables.R
python3 paper/summarize_results_for_manuscript.py
cd paper && python3 generate_supplement.py
```

## Manuscript Build

Run from the project root:

```sh
cd paper
pdflatex -interaction=nonstopmode manuscript.tex
bibtex manuscript
pdflatex -interaction=nonstopmode manuscript.tex
pdflatex -interaction=nonstopmode manuscript.tex
```

Expected output:

```text
paper/manuscript.pdf
```

## Supplement Build

Run from the project root:

```sh
cd paper
python3 generate_supplement.py
pdflatex -interaction=nonstopmode supplement.tex
pdflatex -interaction=nonstopmode supplement.tex
```

Expected output:

```text
paper/supplement.pdf
```

## Full Local Pipeline

The legacy full-project pipeline can still be run from the project root:

```sh
./run_all.sh
```

To recompute outputs without rendering the legacy PDF report:

```sh
NEED_PDF=0 ./run_all.sh
```

Common Makefile targets:

```sh
make all
make correlations ml
make report-only
make diagnose
```

## Docker

Build and run the reproducibility container:

```sh
docker build -t nsmt-report:latest .
docker run --rm -v "$PWD":/workspace nsmt-report:latest
```

## Parameter Configuration

The roughness parameters and optical export columns are configured in `config/params.yaml`:

```yaml
params: [Rp, Rv, Rz, Ra, Rt, Rz1max, Rq, Rsk, Rku, Rsm]
optics_cols: [130, 134, 138, 150, 146, 174, 154, 158, 162, 182]
# tactile_params: [optional alternative labels]
```

After changing this file, rerun the relevant recalculation command.

## Troubleshooting

| Problem | Likely cause | Suggested action |
|---------|--------------|------------------|
| Pandoc is missing | Pandoc is not installed or not on `PATH` | Install Pandoc or use Docker |
| LaTeX build fails | Missing TeX packages or stale intermediates | Run `make diagnose`; clean LaTeX intermediates and rebuild |
| New parameters do not appear | Derived `outputs/<param>_...` files were not regenerated | Rerun the correlation and decision-table scripts |
| ML advanced outputs are missing | `ml_influence.R` was not run | Run `Rscript ml_influence.R` |

## Public Release

Before publishing or archiving, use:

```text
GITHUB_RELEASE_CHECKLIST.md
```

The `.gitignore` file excludes raw `.sur` / `.SUR` files, local environments, and LaTeX build intermediates while keeping generated manuscript PDFs, plots, CSV outputs, and TeX tables trackable.
