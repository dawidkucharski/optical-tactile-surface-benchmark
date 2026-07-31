# Manuscript Reproducibility Manifest

This manifest records the commands and generated files needed to rebuild the manuscript-facing evidence package for the retrospective optical-vs-tactile benchmark. The public GitHub reproducibility package is intended to contain all scripts, configuration files, manuscript sources, derived CSV outputs, generated tables, and generated figures, except raw `.sur` surface files.

Public repository:

```text
https://github.com/dawidkucharski/optical-tactile-surface-benchmark
```

DOI-ready release tag:

```text
v1.0.4
```

Zenodo DOI:

```text
https://doi.org/10.5281/zenodo.19844490
```

## Inputs

- Tactile exports: `tactile/` (`.sur` files excluded from public GitHub release if present)
- Optical exports: `optics/` (`.sur` files excluded from public GitHub release if present)
- Parameter and optics-column configuration: `config/params.yaml`
- Existing derived analysis products: `outputs/`

Raw measurement exports are required for a complete recalculation from original instrument files. The public GitHub package will exclude `.sur` raw surface files, but will include derived CSV outputs, generated manuscript tables, figures, and the scripts used to rebuild the reported manuscript-facing summaries.

The archive does not consistently retain a full metrological metadata table for every measurement. In particular, detailed machining parameters, complete optical instrument settings, common profile-filtering settings, raw profile processing chains, optical repeat measurements for every system-illumination pair, and a combined tactile-optical uncertainty budget are not uniformly available. These omissions are part of the scientific boundary of the study and are why the manuscript reports a retrospective workflow benchmark rather than a formal equivalence, repeatability, or traceability validation.

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
- `outputs/results_workflow_transfer_by_group.csv`
- `outputs/results_workflow_transfer_summary_by_param.csv`
- `outputs/results_workflow_selection_penalty_overview.csv`
- `outputs/results_best_discrepancy_heatmap_by_process_param.csv`
- `outputs/results_rsm_diagnostic_by_stratum.csv`
- `outputs/results_rsm_unit_scale_sensitivity.csv`
- `outputs/results_bootstrap_ci_aggregate_medians.csv`
- `paper/tables/results_decision_by_param.tex`
- `paper/tables/results_decision_by_process.tex`
- `paper/tables/results_decision_by_material.tex`
- `paper/tables/results_fixed_workflow_sensitivity.tex`
- `paper/tables/results_workflow_transfer_summary.tex`
- `paper/tables/results_rsm_diagnostic.tex`
- `paper/tables/results_rsm_unit_scale_sensitivity.tex`
- `paper/tables/results_bootstrap_ci_aggregate_medians.tex`
- `plots/global_best_discrepancy_heatmap.pdf`
- `plots/workflow_selection_penalty_overview.pdf`

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

## Computational Environment Snapshot

The manuscript-facing release includes a lightweight environment snapshot in:

- `paper/reproducibility_session_info.txt`

Regenerate it from the project root with:

```sh
Rscript -e 'pkgs <- c("readxl", "readr", "dplyr", "stringr", "ggplot2", "glmnet", "ranger", "fastshap", "tidyr", "rmarkdown", "jsonlite"); ip <- installed.packages(); keep <- pkgs[pkgs %in% rownames(ip)]; versions <- data.frame(package = keep, version = ip[keep, "Version"], row.names = NULL); writeLines(c(capture.output(sessionInfo()), "", "Project package versions:", capture.output(print(versions, row.names = FALSE))), "paper/reproducibility_session_info.txt")'
```

The main manuscript decision tables and figures use R packages including `readxl`, `readr`, `dplyr`, `stringr`, and `ggplot2`. The optional ML influence workflow additionally uses `glmnet`, `ranger`, `fastshap`, and `tidyr`. The manuscript supplement-generation Python scripts in `paper/` use the Python standard library only.

## Submission Package Recommendation

For journal submission, deposit the project in a public GitHub repository using the `.gitignore` rules in the project root. The repository should include `config/params.yaml`, R/Python scripts, manuscript and supplement sources, generated CSV files, manuscript table `.tex` files, generated figure PDFs, and this manifest. Raw `.sur` surface files should be excluded from GitHub; if reviewers require them, provide them through controlled author request or a journal-approved restricted data channel. After Zenodo archives the DOI-ready release, add the minted DOI to the manuscript Data Availability statement and cite the versioned repository snapshot.
