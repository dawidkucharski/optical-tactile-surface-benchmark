# v1.0.3 - Q1 manuscript evidence update

Repository: https://github.com/dawidkucharski/optical-tactile-surface-benchmark

This release updates the manuscript-facing reproducibility package after the Q1-readiness hardening of the paper and supporting analyses.

## Scientific updates

- Reframed and rebuilt the manuscript as a retrospective workflow benchmark rather than an optical--tactile equivalence claim.
- Added ISO/GPS profile-vs-areal scope anchoring in the manuscript and bibliography.
- Added dedicated `Rsm` diagnostic segmentation by optical system, material, and process.
- Added `Rsm` unit-scale sensitivity for retained optical spacing exports.
- Added non-parametric bootstrap percentile intervals for aggregate median best-achievable discrepancy summaries.
- Added a fixed-workflow sensitivity interpretation that separates retrospective best-achievable screening from preselected optical workflow use.
- Added a lightweight R environment snapshot for reproducibility review.

## Included generated outputs

- `outputs/results_rsm_diagnostic_by_stratum.csv`
- `outputs/results_rsm_unit_scale_sensitivity.csv`
- `outputs/results_bootstrap_ci_aggregate_medians.csv`
- `paper/tables/results_rsm_diagnostic.tex`
- `paper/tables/results_rsm_unit_scale_sensitivity.tex`
- `paper/tables/results_bootstrap_ci_aggregate_medians.tex`
- `paper/reproducibility_session_info.txt`
- Updated `paper/manuscript.pdf` and `paper/supplement.pdf`

## Scope

The release remains a public reproducibility package for the retrospective optical-vs-tactile surface roughness benchmark. It includes scripts, configuration files, manuscript and supplement sources, generated CSV outputs, tables, and figures. Raw `.sur` / `.SUR` surface files remain excluded from the public repository and archive.

## Zenodo DOI

This version was archived by Zenodo under version DOI `10.5281/zenodo.19911951` and concept DOI:

```text
10.5281/zenodo.19844490
```