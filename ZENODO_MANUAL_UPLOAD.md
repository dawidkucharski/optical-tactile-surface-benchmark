# Zenodo Manual Upload Instructions

Use this route if Zenodo's GitHub integration does not list `dawidkucharski/optical-tactile-surface-benchmark`.

## Upload File

Upload the local archive created from the exact `v1.0.1` Git tag:

```text
zenodo_upload_v1.0.1.zip
```

This archive is generated with:

```sh
git archive --format=zip --output=zenodo_upload_v1.0.1.zip v1.0.1
```

The archive contains the tracked reproducibility package and excludes raw `.sur` / `.SUR` files, local environments, and build intermediates according to the committed release state.

## Zenodo Metadata

Resource type:

```text
Dataset
```

Title:

```text
Retrospective Benchmark of Optical Surface Roughness Workflows Against an Internal Tactile Baseline Across Multiple Materials and Processes
```

Creators:

```text
Kucharski, Dawid - Poznan University of Technology, Poland
Nieslony, Piotr - Opole University of Technology, Poland
Krolczyk, Jolanta - Opole University of Technology, Poland
Krolczyk, Grzegorz - Opole University of Technology, Poland
Nicinska, Katarzyna - Central Office of Measures, Poland
Wojciechowska, Natalia - Central Office of Measures, Poland
Slusarski, Lukasz - Central Office of Measures, Poland
Wieczorowski, Michal - Poznan University of Technology, Poland
Gapinski, Bartosz - Poznan University of Technology, Poland
```

Description:

```text
Reproducibility package for a retrospective benchmark of optical surface roughness workflows against an internal tactile baseline across multiple engineering materials and surface-generation processes. The public package contains scripts, configuration files, manuscript and supplement sources, derived CSV outputs, generated tables, and generated figures. Raw .sur surface files are excluded from the public repository and archive; they are available from the authors on reasonable request, subject to data-transfer constraints.
```

Keywords:

```text
surface metrology; surface roughness; optical measurement; tactile profilometry; reproducibility; benchmarking
```

Related identifier:

```text
https://github.com/dawidkucharski/optical-tactile-surface-benchmark/releases/tag/v1.0.1
```

Relation:

```text
Is supplemented by / Is supplement to the manuscript, depending on Zenodo's available controlled vocabulary.
```

Access:

```text
Open access
```

License:

```text
Choose explicitly before publishing. No repository license has been assigned automatically.
```

## After Publishing

Copy the DOI minted by Zenodo and update:

- `paper/manuscript.tex` Data Availability section.
- `paper/reproducibility_manifest.md`.
- `CITATION.cff` using the `doi:` field.
- `.zenodo.json` if a follow-up release is needed.