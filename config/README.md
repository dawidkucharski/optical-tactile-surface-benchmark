This folder can hold optional configuration files.

- params.yaml (optional):
  params: [rp, rv, rz, ra, rt, rz1max, ...]

If provided, the report can prioritize the order of parameters to display.

Workflow (after editing params.yaml):
1. Place/update raw Excel files in tactile/ and optics/.
2. Run: ./run_all.sh
3. Final PDF: summary_report.pdf

To override from CLI (ignoring params.yaml):
Rscript korelacje_tactile_optics.R params=Rp,Rv optics_cols=130,134

Alternative: Makefile targets
 make install       # only packages
 make correlations  # tactile vs optics all params
 make ml            # ML + advanced
 make report        # build PDF
 make all           # full pipeline

Docker usage (reproducible):
 docker build -t nsmt-report:latest .
 docker run --rm -v "$PWD":/workspace nsmt-report:latest
 # Output summary_report.pdf will appear in current host directory
