#!/usr/bin/env bash
set -euo pipefail

# Env flags:
#   NEED_PDF=0  -> skip report rendering
#   FORCE_DOCKER=1 -> force docker render even if pandoc present
NEED_PDF=${NEED_PDF:-1}
FORCE_DOCKER=${FORCE_DOCKER:-0}

# Orchestrates full recompute: correlations (multi-param), ML (base+advanced), and final report.
# Requires Rscript in PATH and packages installed.

echo "[0/5] Checking / installing required R packages ..."
/usr/local/bin/Rscript install_packages.R || { echo "ERROR: package installation failed"; exit 1; }

echo "[1/5] Recomputing param correlations (tactile vs optics) from config/params.yaml ..."
/usr/local/bin/Rscript korelacje_tactile_optics.R || { echo "ERROR: correlations step failed"; exit 1; }

echo "[2/5] Running machine learning analyses (base + advanced) ..."
/usr/local/bin/Rscript ml_influence.R || { echo "ERROR: ML step failed"; exit 1; }

if [ "$NEED_PDF" = "1" ]; then
	echo "[3/5] Rendering consolidated report (with ML & advanced) ..."
	USE_DOCKER=0
	if [ "$FORCE_DOCKER" = "1" ]; then
		USE_DOCKER=1
	elif ! command -v pandoc >/dev/null 2>&1; then
		if command -v docker >/dev/null 2>&1; then
			echo "[INFO] pandoc not found locally, using docker fallback."
			USE_DOCKER=1
		else
			echo "ERROR: pandoc not found and docker unavailable. Install pandoc: brew install pandoc" >&2
			exit 2
		fi
	fi
	if [ $USE_DOCKER -eq 1 ]; then
		docker build -t nsmt-report:render . || { echo "ERROR: docker build failed"; exit 1; }
		docker run --rm -v "$PWD":/workspace nsmt-report:render bash -lc "Rscript -e 'rmarkdown::render(\"summary_report.Rmd\", output_file=\"summary_report.pdf\")'" || { echo "ERROR: dockerized render failed"; exit 1; }
	else
		/usr/local/bin/Rscript -e "rmarkdown::render('summary_report.Rmd', output_file='summary_report.pdf')" || { echo "ERROR: report render failed"; exit 1; }
	fi
else
	echo "[3/5] Skipping PDF render (NEED_PDF=0)."
fi

echo "[4/5] (Optional) You can re-run only ML + report by: Rscript ml_influence.R && Rscript -e \"rmarkdown::render('summary_report.Rmd')\""
if [ "$NEED_PDF" = "1" ]; then
	echo "[5/5] Done. Output: summary_report.pdf"
else
	echo "[5/5] Done (no PDF render)."
fi
