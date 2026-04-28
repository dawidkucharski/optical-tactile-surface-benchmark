# Make targets for convenience
.PHONY: all install correlations ml report clean docker report-only diagnose

all: install correlations ml report

RSCRIPT := $(shell command -v Rscript 2>/dev/null || echo /usr/local/bin/Rscript)

install:
	$(RSCRIPT) install_packages.R || { echo 'ERROR: Rscript not found. Install R (https://cran.r-project.org/)'; exit 2; }

correlations:
	$(RSCRIPT) korelacje_tactile_optics.R

ml:
	$(RSCRIPT) ml_influence.R

report:
	bash report_only.sh

# Generate / reuse TEX then attempt PDF (unless SKIP_PDF=1) and HTML (unless SKIP_HTML=1). FORCE_REGENERATE=1 to force rerender.
report-only:
	bash report_only.sh

# Environment diagnostics (creates script if missing then runs)
diagnose: diagnose_env.sh
	bash diagnose_env.sh

diagnose_env.sh:
	@echo '#!/usr/bin/env bash' > diagnose_env.sh
	@echo 'set -euo pipefail' >> diagnose_env.sh
	@echo 'echo "[diag] R: $(command -v R || echo missing)"' >> diagnose_env.sh
	@echo 'echo "[diag] pandoc: $(command -v pandoc || echo missing)"' >> diagnose_env.sh
	@echo 'echo "[diag] xelatex: $(command -v xelatex || echo missing)"' >> diagnose_env.sh
	@echo 'echo "[diag] pdflatex: $(command -v pdflatex || echo missing)"' >> diagnose_env.sh
	@echo 'echo "[diag] docker: $(command -v docker || echo missing)"' >> diagnose_env.sh
	@echo 'echo "[diag] PATH=$$PATH"' >> diagnose_env.sh
	@chmod +x diagnose_env.sh

clean:
	rm -f summary_report.pdf summary_report.tex

# Build docker image

docker:
	docker build -t nsmt-report:latest .
