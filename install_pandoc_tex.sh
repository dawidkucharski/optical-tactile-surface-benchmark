#!/usr/bin/env bash
set -euo pipefail
# Manual installer for pandoc + TinyTeX (Mac focus)
if ! command -v pandoc >/dev/null 2>&1; then
  if command -v brew >/dev/null 2>&1; then
    echo "[install] Installing pandoc via brew";
    brew install pandoc;
  else
    echo "[install] Homebrew not found. Install Homebrew first: https://brew.sh";
  fi
else
  echo "[install] pandoc already present";
fi

# TinyTeX (only if no LaTeX engine)
if ! command -v xelatex >/dev/null 2>&1 && ! command -v pdflatex >/dev/null 2>&1; then
  echo "[install] Installing TinyTeX via R";
  /usr/local/bin/Rscript -e "if (!requireNamespace('tinytex', quietly=TRUE)) install.packages('tinytex'); tinytex::install_tinytex()"
else
  echo "[install] LaTeX engine already present";
fi

echo "[install] Done."