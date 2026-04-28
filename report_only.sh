#!/usr/bin/env bash
set -euo pipefail

# PATH bootstrap: ensure typical locations for pandoc / LaTeX / Homebrew are present
for D in /usr/local/bin /opt/homebrew/bin /Library/TeX/texbin /usr/texbin; do
  if [ -d "$D" ]; then
    case ":$PATH:" in *":$D:"*) :;; *) PATH="$D:$PATH";; esac
  fi
done
export PATH
echo "[report-only] PATH bootstrap done. pandoc=$(command -v pandoc || echo missing) xelatex=$(command -v xelatex || echo missing)"

# Tryb pełniejszy: generacja TEX + (opcjonalnie) PDF i HTML.
# Zmienne sterujące:
#   FORCE_REGENERATE=1   – nadal wpływa na PDF/HTML odświeżenie, ale TEX i tak zawsze jest renderowany
#   SKIP_PDF=1           – pomiń kompilację PDF
#   SKIP_HTML=1          – pomiń HTML
# Zawsze najpierw powstaje summary_report.tex (latex_document), potem PDF (jeśli nie SKIP_PDF), potem HTML (jeśli nie SKIP_HTML).

FORCE_REGENERATE=${FORCE_REGENERATE:-0}
SKIP_PDF=${SKIP_PDF:-0}
SKIP_HTML=${SKIP_HTML:-0}
RSCRIPT_BIN="$(command -v Rscript || echo /usr/local/bin/Rscript)"

have_pandoc() { command -v pandoc >/dev/null 2>&1; }
have_xelatex() { command -v xelatex >/dev/null 2>&1; }

if ! have_pandoc; then
  echo "[report-only] Brak pandoc – nie mogę zrenderować .tex." >&2
  exit 2
fi
echo "[report-only] Render TEX (latex_document) (zawsze)"
$RSCRIPT_BIN -e "rmarkdown::render('summary_report.Rmd', output_format='latex_document', output_file='summary_report.tex', clean=FALSE)" || { echo '[report-only] Błąd renderowania TEX'; exit 2; }

if [ "$SKIP_PDF" = "1" ]; then
  echo "[report-only] SKIP_PDF=1 – pomijam PDF."
else
  if have_pandoc && have_xelatex; then
    echo "[report-only] Próba PDF (xelatex)."
    # Używamy rmarkdown aby zachować tę samą logikę (osadzanie zasobów); fallback jeśli błąd.
    if ! $RSCRIPT_BIN -e "rmarkdown::render('summary_report.Rmd', output_file='summary_report.pdf')"; then
      echo "[report-only] PDF nie powstał (błąd LaTeX) – kontynuuję bez PDF." >&2
    fi
  else
    echo "[report-only] Brak narzędzi do PDF (pandoc/xelatex) – pomijam." >&2
  fi
fi

if [ "$SKIP_HTML" = "1" ]; then
  echo "[report-only] SKIP_HTML=1 – pomijam HTML."
else
  if have_pandoc; then
    echo "[report-only] Generuję HTML."
    $RSCRIPT_BIN -e "rmarkdown::render('summary_report.Rmd', output_format='html_document', output_file='summary_report.html')" || echo "[report-only] Błąd renderowania HTML" >&2
  else
    echo "[report-only] Brak pandoc – brak HTML." >&2
  fi
fi

echo "[report-only] Zakończono. Dostępne pliki (o ile się udało): summary_report.tex $( [ -f summary_report.pdf ] && echo summary_report.pdf ) $( [ -f summary_report.html ] && echo summary_report.html )"
exit 0
