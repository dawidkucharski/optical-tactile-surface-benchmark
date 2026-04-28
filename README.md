# Raport (PDF via LaTeX) – Instrukcja

## 1. Struktura
- `korelacje_tactile_optics.R` – generuje wyniki parametryczne (porównania optics vs tactile) dla listy z `config/params.yaml`.
- `make_results_decision_tables.R` – generuje tabele decyzyjne do manuskryptu, analizę czułości stałych konfiguracji optycznych oraz globalną mapę cieplną proces-parametr.
- `ml_influence.R` – modele ML (Lasso/Ridge, Random Forest, SHAP) + advanced CV.
- `summary_report.Rmd` – konsolidacja (Polski PDF, LaTeX, sekcje ML, advanced, proste wyjaśnienie).
- `run_all.sh` – pełny pipeline (można sterować `NEED_PDF=0`).
- `report_only.sh` – tylko render PDF (bez ponownego liczenia).
- `Dockerfile` – reprodukowalny kontener (z LaTeX przez TinyTeX + pandoc).

## Manuskrypt Q1/high-impact
Dedykowany pakiet manuskryptu znajduje się w `paper/`. Najważniejszy plik kontrolny to `paper/reproducibility_manifest.md`, który opisuje wejścia, komendy odtworzeniowe oraz pliki przeznaczone do archiwizacji przy zgłoszeniu do czasopisma. Planowany publiczny pakiet GitHub obejmuje cały projekt potrzebny do reprodukcji tabel, figur i wniosków manuskryptu, z wyłączeniem surowych plików powierzchni `.sur` / `.SUR`.

Adres repozytorium GitHub:
```text
https://github.com/dawidkucharski/optical-tactile-surface-benchmark
```

Minimalna ścieżka odtworzeniowa dla aktualnych tabel i figur manuskryptu:
```zsh
Rscript make_results_decision_tables.R
python3 paper/summarize_results_for_manuscript.py
cd paper && python3 generate_supplement.py
```

Nowe artefakty manuskryptu obejmują:
- `paper/tables/results_fixed_workflow_sensitivity.tex` – analiza czułości względem wyboru jednej stałej konfiguracji system+barwa.
- `plots/global_best_discrepancy_heatmap.pdf` – mapa procesu i parametru dla najlepszej osiągalnej rozbieżności optyczno-dotykowej.
- `outputs/results_fixed_workflow_sensitivity.csv` oraz `outputs/results_best_discrepancy_heatmap_by_process_param.csv` – dane źródłowe do tabeli i figury.

Przy publikacji na GitHub użyj `GITHUB_RELEASE_CHECKLIST.md`. Plik `.gitignore` wyklucza `.sur` / `.SUR`, środowiska lokalne i pliki tymczasowe LaTeX, ale pozwala śledzić wygenerowane PDF-y manuskryptu/suplementu, wykresy, CSV-y oraz tabele TeX.

## 2. Wymagane narzędzia (lokalnie)
- R + Rscript (działa: `/usr/local/bin/Rscript`).
- Pandoc (konwersja RMarkdown -> LaTeX -> PDF).
- LaTeX/XeLaTeX (instalujemy lekką dystrybucję TinyTeX).

### Instalacja (macOS Homebrew)
```zsh
brew install pandoc
/usr/local/bin/Rscript -e "install.packages('tinytex', repos='https://cloud.r-project.org'); tinytex::install_tinytex()"
```
Sprawdzenie:
```zsh
which pandoc
/usr/local/bin/Rscript -e "rmarkdown::pandoc_version()"
```

## 3. Pełny pipeline z PDF
```zsh
./run_all.sh
```
Jeśli chcesz tylko wyniki (bez PDF):
```zsh
NEED_PDF=0 ./run_all.sh
```

## 4. Tylko PDF (bez liczenia od nowa)
```zsh
./report_only.sh
```
Fallback:
- Jeśli brak pandoc i jest Docker: `./report_only.sh --docker`.
- Jeśli brak obydwu: skrypt wyświetli instrukcję instalacji.

## 5. Docker (pełna reprodukcja)
```zsh
docker build -t nsmt-report:latest .
docker run --rm -v "$PWD":/workspace nsmt-report:latest
```
PDF: `summary_report.pdf` w katalogu projektu.

## 6. Konfiguracja parametrów
`config/params.yaml`:
```yaml
params: [Rp, Rv, Rz, Ra, Rt, Rz1max, Rq, Rsk, Rku, Rsm]
optics_cols: [130, 134, 138, 150, 146, 174, 154, 158, 162, 182]
# tactile_params: [opcjonalne inne etykiety]
```
Zmiana listy parametrów = edycja tego pliku + ponowny `./run_all.sh`.

## 7. ML Advanced
Pliki w `outputs/ml/advanced/`: `cv_metrics.csv`, `importance_rf_cv.png`, `importance_glmnet_cv.png` + CSV ważności. Wszystkie osadzane w sekcji „Uczenie maszynowe — wyniki walidacji zaawansowanej (CV)”.

## 8. Typowe problemy
| Problem | Przyczyna | Rozwiązanie |
|---------|-----------|-------------|
| brak pandoc | nie zainstalowano | `brew install pandoc` lub Docker |
| Unicode minus (−) w PDF | użyto pdflatex | Upewnij się, że w YAML jest `latex_engine: xelatex` |
| Brak nowych parametrów w raporcie | pliki `outputs/<param>_...` nie powstały | Uruchom ponownie korelacje lub sprawdź indeks kolumn |
| Brak sekcji ML advanced | Nie uruchomiono `ml_influence.R` | `Rscript ml_influence.R` |

## 9. Tryb diagnostyczny
Możesz sprawdzić czy pandoc dostępny:
```zsh
/usr/local/bin/Rscript -e "rmarkdown::pandoc_available()"
```

## Konwencja poleceń (ważne w tej współpracy)

W ramach tego projektu ustalamy krótkie frazy sterujące:

- "policz ponownie" – uruchamiamy pełny pipeline obliczeń w R:
	1. `Rscript korelacje_tactile_optics.R` (wszystkie parametry z `config/params.yaml`)
	2. `Rscript ml_influence.R` (ML + zaawansowana walidacja)
	3. (opcjonalnie) render PDF jeśli wyraźnie dołożysz: "i wygeneruj raport".

- "wykonaj raport" – tylko renderowanie aktualnego `summary_report.Rmd` do PDF (bez ponownego liczenia danych) przy użyciu:
	- `bash report_only.sh` lub `make report-only`.

Jeżeli podasz jednocześnie: "policz ponownie i wykonaj raport" – najpierw liczymy cały pipeline, a potem render.

## Najczęstsze komendy

Pełny restart + raport:
```
make all
```
Tylko ponowne liczenie (bez PDF):
```
make correlations ml
```
Tylko raport (bez liczenia):
```
make report-only
```
Diagnostyka środowiska:
```
make diagnose
```

## Krótki FAQ
**PDF nie działa?** Sprawdź `make diagnose` – brak pandoc to najczęstsza przyczyna. Zainstaluj `pandoc`, upewnij się, że jest w PATH.


## 10. Rozszerzenia (opcjonalnie)
- Kompresja PDF (ghostscript)
- Eksport HTML fallback (bez LaTeX) – do dodania jeśli potrzebne
- Grupowanie parametrów w raporcie (np. podstawowe vs momenty) – na życzenie

---
Jeśli chcesz dodać fallback HTML lub kompresję – napisz. Raport tą samą ścieżką LaTeX zachowuje wcześniejszą strukturę (tytuł, spis treści, listę figur, ML, advanced, proste wyjaśnienie).
