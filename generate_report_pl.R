# Generates a Polish HTML report aggregating results for all materials and parameters
# Inputs: expects outputs/ and plots/ folders populated by korelacje_tactile_optics.R
# Output: raport_optyka_vs_taktylne_PL.html in the workspace root

suppressPackageStartupMessages({
  library(tools)
  library(jsonlite)
})

OUT_DIR <- 'outputs'
PLOTS_DIR <- 'plots'
PARAMS <- c('Rp','Rv','Rz','Ra','Rt','Rz1max')
METRIC_SUFFIX <- '_abs'  # default run uses absolute percent diff labels

# Discover all comparison CSVs
files <- list.files(OUT_DIR, pattern = '_optics_vs_tactile[.]csv$', full.names = TRUE)
if (!length(files)) {
  cat('Brak plików wynikowych w katalogu outputs/. Uruchom najpierw korelacje_tactile_optics.R.\n')
  quit(status = 1)
}

# Extract MAT_SLUG and PARAM from filenames
parse_info <- function(path) {
  bn <- basename(path)
  # patterns like: rp_abs_<mat>_optics_vs_tactile.csv or rp_<mat>_optics_vs_tactile.csv
  m <- regexec('^([a-z0-9]+)(_abs)?_([a-z0-9_\-]+)_optics_vs_tactile[.]csv$', bn)
  r <- regmatches(bn, m)[[1]]
  if (length(r) >= 4) {
    list(param = toupper(r[2]), abs = nzchar(r[3]), mat = r[4], file = path)
  } else NULL
}
infos <- Filter(Negate(is.null), lapply(files, parse_info))
if (!length(infos)) {
  cat('Nie udało się rozpoznać nazw plików w outputs/.\n')
  quit(status = 2)
}

# Group by material
by_mat <- split(infos, vapply(infos, function(x) x$mat, character(1)))

# Helpers
safe_read_csv <- function(path) {
  tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
}
safe_read_lines <- function(path) {
  tryCatch(readLines(path, warn = FALSE), error = function(e) character(0))
}

html_escape <- function(x) {
  x <- gsub('&', '&amp;', x, fixed = TRUE)
  x <- gsub('<', '&lt;', x, fixed = TRUE)
  x <- gsub('>', '&gt;', x, fixed = TRUE)
  x
}

# Build HTML
out_html <- 'raport_optyka_vs_taktylne_PL.html'
con <- file(out_html, open = 'w', encoding = 'UTF-8')
writeLines('<!DOCTYPE html>\n<html lang="pl">\n<head>\n<meta charset="UTF-8">\n<title>Raport: optyka vs taktylne</title>\n<style> body{font-family: -apple-system, Segoe UI, Roboto, Arial, sans-serif; margin: 20px;} h1{margin-top:0} h2{border-bottom:1px solid #ddd; padding-bottom:4px} table{border-collapse: collapse; margin: 12px 0;} th,td{border:1px solid #ccc; padding:6px 8px;} .small{font-size: 90%; color:#555} .plots a{display:inline-block; margin:4px 8px 8px 0;} code{background:#f5f5f5; padding:1px 4px; border-radius:4px} </style>\n</head>\n<body>', con)
writeLines(sprintf('<h1>Raport: porównanie pomiarów optycznych z referencją taktylną</h1>\n<p class="small">Data generacji: %s</p>', format(Sys.time(), '%Y-%m-%d %H:%M')), con)

# Table of contents
writeLines('<h2>Spis treści</h2><ul>', con)
for (mat in names(by_mat)) {
  writeLines(sprintf('<li><a href="#%s">Materiał/proces: %s</a></li>', html_escape(mat), html_escape(mat)), con)
}
writeLines('</ul>', con)

# Per material sections
for (mat in sort(names(by_mat))) {
  writeLines(sprintf('<h2 id="%s">Materiał/proces: %s</h2>', html_escape(mat), html_escape(mat)), con)
  # Ordered params
  items <- by_mat[[mat]]
  # Keep order as PARAMS
  items <- items[order(match(vapply(items, function(x) x$param, character(1)), PARAMS))]

  for (info in items) {
    param <- info$param
    comp_csv <- info$file
    comp <- safe_read_csv(comp_csv)
    if (is.null(comp) || nrow(comp) == 0) next
    # Use first row to fetch tactile meta
    t_ref <- comp$tactile_reference[1]
    t_lab <- if ('tactile_col_label' %in% names(comp)) comp$tactile_col_label[1] else NA
    t_n   <- if ('tactile_n' %in% names(comp)) comp$tactile_n[1] else NA
    t_sd  <- if ('tactile_sd' %in% names(comp)) comp$tactile_sd[1] else NA
    t_se  <- if ('tactile_se' %in% names(comp)) comp$tactile_se[1] else NA

    writeLines(sprintf('<h3>%s</h3>', html_escape(param)), con)
    writeLines('<p>', con)
    writeLines(sprintf('Odniesienie taktylne (średnia): <b>%s</b> [µm]', if (is.finite(t_ref)) sprintf('%.6f', t_ref) else 'NA'), con)
    if (is.finite(t_n))   writeLines(sprintf('<br/>Liczba pomiarów: %s', t_n), con)
    if (is.finite(t_sd))  writeLines(sprintf('<br/>Odchylenie standardowe: %s', sprintf('%.6f', t_sd)), con)
    if (is.finite(t_se))  writeLines(sprintf('<br/>Błąd standardowy (SE): %s', sprintf('%.6f', t_se)), con)
    if (!is.na(t_lab) && nzchar(t_lab)) writeLines(sprintf('<br/>Kolumna w arkuszu taktylnym: <code>%s</code>', html_escape(as.character(t_lab))), con)
    writeLines('</p>', con)

    # Links to key plots
    base <- tolower(param)
    prefix <- paste0(base, METRIC_SUFFIX, '_', mat, '_')
    plot_files <- c( paste0(prefix, 'optics_vs_tactile.pdf'),
                     paste0(prefix, 'effects_by_system.pdf'),
                     paste0(prefix, 'effects_by_color.pdf'),
                     paste0(prefix, 'diff_vs_wavelength.pdf'),
                     paste0(prefix, 'slope_by_system_vs_nm.pdf'),
                     paste0(prefix, 'tech_distance_heatmap.pdf') )
    plot_paths <- file.path(PLOTS_DIR, plot_files)
    writeLines('<div class="plots">', con)
    for (i in seq_along(plot_paths)) {
      if (file.exists(plot_paths[i])) {
        writeLines(sprintf('<a href="%s" target="_blank">%s</a>', html_escape(plot_paths[i]), html_escape(plot_files[i])), con)
      }
    }
    writeLines('</div>', con)

    # Include slope table if exists
    slope_csv <- file.path(OUT_DIR, paste0(prefix, 'slope_by_system_vs_nm.csv'))
    if (file.exists(slope_csv)) {
      slope <- safe_read_csv(slope_csv)
      if (!is.null(slope) && nrow(slope)) {
        writeLines('<details><summary>Nachylenie (zmiana na nm) wg systemu</summary>', con)
        writeLines('<table><thead><tr>', con)
        for (nm in names(slope)) writeLines(sprintf('<th>%s</th>', html_escape(nm)), con)
        writeLines('</tr></thead><tbody>', con)
        for (r in seq_len(nrow(slope))) {
          writeLines('<tr>', con)
          for (nm in names(slope)) writeLines(sprintf('<td>%s</td>', html_escape(as.character(slope[r, nm]))), con)
          writeLines('</tr>', con)
        }
        writeLines('</tbody></table></details>', con)
      }
    }

    # Include ANOVA effects if exists
    eff_csv <- file.path(OUT_DIR, paste0(prefix, 'anova_effect_sizes.csv'))
    if (file.exists(eff_csv)) {
      eff <- safe_read_csv(eff_csv)
      if (!is.null(eff) && nrow(eff)) {
        writeLines('<details><summary>Udziały efektów (eta²) z ANOVA</summary>', con)
        writeLines('<table><thead><tr>', con)
        for (nm in names(eff)) writeLines(sprintf('<th>%s</th>', html_escape(nm)), con)
        writeLines('</tr></thead><tbody>', con)
        for (r in seq_len(nrow(eff))) {
          writeLines('<tr>', con)
          for (nm in names(eff)) writeLines(sprintf('<td>%s</td>', html_escape(as.character(eff[r, nm]))), con)
          writeLines('</tr>', con)
        }
        writeLines('</tbody></table></details>', con)
      }
    }

    # Include correlations text if exists
    corr_txt <- file.path(OUT_DIR, paste0(prefix, 'corr_wavelength.txt'))
    if (file.exists(corr_txt)) {
      lines <- safe_read_lines(corr_txt)
      if (length(lines)) {
        writeLines('<details><summary>Korelacja z długością fali</summary><pre>', con)
        writeLines(html_escape(paste(lines, collapse = '\n')), con)
        writeLines('</pre></details>', con)
      }
    }
  }
}

writeLines('</body></html>', con)
close(con)

cat('Zapisano raport: ', out_html, '\n', sep = '')
