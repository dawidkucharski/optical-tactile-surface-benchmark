#!/usr/bin/env Rscript
suppressWarnings(suppressMessages({
  library(stringr)
  library(readr)
  library(dplyr)
}))

out_dir <- "outputs"
plots_dir <- "plots"

params <- c("rp","rv","rz","ra","rt","rz1max")

# Find slug from existing files (accept both legacy and current naming):
#  - rp_abs_<slug>_slopes_by_system.csv (legacy)
#  - rp_abs_<slug>_slope_by_system_vs_nm.csv (current)
slopes_files <- list.files(out_dir, pattern = "^.*_abs_.*_(slopes_by_system|slope_by_system_vs_nm)\\.csv$", full.names = TRUE)
if (length(slopes_files) == 0) {
  stop("No slopes_by_system files found in outputs/. Run the main analysis first.")
}
extract_slug <- function(path) {
  fname <- basename(path)
  # patterns: <param>_abs_<slug>_(slopes_by_system|slope_by_system_vs_nm).csv
  m <- str_match(fname, "^([a-z0-9]+)_abs_(.+)_(slopes_by_system|slope_by_system_vs_nm)\\.csv$")
  if (is.na(m[1,3])) return(NA_character_)
  m[1,3]
}
slug <- extract_slug(slopes_files[1])
if (is.na(slug)) slug <- "material"

read_corr <- function(param) {
  # Prefer per-material file: <param>_abs_<slug>_corr_wavelength.txt
  f <- file.path(out_dir, sprintf("%s_abs_%s_corr_wavelength.txt", param, slug))
  if (!file.exists(f)) {
    # Fallback to legacy name without slug
    f <- file.path(out_dir, sprintf("%s_abs_corr_wavelength.txt", param))
    if (!file.exists(f)) return(tibble(param = param, pearson_r = NA_real_, spearman_r = NA_real_))
  }
  txt <- readLines(f, warn = FALSE)
  pr <- as.numeric(str_match(txt[grepl("Pearson r", txt)], "([-+]?[0-9]*\\.?[0-9]+)")[,2])
  sr <- as.numeric(str_match(txt[grepl("Spearman rho", txt)], "([-+]?[0-9]*\\.?[0-9]+)")[,2])
  tibble(param = param, pearson_r = pr[1], spearman_r = sr[1])
}

read_anova_eta2 <- function(param) {
  # Prefer per-material file: <param>_abs_<slug>_anova_effect_sizes.csv
  f <- file.path(out_dir, sprintf("%s_abs_%s_anova_effect_sizes.csv", param, slug))
  if (!file.exists(f)) {
    # Fallback to legacy name without slug
    f <- file.path(out_dir, sprintf("%s_abs_anova_effect_sizes.csv", param))
    if (!file.exists(f)) return(tibble(param = param, eta2_system = NA_real_, eta2_color = NA_real_))
  }
  df <- suppressMessages(readr::read_csv(f, show_col_types = FALSE))
  e_sys <- df$eta2[df$term == "system"]
  e_col <- df$eta2[df$term == "color"]
  tibble(param = param, eta2_system = e_sys %||% NA_real_, eta2_color = e_col %||% NA_real_)
}

read_slopes <- function(param) {
  # Prefer current filename (<param>_abs_<slug>_slope_by_system_vs_nm.csv), fallback to legacy
  f1 <- file.path(out_dir, sprintf("%s_abs_%s_slope_by_system_vs_nm.csv", param, slug))
  f2 <- file.path(out_dir, sprintf("%s_abs_%s_slopes_by_system.csv", param, slug))
  f <- if (file.exists(f1)) f1 else if (file.exists(f2)) f2 else return(tibble())
  df <- suppressMessages(readr::read_csv(f, show_col_types = FALSE))
  # Normalize column names to a standard schema
  nms <- names(df)
  df_std <- tibble(
    system = df[[which(nms == "system")]],
    n = if ("n" %in% nms) df[["n"]] else if ("n_points" %in% nms) df[["n_points"]] else NA_real_,
    slope = if ("slope" %in% nms) df[["slope"]] else if ("slope_per_nm" %in% nms) df[["slope_per_nm"]] else NA_real_,
    se = if ("se" %in% nms) df[["se"]] else if ("slope_se" %in% nms) df[["slope_se"]] else NA_real_,
    p = if ("p" %in% nms) df[["p"]] else if ("p_value" %in% nms) df[["p_value"]] else NA_real_,
    ci_lo = if ("ci_lo" %in% nms) df[["ci_lo"]] else if ("ci_low" %in% nms) df[["ci_low"]] else NA_real_,
    ci_hi = if ("ci_hi" %in% nms) df[["ci_hi"]] else if ("ci_high" %in% nms) df[["ci_high"]] else NA_real_
  )
  df_std$param <- param
  df_std <- df_std %>% select(param, system, n, slope, se, p, ci_lo, ci_hi) %>%
    mutate(sig = case_when(
      is.na(p) ~ NA_character_,
      p < 0.05 ~ "significant",
      p < 0.1 ~ "trend",
      TRUE ~ "ns"
    ))
  df_std
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a)) a else b

# Build overview
corrs <- bind_rows(lapply(params, read_corr))
etas  <- bind_rows(lapply(params, read_anova_eta2))
slopes <- bind_rows(lapply(params, read_slopes))

overview <- corrs %>% left_join(etas, by = "param")

# Summaries per param: any significant/trend systems
slopes_notes <- slopes %>%
  mutate(note = sprintf("%s: slope=%.3f (p=%.3f)", system, slope, p)) %>%
  group_by(param) %>%
  summarise(
    significant = paste(note[sig == "significant"], collapse = "; "),
    trends = paste(note[sig == "trend"], collapse = "; ")
  )

overview <- overview %>% left_join(slopes_notes, by = "param")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
readr::write_csv(overview, file.path(out_dir, "summary_overview.csv"))
if (nrow(slopes) > 0) readr::write_csv(slopes, file.path(out_dir, "summary_trends_by_system.csv"))

# Cross-material recommendations (aggregate across ALL materials)
# 1) Collect all slopes across materials per parameter
all_slope_files <- list.files(out_dir, pattern = "^([a-z0-9]+)_abs_([a-z0-9_-]+)_(slope_by_system_vs_nm|slopes_by_system)\\.csv$", full.names = TRUE)
slopes_all <- bind_rows(lapply(all_slope_files, function(path) {
  bn <- basename(path)
  m <- str_match(bn, "^([a-z0-9]+)_abs_([a-z0-9_-]+)_(slope_by_system_vs_nm|slopes_by_system)\\.csv$")
  if (is.na(m[1,2])) return(tibble())
  param <- m[1,2]
  df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  nms <- names(df)
  tibble(
    param = param,
    system = df[[which(nms == "system")]],
    slope = if ("slope" %in% nms) df[["slope"]] else if ("slope_per_nm" %in% nms) df[["slope_per_nm"]] else NA_real_,
    p = if ("p" %in% nms) df[["p"]] else if ("p_value" %in% nms) df[["p_value"]] else NA_real_
  )
}))

slopes_agg <- slopes_all %>%
  filter(!is.na(system), is.finite(slope)) %>%
  group_by(param, system) %>%
  summarise(n_mats = n(), mean_slope = mean(slope, na.rm = TRUE),
            sig_count = sum(is.finite(p) & p < 0.05),
            trend_count = sum(is.finite(p) & p < 0.1 & p >= 0.05),
            neg_share = mean(slope < 0, na.rm = TRUE), .groups = "drop")

# Pick slope-based recommended system per parameter: most negative mean_slope, break ties by significance and neg_share
slope_reco <- slopes_agg %>%
  group_by(param) %>%
  arrange(mean_slope, desc(sig_count), desc(trend_count), desc(neg_share)) %>%
  slice_head(n = 1) %>% ungroup() %>%
  rename(reco_system_by_slope = system, reco_mean_slope = mean_slope)

# 2) Aggregate effects_by_system across materials (choose lowest mean |diff|)
sys_files <- list.files(out_dir, pattern = "^([a-z0-9]+)_abs_([a-z0-9_-]+)_effects_by_system\\.csv$", full.names = TRUE)
sys_all <- bind_rows(lapply(sys_files, function(path) {
  bn <- basename(path)
  m <- str_match(bn, "^([a-z0-9]+)_abs_([a-z0-9_-]+)_effects_by_system\\.csv$")
  if (is.na(m[1,2])) return(tibble())
  param <- m[1,2]
  df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  # expect columns: system, n, mean, sd, se
  if (!all(c("system","mean") %in% names(df))) return(tibble())
  ncolvec <- if ("n" %in% names(df)) as.numeric(df[["n"]]) else rep(NA_real_, nrow(df))
  tibble(param = param, system = as.character(df[["system"]]), mean = as.numeric(df[["mean"]]), n = ncolvec)
}))
sys_agg <- sys_all %>% filter(is.finite(mean)) %>% group_by(param, system) %>%
  summarise(n_mats = n(), mean_of_means = mean(mean, na.rm = TRUE), .groups = "drop")
sys_reco <- sys_agg %>% group_by(param) %>% arrange(mean_of_means) %>% slice_head(n = 1) %>% ungroup() %>%
  rename(reco_system_by_level = system, reco_system_mean = mean_of_means)

# 3) Aggregate effects_by_color across materials (choose lowest mean |diff|)
col_files <- list.files(out_dir, pattern = "^([a-z0-9]+)_abs_([a-z0-9_-]+)_effects_by_color\\.csv$", full.names = TRUE)
col_all <- bind_rows(lapply(col_files, function(path) {
  bn <- basename(path)
  m <- str_match(bn, "^([a-z0-9]+)_abs_([a-z0-9_-]+)_effects_by_color\\.csv$")
  if (is.na(m[1,2])) return(tibble())
  param <- m[1,2]
  df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  if (!all(c("color","mean") %in% names(df))) return(tibble())
  ncolvec <- if ("n" %in% names(df)) as.numeric(df[["n"]]) else rep(NA_real_, nrow(df))
  tibble(param = param, color = as.character(df[["color"]]), mean = as.numeric(df[["mean"]]), n = ncolvec)
}))
col_agg <- col_all %>% filter(is.finite(mean)) %>% group_by(param, color) %>%
  summarise(n_mats = n(), mean_of_means = mean(mean, na.rm = TRUE), .groups = "drop")
col_reco <- col_agg %>% group_by(param) %>% arrange(mean_of_means) %>% slice_head(n = 1) %>% ungroup() %>%
  rename(reco_color = color, reco_color_mean = mean_of_means)

# 4) Merge recommendations
reco <- params %>%
  tibble(param = .) %>%
  left_join(sys_reco, by = "param") %>%
  left_join(col_reco, by = "param") %>%
  left_join(slope_reco, by = "param") %>%
  mutate(param = toupper(param))

if (nrow(reco) > 0) {
  readr::write_csv(reco, file.path(out_dir, "recommendations_overall.csv"))
}

# Markdown report
md <- c(
  sprintf("# Summary report (%s)\n", slug),
  "## What the plots show\n",
  "- optics_vs_tactile: porównanie |diff| [%] względem tactile, na układach i kolorach.",
  "- effects_by_system: średnie |diff| [%] i 95% CI dla każdego systemu.",
  "- effects_by_color: średnie |diff| [%] i 95% CI dla każdego koloru.",
  "- diff_vs_wavelength: rozrzut |diff| [%] vs długość fali z linią trendu.",
  "- slopes_by_system: nachylenia (|diff| [%] na jednostkę wavelength) z 95% CI i p.",
  "- tech_distance_heatmap: podobieństwo systemów (średni |diff|% między technikami).\n",
  "## Cross-parameter highlights\n"
)

add_line <- function(...) md <<- c(md, sprintf(...))

for (i in seq_len(nrow(overview))) {
  r <- overview[i,]
  add_line("- %s: r=%.3f (Pearson), rho=%.3f (Spearman); eta2 system=%.3f, color=%.3f.",
           toupper(r$param), r$pearson_r %||% NaN, r$spearman_r %||% NaN, r$eta2_system %||% NaN, r$eta2_color %||% NaN)
  if (!is.na(r$significant) && nzchar(r$significant)) add_line("  - istotne nachylenia: %s", r$significant)
  if (!is.na(r$trends) && nzchar(r$trends)) add_line("  - trendy (p<0.1): %s", r$trends)
}

# Append compact recommendations (overall)
if (exists("reco") && nrow(reco) > 0) {
  add_line("\n## Rekomendacje (przegląd zbiorczy)")
  for (i in seq_len(nrow(reco))) {
    rr <- reco[i,]
    add_line("- %s: system (średni |diff|) = %s; kolor = %s; trend wavelength (slope) najlepszy w systemie = %s (średnie nachylenie=%.4f; istotne=%d; trendy=%d).",
             rr$param, rr$reco_system_by_level %||% "NA", rr$reco_color %||% "NA", rr$reco_system_by_slope %||% "NA",
             rr$reco_mean_slope %||% NaN, rr$sig_count %||% 0, rr$trend_count %||% 0)
  }
}

md_path <- file.path(out_dir, "summary_report.md")
writeLines(md, md_path)
cat(sprintf("Written %s, %s, %s\n", md_path,
            file.path(out_dir, "summary_overview.csv"),
            if (nrow(slopes) > 0) file.path(out_dir, "summary_trends_by_system.csv") else ""))
