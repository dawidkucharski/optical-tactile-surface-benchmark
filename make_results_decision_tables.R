#!/usr/bin/env Rscript
suppressWarnings(suppressMessages({
  library(stringr)
  library(readr)
  library(dplyr)
  library(ggplot2)
}))

out_dir <- "outputs"
tex_dir <- file.path("paper", "tables")
plots_dir <- "plots"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tex_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

params_keep <- c("ra","rq","rsk","rsm","rt","rv","rz","rz1max")
primary_params <- setdiff(params_keep, "rsk")

to_tex <- function(x) {
  x <- as.character(x)
  # Escape characters that are special in LaTeX.
  x <- gsub("_", "\\_", x, fixed = TRUE)
  x
}

param_tex <- function(p) {
  p <- tolower(p)
  switch(p,
    ra = "$R_a$",
    rq = "$R_q$",
    rsk = "$R_{sk}$",
    rsm = "$R_{sm}$",
    rt = "$R_t$",
    rv = "$R_v$",
    rz = "$R_z$",
    rz1max = "$R_{z1max}$",
    p
  )
}

process_label_tex <- function(p) {
  p <- tolower(as.character(p))
  switch(p,
    mr = "MR (rough milling)",
    mf = "MF (finish milling)",
    tr = "TR (rough face turning)",
    tf = "TF (finish face turning)",
    b = "B (burnishing after MF)",
    wedm_r = "WEDM\\_R (rough wire EDM)",
    wedm_f = "WEDM\\_F (finish wire EDM)",
    gri = "GRI (grinding)",
    gla = "GLA (glass bead blasting)",
    hon = "HON (honing)",
    to_tex(p)
  )
}

process_label_plain <- function(p) {
  p <- tolower(as.character(p))
  switch(p,
    mr = "MR: rough milling",
    mf = "MF: finish milling",
    tr = "TR: rough face turning",
    tf = "TF: finish face turning",
    b = "B: burnishing after MF",
    wedm_r = "WEDM_R: rough wire EDM",
    wedm_f = "WEDM_F: finish wire EDM",
    gri = "GRI: grinding",
    gla = "GLA: glass bead blasting",
    hon = "HON: honing",
    as.character(p)
  )
}

param_label_plain <- function(p) {
  p <- tolower(as.character(p))
  switch(p,
    ra = "Ra",
    rq = "Rq",
    rsk = "Rsk",
    rsm = "Rsm",
    rt = "Rt",
    rv = "Rv",
    rz = "Rz",
    rz1max = "Rz1max",
    as.character(p)
  )
}

material_label_tex <- function(m) {
  m <- tolower(as.character(m))
  switch(m,
    c45 = "C45 steel",
    "14301" = "AISI 304 / EN 1.4301 (14301)",
    ti6al4v = "Ti6Al4V",
    al7075 = "Al7075",
    mo58a = "MO58A brass",
    ellor = "Graphite (ELLOR)",
    to_tex(m)
  )
}

cat_label <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x <= 10 ~ "<10\\%",
    x <= 30 ~ "10--30\\%",
    x <= 100 ~ ">30\\%",
    TRUE ~ ">100\\%"
  )
}

mode_chr <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  tbl <- sort(table(x), decreasing = TRUE)
  names(tbl)[1]
}

# -------------------------
# 1) Decision matrix from *_optics_vs_tactile.csv
# -------------------------
opt_files <- list.files(out_dir, pattern = "_optics_vs_tactile\\.csv$", full.names = TRUE)
summarise_optics_file <- function(path) {
  bn <- basename(path)
  m <- str_match(bn, "^([a-z0-9]+)_abs_([^_]+)_(.+)_optics_vs_tactile\\.csv$")
  if (is.na(m[1,2])) return(tibble())
  param <- m[1,2]
  material <- m[1,3]
  process <- m[1,4]
  if (!(param %in% params_keep)) return(tibble())

  df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  need_cols <- c("system","color","diff_percent_abs")
  if (!all(need_cols %in% names(df))) return(tibble())

  df_sum <- df %>%
    mutate(
      system = as.character(system),
      color = as.character(color),
      diff_percent_abs = as.numeric(diff_percent_abs)
    ) %>%
    filter(is.finite(diff_percent_abs)) %>%
    group_by(system, color) %>%
    summarise(
      n = n(),
      median_abs_diff_pct = median(diff_percent_abs, na.rm = TRUE),
      mean_abs_diff_pct = mean(diff_percent_abs, na.rm = TRUE),
      q25_abs_diff_pct = quantile(diff_percent_abs, 0.25, na.rm = TRUE),
      q75_abs_diff_pct = quantile(diff_percent_abs, 0.75, na.rm = TRUE),
      .groups = "drop"
    )

  if (nrow(df_sum) == 0) return(tibble())

  df_sum %>%
    mutate(
      param = param,
      material = material,
      process = process,
      .before = 1
    )
}

parse_optics_file <- function(path) {
  df_sum <- summarise_optics_file(path)
  if (nrow(df_sum) == 0) return(tibble())

  best <- df_sum %>%
    arrange(median_abs_diff_pct, mean_abs_diff_pct, desc(n)) %>%
    slice_head(n = 1)

  tibble(
    param = best$param,
    material = best$material,
    process = best$process,
    best_system = best$system,
    best_color = best$color,
    best_n = best$n,
    best_median_abs_diff_pct = best$median_abs_diff_pct,
    best_q25_abs_diff_pct = best$q25_abs_diff_pct,
    best_q75_abs_diff_pct = best$q75_abs_diff_pct,
    best_mean_abs_diff_pct = best$mean_abs_diff_pct,
    best_category = cat_label(best$median_abs_diff_pct)
  )
}

config_medians <- bind_rows(lapply(opt_files, summarise_optics_file))
if (nrow(config_medians) == 0) {
  stop("No usable system+colour medians found for manuscript sensitivity summaries.")
}

config_medians <- config_medians %>%
  mutate(
    param = tolower(param),
    material = tolower(material),
    process = tolower(process)
  )

write_csv(config_medians, file.path(out_dir, "results_config_medians_by_group.csv"))

decision <- bind_rows(lapply(opt_files, parse_optics_file))
if (nrow(decision) == 0) {
  stop("No usable *_optics_vs_tactile.csv files found for the manuscript parameter set.")
}

decision <- decision %>%
  mutate(
    param = tolower(param),
    material = tolower(material),
    process = tolower(process)
  )

write_csv(decision, file.path(out_dir, "results_decision_best_optical_by_group.csv"))

decision_primary <- decision %>%
  filter(param %in% primary_params)

if (nrow(decision_primary) == 0) {
  stop("No usable strictly positive-parameter groups found for manuscript decision summaries.")
}

# -------------------------
# 1b) Fixed-configuration sensitivity against retrospective best-achievable selection
# -------------------------
primary_group_count <- nrow(decision_primary)

config_primary <- config_medians %>%
  filter(param %in% primary_params) %>%
  left_join(
    decision_primary %>%
      select(param, material, process, best_median_abs_diff_pct),
    by = c("param", "material", "process")
  ) %>%
  mutate(
    excess_over_best_pct = median_abs_diff_pct - best_median_abs_diff_pct,
    fixed_category = cat_label(median_abs_diff_pct)
  )

fixed_config_summary <- config_primary %>%
  group_by(system, color) %>%
  summarise(
    n_groups = n(),
    coverage_pct = 100 * n_groups / primary_group_count,
    median_fixed = median(median_abs_diff_pct, na.rm = TRUE),
    q25_fixed = quantile(median_abs_diff_pct, 0.25, na.rm = TRUE),
    q75_fixed = quantile(median_abs_diff_pct, 0.75, na.rm = TRUE),
    good_n = sum(median_abs_diff_pct <= 10, na.rm = TRUE),
    mid_n = sum(median_abs_diff_pct > 10 & median_abs_diff_pct <= 30, na.rm = TRUE),
    poor_n = sum(median_abs_diff_pct > 30, na.rm = TRUE),
    median_excess_over_best = median(excess_over_best_pct, na.rm = TRUE),
    q75_excess_over_best = quantile(excess_over_best_pct, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    good_pct = 100 * good_n / n_groups,
    mid_pct = 100 * mid_n / n_groups,
    poor_pct = 100 * poor_n / n_groups,
    workflow = paste(system, color, sep = ", ")
  ) %>%
  arrange(median_fixed, desc(coverage_pct))

write_csv(config_primary, file.path(out_dir, "results_fixed_workflow_group_medians.csv"))
write_csv(fixed_config_summary, file.path(out_dir, "results_fixed_workflow_sensitivity.csv"))

# -------------------------
# 2) Summary tables (param / process / material)
# -------------------------
by_param <- decision_primary %>%
  group_by(param) %>%
  summarise(
    n_groups = n(),
    median_best = median(best_median_abs_diff_pct, na.rm = TRUE),
    q25_best = quantile(best_median_abs_diff_pct, 0.25, na.rm = TRUE),
    q75_best = quantile(best_median_abs_diff_pct, 0.75, na.rm = TRUE),
    good_n = sum(best_median_abs_diff_pct <= 10, na.rm = TRUE),
    mid_n = sum(best_median_abs_diff_pct > 10 & best_median_abs_diff_pct <= 30, na.rm = TRUE),
    poor_n = sum(best_median_abs_diff_pct > 30, na.rm = TRUE),
    best_system_mode = mode_chr(best_system),
    best_color_mode = mode_chr(best_color),
    .groups = "drop"
  ) %>%
  mutate(
    good_pct = 100 * good_n / n_groups,
    mid_pct = 100 * mid_n / n_groups,
    poor_pct = 100 * poor_n / n_groups
  )

by_process <- decision_primary %>%
  group_by(process) %>%
  summarise(
    n_groups = n(),
    good_n = sum(best_median_abs_diff_pct <= 10, na.rm = TRUE),
    mid_n = sum(best_median_abs_diff_pct > 10 & best_median_abs_diff_pct <= 30, na.rm = TRUE),
    poor_n = sum(best_median_abs_diff_pct > 30, na.rm = TRUE),
    median_best = median(best_median_abs_diff_pct, na.rm = TRUE),
    q25_best = quantile(best_median_abs_diff_pct, 0.25, na.rm = TRUE),
    q75_best = quantile(best_median_abs_diff_pct, 0.75, na.rm = TRUE),
    best_system_mode = mode_chr(best_system),
    best_color_mode = mode_chr(best_color),
    .groups = "drop"
  ) %>%
  mutate(
    good_pct = 100 * good_n / n_groups,
    mid_pct = 100 * mid_n / n_groups,
    poor_pct = 100 * poor_n / n_groups
  ) %>%
  arrange(desc(good_pct), median_best)

by_material <- decision_primary %>%
  group_by(material) %>%
  summarise(
    n_groups = n(),
    good_n = sum(best_median_abs_diff_pct <= 10, na.rm = TRUE),
    mid_n = sum(best_median_abs_diff_pct > 10 & best_median_abs_diff_pct <= 30, na.rm = TRUE),
    poor_n = sum(best_median_abs_diff_pct > 30, na.rm = TRUE),
    median_best = median(best_median_abs_diff_pct, na.rm = TRUE),
    q25_best = quantile(best_median_abs_diff_pct, 0.25, na.rm = TRUE),
    q75_best = quantile(best_median_abs_diff_pct, 0.75, na.rm = TRUE),
    best_system_mode = mode_chr(best_system),
    best_color_mode = mode_chr(best_color),
    .groups = "drop"
  ) %>%
  mutate(
    good_pct = 100 * good_n / n_groups,
    mid_pct = 100 * mid_n / n_groups,
    poor_pct = 100 * poor_n / n_groups
  ) %>%
  arrange(desc(good_pct), median_best)

write_csv(by_param, file.path(out_dir, "results_decision_summary_by_param.csv"))
write_csv(by_process, file.path(out_dir, "results_decision_summary_by_process.csv"))
write_csv(by_material, file.path(out_dir, "results_decision_summary_by_material.csv"))
write_csv(
  decision %>% filter(param == "rsk"),
  file.path(out_dir, "results_decision_rsk_screening_groups.csv")
)

by_process_param <- decision_primary %>%
  group_by(process, param) %>%
  summarise(
    n_groups = n(),
    median_best = median(best_median_abs_diff_pct, na.rm = TRUE),
    good_pct = 100 * sum(best_median_abs_diff_pct <= 10, na.rm = TRUE) / n_groups,
    .groups = "drop"
  ) %>%
  mutate(
    process_label = vapply(process, process_label_plain, character(1)),
    param_label = vapply(param, param_label_plain, character(1)),
    median_best_capped = pmin(median_best, 100),
    heatmap_label = ifelse(median_best >= 99.5, "100", sprintf("%.1f", median_best))
  )

write_csv(by_process_param, file.path(out_dir, "results_best_discrepancy_heatmap_by_process_param.csv"))

# -------------------------
# 3) LaTeX table writers
# -------------------------
write_table_by_param <- function(df, path) {
  lines <- c(
    "% Auto-generated by make_results_decision_tables.R",
    "\\begin{table}[H]",
    "\\centering",
    "\\small",
    "\\caption{Decision-oriented summary of optical--tactile agreement by strictly positive roughness parameter (best system+colour chosen per material--process group)}",
    "\\label{tab:decision_by_param}",
    "\\setlength\\tabcolsep{4pt}",
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{lccccccc}",
    "\\toprule",
    "Parameter & $N$ & Median best $|\\mathrm{diff}|$ [\\%] & Q1--Q3 & $\\leq 10\\%$ & 10--30\\% & >30\\% & Most frequent best (system, colour) \\\\",
    "\\midrule"
  )

  df <- df %>% mutate(param_label = vapply(param, param_tex, character(1)))
  df <- df %>% arrange(match(param, params_keep))

  for (i in seq_len(nrow(df))) {
    r <- df[i,]
    med <- sprintf("%.1f", r$median_best)
    iqr <- sprintf("%.1f--%.1f", r$q25_best, r$q75_best)
    good <- sprintf("%d (%.0f\\%%)", r$good_n, r$good_pct)
    mid <- sprintf("%d (%.0f\\%%)", r$mid_n, r$mid_pct)
    poor <- sprintf("%d (%.0f\\%%)", r$poor_n, r$poor_pct)
    best_pair <- sprintf("%s, %s", to_tex(r$best_system_mode), to_tex(r$best_color_mode))
    lines <- c(lines, sprintf("%s & %d & %s & %s & %s & %s & %s & %s \\\\",
                              r$param_label, r$n_groups, med, iqr, good, mid, poor, best_pair))
  }

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    "}",
    "\\vspace{0.5ex}",
    "\\begin{minipage}{\\linewidth}",
    "\\footnotesize\\textit{Note:} $N$ is the number of available material--process groups for the seven strictly positive parameters. For each group, the optical configuration (system+illumination colour) that minimises the median $|\\mathrm{diff}|(\\%)$ is selected. Q1--Q3 are quartiles of the selected medians. The band columns ($\\leq 10\\%$, 10--30\\%, >30\\%) count how many groups fall into each range; values are shown as count (percent of $N$), e.g. 7 (15\\%) means 7 groups, i.e. 15\\% of $N$. Most frequent best denotes the modal selected (system, colour) across groups. Percentage-based $R_{sk}$ rankings are excluded from the manuscript decision tables because normalisation becomes unstable near zero; archive-level screening rows are retained separately in the exported outputs.",
    "\\end{minipage}",
    "\\end{table}"
  )
  writeLines(lines, path)
}

write_table_generic <- function(df, path, caption, label, first_col_name, label_formatter = to_tex) {
  lines <- c(
    "% Auto-generated by make_results_decision_tables.R",
    "\\begin{table}[H]",
    "\\centering",
    "\\small",
    paste0("\\caption{", caption, "}"),
    sprintf("\\label{%s}", label),
    "\\setlength\\tabcolsep{4pt}",
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{lccccccc}",
    "\\toprule",
    paste0(first_col_name, " & $N$ & Median best $|\\mathrm{diff}|$ [\\%] & Q1--Q3 & $\\leq 10\\%$ & 10--30\\% & >30\\% & Most frequent best (system, colour) \\\\") ,
    "\\midrule"
  )

  for (i in seq_len(nrow(df))) {
    r <- df[i,]
    med <- sprintf("%.1f", r$median_best)
    iqr <- sprintf("%.1f--%.1f", r$q25_best, r$q75_best)
    good <- sprintf("%d (%.0f\\%%)", r$good_n, r$good_pct)
    mid <- sprintf("%d (%.0f\\%%)", r$mid_n, r$mid_pct)
    poor <- sprintf("%d (%.0f\\%%)", r$poor_n, r$poor_pct)
    best_pair <- sprintf("%s, %s", to_tex(r$best_system_mode), to_tex(r$best_color_mode))
    lines <- c(lines, sprintf("%s & %d & %s & %s & %s & %s & %s & %s \\\\",
                              label_formatter(r[[1]]), r$n_groups, med, iqr, good, mid, poor, best_pair))
  }

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    "}",
    "\\vspace{0.5ex}",
    "\\begin{minipage}{\\linewidth}",
    "\\footnotesize\\textit{Note:} $N$ is the number of contributing strictly positive parameter--material--process groups. For each group, the optical configuration (system+illumination colour) that minimises the median $|\\mathrm{diff}|(\\%)$ is selected. Q1--Q3 are quartiles of the selected medians. The band columns ($\\leq 10\\%$, 10--30\\%, >30\\%) count how many groups fall into each range; values are shown as count (percent of $N$), e.g. 7 (15\\%) means 7 groups, i.e. 15\\% of $N$. Most frequent best denotes the modal selected (system, colour) across groups. Archive identifiers in the first column are expanded for readability.",
    "\\end{minipage}",
    "\\end{table}"
  )
  writeLines(lines, path)
}

write_table_fixed_workflows <- function(df, path, primary_group_count) {
  df <- df %>% slice_head(n = min(8, nrow(df)))
  table_row_end <- "\\\\"
  lines <- c(
    "% Auto-generated by make_results_decision_tables.R",
    "\\begin{table}[H]",
    "\\centering",
    "\\small",
    "\\caption{Fixed-configuration sensitivity analysis for the strictly positive roughness parameters}",
    "\\label{tab:fixed_workflow_sensitivity}",
    "\\setlength\\tabcolsep{4pt}",
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{lccccccc}",
    "\\toprule",
    paste0("Fixed workflow & $N$ & Coverage & Median $|\\mathrm{diff}|$ [\\%] & ",
           "Q1--Q3 & $\\leq 10\\%$ & >30\\% & Median excess vs. best [\\%] ",
           table_row_end),
    "\\midrule"
  )

  for (i in seq_len(nrow(df))) {
    r <- df[i,]
    workflow <- sprintf("%s, %s", to_tex(r$system), to_tex(r$color))
    coverage <- sprintf("%.0f\\%%", r$coverage_pct)
    med <- sprintf("%.1f", r$median_fixed)
    iqr <- sprintf("%.1f--%.1f", r$q25_fixed, r$q75_fixed)
    good <- sprintf("%d (%.0f\\%%)", r$good_n, r$good_pct)
    poor <- sprintf("%d (%.0f\\%%)", r$poor_n, r$poor_pct)
    excess <- sprintf("%.1f", r$median_excess_over_best)
    lines <- c(lines, sprintf("%s & %d & %s & %s & %s & %s & %s & %s %s",
            workflow, r$n_groups, coverage, med, iqr, good, poor, excess, table_row_end))
  }

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    "}",
    "\\vspace{0.5ex}",
    "\\begin{minipage}{\\linewidth}",
    sprintf("\\footnotesize\\textit{Note:} Fixed workflow means that one optical system+illumination pair is evaluated wherever it is available, without selecting a different best configuration for each parameter--surface group. Coverage is relative to the %d strictly positive parameter--surface groups used in the manuscript decision summaries. The table shows the eight lowest median fixed workflows; the complete fixed-workflow ranking and group-level medians are exported as CSV files. Excess vs. best is the median difference between the fixed-workflow median discrepancy and the retrospective best-achievable median discrepancy within the same group.", primary_group_count),
    "\\end{minipage}",
    "\\end{table}"
  )
  writeLines(lines, path)
}

write_global_heatmap <- function(df, path, process_order) {
  df <- df %>%
    mutate(
      param_label = factor(param_label, levels = vapply(primary_params, param_label_plain, character(1))),
      process_label = factor(process_label, levels = rev(vapply(process_order, process_label_plain, character(1))))
    )

  p <- ggplot(df, aes(x = param_label, y = process_label, fill = median_best_capped)) +
    geom_tile(color = "white", linewidth = 0.35) +
    geom_text(aes(label = heatmap_label), size = 3.0, color = "black") +
    scale_fill_gradientn(
      colours = c("#2c7bb6", "#ffffbf", "#d7191c"),
      limits = c(0, 100),
      name = "Median best\n|diff| (%)"
    ) +
    labs(
      x = "Roughness parameter",
      y = "Surface-generation process",
      title = "Best-achievable optical--tactile discrepancy by process and parameter"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.title = element_text(face = "bold", size = 11),
      legend.position = "right"
    )

  ggsave(path, plot = p, width = 9.2, height = 5.6, units = "in")
}

write_table_by_param(by_param, file.path(tex_dir, "results_decision_by_param.tex"))

write_table_generic(
  by_process,
  file.path(tex_dir, "results_decision_by_process.tex"),
  caption = "Decision summary aggregated by surface-generation process across all materials and strictly positive parameters",
  label = "tab:decision_by_process",
  first_col_name = "Process",
  label_formatter = process_label_tex
)

write_table_generic(
  by_material,
  file.path(tex_dir, "results_decision_by_material.tex"),
  caption = "Decision summary aggregated by material across all processes and strictly positive parameters",
  label = "tab:decision_by_material",
  first_col_name = "Material",
  label_formatter = material_label_tex
)

write_table_fixed_workflows(
  fixed_config_summary,
  file.path(tex_dir, "results_fixed_workflow_sensitivity.tex"),
  primary_group_count = primary_group_count
)

write_global_heatmap(
  by_process_param,
  file.path(plots_dir, "global_best_discrepancy_heatmap.pdf"),
  process_order = by_process$process
)

cat("Wrote:\n")
cat("- ", file.path(out_dir, "results_decision_best_optical_by_group.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "results_config_medians_by_group.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "results_decision_summary_by_param.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "results_decision_rsk_screening_groups.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "results_fixed_workflow_sensitivity.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "results_best_discrepancy_heatmap_by_process_param.csv"), "\n", sep = "")
cat("- ", file.path(tex_dir, "results_decision_by_param.tex"), "\n", sep = "")
cat("- ", file.path(tex_dir, "results_decision_by_process.tex"), "\n", sep = "")
cat("- ", file.path(tex_dir, "results_decision_by_material.tex"), "\n", sep = "")
cat("- ", file.path(tex_dir, "results_fixed_workflow_sensitivity.tex"), "\n", sep = "")
cat("- ", file.path(plots_dir, "global_best_discrepancy_heatmap.pdf"), "\n", sep = "")
