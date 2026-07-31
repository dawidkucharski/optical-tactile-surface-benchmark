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

bootstrap_median_ci <- function(values, n_boot = 2000L, conf = 0.95) {
  values <- values[is.finite(values)]
  n <- length(values)
  if (n == 0) {
    return(tibble(
      n = 0L,
      median = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      n_boot = n_boot,
      conf_level = conf
    ))
  }
  if (n == 1) {
    return(tibble(
      n = 1L,
      median = values[1],
      ci_low = values[1],
      ci_high = values[1],
      n_boot = n_boot,
      conf_level = conf
    ))
  }

  boot_medians <- replicate(n_boot, median(sample(values, size = n, replace = TRUE), na.rm = TRUE))
  alpha <- (1 - conf) / 2
  tibble(
    n = n,
    median = median(values, na.rm = TRUE),
    ci_low = quantile(boot_medians, alpha, na.rm = TRUE, names = FALSE),
    ci_high = quantile(boot_medians, 1 - alpha, na.rm = TRUE, names = FALSE),
    n_boot = n_boot,
    conf_level = conf
  )
}

summarise_bootstrap_ci <- function(df, scope, group_col, label_fun, order_values = NULL) {
  group_col <- rlang::ensym(group_col)
  out <- df %>%
    group_by(!!group_col) %>%
    summarise(
      bootstrap_median_ci(best_median_abs_diff_pct),
      .groups = "drop"
    ) %>%
    rename(group = !!group_col) %>%
    mutate(
      scope = scope,
      group_label = vapply(group, label_fun, character(1)),
      .before = 1
    )

  if (!is.null(order_values)) {
    out <- out %>% arrange(match(group, order_values), group)
  } else {
    out <- out %>% arrange(median, group)
  }
  out
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
# 1b) Fixed-configuration sensitivity against retrospective lowest-discrepancy selection
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
    excess_over_lowest_pct = median_abs_diff_pct - best_median_abs_diff_pct,
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
    median_excess_over_lowest = median(excess_over_lowest_pct, na.rm = TRUE),
    q75_excess_over_lowest = quantile(excess_over_lowest_pct, 0.75, na.rm = TRUE),
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
# 1c) Leave-one-surface workflow-transfer check
# -------------------------
workflow_transfer_by_group <- bind_rows(lapply(seq_len(nrow(decision_primary)), function(i) {
  g <- decision_primary[i, ]
  train <- config_primary %>%
    filter(
      param == g$param,
      !(material == g$material & process == g$process)
    )
  heldout <- config_primary %>%
    filter(param == g$param, material == g$material, process == g$process)

  if (nrow(train) == 0 || nrow(heldout) == 0) return(tibble())

  train_rank <- train %>%
    group_by(system, color) %>%
    summarise(
      training_n_groups = n(),
      training_median_abs_diff_pct = median(median_abs_diff_pct, na.rm = TRUE),
      training_q25_abs_diff_pct = quantile(median_abs_diff_pct, 0.25, na.rm = TRUE),
      training_q75_abs_diff_pct = quantile(median_abs_diff_pct, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(training_median_abs_diff_pct, desc(training_n_groups), system, color)

  selected <- train_rank %>%
    semi_join(heldout %>% select(system, color), by = c("system", "color")) %>%
    slice_head(n = 1)

  if (nrow(selected) == 0) return(tibble())

  selected %>%
    left_join(
      heldout %>%
        select(
          system, color,
          heldout_n = n,
          heldout_median_abs_diff_pct = median_abs_diff_pct,
          heldout_mean_abs_diff_pct = mean_abs_diff_pct,
          heldout_q25_abs_diff_pct = q25_abs_diff_pct,
          heldout_q75_abs_diff_pct = q75_abs_diff_pct,
          retrospective_lowest_abs_diff_pct = best_median_abs_diff_pct
        ),
      by = c("system", "color")
    ) %>%
    transmute(
      param = g$param,
      material = g$material,
      process = g$process,
      selected_system = system,
      selected_color = color,
      training_n_groups,
      training_median_abs_diff_pct,
      training_q25_abs_diff_pct,
      training_q75_abs_diff_pct,
      heldout_n,
      heldout_median_abs_diff_pct,
      heldout_mean_abs_diff_pct,
      heldout_q25_abs_diff_pct,
      heldout_q75_abs_diff_pct,
      retrospective_lowest_abs_diff_pct,
      excess_over_retrospective_lowest_pct = heldout_median_abs_diff_pct - retrospective_lowest_abs_diff_pct,
      heldout_category = cat_label(heldout_median_abs_diff_pct)
    )
}))

workflow_transfer_summary_by_param <- bind_rows(
  workflow_transfer_by_group %>%
    summarise(
      param = "all",
      n_groups = n(),
      median_holdout = median(heldout_median_abs_diff_pct, na.rm = TRUE),
      q25_holdout = quantile(heldout_median_abs_diff_pct, 0.25, na.rm = TRUE),
      q75_holdout = quantile(heldout_median_abs_diff_pct, 0.75, na.rm = TRUE),
      good_n = sum(heldout_median_abs_diff_pct <= 10, na.rm = TRUE),
      mid_n = sum(heldout_median_abs_diff_pct > 10 & heldout_median_abs_diff_pct <= 30, na.rm = TRUE),
      poor_n = sum(heldout_median_abs_diff_pct > 30, na.rm = TRUE),
      median_excess_over_lowest = median(excess_over_retrospective_lowest_pct, na.rm = TRUE),
      selected_system_mode = mode_chr(selected_system),
      selected_color_mode = mode_chr(selected_color),
      .groups = "drop"
    ),
  workflow_transfer_by_group %>%
    group_by(param) %>%
    summarise(
      n_groups = n(),
      median_holdout = median(heldout_median_abs_diff_pct, na.rm = TRUE),
      q25_holdout = quantile(heldout_median_abs_diff_pct, 0.25, na.rm = TRUE),
      q75_holdout = quantile(heldout_median_abs_diff_pct, 0.75, na.rm = TRUE),
      good_n = sum(heldout_median_abs_diff_pct <= 10, na.rm = TRUE),
      mid_n = sum(heldout_median_abs_diff_pct > 10 & heldout_median_abs_diff_pct <= 30, na.rm = TRUE),
      poor_n = sum(heldout_median_abs_diff_pct > 30, na.rm = TRUE),
      median_excess_over_lowest = median(excess_over_retrospective_lowest_pct, na.rm = TRUE),
      selected_system_mode = mode_chr(selected_system),
      selected_color_mode = mode_chr(selected_color),
      .groups = "drop"
    )
) %>%
  mutate(
    good_pct = 100 * good_n / n_groups,
    mid_pct = 100 * mid_n / n_groups,
    poor_pct = 100 * poor_n / n_groups
  )

write_csv(workflow_transfer_by_group, file.path(out_dir, "results_workflow_transfer_by_group.csv"))
write_csv(workflow_transfer_summary_by_param, file.path(out_dir, "results_workflow_transfer_summary_by_param.csv"))

best_complete_fixed <- fixed_config_summary %>%
  filter(n_groups == primary_group_count) %>%
  slice_min(median_fixed, n = 1, with_ties = FALSE)
if (nrow(best_complete_fixed) == 0) {
  best_complete_fixed <- fixed_config_summary %>%
    slice_min(median_fixed, n = 1, with_ties = FALSE)
}
transfer_all <- workflow_transfer_summary_by_param %>%
  filter(param == "all") %>%
  slice_head(n = 1)

selection_penalty_overview <- tibble(
  analysis_view = c(
    "Retrospective lowest-discrepancy selection",
    "Lowest-median complete-coverage fixed workflow",
    "Leave-one-surface workflow transfer"
  ),
  median_abs_diff_pct = c(
    median(decision_primary$best_median_abs_diff_pct, na.rm = TRUE),
    best_complete_fixed$median_fixed,
    transfer_all$median_holdout
  ),
  high_discrepancy_pct = c(
    100 * sum(decision_primary$best_median_abs_diff_pct > 30, na.rm = TRUE) / nrow(decision_primary),
    best_complete_fixed$poor_pct,
    transfer_all$poor_pct
  ),
  workflow_or_selection = c(
    "selected within parameter--surface group",
    paste(best_complete_fixed$system, best_complete_fixed$color, sep = ", "),
    paste(transfer_all$selected_system_mode, transfer_all$selected_color_mode, sep = ", ")
  )
)
write_csv(selection_penalty_overview, file.path(out_dir, "results_workflow_selection_penalty_overview.csv"))

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

set.seed(21920)
bootstrap_ci <- bind_rows(
  summarise_bootstrap_ci(
    decision_primary,
    scope = "Parameter",
    group_col = param,
    label_fun = param_tex,
    order_values = primary_params
  ),
  summarise_bootstrap_ci(
    decision_primary,
    scope = "Process",
    group_col = process,
    label_fun = process_label_tex,
    order_values = by_process$process
  ),
  summarise_bootstrap_ci(
    decision_primary,
    scope = "Material",
    group_col = material,
    label_fun = material_label_tex,
    order_values = by_material$material
  )
)

write_csv(bootstrap_ci, file.path(out_dir, "results_bootstrap_ci_aggregate_medians.csv"))

rsm_decision <- decision %>%
  filter(param == "rsm")

rsm_config <- config_medians %>%
  filter(param == "rsm")

rsm_system_diag <- rsm_config %>%
  mutate(system_abs_diff_pct = median_abs_diff_pct) %>%
  group_by(view = "Optical system", stratum = system) %>%
  summarise(
    n = n(),
    good_n = sum(system_abs_diff_pct <= 10, na.rm = TRUE),
    mid_n = sum(system_abs_diff_pct > 10 & system_abs_diff_pct <= 30, na.rm = TRUE),
    poor_n = sum(system_abs_diff_pct > 30, na.rm = TRUE),
    median_abs_diff_pct = median(system_abs_diff_pct, na.rm = TRUE),
    q25_abs_diff_pct = quantile(system_abs_diff_pct, 0.25, na.rm = TRUE),
    q75_abs_diff_pct = quantile(system_abs_diff_pct, 0.75, na.rm = TRUE),
    modal_workflow = "all colours",
    .groups = "drop"
  )

rsm_material_diag <- rsm_decision %>%
  group_by(view = "Material", stratum = material) %>%
  summarise(
    n = n(),
    median_abs_diff_pct = median(best_median_abs_diff_pct, na.rm = TRUE),
    q25_abs_diff_pct = quantile(best_median_abs_diff_pct, 0.25, na.rm = TRUE),
    q75_abs_diff_pct = quantile(best_median_abs_diff_pct, 0.75, na.rm = TRUE),
    good_n = sum(best_median_abs_diff_pct <= 10, na.rm = TRUE),
    mid_n = sum(best_median_abs_diff_pct > 10 & best_median_abs_diff_pct <= 30, na.rm = TRUE),
    poor_n = sum(best_median_abs_diff_pct > 30, na.rm = TRUE),
    best_system_mode = mode_chr(best_system),
    best_color_mode = mode_chr(best_color),
    .groups = "drop"
  ) %>%
  mutate(modal_workflow = paste(best_system_mode, best_color_mode, sep = ", ")) %>%
  select(-best_system_mode, -best_color_mode)

rsm_process_diag <- rsm_decision %>%
  group_by(view = "Process", stratum = process) %>%
  summarise(
    n = n(),
    median_abs_diff_pct = median(best_median_abs_diff_pct, na.rm = TRUE),
    q25_abs_diff_pct = quantile(best_median_abs_diff_pct, 0.25, na.rm = TRUE),
    q75_abs_diff_pct = quantile(best_median_abs_diff_pct, 0.75, na.rm = TRUE),
    good_n = sum(best_median_abs_diff_pct <= 10, na.rm = TRUE),
    mid_n = sum(best_median_abs_diff_pct > 10 & best_median_abs_diff_pct <= 30, na.rm = TRUE),
    poor_n = sum(best_median_abs_diff_pct > 30, na.rm = TRUE),
    best_system_mode = mode_chr(best_system),
    best_color_mode = mode_chr(best_color),
    .groups = "drop"
  ) %>%
  mutate(modal_workflow = paste(best_system_mode, best_color_mode, sep = ", ")) %>%
  select(-best_system_mode, -best_color_mode)

rsm_diagnostic <- bind_rows(rsm_system_diag, rsm_material_diag, rsm_process_diag) %>%
  mutate(
    good_pct = 100 * good_n / n,
    mid_pct = 100 * mid_n / n,
    poor_pct = 100 * poor_n / n,
    view_order = match(view, c("Optical system", "Material", "Process")),
    stratum_order = case_when(
      view == "Optical system" ~ match(stratum, c("conf", "FV", "fusion", "int")),
      view == "Material" ~ match(stratum, c("14301", "al7075", "c45", "ellor", "mo58a", "ti6al4v")),
      view == "Process" ~ match(stratum, c("mr", "mf", "tr", "tf", "b", "wedm_r", "wedm_f", "gri", "gla", "hon")),
      TRUE ~ NA_integer_
    )
  ) %>%
  arrange(view_order, stratum_order, stratum) %>%
  select(-view_order, -stratum_order)

write_csv(rsm_diagnostic, file.path(out_dir, "results_rsm_diagnostic_by_stratum.csv"))

summarise_rsm_scale_file <- function(path) {
  bn <- basename(path)
  m <- str_match(bn, "^([a-z0-9]+)_abs_([^_]+)_(.+)_optics_vs_tactile\\.csv$")
  if (is.na(m[1,2]) || m[1,2] != "rsm") return(tibble())

  df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  need_cols <- c("system", "color", "optics_value", "tactile_reference", "diff_percent_abs")
  if (!all(need_cols %in% names(df))) return(tibble())

  df %>%
    transmute(
      material = m[1,3],
      process = m[1,4],
      system = as.character(system),
      color = as.character(color),
      original_abs_diff_pct = as.numeric(diff_percent_abs),
      scaled_abs_diff_pct = 100 * abs(((as.numeric(optics_value) * 1000) - as.numeric(tactile_reference)) / as.numeric(tactile_reference)),
      exported_tactile_to_optical_ratio = ifelse(as.numeric(optics_value) == 0, NA_real_, as.numeric(tactile_reference) / as.numeric(optics_value)),
      scaled_tactile_to_optical_ratio = ifelse(as.numeric(optics_value) == 0, NA_real_, as.numeric(tactile_reference) / (as.numeric(optics_value) * 1000))
    )
}

summarise_rsm_scale <- function(scope, assumption, df, diff_col, ratio_col) {
  values <- df[[diff_col]]
  ratio_values <- df[[ratio_col]]
  tibble(
    scope = scope,
    scale_assumption = assumption,
    n = sum(is.finite(values)),
    median_abs_diff_pct = median(values, na.rm = TRUE),
    q25_abs_diff_pct = quantile(values, 0.25, na.rm = TRUE),
    q75_abs_diff_pct = quantile(values, 0.75, na.rm = TRUE),
    good_n = sum(values <= 10, na.rm = TRUE),
    mid_n = sum(values > 10 & values <= 30, na.rm = TRUE),
    poor_n = sum(values > 30, na.rm = TRUE),
    ratio_median = median(ratio_values, na.rm = TRUE)
  )
}

rsm_scale_entries <- bind_rows(lapply(opt_files, summarise_rsm_scale_file))

rsm_scale_best_exported <- rsm_scale_entries %>%
  group_by(material, process) %>%
  arrange(original_abs_diff_pct, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

rsm_scale_best_scaled <- rsm_scale_entries %>%
  group_by(material, process) %>%
  arrange(scaled_abs_diff_pct, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

rsm_scale_sensitivity <- bind_rows(
  summarise_rsm_scale(
    "All retained system-colour entries",
    "Exported optical value",
    rsm_scale_entries,
    "original_abs_diff_pct",
    "exported_tactile_to_optical_ratio"
  ),
  summarise_rsm_scale(
    "All retained system-colour entries",
    "Optical value multiplied by 1000",
    rsm_scale_entries,
    "scaled_abs_diff_pct",
    "scaled_tactile_to_optical_ratio"
  ),
  summarise_rsm_scale(
    "Lowest-discrepancy workflow per material-process group",
    "Exported optical value",
    rsm_scale_best_exported,
    "original_abs_diff_pct",
    "exported_tactile_to_optical_ratio"
  ),
  summarise_rsm_scale(
    "Lowest-discrepancy workflow per material-process group",
    "Optical value multiplied by 1000",
    rsm_scale_best_scaled,
    "scaled_abs_diff_pct",
    "scaled_tactile_to_optical_ratio"
  )
) %>%
  mutate(
    good_pct = 100 * good_n / n,
    mid_pct = 100 * mid_n / n,
    poor_pct = 100 * poor_n / n
  )

write_csv(rsm_scale_sensitivity, file.path(out_dir, "results_rsm_unit_scale_sensitivity.csv"))

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
    "\\caption{Decision-oriented summary of optical--tactile discrepancy by strictly positive roughness parameter (lowest-discrepancy system+colour chosen per material--process group)}",
    "\\label{tab:decision_by_param}",
    "\\setlength\\tabcolsep{4pt}",
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{lccccccc}",
    "\\toprule",
    "Parameter & $N$ & Lowest median $|\\mathrm{diff}|$ [\\%] & Q1--Q3 & $\\leq 10\\%$ & 10--30\\% & >30\\% & Most frequent lowest (system, colour) \\\\",
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
    "\\footnotesize\\textit{Note:} $N$ is the number of available material--process groups for the seven strictly positive parameters. For each group, the optical configuration (system+illumination colour) that minimises the median $|\\mathrm{diff}|(\\%)$ is selected. Q1--Q3 are quartiles of the selected medians. The band columns ($\\leq 10\\%$, 10--30\\%, >30\\%) count how many groups fall into each range; values are shown as count (percent of $N$), e.g. 7 (15\\%) means 7 groups, i.e. 15\\% of $N$. Most frequent lowest denotes the modal selected (system, colour) across groups. Percentage-based $R_{sk}$ rankings are excluded from the manuscript decision tables because normalisation becomes unstable near zero; archive-level screening rows are retained separately in the exported outputs.",
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
    paste0(first_col_name, " & $N$ & Lowest median $|\\mathrm{diff}|$ [\\%] & Q1--Q3 & $\\leq 10\\%$ & 10--30\\% & >30\\% & Most frequent lowest (system, colour) \\\\") ,
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
    "\\footnotesize\\textit{Note:} $N$ is the number of contributing strictly positive parameter--material--process groups. For each group, the optical configuration (system+illumination colour) that minimises the median $|\\mathrm{diff}|(\\%)$ is selected. Q1--Q3 are quartiles of the selected medians. The band columns ($\\leq 10\\%$, 10--30\\%, >30\\%) count how many groups fall into each range; values are shown as count (percent of $N$), e.g. 7 (15\\%) means 7 groups, i.e. 15\\% of $N$. Most frequent lowest denotes the modal selected (system, colour) across groups. Archive identifiers in the first column are expanded for readability.",
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
          "Q1--Q3 & $\\leq 10\\%$ & >30\\% & Median excess vs. lowest [\\%] ",
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
    excess <- sprintf("%.1f", r$median_excess_over_lowest)
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
    sprintf("\\footnotesize\\textit{Note:} Fixed workflow means that one optical system+illumination pair is evaluated wherever it is available, without selecting a different configuration for each parameter--surface group. Coverage is relative to the %d strictly positive parameter--surface groups used in the manuscript decision summaries. The table shows the eight lowest median fixed workflows; the complete fixed-workflow ranking and group-level medians are exported as CSV files. Excess vs. lowest is the median difference between the fixed-workflow median discrepancy and the retrospective lowest-discrepancy median within the same group.", primary_group_count),
    "\\end{minipage}",
    "\\end{table}"
  )
  writeLines(lines, path)
}

write_table_workflow_transfer <- function(df, path) {
  table_row_end <- "\\\\"
  df <- df %>%
    mutate(order_id = ifelse(param == "all", 0L, match(param, primary_params))) %>%
    arrange(order_id, param)

  lines <- c(
    "% Auto-generated by make_results_decision_tables.R",
    "\\begin{table}[H]",
    "\\centering",
    "\\small",
    "\\caption{Leave-one-surface workflow-transfer check for the strictly positive roughness parameters}",
    "\\label{tab:workflow_transfer}",
    "\\setlength\\tabcolsep{4pt}",
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{lccccccc}",
    "\\toprule",
    paste0("Parameter & $N$ & Held-out median $|\\mathrm{diff}|$ [\\%] & Q1--Q3 & $\\leq 10\\%$ & >30\\% & Median excess vs. lowest [\\%] & Most frequent selected workflow ", table_row_end),
    "\\midrule"
  )

  for (i in seq_len(nrow(df))) {
    r <- df[i,]
    param_label <- if (r$param == "all") "All" else param_tex(r$param)
    med <- sprintf("%.1f", r$median_holdout)
    iqr <- sprintf("%.1f--%.1f", r$q25_holdout, r$q75_holdout)
    good <- sprintf("%d (%.0f\\%%)", r$good_n, r$good_pct)
    poor <- sprintf("%d (%.0f\\%%)", r$poor_n, r$poor_pct)
    excess <- sprintf("%.1f", r$median_excess_over_lowest)
    workflow <- sprintf("%s, %s", to_tex(r$selected_system_mode), to_tex(r$selected_color_mode))
    lines <- c(lines, sprintf("%s & %d & %s & %s & %s & %s & %s & %s %s",
                              param_label, r$n_groups, med, iqr, good, poor, excess, workflow, table_row_end))
  }

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    "}",
    "\\vspace{0.5ex}",
    "\\begin{minipage}{\\linewidth}",
    "\\footnotesize\\textit{Note:} For each held-out material--process group, system+illumination workflows were ranked using all other material--process groups for the same parameter. The table evaluates the highest-ranked workflow that was available for the held-out group. The held-out group was not used to rank workflows. Excess vs. lowest is the median increase relative to the retrospective lowest-discrepancy result in the same held-out groups. This check estimates transfer of workflow selection across surfaces within the archive and is not a prospective validation interval.",
    "\\end{minipage}",
    "\\end{table}"
  )
  writeLines(lines, path)
}

write_table_rsm_diagnostic <- function(df, path) {
  table_row_end <- "\\\\"
  lines <- c(
    "% Auto-generated by make_results_decision_tables.R",
    "\\begin{table}[H]",
    "\\centering",
    "\\small",
    "\\caption{Diagnostic segmentation of $R_{sm}$ optical--tactile discrepancy by optical system, material, and process}",
    "\\label{tab:rsm_diagnostic}",
    "\\setlength\\tabcolsep{4pt}",
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{llcccccc}",
    "\\toprule",
    paste0("View & Stratum & $N$ & Median $|\\mathrm{diff}|$ [\\%] & Q1--Q3 & $\\leq 10\\%$ & >30\\% & Modal workflow ", table_row_end),
    "\\midrule"
  )

  last_view <- NA_character_
  for (i in seq_len(nrow(df))) {
    r <- df[i,]
    if (!is.na(last_view) && r$view != last_view) {
      lines <- c(lines, "\\addlinespace")
    }
    last_view <- r$view

    stratum <- if (r$view == "Material") {
      material_label_tex(r$stratum)
    } else if (r$view == "Process") {
      process_label_tex(r$stratum)
    } else {
      to_tex(r$stratum)
    }
    med <- sprintf("%.1f", r$median_abs_diff_pct)
    iqr <- sprintf("%.1f--%.1f", r$q25_abs_diff_pct, r$q75_abs_diff_pct)
    good <- sprintf("%d (%.0f\\%%)", r$good_n, r$good_pct)
    poor <- sprintf("%d (%.0f\\%%)", r$poor_n, r$poor_pct)
    workflow <- to_tex(r$modal_workflow)
    lines <- c(lines, sprintf("%s & %s & %d & %s & %s & %s & %s & %s %s",
                              to_tex(r$view), stratum, r$n, med, iqr, good, poor, workflow, table_row_end))
  }

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    "}",
    "\\vspace{0.5ex}",
    "\\begin{minipage}{\\linewidth}",
    "\\footnotesize\\textit{Note:} Optical-system rows summarise all retained $R_{sm}$ system--colour entries. Material and process rows summarise the retrospective lowest-discrepancy $R_{sm}$ result per material--process group. The modal workflow is the most frequently selected lowest-discrepancy system+colour within the corresponding material or process stratum; for optical-system rows, all retained illumination colours are pooled.",
    "\\end{minipage}",
    "\\end{table}"
  )
  writeLines(lines, path)
}

write_table_rsm_scale_sensitivity <- function(df, path) {
  table_row_end <- "\\\\"
  lines <- c(
    "% Auto-generated by make_results_decision_tables.R",
    "\\begin{table}[H]",
    "\\centering",
    "\\small",
    "\\caption{Sensitivity of $R_{sm}$ discrepancy to retained optical unit scale}",
    "\\label{tab:rsm_scale_sensitivity}",
    "\\setlength\\tabcolsep{4pt}",
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{llcccccc}",
    "\\toprule",
    paste0("Comparison set & Scale assumption & $N$ & Median $|\\mathrm{diff}|$ [\\%] & Q1--Q3 & $\\leq 10\\%$ & >30\\% & Median tactile/optical ratio ", table_row_end),
    "\\midrule"
  )

  last_scope <- NA_character_
  for (i in seq_len(nrow(df))) {
    r <- df[i,]
    if (!is.na(last_scope) && r$scope != last_scope) {
      lines <- c(lines, "\\addlinespace")
    }
    last_scope <- r$scope

    med <- sprintf("%.1f", r$median_abs_diff_pct)
    iqr <- sprintf("%.1f--%.1f", r$q25_abs_diff_pct, r$q75_abs_diff_pct)
    good <- sprintf("%d (%.0f\\%%)", r$good_n, r$good_pct)
    poor <- sprintf("%d (%.0f\\%%)", r$poor_n, r$poor_pct)
    ratio <- sprintf("%.1f", r$ratio_median)
    lines <- c(lines, sprintf("%s & %s & %d & %s & %s & %s & %s & %s %s",
                              to_tex(r$scope), to_tex(r$scale_assumption), r$n, med, iqr, good, poor, ratio, table_row_end))
  }

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    "}",
    "\\vspace{0.5ex}",
    "\\begin{minipage}{\\linewidth}",
    "\\footnotesize\\textit{Note:} The x1000 rows are a sensitivity check that treats retained optical $R_{sm}$ values as if they were exported in millimetres and converted to micrometres before comparison. They are not used as corrected primary endpoints because complete original unit metadata and raw processing chains are not retained consistently across the archive.",
    "\\end{minipage}",
    "\\end{table}"
  )
  writeLines(lines, path)
}

write_table_bootstrap_ci <- function(df, path) {
  table_row_end <- "\\\\"
  lines <- c(
    "% Auto-generated by make_results_decision_tables.R",
    "\\begin{table}[H]",
    "\\centering",
    "\\small",
    "\\caption{Bootstrap percentile intervals for aggregate retrospective lowest-discrepancy medians}",
    "\\label{tab:supp_bootstrap_ci}",
    "\\setlength\\tabcolsep{4pt}",
    "\\resizebox{\\linewidth}{!}{%",
    "\\begin{tabular}{llccccc}",
    "\\toprule",
    paste0("Summary level & Group & $N$ & Median $|\\mathrm{diff}|$ [\\%] & 95\\% bootstrap CI [\\%] & Resamples & Confidence ", table_row_end),
    "\\midrule"
  )

  last_scope <- NA_character_
  for (i in seq_len(nrow(df))) {
    r <- df[i,]
    if (!is.na(last_scope) && r$scope != last_scope) {
      lines <- c(lines, "\\addlinespace")
    }
    last_scope <- r$scope

    median_text <- sprintf("%.1f", r$median)
    ci_text <- sprintf("%.1f--%.1f", r$ci_low, r$ci_high)
    conf_text <- sprintf("%.0f\\%%", 100 * r$conf_level)
    lines <- c(lines, sprintf("%s & %s & %d & %s & %s & %d & %s %s",
                              to_tex(r$scope), r$group_label, r$n, median_text, ci_text,
                              r$n_boot, conf_text, table_row_end))
  }

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    "}",
    "\\vspace{0.5ex}",
    "\\begin{minipage}{\\linewidth}",
    "\\footnotesize\\textit{Note:} Intervals are non-parametric percentile bootstrap summaries of retrospective lowest-discrepancy median $|\\mathrm{diff}|(\\%)$ within each aggregate level. Resampling was performed across archive-defined parameter--surface groups for the corresponding parameter, process, or material. These intervals describe sampling stability of the retrospective summaries and are not formal prospective validation intervals.",
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
      name = "Lowest median\n|diff| (%)"
    ) +
    labs(
      x = "Roughness parameter",
      y = "Surface-generation process",
      title = "Retrospective lowest-discrepancy optical--tactile result by process and parameter"
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

write_selection_penalty_plot <- function(df, path) {
  df <- df %>%
    mutate(
      analysis_view = factor(.data$analysis_view, levels = rev(.data$analysis_view)),
      label = sprintf("%.1f%% median; %.0f%% >30%%", .data$median_abs_diff_pct, .data$high_discrepancy_pct)
    )

  p <- ggplot(df, aes(x = .data$analysis_view, y = .data$median_abs_diff_pct)) +
    geom_col(fill = "#4d7ea8", width = 0.62) +
    geom_text(aes(label = .data$label), hjust = -0.04, size = 3.3) +
    coord_flip(clip = "off") +
    scale_y_continuous(limits = c(0, max(df$median_abs_diff_pct, na.rm = TRUE) * 1.35), expand = expansion(mult = c(0, 0))) +
    labs(
      x = NULL,
      y = "Median |diff| (%)",
      title = "Workflow-selection penalty across strictly positive parameters"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 11),
      plot.margin = margin(5.5, 48, 5.5, 5.5)
    )

  ggsave(path, plot = p, width = 8.4, height = 3.0, units = "in")
}

write_table_by_param(by_param, file.path(tex_dir, "results_decision_by_param.tex"))

write_table_generic(
  by_process,
  file.path(tex_dir, "results_decision_by_process.tex"),
  caption = "Decision-oriented discrepancy summary aggregated by surface-generation process across all materials and strictly positive parameters",
  label = "tab:decision_by_process",
  first_col_name = "Process",
  label_formatter = process_label_tex
)

write_table_generic(
  by_material,
  file.path(tex_dir, "results_decision_by_material.tex"),
  caption = "Decision-oriented discrepancy summary aggregated by material across all processes and strictly positive parameters",
  label = "tab:decision_by_material",
  first_col_name = "Material",
  label_formatter = material_label_tex
)

write_table_fixed_workflows(
  fixed_config_summary,
  file.path(tex_dir, "results_fixed_workflow_sensitivity.tex"),
  primary_group_count = primary_group_count
)

write_table_workflow_transfer(
  workflow_transfer_summary_by_param,
  file.path(tex_dir, "results_workflow_transfer_summary.tex")
)

write_table_rsm_diagnostic(
  rsm_diagnostic,
  file.path(tex_dir, "results_rsm_diagnostic.tex")
)

write_table_rsm_scale_sensitivity(
  rsm_scale_sensitivity,
  file.path(tex_dir, "results_rsm_unit_scale_sensitivity.tex")
)

write_table_bootstrap_ci(
  bootstrap_ci,
  file.path(tex_dir, "results_bootstrap_ci_aggregate_medians.tex")
)

write_global_heatmap(
  by_process_param,
  file.path(plots_dir, "global_best_discrepancy_heatmap.pdf"),
  process_order = by_process$process
)

write_selection_penalty_plot(
  selection_penalty_overview,
  file.path(plots_dir, "workflow_selection_penalty_overview.pdf")
)

cat("Wrote:\n")
cat("- ", file.path(out_dir, "results_decision_best_optical_by_group.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "results_config_medians_by_group.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "results_decision_summary_by_param.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "results_decision_rsk_screening_groups.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "results_bootstrap_ci_aggregate_medians.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "results_rsm_diagnostic_by_stratum.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "results_rsm_unit_scale_sensitivity.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "results_fixed_workflow_sensitivity.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "results_workflow_transfer_by_group.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "results_workflow_transfer_summary_by_param.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "results_workflow_selection_penalty_overview.csv"), "\n", sep = "")
cat("- ", file.path(out_dir, "results_best_discrepancy_heatmap_by_process_param.csv"), "\n", sep = "")
cat("- ", file.path(tex_dir, "results_decision_by_param.tex"), "\n", sep = "")
cat("- ", file.path(tex_dir, "results_decision_by_process.tex"), "\n", sep = "")
cat("- ", file.path(tex_dir, "results_decision_by_material.tex"), "\n", sep = "")
cat("- ", file.path(tex_dir, "results_fixed_workflow_sensitivity.tex"), "\n", sep = "")
cat("- ", file.path(tex_dir, "results_workflow_transfer_summary.tex"), "\n", sep = "")
cat("- ", file.path(tex_dir, "results_rsm_diagnostic.tex"), "\n", sep = "")
cat("- ", file.path(tex_dir, "results_rsm_unit_scale_sensitivity.tex"), "\n", sep = "")
cat("- ", file.path(tex_dir, "results_bootstrap_ci_aggregate_medians.tex"), "\n", sep = "")
cat("- ", file.path(plots_dir, "global_best_discrepancy_heatmap.pdf"), "\n", sep = "")
cat("- ", file.path(plots_dir, "workflow_selection_penalty_overview.pdf"), "\n", sep = "")
