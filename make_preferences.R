#!/usr/bin/env Rscript
suppressWarnings(suppressMessages({
  library(dplyr); library(readr); library(stringr)
}))

out_dir <- "outputs"
params <- c("rp","rv","rz","ra","rt","rz1max")

# infer slug from existing effects_by_system file
efs <- list.files(out_dir, pattern = "_abs_.*_effects_by_system\\.csv$", full.names = TRUE)
if (length(efs) == 0) stop("Run main analysis first: no effects_by_system files.")
get_slug <- function(path) {
  m <- str_match(basename(path), "^([a-z0-9]+)_abs_(.+)_effects_by_system\\.csv$")
  m[1,3]
}
slug <- get_slug(efs[1])

read_sys <- function(p) {
  f <- file.path(out_dir, sprintf("%s_abs_%s_effects_by_system.csv", p, slug))
  if (!file.exists(f)) return(NULL)
  df <- read_csv(f, show_col_types = FALSE)
  df$param <- toupper(p)
  df
}
read_col <- function(p) {
  f <- file.path(out_dir, sprintf("%s_abs_%s_effects_by_color.csv", p, slug))
  if (!file.exists(f)) return(NULL)
  df <- read_csv(f, show_col_types = FALSE)
  df$param <- toupper(p)
  df
}

sys_all <- bind_rows(lapply(params, read_sys))
col_all <- bind_rows(lapply(params, read_col))

pick_best <- function(df, key) {
  if (is.null(df) || nrow(df) == 0) return(tibble())
  # prefer lower mean |diff|%, tie-break by lower SE, then higher n
  df %>% group_by(param) %>% arrange(mean, se, desc(n)) %>% slice_head(n=1) %>%
    ungroup() %>% select(param, {{key}}, mean, se, n)
}

best_sys <- pick_best(sys_all, system) %>% rename(choice = system, mean_sys = mean, se_sys = se, n_sys = n)
best_col <- pick_best(col_all, color) %>% rename(choice = color, mean_col = mean, se_col = se, n_col = n)

prefs <- full_join(best_sys, best_col, by = "param", suffix = c("_system","_color"))

write_csv(prefs, file.path(out_dir, "preferences.csv"))
cat("Written outputs/preferences.csv\n")
