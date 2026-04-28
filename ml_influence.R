#!/usr/bin/env Rscript
# ML MODULE: Analyze influence of features on diff_percent
# Keeps existing code unchanged. Uses `merged_df` if present; otherwise, reconstructs from outputs/*.csv.
# Steps:
# 1) Prepare features: system, color, param (wavelength is encoded by color)
# 2) Encode categoricals (for linear models)
# 3) Train/test split
# 4) Train models: Lasso, Ridge (glmnet), Random Forest (ranger)
# 5) Feature importance (RF + glmnet abs betas)
# 6) SHAP values for interpretability (fastshap on RF)
# 7) Plot top features affecting diff_percent

suppressWarnings(suppressMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
}))

# Auto-install helper
.install_if_missing <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, repos = "https://cloud.r-project.org", quiet = TRUE)
    }
  }
}

.install_if_missing(c("glmnet", "ranger", "fastshap", "tidyr"))
suppressWarnings(suppressMessages({
  library(glmnet)
  library(ranger)
  library(fastshap)
  library(tidyr)
}))

set.seed(123)

out_dir <- "outputs"
ml_dir <- file.path(out_dir, "ml")
dir.create(ml_dir, recursive = TRUE, showWarnings = FALSE)

# Pretty-print feature names on plots (British English)
pretty_feature <- function(x) {
  x <- as.character(x)
  # colour spelling
  x <- gsub("color", "colour", x, fixed = TRUE)
  x
}

# 0) Get data: use merged_df if available; else reconstruct from outputs
get_merged <- function() {
  if (exists("merged_df", envir = .GlobalEnv)) {
    df <- get("merged_df", envir = .GlobalEnv)
    # Try to harmonize column names
    nms <- names(df)
    # Guess diff column
    diff_cols <- nms[grepl("diff", nms, ignore.case = TRUE)]
    diff_col <- NA_character_
    if (length(diff_cols)) {
      # prefer names containing 'percent' or '%'
      cand <- diff_cols[grepl("percent|%", diff_cols, ignore.case = TRUE)]
      diff_col <- if (length(cand)) cand[1] else diff_cols[1]
    }
    # Guess wavelength
    wav_col <- nms[grepl("wavelength|lambda", nms, ignore.case = TRUE)]
    wav_col <- if (length(wav_col)) wav_col[1] else NA_character_

    # Build safe frame
    tibble(
      diff_percent = as.numeric(df[[diff_col]]),
      system = as.character(df[[if ("system" %in% nms) "system" else NA_character_]]),
      color = as.character(df[[if ("color" %in% nms) "color" else NA_character_]]),
      param = as.character(df[[if ("param" %in% nms) "param" else if ("parameter" %in% nms) "parameter" else NA_character_]]),
      wavelength = if (!is.na(wav_col)) as.numeric(df[[wav_col]]) else NA_real_,
      slug = NA_character_
    )
  } else {
    # Reconstruct from outputs
    files <- list.files(out_dir, pattern = "^[a-z0-9]+_abs_[a-z0-9_-]+_optics_vs_tactile\\.csv$", full.names = TRUE)
    if (length(files) == 0) stop("No source data found (optics_vs_tactile CSVs). Run analysis first.")
    bind_rows(lapply(files, function(path) {
      bn <- basename(path)
      m <- str_match(bn, "^([a-z0-9]+)_abs_([a-z0-9_-]+)_optics_vs_tactile\\.csv$")
      param <- toupper(m[1,2])
      slug  <- m[1,3]
      df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
      nms <- names(df)
      # Find diff column
      diff_cols <- nms[grepl("diff", nms, ignore.case = TRUE)]
      if (length(diff_cols) == 0) return(tibble())
      # prefer percent-named
      cand <- diff_cols[grepl("percent|%|abs", diff_cols, ignore.case = TRUE)]
      dcol <- if (length(cand)) cand[1] else diff_cols[1]
      wav_col <- nms[grepl("wavelength|lambda", nms, ignore.case = TRUE)]
      wav_col <- if (length(wav_col)) wav_col[1] else NA_character_
      tibble(
        diff_percent = as.numeric(df[[dcol]]),
        system = as.character(df[[if ("system" %in% nms) "system" else NA_character_]]),
        color = as.character(df[[if ("color" %in% nms) "color" else NA_character_]]),
        param = param,
        wavelength = if (!is.na(wav_col)) as.numeric(df[[wav_col]]) else NA_real_,
        slug = slug
      )
    }))
  }
}

df_raw <- get_merged() %>%
  filter(is.finite(diff_percent), !is.na(system), !is.na(color), !is.na(param))

# Basic cleaning
# Remove extreme outliers (optional): cap at 99th percentile
cap_val <- quantile(df_raw$diff_percent, 0.99, na.rm = TRUE)
df_raw$diff_percent <- pmin(df_raw$diff_percent, cap_val)

# Factors
df_raw <- df_raw %>% mutate(
  system = factor(system),
  color = factor(color),
  param = factor(param)
)

# Train/test split (stratify by param to keep distribution)
set.seed(123)
idx <- unlist(df_raw %>% mutate(row_id = row_number()) %>% group_by(param) %>%
                group_split() %>% lapply(function(g) sample(g$row_id, size = floor(0.8*nrow(g)))))
train_idx <- unique(idx)
train_df <- df_raw[train_idx, ]
test_df  <- df_raw[-train_idx, ]

# Design matrix for glmnet (lasso/ridge)
# NOTE: wavelength is represented by color; do not include numeric wavelength to avoid collinearity.
form_terms <- c("system", "color", "param")
form <- as.formula(paste("~", paste(form_terms, collapse = " + ")))
X_train <- model.matrix(form, data = train_df)[, -1, drop = FALSE]
X_test  <- model.matrix(form, data = test_df)[, -1, drop = FALSE]
y_train <- train_df$diff_percent

# Helper: core training and reporting for a given data frame and feature set
run_block <- function(block_df, out_subdir, include_param = TRUE) {
  dir.create(out_subdir, recursive = TRUE, showWarnings = FALSE)
  # Ensure minimal rows
  if (nrow(block_df) < 10) {
    readr::write_csv(tibble(note = "Too few rows for robust ML"), file.path(out_subdir, "NOTE.csv"))
    return(invisible(NULL))
  }
  # Factors
  block_df <- block_df %>% mutate(system = factor(system), color = factor(color), param = factor(param))
  # Split
  set.seed(123)
  idx <- unlist(block_df %>% mutate(row_id = row_number()) %>% group_by(param) %>%
                  group_split() %>% lapply(function(g) sample(g$row_id, size = max(1, floor(0.8*nrow(g))))))
  train_idx <- unique(idx)
  train_df <- block_df[train_idx, ]
  test_df  <- block_df[-train_idx, ]
  # Features (no numeric wavelength)
  terms <- c("system", "color")
  if (include_param) terms <- c(terms, "param")
  form <- as.formula(paste("~", paste(terms, collapse = " + ")))
  X_train <- model.matrix(form, data = train_df)[, -1, drop = FALSE]
  X_test  <- model.matrix(form, data = test_df)[, -1, drop = FALSE]
  y_train <- train_df$diff_percent

  # Lasso
  cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1, standardize = TRUE)
  fit_lasso <- glmnet(X_train, y_train, alpha = 1, lambda = cv_lasso$lambda.1se, standardize = TRUE)
  pred_lasso <- as.numeric(predict(fit_lasso, newx = X_test))
  rmse_lasso <- sqrt(mean((pred_lasso - test_df$diff_percent)^2))

  # Ridge
  cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, standardize = TRUE)
  fit_ridge <- glmnet(X_train, y_train, alpha = 0, lambda = cv_ridge$lambda.1se, standardize = TRUE)
  pred_ridge <- as.numeric(predict(fit_ridge, newx = X_test))
  rmse_ridge <- sqrt(mean((pred_ridge - test_df$diff_percent)^2))

  # Random Forest
  rf_form <- as.formula(paste("diff_percent ~", paste(terms, collapse = " + ")))
  fit_rf <- ranger(rf_form, data = train_df, num.trees = 500, importance = "permutation", seed = 123)
  pred_rf <- predict(fit_rf, data = test_df)$predictions
  rmse_rf <- sqrt(mean((pred_rf - test_df$diff_percent)^2))

  perf <- tibble(model = c("lasso", "ridge", "random_forest"), RMSE = c(rmse_lasso, rmse_ridge, rmse_rf))
  readr::write_csv(perf, file.path(out_subdir, "performance.csv"))

  # Importances
  imp_rf <- sort(fit_rf$variable.importance, decreasing = TRUE)
  imp_rf_df <- tibble(feature = names(imp_rf), importance = as.numeric(imp_rf), model = "random_forest")

  coef_lasso <- coef(fit_lasso) %>% as.matrix() %>% as.data.frame() %>% tibble::rownames_to_column("feature")
  colnames(coef_lasso)[2] <- "coef"
  coef_lasso <- coef_lasso %>% filter(feature != "(Intercept)") %>% mutate(importance = abs(coef), model = "lasso") %>% select(feature, importance, model)

  coef_ridge <- coef(fit_ridge) %>% as.matrix() %>% as.data.frame() %>% tibble::rownames_to_column("feature")
  colnames(coef_ridge)[2] <- "coef"
  coef_ridge <- coef_ridge %>% filter(feature != "(Intercept)") %>% mutate(importance = abs(coef), model = "ridge") %>% select(feature, importance, model)

  imp_all <- bind_rows(imp_rf_df, coef_lasso, coef_ridge)
  readr::write_csv(imp_all, file.path(out_subdir, "feature_importance.csv"))

  # Plots
  if (nrow(imp_rf_df) > 0) {
    imp_plot_df <- head(imp_rf_df, 15) %>% mutate(feature_label = pretty_feature(feature))
    p_imp <- ggplot(imp_plot_df, aes(x = reorder(feature_label, importance), y = importance)) +
      geom_col(fill = "#2C7FB8") + coord_flip() +
      labs(title = "Top features — RF importance", x = "Feature", y = "Permutation importance") +
      theme_minimal(base_size = 11)
    ggsave(filename = file.path(out_subdir, "feature_importance_rf.png"), plot = p_imp, width = 7, height = 5, dpi = 120)
  }

  # SHAP (guard for tiny data)
  if (nrow(train_df) >= 20) {
    X_all <- train_df %>% select(all_of(terms))
    pred_fun <- function(object, newdata) predict(object, data = newdata)$predictions
    shap_sample <- if (nrow(train_df) > 1000) sample(seq_len(nrow(train_df)), 1000) else seq_len(nrow(train_df))
    shap_vals <- fastshap::explain(object = fit_rf, X = X_all[shap_sample, ], pred_wrapper = pred_fun, nsim = 100, adjust = TRUE)
    shap_imp <- as.data.frame(shap_vals) %>% tibble::rownames_to_column("row") %>% mutate(row = as.integer(row)) %>%
      tidyr::pivot_longer(-row, names_to = "feature", values_to = "shap") %>%
      group_by(feature) %>% summarise(mean_abs_shap = mean(abs(shap), na.rm = TRUE), .groups = "drop") %>% arrange(desc(mean_abs_shap))
    readr::write_csv(shap_imp, file.path(out_subdir, "shap_importance.csv"))
    if (nrow(shap_imp) > 0) {
      shap_plot_df <- head(shap_imp, 15) %>% mutate(feature_label = pretty_feature(feature))
      p_shap <- ggplot(shap_plot_df, aes(x = reorder(feature_label, mean_abs_shap), y = mean_abs_shap)) +
        geom_col(fill = "#41AE76") + coord_flip() +
        labs(title = "Top features — mean |SHAP| (RF)", x = "Feature", y = "Mean |SHAP|") +
        theme_minimal(base_size = 11)
      ggsave(filename = file.path(out_subdir, "shap_importance_rf.png"), plot = p_shap, width = 7, height = 5, dpi = 120)
    }
  } else {
    readr::write_csv(tibble(note = "SHAP skipped: too few rows (n<20)"), file.path(out_subdir, "NOTE_SHAP.csv"))
  }

  invisible(NULL)
}

# Lasso
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1, standardize = TRUE)
fit_lasso <- glmnet(X_train, y_train, alpha = 1, lambda = cv_lasso$lambda.1se, standardize = TRUE)
pred_lasso <- as.numeric(predict(fit_lasso, newx = X_test))
rmse_lasso <- sqrt(mean((pred_lasso - test_df$diff_percent)^2))

# Ridge
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, standardize = TRUE)
fit_ridge <- glmnet(X_train, y_train, alpha = 0, lambda = cv_ridge$lambda.1se, standardize = TRUE)
pred_ridge <- as.numeric(predict(fit_ridge, newx = X_test))
rmse_ridge <- sqrt(mean((pred_ridge - test_df$diff_percent)^2))

# Random Forest via ranger (align predictors with form_terms)
rf_form <- as.formula(paste("diff_percent ~", paste(form_terms, collapse = " + ")))
fit_rf <- ranger(rf_form, data = train_df, num.trees = 500, importance = "permutation", seed = 123)
pred_rf <- predict(fit_rf, data = test_df)$predictions
rmse_rf <- sqrt(mean((pred_rf - test_df$diff_percent)^2))

# Summarize performance
perf <- tibble(model = c("lasso", "ridge", "random_forest"), RMSE = c(rmse_lasso, rmse_ridge, rmse_rf))
readr::write_csv(perf, file.path(ml_dir, "performance.csv"))

# Feature importance
imp_rf <- sort(fit_rf$variable.importance, decreasing = TRUE)
imp_rf_df <- tibble(feature = names(imp_rf), importance = as.numeric(imp_rf), model = "random_forest")

# Coefficients (absolute) for glmnet
coef_lasso <- coef(fit_lasso) %>% as.matrix() %>% as.data.frame() %>% tibble::rownames_to_column("feature")
colnames(coef_lasso)[2] <- "coef"
coef_lasso <- coef_lasso %>% filter(feature != "(Intercept)") %>% mutate(importance = abs(coef), model = "lasso") %>% select(feature, importance, model)

coef_ridge <- coef(fit_ridge) %>% as.matrix() %>% as.data.frame() %>% tibble::rownames_to_column("feature")
colnames(coef_ridge)[2] <- "coef"
coef_ridge <- coef_ridge %>% filter(feature != "(Intercept)") %>% mutate(importance = abs(coef), model = "ridge") %>% select(feature, importance, model)

imp_all <- bind_rows(imp_rf_df, coef_lasso, coef_ridge)
readr::write_csv(imp_all, file.path(ml_dir, "feature_importance.csv"))

# Plot top features by RF importance
p_imp_df <- head(imp_rf_df, 15) %>% mutate(feature_label = pretty_feature(feature))
p_imp <- ggplot(p_imp_df, aes(x = reorder(feature_label, importance), y = importance)) +
  geom_col(fill = "#2C7FB8") + coord_flip() +
  labs(title = "Top features — Random Forest importance", x = "Feature", y = "Permutation importance") +
  theme_minimal(base_size = 11)
ggplot2::ggsave(filename = file.path(ml_dir, "feature_importance_rf.png"), plot = p_imp, width = 7, height = 5, dpi = 120)

# SHAP values (fastshap) for RF — use the same predictors as in the RF model
X_all <- train_df %>% select(all_of(form_terms))
# fastshap handles factors; define prediction function
pred_fun <- function(object, newdata) predict(object, data = newdata)$predictions

# Subsample for speed if very large
shap_sample <- if (nrow(train_df) > 1000) sample(seq_len(nrow(train_df)), 1000) else seq_len(nrow(train_df))
shap_vals <- fastshap::explain(object = fit_rf, X = X_all[shap_sample, ], pred_wrapper = pred_fun, nsim = 100, adjust = TRUE)

# Aggregate mean |SHAP| per feature
shap_imp <- as.data.frame(shap_vals) %>% tibble::rownames_to_column("row") %>% mutate(row = as.integer(row)) %>%
  tidyr::pivot_longer(-row, names_to = "feature", values_to = "shap") %>%
  group_by(feature) %>% summarise(mean_abs_shap = mean(abs(shap), na.rm = TRUE), .groups = "drop") %>% arrange(desc(mean_abs_shap))
readr::write_csv(shap_imp, file.path(ml_dir, "shap_importance.csv"))

p_shap_df <- head(shap_imp, 15) %>% mutate(feature_label = pretty_feature(feature))
p_shap <- ggplot(p_shap_df, aes(x = reorder(feature_label, mean_abs_shap), y = mean_abs_shap)) +
  geom_col(fill = "#41AE76") + coord_flip() +
  labs(title = "Top features — mean |SHAP| (RF)", x = "Feature", y = "Mean |SHAP|") +
  theme_minimal(base_size = 11)
ggplot2::ggsave(filename = file.path(ml_dir, "shap_importance_rf.png"), plot = p_shap, width = 7, height = 5, dpi = 120)

cat(sprintf("Written %s, %s, %s, %s\n",
            file.path(ml_dir, "performance.csv"),
            file.path(ml_dir, "feature_importance.csv"),
            file.path(ml_dir, "feature_importance_rf.png"),
            file.path(ml_dir, "shap_importance_rf.png")))

# By-parameter breakdown (do not include 'param' as a feature since it's constant)
by_param_dir <- file.path(ml_dir, "by_param")
dir.create(by_param_dir, showWarnings = FALSE, recursive = TRUE)
for (pp in unique(df_raw$param)) {
  sub <- df_raw %>% filter(param == pp)
  run_block(sub, file.path(by_param_dir, as.character(pp)), include_param = FALSE)
}

# By-material (slug) breakdown (include 'param' to retain parameter effects inside a material)
if ("slug" %in% names(df_raw) && any(!is.na(df_raw$slug))) {
  by_mat_dir <- file.path(ml_dir, "by_material")
  dir.create(by_mat_dir, showWarnings = FALSE, recursive = TRUE)
  for (ss in unique(stats::na.omit(df_raw$slug))) {
    sub <- df_raw %>% filter(slug == ss)
    run_block(sub, file.path(by_mat_dir, as.character(ss)), include_param = TRUE)
  }
}

# =============================
# Advanced CV/tuning (global)
# =============================
adv_dir <- file.path(ml_dir, "advanced")
dir.create(adv_dir, showWarnings = FALSE, recursive = TRUE)

# Build features (global): system + color + param
terms_adv <- c("system", "color", "param")
form_adv <- as.formula(paste("~", paste(terms_adv, collapse = " + ")))
X_all <- model.matrix(form_adv, data = df_raw)[, -1, drop = FALSE]
y_all <- df_raw$diff_percent

# Target transform for stability
y_all_tr <- log1p(pmax(y_all, 0))

# Create stratified K folds by param
make_folds <- function(n, k = 5, strata) {
  set.seed(123)
  idx <- split(seq_len(n), strata)
  # initialize k empty lists
  folds <- vector("list", k)
  for (g in idx) {
    # shuffle group indices and split into k parts
    g <- sample(g, length(g))
    parts <- split(g, rep(1:k, length.out = length(g)))
    for (i in seq_len(k)) {
      folds[[i]] <- c(folds[[i]], parts[[i]])
    }
  }
  lapply(folds, sort)
}

n <- nrow(df_raw)
k <- min(5L, max(2L, min(table(df_raw$param))))
folds <- make_folds(n, k = k, strata = df_raw$param)

# GLMNET: tune alpha per fold, choose lambda via inner CV
alphas <- c(0, 0.25, 0.5, 0.75, 1)
cv_rows <- list()
glm_imp_list <- list()
rf_imp_list <- list()

for (fi in seq_along(folds)) {
  test_idx <- folds[[fi]]
  train_idx <- setdiff(seq_len(n), test_idx)
  Xtr <- X_all[train_idx, , drop = FALSE]
  Xte <- X_all[test_idx, , drop = FALSE]
  ytr <- y_all_tr[train_idx]
  yte <- y_all[test_idx]  # metrics on original scale

  # choose best alpha via inner cv
  best_alpha <- NULL; best_cvm <- Inf; best_fit <- NULL
  for (a in alphas) {
    cv <- cv.glmnet(Xtr, ytr, alpha = a, standardize = TRUE)
    if (min(cv$cvm) < best_cvm) {
      best_cvm <- min(cv$cvm); best_alpha <- a
      best_fit <- glmnet(Xtr, ytr, alpha = a, lambda = cv$lambda.1se, standardize = TRUE)
    }
  }
  pred_glm <- as.numeric(predict(best_fit, newx = Xte))
  pred_glm <- pmax(expm1(pred_glm), 0)
  rmse_glm <- sqrt(mean((pred_glm - yte)^2))
  mae_glm  <- mean(abs(pred_glm - yte))

  # RF tuning grid
  # For ranger, number of variables equals number of terms in the formula, not dummy-expanded columns
  p <- length(terms_adv)
  mtry_grid <- unique(pmax(1L, c(floor(sqrt(p)), floor(p/2), p)))
  min_nodes <- c(1L, 5L)
  best_rf_rmse <- Inf; best_rf <- NULL; best_mtry <- NA; best_node <- NA
  # Need a data.frame for ranger with original factors
  tr_df <- df_raw[train_idx, c("diff_percent", terms_adv), drop = FALSE]
  te_df <- df_raw[test_idx, c("diff_percent", terms_adv), drop = FALSE]
  for (mt in mtry_grid) for (mn in min_nodes) {
    rf <- ranger(as.formula(paste("diff_percent ~", paste(terms_adv, collapse = " + "))),
                 data = tr_df, num.trees = 500, mtry = mt, min.node.size = mn,
                 importance = "permutation", seed = 123)
    pr <- predict(rf, data = te_df)$predictions
    rm <- sqrt(mean((pr - te_df$diff_percent)^2))
    if (rm < best_rf_rmse) { best_rf_rmse <- rm; best_rf <- rf; best_mtry <- mt; best_node <- mn }
  }
  pr_rf <- predict(best_rf, data = te_df)$predictions
  rmse_rf_cv <- sqrt(mean((pr_rf - te_df$diff_percent)^2))
  mae_rf_cv  <- mean(abs(pr_rf - te_df$diff_percent))

  # record metrics
  cv_rows[[length(cv_rows)+1]] <- tibble(fold = fi, model = "glmnet", alpha = best_alpha, RMSE = rmse_glm, MAE = mae_glm)
  cv_rows[[length(cv_rows)+1]] <- tibble(fold = fi, model = "random_forest", mtry = best_mtry, min.node.size = best_node, RMSE = rmse_rf_cv, MAE = mae_rf_cv)

  # collect importances
  # glmnet: absolute betas
  bi <- coef(best_fit) %>% as.matrix() %>% as.data.frame() %>% tibble::rownames_to_column("feature")
  names(bi)[2] <- "coef"
  glm_imp_list[[length(glm_imp_list)+1]] <- bi %>% filter(feature != "(Intercept)") %>% mutate(importance = abs(coef)) %>% select(feature, importance)
  # rf: permutation importance
  rf_imp <- sort(best_rf$variable.importance, decreasing = TRUE)
  rf_imp_list[[length(rf_imp_list)+1]] <- tibble(feature = names(rf_imp), importance = as.numeric(rf_imp))
}

cv_df <- bind_rows(cv_rows)
readr::write_csv(cv_df, file.path(adv_dir, "cv_metrics.csv"))

agg_glm <- bind_rows(glm_imp_list) %>% group_by(feature) %>% summarise(mean_importance = mean(importance, na.rm = TRUE), .groups = "drop") %>% arrange(desc(mean_importance))
agg_rf  <- bind_rows(rf_imp_list) %>% group_by(feature) %>% summarise(mean_importance = mean(importance, na.rm = TRUE), sd_importance = sd(importance, na.rm = TRUE), .groups = "drop") %>% arrange(desc(mean_importance))
readr::write_csv(agg_glm, file.path(adv_dir, "importance_glmnet_cv.csv"))
readr::write_csv(agg_rf,  file.path(adv_dir, "importance_rf_cv.csv"))

if (nrow(agg_rf) > 0) {
  rf_cv_df <- head(agg_rf, 15) %>% mutate(feature_label = pretty_feature(feature))
  p_rf_cv <- ggplot(rf_cv_df, aes(x = reorder(feature_label, mean_importance), y = mean_importance)) +
    geom_col(fill = "#253494") + coord_flip() +
    geom_errorbar(aes(ymin = pmax(mean_importance - sd_importance, 0), ymax = mean_importance + sd_importance), width = 0.2) +
    labs(title = "RF importance (mean±sd) — CV", x = "Feature", y = "Importance (perm.)") + theme_minimal(base_size = 11)
  ggsave(file.path(adv_dir, "importance_rf_cv.png"), p_rf_cv, width = 7, height = 5, dpi = 120)
}

if (nrow(agg_glm) > 0) {
  glm_cv_df <- head(agg_glm, 15) %>% mutate(feature_label = pretty_feature(feature))
  p_glm_cv <- ggplot(glm_cv_df, aes(x = reorder(feature_label, mean_importance), y = mean_importance)) +
    geom_col(fill = "#7A0177") + coord_flip() +
    labs(title = "GLMNET importance (|beta|) — CV", x = "Feature", y = "Mean |beta|") + theme_minimal(base_size = 11)
  ggsave(file.path(adv_dir, "importance_glmnet_cv.png"), p_glm_cv, width = 7, height = 5, dpi = 120)
}

cat(sprintf("Advanced CV written: %s, %s, %s\n",
            file.path(adv_dir, "cv_metrics.csv"),
            file.path(adv_dir, "importance_rf_cv.png"),
            file.path(adv_dir, "importance_glmnet_cv.png")))
