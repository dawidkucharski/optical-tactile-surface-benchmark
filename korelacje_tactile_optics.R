# Purpose: Compare tactile reference vs optics measurements for one or more parameters (default: Rp)
# Inputs:
#  - tactile: folder with Excel files (reference). Columns: material-treatment; scan rows to find a row with the parameter name (e.g., 'Rp');
#             the next N rows (default 21) are the measurements block for that parameter.
#  - optics: Excel file or folder. For optics, skip first 2 rows; row 3 contains parameter names; row 4 units;
#            techniques are in rows 5..17 (column 1), and parameter column is known (default 132) or searched by name.
# Outputs:
#  - outputs/<param>_optics_vs_tactile.csv: table with technique, optics value, tactile reference, diff, ratio, percent difference.
#  - plots/<param>_optics_vs_tactile.pdf: bar chart of optics values with reference line from tactile.
#
# CLI args:
#  Single param mode:
#    tactile=./tactile optics=./optics param=Rp tactile_block=21 tactile_col="14301 frez mon." optics_col=132 sheet=1
#  Multi-param mode (run multiple in one go):
#    params=Rp,Rv optics_cols=132,136  (order-aligned; if optics_cols missing or mismatched, falls back to header search or single optics_col)
#    Optional tactile param label override(s): tactile_param=Ra or tactile_params=Rp,Rv,"Rz","Ra [\u00B5m]",Rt
#  Limiting optics to selected files (by basename, substring, or explicit paths):
#    optics_files=fileA.xlsx,fileB.xlsx    (exact files)
#    optics_files=matA,matB                (substrings matched against basenames inside optics dir)

suppressPackageStartupMessages({
  library(readxl)
  library(ggplot2)
  suppressWarnings({
    if (requireNamespace('yaml', quietly = TRUE)) {
      # yaml available
    }
  })
})

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(key, default = NULL) {
  pat1 <- paste0('^', key, '=')
  pat2 <- paste0('^--', key, '=')
  hit <- args[grepl(pat1, args) | grepl(pat2, args)]
  if (length(hit) == 0) return(default)
  sub('^[^=]*=', '', hit[[1]])
}

# Args and defaults
TACTILE_DIR <- get_arg('tactile', 'tactile')
OPTICS_PATH <- get_arg('optics', 'optics')
PARAM_NAME  <- get_arg('param', 'Rp')
# Tactile-related selectors
TACTILE_BLOCK <- suppressWarnings(as.integer(get_arg('tactile_block', '21')))
TACTILE_COL_NAME <- get_arg('tactile_col', '')
TACTILE_COL_CONTAINS <- get_arg('tactile_col_contains', '')
TACTILE_COL_IDX <- suppressWarnings(as.integer(get_arg('tactile_col_idx', NA_character_)))
OPTICS_COL <- suppressWarnings(as.integer(get_arg('optics_col', '132')))
PARAMS_ARG <- get_arg('params', NULL)
OPTICS_COLS_ARG <- get_arg('optics_cols', NULL)
OPTICS_FILES_ARG <- get_arg('optics_files', NULL)
TACTILE_PARAM_SINGLE <- get_arg('tactile_param', NULL)
TACTILE_PARAMS_VEC_ARG <- get_arg('tactile_params', NULL)
SHEET_IDX <- suppressWarnings(as.integer(get_arg('sheet', '1')))
USE_ABS <- {
  ua <- get_arg('use_abs', '1');
  if (is.null(ua)) FALSE else tolower(ua) %in% c('1','true','yes','y')
}

# Parallelization config
PARALLEL_MODE <- tolower(trimws(as.character(get_arg('parallel', 'files'))))  # 'files' or 'off'
CORES <- suppressWarnings(as.integer(get_arg('cores', NA_character_)))
if (is.na(CORES) || !is.finite(CORES) || CORES < 1) {
  n_cores <- 1L
  # Try to detect cores on Unix-like systems
  if (.Platform$OS.type == 'unix') {
    dc <- try(parallel::detectCores(logical = TRUE), silent = TRUE)
    if (!inherits(dc, 'try-error') && is.finite(dc) && dc >= 2) n_cores <- max(1L, dc - 1L)
  }
  CORES <- n_cores
}

# Safe parallel map across files (fork on Unix; fallback to serial otherwise)
parallel_map <- function(X, FUN, cores = CORES) {
  if (length(X) == 0) return(invisible(list()))
  use_parallel <- (.Platform$OS.type == 'unix') && is.finite(cores) && cores > 1 && PARALLEL_MODE != 'off'
  if (use_parallel) {
    parallel::mclapply(X, FUN, mc.cores = min(cores, length(X)))
  } else {
    lapply(X, FUN)
  }
}

# Default optics column indices for common parameters
get_default_optics_col_for_param <- function(p) {
  switch(tolower(trimws(as.character(p))),
         'rp' = 130L,
         'rv' = 134L,
         'rz' = 138L,
         'ra' = 150L,
         'rt' = 146L,
         'rz1max' = 174L,
         OPTICS_COL)
}

# IO dirs
PLOTS_DIR <- 'plots'; if (!dir.exists(PLOTS_DIR)) dir.create(PLOTS_DIR, recursive = TRUE, showWarnings = FALSE)
OUT_DIR <- 'outputs'; if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Helpers
clean_numeric <- function(v) {
  v_chr <- as.character(v)
  v_chr <- gsub(',', '.', v_chr, fixed = TRUE)
  v_chr <- gsub("[^0-9eE+\t\n\r .-]", "", v_chr)
  suppressWarnings(as.numeric(v_chr))
}
normalize_label <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub('[^a-z0-9]+', ' ', x)
  gsub(' +', ' ', x)
}

# Safe slug for filenames
make_slug <- function(x) {
  if (is.null(x) || !nzchar(x)) return('unknown')
  s <- tolower(as.character(x))
  s <- tryCatch(iconv(s, from = '', to = 'ASCII//TRANSLIT'), error = function(e) s)
  s <- gsub('[^a-z0-9]+', '_', s)
  s <- gsub('_+', '_', s)
  s <- gsub('^_|_$', '', s)
  if (!nzchar(s)) 'unknown' else s
}

list_excel_files <- function(path) {
  if (!dir.exists(path)) return(character(0))
  files <- list.files(path, pattern = "\\.(xlsx|xls)$", full.names = TRUE, ignore.case = TRUE)
  files <- files[!grepl('/~[$]|/([.]|~[$])', files)]
  files
}

# Resolve explicit optics file selections from CLI (supports absolute/relative paths or substrings)
parse_optics_files <- function(base_path, arg) {
  if (is.null(arg) || !nzchar(arg)) return(character(0))
  toks <- trimws(strsplit(arg, ',')[[1]])
  toks <- toks[nzchar(toks)]
  if (!length(toks)) return(character(0))
  out <- character(0)
  for (t in toks) {
    t_exp <- path.expand(t)
    if (file.exists(t_exp)) {
      out <- c(out, normalizePath(t_exp, winslash = '/', mustWork = FALSE))
      next
    }
    if (dir.exists(base_path)) {
      # try as basename inside optics dir
      cand <- file.path(base_path, t)
      if (file.exists(cand)) {
        out <- c(out, normalizePath(cand, winslash = '/', mustWork = FALSE))
      } else {
        # substring match within optics dir
        all <- list_excel_files(base_path)
        hits <- all[grepl(t, basename(all), ignore.case = TRUE)]
        out <- c(out, hits)
      }
    } else {
      # base_path is a file; search its directory
      d <- dirname(base_path)
      cand2 <- file.path(d, t)
      if (file.exists(cand2)) {
        out <- c(out, normalizePath(cand2, winslash = '/', mustWork = FALSE))
      } else {
        all <- list.files(d, pattern = "\\.(xlsx|xls)$", full.names = TRUE, ignore.case = TRUE)
        all <- all[!grepl('/~[$]|/([.]|~[$])', all)]
        hits2 <- all[grepl(t, basename(all), ignore.case = TRUE)]
        out <- c(out, hits2)
      }
    }
  }
  unique(out)
}

find_param_block_start <- function(df_char, param) {
  # Prefer matching parameter row in the FIRST column (tactile sheets typically list params in col 1)
  target <- normalize_label(param)
  if (!nzchar(target)) return(NA_integer_)
  pat <- paste0('(^| )', target, '( |$)')
  # 1) Strong preference: first column match
  col1 <- normalize_label(df_char[[1]])
  idx1 <- which(col1 == target | grepl(pat, col1))
  if (length(idx1)) return(idx1[1])
  # 2) Fallback: scan all columns row-wise
  for (i in seq_len(nrow(df_char))) {
    row_vals <- normalize_label(unlist(df_char[i, , drop = TRUE]))
    if (any(row_vals == target, na.rm = TRUE)) return(i)
    if (any(grepl(pat, row_vals))) return(i)
  }
  NA_integer_
}

find_col_by_name <- function(headers, target_name) {
  h_norm <- normalize_label(headers)
  t_norm <- normalize_label(target_name)
  hit <- which(h_norm == t_norm)
  if (length(hit)) return(hit[1])
  # fallback: contains
  hit2 <- which(grepl(t_norm, h_norm, fixed = TRUE))
  if (length(hit2)) return(hit2[1])
  NA_integer_
}

choose_tactile_col <- function(headers) {
  # 1) explicit index
  if (!is.na(TACTILE_COL_IDX) && TACTILE_COL_IDX >= 1 && TACTILE_COL_IDX <= length(headers)) return(TACTILE_COL_IDX)
  # 2) exact/contains by tactile_col
  idx <- find_col_by_name(headers, TACTILE_COL_NAME)
  if (!is.na(idx)) return(idx)
  # 3) contains helper
  if (!is.null(TACTILE_COL_CONTAINS) && nzchar(TACTILE_COL_CONTAINS)) {
    h_norm <- normalize_label(headers)
    t_norm <- normalize_label(TACTILE_COL_CONTAINS)
    hit <- which(grepl(t_norm, h_norm, fixed = TRUE))
    if (length(hit)) return(hit[1])
  }
  NA_integer_
}

# Flexible tactile column chooser with optional exact/contains overrides
choose_tactile_col2 <- function(headers, exact_name = NULL, contains = NULL, idx = NULL) {
  if (!is.null(idx) && is.finite(idx) && idx >= 1 && idx <= length(headers)) return(idx)
  h_norm <- normalize_label(headers)
  if (!is.null(exact_name) && nzchar(exact_name)) {
    t_norm <- normalize_label(exact_name)
    hit <- which(h_norm == t_norm)
    if (length(hit)) return(hit[1])
  }
  if (!is.null(contains) && nzchar(contains)) {
    t_norm2 <- normalize_label(contains)
    hit2 <- which(grepl(t_norm2, h_norm, fixed = TRUE))
    if (length(hit2)) return(hit2[1])
  }
  NA_integer_
}

read_tactile_reference <- function(dir_path, param, block_len, col_name = NULL, col_contains = NULL, sheet = 1) {
  files <- list_excel_files(dir_path)
  if (length(files) == 0) {
    message('No tactile Excel files in: ', dir_path)
    return(NULL)
  }
  # Helper: choose best-matching column by material/process basename
  pick_tactile_col_by_mat <- function(headers, mat_base, explicit_name = NULL, explicit_contains = NULL, idx = NULL) {
    # 0) explicit index/name/contains if provided
    hit <- choose_tactile_col2(headers, exact_name = explicit_name, contains = explicit_contains, idx = idx)
    if (!is.na(hit)) return(hit)
    # 1) scoring by similarity to mat_base
    h <- as.character(headers)
    h_norm <- normalize_label(h)
    mb_norm <- normalize_label(mat_base)
    # numeric core
    num_core <- paste0(unlist(regmatches(mb_norm, gregexpr('[0-9]+', mb_norm))), collapse = ' ')
    # last token (e.g., B, GLA)
    toks <- strsplit(mb_norm, ' +')[[1]]
    last_tok <- if (length(toks)) toks[length(toks)] else ''
    # additional synonym: AU_<last_tok>
    syn1 <- if (nzchar(last_tok)) normalize_label(paste0('AU_', toupper(last_tok))) else ''
    syn2 <- if (nzchar(last_tok)) normalize_label(paste('AU', toupper(last_tok))) else ''
    score <- rep(0L, length(h_norm))
    # full string match gets 3
    score <- score + ifelse(grepl(mb_norm, h_norm, fixed = TRUE), 3L, 0L)
    # numeric core presence gets 2
    if (nzchar(num_core)) score <- score + ifelse(grepl(num_core, h_norm, fixed = TRUE), 2L, 0L)
    # last token presence gets 1
    if (nzchar(last_tok)) score <- score + ifelse(grepl(paste0('(^| )', last_tok, '( |$)'), h_norm), 1L, 0L)
    # synonyms
    if (nzchar(syn1)) score <- score + ifelse(grepl(syn1, h_norm, fixed = TRUE), 1L, 0L)
    if (nzchar(syn2)) score <- score + ifelse(grepl(syn2, h_norm, fixed = TRUE), 1L, 0L)
    # Avoid selecting obvious parameter columns (rp, rv, rz, ra, rt, rz1max) by downweighting them heavily
    is_param_col <- h_norm %in% c('rp','rv','rz','ra','rt','rz1max')
    score[is_param_col] <- score[is_param_col] - 10L
    best <- which.max(score)
    if (length(best) && length(score) && max(score, na.rm = TRUE) > 0) best else NA_integer_
  }
  # Helper: score header similarity for all columns
  score_headers_all <- function(headers, mat_base) {
    h <- as.character(headers)
    h_norm <- normalize_label(h)
    mb_norm <- normalize_label(mat_base)
    num_core <- paste0(unlist(regmatches(mb_norm, gregexpr('[0-9]+', mb_norm))), collapse = ' ')
    toks <- strsplit(mb_norm, ' +')[[1]]
    last_tok <- if (length(toks)) toks[length(toks)] else ''
    syn1 <- if (nzchar(last_tok)) normalize_label(paste0('AU_', toupper(last_tok))) else ''
    syn2 <- if (nzchar(last_tok)) normalize_label(paste('AU', toupper(last_tok))) else ''
    score <- rep(0L, length(h_norm))
    score <- score + ifelse(grepl(mb_norm, h_norm, fixed = TRUE), 3L, 0L)
    if (nzchar(num_core)) score <- score + ifelse(grepl(num_core, h_norm, fixed = TRUE), 2L, 0L)
    if (nzchar(last_tok)) score <- score + ifelse(grepl(paste0('(^| )', last_tok, '( |$)'), h_norm), 2L, 0L)
    if (nzchar(syn1)) score <- score + ifelse(grepl(syn1, h_norm, fixed = TRUE), 1L, 0L)
    if (nzchar(syn2)) score <- score + ifelse(grepl(syn2, h_norm, fixed = TRUE), 1L, 0L)
    is_param_col <- h_norm %in% c('rp','rv','rz','ra','rt','rz1max')
    score[is_param_col] <- score[is_param_col] - 10L
    score
  }
  all_vals <- numeric(0)
  per_file <- data.frame(file = character(0), n = integer(0), mean = numeric(0), stringsAsFactors = FALSE)
  header_debug_written <- FALSE
  col_label <- NULL
  for (f in files) {
    raw <- try(readxl::read_excel(f, sheet = sheet, col_names = FALSE), silent = TRUE)
    if (inherits(raw, 'try-error') || nrow(raw) < (block_len + 2)) next
    # 1) Find parameter row in the FIRST column over the whole sheet
    raw_chr <- as.data.frame(lapply(raw, as.character), stringsAsFactors = FALSE)
    p_row <- find_param_block_start(raw_chr, param)
    if (is.na(p_row)) next
    # 2) The next row is the header row with material/process names
    header_row <- p_row + 1L
    if (header_row > nrow(raw)) next
    header <- as.character(unlist(raw[header_row, ], use.names = FALSE))
    # Dump headers of first tactile file to help debugging
    if (!header_debug_written) {
      dbg <- data.frame(index = seq_along(header), header = header, stringsAsFactors = FALSE)
      dbg_file <- file.path(OUT_DIR, 'tactile_headers_first_file.tsv')
      if (!file.exists(dbg_file)) {
        utils::write.table(dbg, file = dbg_file, sep = '\t', row.names = FALSE)
      }
      header_debug_written <- TRUE
    }
    # 3) Data block starts after the header row
    start_row <- header_row + 1L
    end_row <- min(nrow(raw), start_row + block_len - 1L)
    if ((end_row - start_row) < 0) next
    dat <- as.data.frame(raw[seq(start_row, end_row), , drop = FALSE], stringsAsFactors = FALSE)
    colnames(dat) <- header
    # 4) Pick the correct material column using headers similarity and numeric content, restricted to 2..60
    hdr_scores <- score_headers_all(colnames(dat), mat_base = if (!is.null(col_contains) && nzchar(col_contains)) col_contains else '')
    upper <- min(60L, ncol(dat))
    cand_cols <- if (upper >= 2L) 2L:upper else integer(0)
    if (!length(cand_cols)) next
    if (max(hdr_scores[cand_cols], na.rm = TRUE) <= 0) next
    content_counts <- sapply(cand_cols, function(j) {
      vals_j <- clean_numeric(dat[, j])
      sum(is.finite(vals_j))
    })
    names(content_counts) <- cand_cols
    combined <- (hdr_scores[cand_cols] * 10L) + content_counts
    combined[content_counts < 10] <- combined[content_counts < 10] - 100L
    if (!is.null(col_name) && nzchar(col_name)) {
      jx <- choose_tactile_col2(colnames(dat), exact_name = col_name)
      if (!is.na(jx)) combined[as.character(jx)] <- combined[as.character(jx)] + 1000L
    }
    if (!is.null(col_contains) && nzchar(col_contains)) {
      jc <- choose_tactile_col2(colnames(dat), contains = col_contains)
      if (!is.na(jc)) combined[as.character(jc)] <- combined[as.character(jc)] + 200L
    }
    col_idx <- NA_integer_
    if (length(combined)) {
      best_idx <- as.integer(names(which.max(combined)))
      if (length(best_idx) && is.finite(best_idx)) col_idx <- best_idx
    }
    if (is.na(col_idx)) col_idx <- choose_tactile_col2(colnames(dat), exact_name = col_name, contains = col_contains, idx = TACTILE_COL_IDX)
    if (is.na(col_idx)) col_idx <- choose_tactile_col(colnames(dat))
    if (is.na(col_idx)) next
    if (is.null(col_label) && col_idx >= 1 && col_idx <= ncol(dat)) {
      col_label <- colnames(dat)[col_idx]
    }
    vals <- clean_numeric(dat[, col_idx])
    vals <- vals[is.finite(vals)]
    if (length(vals)) {
      all_vals <- c(all_vals, vals)
      per_file <- rbind(per_file, data.frame(file = basename(f), n = length(vals), mean = mean(vals), stringsAsFactors = FALSE))
    }
  }
  if (!length(all_vals)) return(NULL)
  m <- mean(all_vals)
  s <- stats::sd(all_vals)
  n <- length(all_vals)
  se <- if (is.finite(s) && n > 1) s / sqrt(n) else NA_real_
  list(reference = m, sd = s, n = n, se = se, details = per_file, col_label = col_label)
}

read_optics_values <- function(path_or_dir, param, optics_col = 132, tech_rows = 5:17, tech_name_col = 1, sheet = 1) {
  files <- if (dir.exists(path_or_dir)) list_excel_files(path_or_dir) else path_or_dir
  # If user provided explicit optics files selection, restrict to those
  if (!is.null(OPTICS_FILES_ARG) && nzchar(OPTICS_FILES_ARG)) {
    selected <- parse_optics_files(path_or_dir, OPTICS_FILES_ARG)
    if (length(selected) > 0) files <- selected
  }
  if (length(files) == 0 || (is.character(files) && length(files) == 1 && !file.exists(files))) {
    message('No optics Excel files in: ', path_or_dir)
    return(NULL)
  }
  out_all <- list()
  idx_out <- 1L
  for (f in if (length(files) > 1) files else c(files)) {
    raw <- try(readxl::read_excel(f, sheet = sheet, col_names = FALSE), silent = TRUE)
    if (inherits(raw, 'try-error')) next
    # Determine column: if valid index provided, trust it; otherwise search param name in row 3
    col_idx <- optics_col
    if (is.na(col_idx) || col_idx < 1 || col_idx > ncol(raw)) {
      row3 <- as.character(unlist(raw[3, , drop = TRUE]))
      hits <- which(normalize_label(row3) == normalize_label(param))
      if (length(hits)) col_idx <- hits[1] else next
    }
    # Dump row 3 headers for the first processed optics file
    if (idx_out == 1L) {
      row3 <- as.character(unlist(raw[3, , drop = TRUE]))
      dbg <- data.frame(index = seq_along(row3), row3 = row3, stringsAsFactors = FALSE)
      utils::write.table(dbg, file = file.path(OUT_DIR, paste0('optics_row3_headers_', tools::file_path_sans_ext(basename(f)), '.tsv')),
                         sep = '\t', row.names = FALSE)
    }
    # Techniques and values
    tech_names <- as.character(unlist(raw[tech_rows, tech_name_col, drop = TRUE]))
    vals <- clean_numeric(unlist(raw[tech_rows, col_idx, drop = TRUE]))
    df <- data.frame(technique = tech_names, value = vals, stringsAsFactors = FALSE)
    df$file <- basename(f)
    out_all[[idx_out]] <- df
    idx_out <- idx_out + 1L
  }
  if (length(out_all) == 0) return(NULL)
  do.call(rbind, out_all)
}

run_for_param <- function(param_name, optics_col_idx, tactile_param_name = NULL) {
  # Tactile parameter label
  tp <- if (!is.null(tactile_param_name) && nzchar(tactile_param_name)) tactile_param_name else param_name

  # Optics values for this parameter
  opt <- read_optics_values(OPTICS_PATH, param_name, optics_col = optics_col_idx, sheet = SHEET_IDX)
  if (is.null(opt) || nrow(opt) == 0) {
    message('No optics values found. Check optics path/column/param.')
    return(invisible(NULL))
  }

  # Choose metric (signed or absolute percent diff) and filename suffix
  metric_col <- if (USE_ABS) 'diff_percent_abs' else 'diff_percent'
  metric_suffix <- if (USE_ABS) '_abs' else ''

  # Extract system_color from first column
  extract_sys_col <- function(s) {
    ss <- tolower(trimws(as.character(s)))
    # Prefer the LAST occurrence (closest to .mnt) like ..._x10_conf_green_5x5.mnt
    pat <- '(fusion|conf|fv|int)[ _-]+(blue|green|red|white)'
    mm <- gregexpr(pat, ss, perl = TRUE)
    ms <- regmatches(ss, mm)[[1]]
    if (length(ms) >= 1 && !identical(ms[1], character(0))) {
      last <- ms[length(ms)]
      hit2 <- regexec(pat, last, perl = TRUE)
      cap <- regmatches(last, hit2)[[1]]
      if (length(cap) >= 3) {
        sys <- cap[2]; col <- cap[3]
        sys <- if (sys == 'fv') 'FV' else sys
        return(c(system = sys, color = col))
      }
    }
    # secondary: attempt end-focused match near .mnt
    m_end <- regexec('(fusion|conf|fv|int)[ _-]+(blue|green|red|white)[^a-z]*\\.mnt\\s*$', ss, perl = TRUE)
    hit_end <- regmatches(ss, m_end)[[1]]
    if (length(hit_end) >= 3) {
      sys <- hit_end[2]; col <- hit_end[3]
      sys <- if (sys == 'fv') 'FV' else sys
      return(c(system = sys, color = col))
    }
    # fallback: find color and system anywhere (may be less specific)
    col_hit <- regmatches(ss, regexpr('(blue|green|red|white)', ss, perl = TRUE))
    sys_hit <- regmatches(ss, regexpr('(fusion|conf|fv|int)', ss, perl = TRUE))
    col <- if (length(col_hit) && nzchar(col_hit)) col_hit else 'unknown'
    sys <- if (length(sys_hit) && nzchar(sys_hit)) sys_hit else 'other'
    if (identical(sys, 'fv')) sys <- 'FV'
    c(system = sys, color = col)
  }

  optics_files <- unique(opt$file)

  worker <- function(of) {
    # Derive material/process name from optics filename and compute tactile reference for this file
    MAT_BASE <- tools::file_path_sans_ext(basename(of))
    ref <- read_tactile_reference(TACTILE_DIR, tp, TACTILE_BLOCK,
                                  col_name = TACTILE_COL_NAME,
                                  col_contains = MAT_BASE,
                                  sheet = SHEET_IDX)
    if (is.null(ref)) {
      message('Reference not computed (tactile) for optics file: ', MAT_BASE, ' — check tactile column match (try tactile_col or tactile_col_contains).')
      return(invisible(NULL))
    }
    message(sprintf('Reference %s (tactile mean) for %s = %.6f', param_name, MAT_BASE, ref$reference))

    opt_f <- opt[opt$file == of, , drop = FALSE]
    # Build comparison table for this optics file
  res <- data.frame(
      technique = opt_f$technique,
      optics_value = opt_f$value,
      tactile_reference = ref$reference,
      diff = opt_f$value - ref$reference,
      ratio = opt_f$value / ref$reference,
      diff_percent = 100 * (opt_f$value - ref$reference) / ref$reference,
      optics_file = opt_f$file,
      stringsAsFactors = FALSE
    )
  # Add tactile summary columns (useful for verification)
  res$tactile_col_label <- if (!is.null(ref$col_label)) ref$col_label else NA_character_
  res$tactile_n  <- if (!is.null(ref$n)) ref$n else NA_integer_
  res$tactile_sd <- if (!is.null(ref$sd)) ref$sd else NA_real_
  res$tactile_se <- if (!is.null(ref$se)) ref$se else NA_real_
    res$diff_abs <- abs(res$diff)
    res$diff_percent_abs <- abs(res$diff_percent)

    tmp <- t(vapply(res$technique, extract_sys_col, character(2)))
    res$system <- tmp[, 'system']
    res$color  <- tmp[, 'color']
    res$label  <- paste0(res$system, '_', res$color)  # system_color with underscore

    # Ordering by wavelength (blue < green < red < white < unknown) and within color by system order
    color_order <- c('blue','green','red','white','unknown')
    system_order <- c('fusion','conf','FV','int','other')
    res$color <- factor(res$color, levels = color_order)
    res$system <- factor(res$system, levels = system_order)
    ord <- order(as.integer(res$color), as.integer(res$system))
    res$label <- factor(res$label, levels = unique(res$label[ord]))

    # Material/process slug from the optics filename (basename without extension)
    MAT_SLUG <- make_slug(tools::file_path_sans_ext(basename(of)))

    # Save CSV per optics file
    out_csv <- file.path(OUT_DIR, paste0(tolower(param_name), metric_suffix, '_', MAT_SLUG, '_optics_vs_tactile.csv'))
    utils::write.csv(res, out_csv, row.names = FALSE, fileEncoding = 'UTF-8')
    message('Saved comparison CSV: ', out_csv)

    # Plot per optics file
    plot_file <- file.path(PLOTS_DIR, paste0(tolower(param_name), metric_suffix, '_', MAT_SLUG, '_optics_vs_tactile.pdf'))
    pdf(plot_file, width = 11, height = 6)
    g <- ggplot(res, aes(x = label, y = optics_value, fill = color)) +
      {
        if (!is.null(ref$se) && is.finite(ref$se) && !is.null(ref$n) && ref$n > 1) {
          annotate('rect', xmin = -Inf, xmax = Inf,
                   ymin = ref$reference - 2*ref$se, ymax = ref$reference + 2*ref$se,
                   fill = 'grey80', alpha = 0.35)
        }
      } +
      geom_col() +
      geom_hline(yintercept = ref$reference, linetype = 'dashed', color = 'black') +
      {
        if (USE_ABS) {
          geom_text(aes(label = sprintf('%.1f%%', diff_percent_abs)), vjust = -0.4, size = 3)
        } else {
          geom_text(aes(label = sprintf('%+.1f%%', diff_percent)), vjust = -0.4, size = 3)
        }
      } +
      scale_fill_manual(values = c(blue = '#2C7FB8', green = '#41ab5d', red = '#e34a33', white = '#d9d9d9', unknown = '#969696')) +
      labs(
        x = '',
        y = paste0(param_name, ' [µm]'),
        title = paste0(param_name, ' optics vs tactile reference', if (USE_ABS) ' (labels: |diff|%)' else ' (labels: diff%)'),
        subtitle = paste0('Material/process: ', tools::file_path_sans_ext(basename(of))),
        fill = 'Color'
      ) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = margin(10, 20, 10, 10))
    print(g)
    dev.off()
    message('Saved comparison plot: ', plot_file)

    # Advanced analysis for this optics file
    {
      dfm <- res
      dfm <- dfm[is.finite(dfm[[metric_col]]), c(metric_col,'system','color','label')]
      dfm$system <- droplevels(dfm$system)
      dfm$color  <- droplevels(dfm$color)
      names(dfm)[1] <- 'metric'
      if (nrow(dfm) == 0) {
        message('No finite ', if (USE_ABS) '|diff|%' else 'diff%', ' values; skipping ANOVA/regression/heatmap for ', MAT_SLUG, '.')
      } else {
        # ANOVA
        fit_aov <- try(stats::lm(metric ~ system + color, data = dfm), silent = TRUE)
        if (!inherits(fit_aov, 'try-error')) {
          aov_tab <- stats::anova(fit_aov)
          out_aov <- file.path(OUT_DIR, paste0(tolower(param_name), metric_suffix, '_', MAT_SLUG, '_anova_main_effects.csv'))
          utils::write.csv(as.data.frame(aov_tab), out_aov, row.names = TRUE, fileEncoding = 'UTF-8')
          message('Saved ANOVA (system + color): ', out_aov)
          # Effect sizes (eta^2)
          aov_df <- as.data.frame(aov_tab)
          if (all(c('Sum Sq') %in% colnames(aov_df))) {
            ss_total <- sum(aov_df$`Sum Sq`, na.rm = TRUE)
            eff <- data.frame(
              term = rownames(aov_df),
              eta2 = if (ss_total > 0) aov_df$`Sum Sq` / ss_total else NA_real_,
              stringsAsFactors = FALSE
            )
            out_eff <- file.path(OUT_DIR, paste0(tolower(param_name), metric_suffix, '_', MAT_SLUG, '_anova_effect_sizes.csv'))
            utils::write.csv(eff, out_eff, row.names = FALSE, fileEncoding = 'UTF-8')
            message('Saved ANOVA effect sizes (eta^2): ', out_eff)
          }
        } else {
          message('ANOVA model failed to fit for ', MAT_SLUG, '.')
        }

        # Regression vs wavelength (exclude white)
        wavelength_map <- c(blue = 450, green = 525, red = 635)
        dfm$wavelength <- as.numeric(wavelength_map[as.character(dfm$color)])
        dfm2 <- dfm[is.finite(dfm$wavelength), , drop = FALSE]
        if (nrow(dfm2) >= 5) {
          fit_lm <- try(stats::lm(metric ~ system + wavelength, data = dfm2), silent = TRUE)
          if (!inherits(fit_lm, 'try-error')) {
            out_lm <- file.path(OUT_DIR, paste0(tolower(param_name), metric_suffix, '_', MAT_SLUG, '_lm_system_wavelength.txt'))
            base::writeLines(capture.output(summary(fit_lm)), con = out_lm, useBytes = TRUE)
            message('Saved regression summary (system + wavelength): ', out_lm)
            # Correlations
            pear <- suppressWarnings(stats::cor(dfm2$metric, dfm2$wavelength, method = 'pearson', use = 'complete.obs'))
            spear <- suppressWarnings(stats::cor(dfm2$metric, dfm2$wavelength, method = 'spearman', use = 'complete.obs'))
            out_cor <- file.path(OUT_DIR, paste0(tolower(param_name), metric_suffix, '_', MAT_SLUG, '_corr_wavelength.txt'))
            base::writeLines(c(
              paste0('Pearson r (', if (USE_ABS) '|diff|%' else 'diff%', ' vs wavelength): ', sprintf('%.4f', pear)),
              paste0('Spearman rho (', if (USE_ABS) '|diff|%' else 'diff%', ' vs wavelength): ', sprintf('%.4f', spear))
            ), con = out_cor, useBytes = TRUE)
            message('Saved correlation with wavelength: ', out_cor)

            # NEW: per-system slope (per nm) of metric vs wavelength
            # Compute slope, SE and 95% CI for each system separately (exclude white)
            sys_levels <- unique(droplevels(dfm2$system))
            slopes <- lapply(as.character(sys_levels), function(sys) {
              d <- dfm2[dfm2$system == sys & is.finite(dfm2$wavelength) & is.finite(dfm2$metric), c('wavelength','metric')]
              d <- d[complete.cases(d), , drop = FALSE]
              # Need at least 2 distinct wavelengths to estimate slope
              if (nrow(d) < 2 || length(unique(d$wavelength)) < 2) return(NULL)
              m <- try(stats::lm(metric ~ wavelength, data = d), silent = TRUE)
              if (inherits(m, 'try-error')) return(NULL)
              co <- try(stats::coef(summary(m)), silent = TRUE)
              if (inherits(co, 'try-error') || nrow(co) < 2) return(NULL)
              slope <- unname(co['wavelength', 'Estimate'])
              se    <- unname(co['wavelength', 'Std. Error'])
              tval  <- unname(co['wavelength', 't value'])
              pval  <- unname(co['wavelength', 'Pr(>|t|)'])
              lo <- slope - 1.96 * se
              hi <- slope + 1.96 * se
              data.frame(system = sys, n_points = nrow(d), slope_per_nm = slope, slope_se = se, ci_low = lo, ci_high = hi, p_value = pval, stringsAsFactors = FALSE)
            })
            slopes <- do.call(rbind, slopes)
            if (!is.null(slopes) && nrow(slopes) > 0) {
              # Save CSV
              slopes_file <- file.path(OUT_DIR, paste0(tolower(param_name), metric_suffix, '_', MAT_SLUG, '_slope_by_system_vs_nm.csv'))
              utils::write.csv(slopes, slopes_file, row.names = FALSE, fileEncoding = 'UTF-8')
              message('Saved slopes by system (per nm): ', slopes_file)
              # Plot slopes with 95% CI
              slopes_plot <- file.path(PLOTS_DIR, paste0(tolower(param_name), metric_suffix, '_', MAT_SLUG, '_slope_by_system_vs_nm.pdf'))
              grDevices::pdf(slopes_plot, width = 7, height = 5)
              gp_sl <- ggplot(slopes, aes(x = factor(system, levels = levels(dfm2$system)), y = slope_per_nm)) +
                geom_hline(yintercept = 0, linetype = 'dotted', color = 'grey40') +
                geom_point(size = 3) +
                geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15) +
                labs(
                  title = paste0(param_name, ' slope vs wavelength (per nm) by system'),
                  subtitle = paste0('Material/process: ', tools::file_path_sans_ext(basename(of))),
                  x = 'System',
                  y = if (USE_ABS) 'Slope |diff| [%] per nm' else 'Slope diff [%] per nm'
                ) +
                theme_minimal(base_size = 12)
              print(gp_sl)
              dev.off()
            } else {
              message('Not enough data to compute per-system slopes for ', MAT_SLUG, '.')
            }
          } else {
            message('Regression model failed to fit for ', MAT_SLUG, '.')
          }
        } else {
          message('Not enough data for wavelength regression for ', MAT_SLUG, ' (need >=5 non-white points).')
        }

        # Technique distance heatmap based on |difference in %|
        vals <- stats::setNames(dfm$metric, as.character(dfm$label))
        tech_labels <- names(vals)
        if (length(vals) >= 2) {
          dm <- abs(outer(vals, vals, FUN = function(a, b) a - b))
          rownames(dm) <- tech_labels
          colnames(dm) <- tech_labels
          out_dm <- file.path(OUT_DIR, paste0(tolower(param_name), metric_suffix, '_', MAT_SLUG, '_tech_distance_abs_pct.csv'))
          utils::write.csv(dm, out_dm, row.names = TRUE, fileEncoding = 'UTF-8')
          message('Saved technique distance matrix: ', out_dm)
          mm <- as.data.frame(as.table(dm), stringsAsFactors = FALSE)
          colnames(mm) <- c('tech_x','tech_y','abs_delta_pct')
          hm_file <- file.path(PLOTS_DIR, paste0(tolower(param_name), metric_suffix, '_', MAT_SLUG, '_tech_distance_heatmap.pdf'))
          grDevices::pdf(hm_file, width = 8, height = 7)
          gh <- ggplot(mm, aes(x = tech_x, y = tech_y, fill = abs_delta_pct)) +
            geom_tile() +
            scale_fill_gradient(low = '#f7fbff', high = '#08306b', name = '|diff| [%]') +
            labs(
              title = paste0(param_name, ' technique distance heatmap (|diff|%)'),
              subtitle = paste0('Material/process: ', tools::file_path_sans_ext(basename(of))),
              x = '', y = ''
            ) +
            theme_minimal(base_size = 11) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), legend.position = 'right', plot.margin = margin(10, 20, 10, 10))
          print(gh)
          dev.off()
          message('Saved technique distance heatmap: ', hm_file)
        }
      }
    }

    # Visualization of ANOVA factors (per optics file)
    {
      dfm <- res
      dfm <- dfm[is.finite(dfm[[metric_col]]), c(metric_col,'system','color','label')]
      dfm$system <- droplevels(dfm$system)
      dfm$color  <- droplevels(dfm$color)
      names(dfm)[1] <- 'metric'
      if (nrow(dfm) > 0) {
        summarise_vec <- function(x) {
          n <- length(x); m <- mean(x); s <- stats::sd(x)
          se <- if (n > 1) s / sqrt(n) else NA_real_
          c(n = n, mean = m, sd = s, se = se)
        }

        # By system
        sys_ag <- aggregate(dfm$metric, by = list(system = dfm$system), FUN = summarise_vec)
        if (nrow(sys_ag)) {
          vals <- sys_ag$x
          M <- if (is.list(vals)) do.call(rbind, vals) else if (is.matrix(vals)) vals else if (is.data.frame(vals)) as.matrix(vals) else matrix(NA_real_, nrow = 0, ncol = 4, dimnames = list(NULL, c('n','mean','sd','se')))
          colnames(M) <- if (!is.null(colnames(M)) && all(c('n','mean','sd','se') %in% colnames(M))) colnames(M) else c('n','mean','sd','se')
          sys_ag$n <- as.numeric(M[, 'n']); sys_ag$mean <- as.numeric(M[, 'mean']); sys_ag$sd <- as.numeric(M[, 'sd']); sys_ag$se <- as.numeric(M[, 'se'])
          sys_file <- file.path(OUT_DIR, paste0(tolower(param_name), metric_suffix, '_', MAT_SLUG, '_effects_by_system.csv'))
          utils::write.csv(sys_ag[, c('system','n','mean','sd','se')], sys_file, row.names = FALSE, fileEncoding = 'UTF-8')
          sys_plot <- file.path(PLOTS_DIR, paste0(tolower(param_name), metric_suffix, '_', MAT_SLUG, '_effects_by_system.pdf'))
          grDevices::pdf(sys_plot, width = 7, height = 5)
          gp <- ggplot(sys_ag, aes(x = system, y = mean)) +
            geom_hline(yintercept = 0, linetype = 'dotted', color = 'grey40') +
            geom_point(size = 3) +
            geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.15, na.rm = TRUE) +
            labs(title = paste0(param_name, ' effect by system'), subtitle = paste0('Material/process: ', tools::file_path_sans_ext(basename(of))), x = 'System', y = if (USE_ABS) 'Mean |diff| [%] ±95% CI' else 'Mean diff [%] ±95% CI') +
            theme_minimal(base_size = 12)
          print(gp)
          dev.off()
        }

        # By color
        col_ag <- aggregate(dfm$metric, by = list(color = dfm$color), FUN = summarise_vec)
        if (nrow(col_ag)) {
          vals2 <- col_ag$x
          M2 <- if (is.list(vals2)) do.call(rbind, vals2) else if (is.matrix(vals2)) vals2 else if (is.data.frame(vals2)) as.matrix(vals2) else matrix(NA_real_, nrow = 0, ncol = 4, dimnames = list(NULL, c('n','mean','sd','se')))
          colnames(M2) <- if (!is.null(colnames(M2)) && all(c('n','mean','sd','se') %in% colnames(M2))) colnames(M2) else c('n','mean','sd','se')
          col_ag$n <- as.numeric(M2[, 'n']); col_ag$mean <- as.numeric(M2[, 'mean']); col_ag$sd <- as.numeric(M2[, 'sd']); col_ag$se <- as.numeric(M2[, 'se'])
          col_file <- file.path(OUT_DIR, paste0(tolower(param_name), metric_suffix, '_', MAT_SLUG, '_effects_by_color.csv'))
          utils::write.csv(col_ag[, c('color','n','mean','sd','se')], col_file, row.names = FALSE, fileEncoding = 'UTF-8')
          col_plot <- file.path(PLOTS_DIR, paste0(tolower(param_name), metric_suffix, '_', MAT_SLUG, '_effects_by_color.pdf'))
          grDevices::pdf(col_plot, width = 7, height = 5)
          gp2 <- ggplot(col_ag, aes(x = color, y = mean, fill = color)) +
            geom_hline(yintercept = 0, linetype = 'dotted', color = 'grey40') +
            geom_col(width = 0.6) +
            geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.15, na.rm = TRUE) +
            scale_fill_manual(values = c(blue = '#2C7FB8', green = '#41ab5d', red = '#e34a33', white = '#d9d9d9', unknown = '#969696'), guide = 'none') +
            labs(title = paste0(param_name, ' effect by color'), subtitle = paste0('Material/process: ', tools::file_path_sans_ext(basename(of))), x = 'Color', y = if (USE_ABS) 'Mean |diff| [%] ±95% CI' else 'Mean diff [%] ±95% CI') +
            theme_minimal(base_size = 12)
          print(gp2)
          dev.off()
        }

        # Scatter diff% vs wavelength (non-white)
        wavelength_map <- c(blue = 450, green = 525, red = 635)
        dfm$wavelength <- as.numeric(wavelength_map[as.character(dfm$color)])
        dfm_sc <- dfm[is.finite(dfm$wavelength), , drop = FALSE]
        if (nrow(dfm_sc) >= 2) {
          sc_plot <- file.path(PLOTS_DIR, paste0(tolower(param_name), metric_suffix, '_', MAT_SLUG, '_diff_vs_wavelength.pdf'))
          grDevices::pdf(sc_plot, width = 7, height = 5)
          gs <- ggplot(dfm_sc, aes(x = wavelength, y = metric, color = system)) +
            geom_hline(yintercept = 0, linetype = 'dotted', color = 'grey40') +
            geom_point(size = 2.8) +
            geom_smooth(method = 'lm', se = TRUE, color = 'black', linetype = 'dashed') +
            scale_x_continuous(breaks = c(450, 525, 635)) +
            labs(title = paste0(param_name, if (USE_ABS) ' |diff|% vs wavelength' else ' diff% vs wavelength'),
                 subtitle = paste0('Material/process: ', tools::file_path_sans_ext(basename(of))),
                 x = 'Wavelength [nm]', y = if (USE_ABS) '|diff| [%]' else 'diff [%]') +
            theme_minimal(base_size = 12)
          print(gs)
          dev.off()
        }
      }
    }
    invisible(TRUE)
  }

  # Execute per optics file, possibly in parallel
  invisible(parallel_map(optics_files, worker, cores = CORES))

  invisible(TRUE)
}

# Decide which parameters to run
# 1) CLI params override
if (!is.null(PARAMS_ARG) && nzchar(PARAMS_ARG)) {
  params <- trimws(strsplit(PARAMS_ARG, ',')[[1]])
  params <- params[nzchar(params)]
  # optics cols vector
  if (!is.null(OPTICS_COLS_ARG) && nzchar(OPTICS_COLS_ARG)) {
    oc_vec <- trimws(strsplit(OPTICS_COLS_ARG, ',')[[1]])
    oc_vec <- suppressWarnings(as.integer(oc_vec))
  } else {
  # Use default mapping when optics_cols not provided
  oc_vec <- vapply(params, get_default_optics_col_for_param, integer(1))
  }
  # pad or trim to match length
  if (length(oc_vec) < length(params)) oc_vec <- c(oc_vec, rep(NA_integer_, length(params) - length(oc_vec)))
  if (length(oc_vec) > length(params)) oc_vec <- oc_vec[seq_along(params)]
  # tactile param labels vector
  if (!is.null(TACTILE_PARAMS_VEC_ARG) && nzchar(TACTILE_PARAMS_VEC_ARG)) {
    tp_vec <- trimws(strsplit(TACTILE_PARAMS_VEC_ARG, ',')[[1]])
    # Remove surrounding quotes if present
    tp_vec <- gsub('^"|"$', '', tp_vec)
  } else if (!is.null(TACTILE_PARAM_SINGLE) && nzchar(TACTILE_PARAM_SINGLE)) {
    tp_vec <- rep(TACTILE_PARAM_SINGLE, length(params))
  } else {
    tp_vec <- params
  }
  if (length(tp_vec) < length(params)) tp_vec <- c(tp_vec, rep('', length(params) - length(tp_vec)))
  if (length(tp_vec) > length(params)) tp_vec <- tp_vec[seq_along(params)]
  # Run for each
  for (i in seq_along(params)) {
    run_for_param(params[[i]], oc_vec[[i]], tactile_param_name = tp_vec[[i]])
  }
} else {
  # 2) Try config/params.yaml if present
  cfg_params_file <- file.path('config', 'params.yaml')
  if (file.exists(cfg_params_file)) {
    cfg <- tryCatch({
      if (requireNamespace('yaml', quietly = TRUE)) yaml::read_yaml(cfg_params_file) else NULL
    }, error = function(e) NULL)
    if (!is.null(cfg) && !is.null(cfg$params)) {
      params <- as.character(cfg$params)
      params <- params[nzchar(params)]
      oc_vec <- NULL
      if (!is.null(cfg$optics_cols)) {
        oc_vec <- suppressWarnings(as.integer(cfg$optics_cols))
      }
      if (is.null(oc_vec) || !length(oc_vec)) {
        oc_vec <- vapply(params, get_default_optics_col_for_param, integer(1))
      }
      if (length(oc_vec) < length(params)) oc_vec <- c(oc_vec, rep(NA_integer_, length(params) - length(oc_vec)))
      if (length(oc_vec) > length(params)) oc_vec <- oc_vec[seq_along(params)]
      tp_vec <- if (!is.null(cfg$tactile_params)) as.character(cfg$tactile_params) else params
      if (length(tp_vec) < length(params)) tp_vec <- c(tp_vec, rep('', length(params) - length(tp_vec)))
      if (length(tp_vec) > length(params)) tp_vec <- tp_vec[seq_along(params)]
      for (i in seq_along(params)) {
        run_for_param(params[[i]], oc_vec[[i]], tactile_param_name = tp_vec[[i]])
      }
      quit(save = 'no')
    }
  }
  # Single param mode
  # If optics_col not provided, use default mapping for the parameter
  optics_idx <- OPTICS_COL
  if (is.na(optics_idx) || !is.finite(optics_idx)) optics_idx <- get_default_optics_col_for_param(PARAM_NAME)
  run_for_param(PARAM_NAME, optics_idx, tactile_param_name = if (!is.null(TACTILE_PARAM_SINGLE)) TACTILE_PARAM_SINGLE else NULL)
}
