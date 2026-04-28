library(readxl)
library(ggplot2)

# Proste parsowanie argumentów CLI: x=, y=; akceptuje nazwy, indeksy oraz x=index
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(key, default = NULL) {
	# obsługuje formy: key=val lub --key=val
	pat1 <- paste0('^', key, '=')
	pat2 <- paste0('^--', key, '=')
	hit <- args[grepl(pat1, args) | grepl(pat2, args)]
	if (length(hit) == 0) return(default)
	sub('^[^=]*=', '', hit[[1]])
}

# Ścieżka do pliku Excel (opcjonalnie CLI: file=...), domyślnie bieżący plik
excel_file <- get_arg('file', default = "AL7075_B.xlsx")

run_for_file <- function(excel_file_current) {
	# Wczytaj cały arkusz bez nazw kolumn; 3. wiersz = parametr (etykieta Y),
	# 4. wiersz = jednostka, 5. wiersz = nagłówki kolumn, dane od 6. wiersza
	raw_sheet <- read_excel(excel_file_current, sheet = 1, col_names = FALSE)
	if (nrow(raw_sheet) < 5) stop("Arkusz ma za mało wierszy (>=5: 1-2 pomijamy, 3 parametr, 4 jednostka, 5 nagłówki)")
	header_row <- as.character(unlist(raw_sheet[5, ], use.names = FALSE))
	meta_row1 <- as.character(unlist(raw_sheet[3, ], use.names = FALSE)) # opis parametru (oś Y)
	meta_row2 <- as.character(unlist(raw_sheet[4, ], use.names = FALSE)) # jednostki parametru (oś Y)

	# Skonstruuj ramkę danych z wierszy 6..n i ustaw nazwy kolumn z wiersza 5
	dane <- as.data.frame(raw_sheet[-c(1, 2, 3, 4, 5), , drop = FALSE], stringsAsFactors = FALSE)
	colnames(dane) <- header_row

	# Etykiety z pierwszej kolumny (do osi X, gdy to nie są liczby)
	x_labels_all <- as.character(dane[[colnames(dane)[1]]])

	# Funkcja tworząca etykietę osi Y dla danej kolumny (na podstawie meta wierszy 1 i 2)
	make_ylab_for_idx <- function(idx) {
		name <- if (!is.na(meta_row1[idx]) && nzchar(meta_row1[idx])) meta_row1[idx] else header_row[idx]
		unit <- if (!is.na(meta_row2[idx]) && nzchar(meta_row2[idx])) meta_row2[idx] else ""
		if (nzchar(unit)) paste0(name, " [", unit, "]") else name
	}

	# Nazwa parametru (bez jednostki) do użycia w nazwie pliku
	param_name_for_idx <- function(idx) {
		if (!is.na(meta_row1[idx]) && nzchar(meta_row1[idx])) meta_row1[idx] else header_row[idx]
	}

	# Bazowa nazwa pliku xlsx (bez rozszerzenia) do prefiksu nazw wykresów
	xlsx_base <- tools::file_path_sans_ext(basename(excel_file_current))

	# Wyświetl nazwy kolumn
	print(colnames(dane))

	# Funkcja pomocnicza: czyści i konwertuje wektor do typu numeric
	clean_numeric <- function(v) {
		v_chr <- as.character(v)
		# zamiana przecinków na kropki, usunięcie znaków poza cyframi/znakami liczbowymi
		v_chr <- gsub(",", ".", v_chr, fixed = TRUE)
		v_chr <- gsub("[^0-9eE+\t\n\r .-]", "", v_chr)
		suppressWarnings(as.numeric(v_chr))
	}

	# Wybór kolumn x/y: domyślnie 1 i 120, z bezpiecznym zapasem jeśli brak tylu kolumn
	pick_col <- function(arg, fallbackIdx) {
		if (is.null(arg)) return(fallbackIdx)
		a <- arg
		if (tolower(a) %in% c('index', 'idx', 'i')) return('ROW_INDEX')
		if (suppressWarnings(!is.na(as.integer(a)))) {
			i <- as.integer(a)
			if (i >= 1 && i <= ncol(dane)) return(i)
		}
		# nazwa kolumny
		if (a %in% colnames(dane)) return(which(colnames(dane) == a)[1])
		warning(sprintf("Nie znaleziono kolumny '%s' – używam domyślnej.", a))
		fallbackIdx
	}

	default_x_idx <- 1
	default_y_idx <- if (ncol(dane) >= 120) 120 else ncol(dane)

	sel_x <- pick_col(x_arg, default_x_idx)
	# Nie próbuj rozwiązywać kolumny y, jeśli prosimy o y=all (unikamy ostrzeżeń)
	if (!(!is.null(y_arg) && tolower(y_arg) == 'all')) {
		sel_y <- pick_col(y_arg, default_y_idx)
	} else {
		sel_y <- default_y_idx
	}

	x_col <- if (identical(sel_x, 'ROW_INDEX')) 'ROW_INDEX' else colnames(dane)[sel_x]
	y_col <- colnames(dane)[sel_y]
	use_label_axis <- identical(x_col, 'ROW_INDEX')

	# Tryb wsadowy: rysuj dla wszystkich kolumn y (poza 1-szą etykietową)
	if (!is.null(y_arg) && tolower(y_arg) == 'all') {
		# Wymuś etykiety z kolumny 1 na osi X
		x_col <- 'ROW_INDEX'
		x_num <- seq_len(nrow(dane))
		use_label_axis <- TRUE
		if (!is.null(out_arg)) {
			cat('Uwaga: parametr out= jest ignorowany dla y=all (pliki nazwy automatycznie).\n')
		}

		# Funkcja do bezpiecznych nazw plików
		safe_name <- function(s) {
			s <- gsub("[^A-Za-z0-9._-]+", "_", s)
			s <- gsub("_+", "_", s)
			s
		}

		y_indices <- setdiff(seq_len(ncol(dane)), 1L)
		n_grid <- if (!is.null(n_arg)) {
			val <- suppressWarnings(as.integer(n_arg))
			if (is.na(val) || val < 50) 500 else min(val, 20000)
		} else {
			500
		}

		for (yi in y_indices) {
			this_y_col <- colnames(dane)[yi]
			ylab_text <- make_ylab_for_idx(yi)
			y_raw <- dane[[yi]]
			y_num <- clean_numeric(y_raw)
			df <- data.frame(x = x_num, y = y_num, label = x_labels_all)
			df <- df[is.finite(df$x) & is.finite(df$y), , drop = FALSE]
			if (nrow(df) < 5 || length(unique(df$x)) < 3) {
				next
			}
			uniq_x <- length(unique(df$x))
			if (!is.null(df_arg)) {
				degFree <- suppressWarnings(as.numeric(df_arg))
				if (is.na(degFree)) next
				degFree <- min(max(2, degFree), max(2, uniq_x - 1))
			} else {
				degFree <- min(27, max(2, uniq_x - 1))
			}

			# Dopasuj spline
			if (!is.null(spar_arg)) {
				spar_val <- suppressWarnings(as.numeric(spar_arg))
				if (is.na(spar_val)) next
				fit1 <- try(smooth.spline(df$x, df$y, spar = spar_val), silent = TRUE)
			} else {
				fit1 <- try(smooth.spline(df$x, df$y, df = degFree), silent = TRUE)
			}
			if (inherits(fit1, 'try-error')) next

			x_seq <- seq(min(df$x), max(df$x), length.out = n_grid)
			pred_fit <- predict(fit1, x = x_seq)
			x_lim <- grDevices::extendrange(range(df$x), f = 0.02)
			y_lim <- grDevices::extendrange(range(c(df$y, pred_fit$y), finite = TRUE), f = 0.05)

			# Nazwa pliku: <xlsx>_<parametr>.pdf
			out_pdf <- file.path(plots_dir, paste0(safe_name(xlsx_base), '_', safe_name(param_name_for_idx(yi)), '.pdf'))
			pdf(out_pdf, width = 8, height = 6)
			ord <- order(df$x)
			plot(NA, NA, xlim = x_lim, ylim = y_lim, main = '', xlab = '', ylab = ylab_text, xaxt = if (use_label_axis) 'n' else 's')
			if (use_label_axis) axis(1, at = df$x[ord], labels = df$label[ord], las = 2, cex.axis = 0.7)
			points(df$x[ord], df$y[ord], pch = 16)
			lines(pred_fit$x, pred_fit$y, col = 'red', lwd = 2, lty = 1)
			dev.off()
			cat('Zapisano wykres:', out_pdf, '\n')
		}
		return(invisible(TRUE))
	}

	# Tworzymy surowe wektory x i y
	if (identical(x_col, 'ROW_INDEX')) {
		x_raw <- seq_len(nrow(dane))
	} else {
		x_raw <- dane[[x_col]]
	}
	y_raw <- dane[[y_col]]

	# Diagnostyka surowych danych
	cat('Przykładowe x_raw:', utils::capture.output(print(utils::head(x_raw)))[1:1], '\n')

	# Konwersja z czyszczeniem
	if (identical(x_col, 'ROW_INDEX')) {
		x_num <- as.numeric(x_raw)
	} else {
		x_num <- clean_numeric(x_raw)
	}
	y_num <- clean_numeric(y_raw)

	# Jeśli x nie jest numeryczny (same NA), znajdź alternatywną kolumnę x
	if (!identical(x_col, 'ROW_INDEX') && all(is.na(x_num))) {
		best_idx <- NA_integer_
		best_non_na <- -1
		# Jeżeli problem dotyczy pierwszej kolumny (etykiety), nie szukamy innej kolumny x
		if (identical(colnames(dane)[1], x_col)) {
			x_col <- 'ROW_INDEX'
			x_num <- seq_len(nrow(dane))
			use_label_axis <- TRUE
			cat('Uwaga: pierwsza kolumna x to etykiety – użyto indeksu wiersza jako x (etykiety na osi X z kolumny 1).\\n')
		} else {
			for (i in seq_len(ncol(dane))) {
				if (colnames(dane)[i] == y_col) next
				xi <- clean_numeric(dane[[i]])
				non_na <- sum(is.finite(xi))
				if (non_na > best_non_na) {
					best_non_na <- non_na
					best_idx <- i
					x_num <- xi
				}
			}
			if (!is.na(best_idx) && best_non_na >= 3) {
				x_col <- colnames(dane)[best_idx]
				x_raw <- dane[[best_idx]]
				cat('Uwaga: kolumna x zmieniona automatycznie na:', x_col, ' (nie udało się użyć wskazanej)\\n')
			} else {
				# Ostateczny fallback – użyj indeksu wiersza
				x_col <- 'ROW_INDEX'
				x_num <- seq_len(nrow(dane))
				use_label_axis <- TRUE
				cat('Uwaga: brak kolumny x z liczbami – użyto indeksu wiersza jako x (etykiety na osi X z kolumny 1).\\n')
			}
		}
	}

	# Budujemy data frame i filtrujemy do skończonych wartości
	df <- data.frame(x = x_num, y = y_num, label = x_labels_all)
	df <- df[is.finite(df$x) & is.finite(df$y), , drop = FALSE]

	# Diagnostyka po konwersji
	cat('Typ x:', typeof(df$x), '\n')
	cat('Typ y:', typeof(df$y), '\n')
	cat('Pierwsze wartości x:', utils::head(df$x), '\n')
	cat('Pierwsze wartości y:', utils::head(df$y), '\n')
	cat('Użyte kolumny: x =', x_col, '; y =', y_col, '\\n')
	cat('# wierszy po filtracji:', nrow(df), '; # unikalnych x:', length(unique(df$x)), '\\n')
	cat('Parametry: df=', ifelse(is.null(df_arg), 'auto', df_arg), ', spar=', ifelse(is.null(spar_arg), 'auto', spar_arg), ', deriv=', deriv_arg, '\\n')
	if (!is.null(n_arg)) cat('Siatka predykcji n=', n_arg, '\\n')

	if (nrow(df) < 5 || length(unique(df$x)) < 3) {
		stop('Za mało poprawnych danych do narysowania i dopasowania spline (potrzeba >=5 punktów i >=3 unikalnych x).')
	}

	# Funkcja do bezpiecznych nazw plików
	safe_name <- function(s) {
		s <- gsub("[^A-Za-z0-9._-]+", "_", s)
		s <- gsub("_+", "_", s)
		s
	}

	# Dostosuj stopnie swobody do liczby unikalnych x
	uniq_x <- length(unique(df$x))
	if (!is.null(df_arg)) {
		degFree <- suppressWarnings(as.numeric(df_arg))
		if (is.na(degFree)) stop('Nieprawidłowa wartość df=')
		degFree <- min(max(2, degFree), max(2, uniq_x - 1))
	} else {
		degFree <- min(27, max(2, uniq_x - 1))
	}

	# Zapis wykresów do jednego PDF (2 strony) – domyślnie do folderu plots
	if (!is.null(out_arg)) {
		# Jeśli użytkownik podał ścieżkę z katalogiem, szanuj ją; w przeciwnym razie zapisz do plots/
		if (grepl("[/\\\\]", out_arg)) {
			out_pdf <- out_arg
		} else {
			out_pdf <- file.path(plots_dir, out_arg)
		}
	} else {
		# Domyślna nazwa: <xlsx>_<parametr>.pdf
		out_pdf <- file.path(plots_dir, paste0(safe_name(xlsx_base), '_', safe_name(param_name_for_idx(sel_y)), '.pdf'))
	}
	pdf(out_pdf, width = 8, height = 6)

	# Klasyczny wykres i spline fit (sortujemy po x dla czytelności)
	ord <- order(df$x)
	# Najpierw dopasuj spline
	if (!is.null(spar_arg)) {
		spar_val <- suppressWarnings(as.numeric(spar_arg))
		if (is.na(spar_val)) stop('Nieprawidłowa wartość spar=')
		fit1 <- smooth.spline(df$x, df$y, spar = spar_val)
	} else {
		fit1 <- smooth.spline(df$x, df$y, df = degFree)
	}
	# Rysuj po gęstej siatce, aby krzywa była płynna wizualnie
	n_grid <- if (!is.null(n_arg)) {
		val <- suppressWarnings(as.integer(n_arg))
		if (is.na(val) || val < 50) 500 else min(val, 20000)
	} else {
		500
	}
	x_seq <- seq(min(df$x), max(df$x), length.out = n_grid)
	pred_fit <- predict(fit1, x = x_seq)

	# Ustaw limity osi tak, by objąć i punkty, i spline
	x_lim <- grDevices::extendrange(range(df$x), f = 0.02)
	y_lim <- grDevices::extendrange(range(c(df$y, pred_fit$y), finite = TRUE), f = 0.05)

	# Teraz rysuj wykres z odpowiednimi limitami, a potem punkty i spline
	ylab_single <- make_ylab_for_idx(sel_y)
	plot(NA, NA, xlim = x_lim, ylim = y_lim, main = '', xlab = '', ylab = ylab_single, xaxt = if (use_label_axis) 'n' else 's')
	if (use_label_axis) {
		axis(1, at = df$x[ord], labels = df$label[ord], las = 2, cex.axis = 0.7)
	}
	points(df$x[ord], df$y[ord], pch = 16)
	lines(pred_fit$x, pred_fit$y, col = 'red', lwd = 2, lty = 1)

	# Wykres pochodnej (deriv=1) jako osobny rysunek
	if (!is.na(suppressWarnings(as.integer(deriv_arg))) && as.integer(deriv_arg) != 0) {
		pred1 <- predict(fit1, x = x_seq, deriv = suppressWarnings(as.integer(deriv_arg)))
		plot(pred1$x, pred1$y, type = 'l', col = 'blue', lwd = 2, lty = 1,
			main = '', xlab = '', ylab = '')
	}

	dev.off()
	cat('Zapisano wykresy do pliku:', out_pdf, '\\n')
}

# Driver: jeżeli file= wskazuje katalog (np. 'xlsx'), przetwarzaj wszystkie pliki w nim
# w przeciwnym razie przetwórz pojedynczy plik.

# Parametry wygładzania i wyjścia (parsujemy wcześnie, bo używane też w trybie y=all)
x_arg <- get_arg('x', default = NULL)
y_arg <- get_arg('y', default = NULL)
df_arg <- get_arg('df', default = NULL)
spar_arg <- get_arg('spar', default = NULL)
deriv_arg <- get_arg('deriv', default = '0')
out_arg <- get_arg('out', default = NULL)
n_arg <- get_arg('n', default = NULL)
cor_method <- tolower(get_arg('cor', default = 'spearman'))
meta_arg <- get_arg('meta', default = NULL)

# Folder wyjściowy na wykresy
plots_dir <- 'plots'
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

if (dir.exists(excel_file)) {
	# Tryb łączenia: jeden wykres na parametr, serie z wielu plików z legendą
	run_for_dir <- function(excel_dir) {
		files <- list.files(excel_dir, pattern = "\\.(xlsx|xls)$", full.names = TRUE, ignore.case = TRUE)
		# Pomiń pliki tymczasowe/lock (np. zaczynające się od '~$') i ukryte
		files <- files[!grepl("/~\\$|/(\\.|~\\$)", files)]
		cat('Znaleziono plików Excel w', excel_dir, ':', length(files), '\n')
		if (length(files) == 0) return(invisible(TRUE))

		# Wczytaj wszystkie pliki jako zestawy
		read_one <- function(f) {
			raw_sheet <- tryCatch(read_excel(f, sheet = 1, col_names = FALSE), error = function(e) NULL)
			if (is.null(raw_sheet) || nrow(raw_sheet) < 5) return(NULL)
			header_row <- as.character(unlist(raw_sheet[5, ], use.names = FALSE))
			meta_row1 <- as.character(unlist(raw_sheet[3, ], use.names = FALSE))
			meta_row2 <- as.character(unlist(raw_sheet[4, ], use.names = FALSE))
			dane <- as.data.frame(raw_sheet[-c(1, 2, 3, 4, 5), , drop = FALSE], stringsAsFactors = FALSE)
			colnames(dane) <- header_row
			list(
				file = f,
				base = tools::file_path_sans_ext(basename(f)),
				header = header_row,
				meta1 = meta_row1,
				meta2 = meta_row2,
				dane = dane,
				x_labels = as.character(dane[[header_row[1]]])
			)
		}
		sets <- Filter(Negate(is.null), lapply(files, read_one))
		if (length(sets) == 0) return(invisible(TRUE))

		# Funkcje pomocnicze
		safe_name <- function(s) { s <- gsub("[^A-Za-z0-9._-]+", "_", s); gsub("_+", "_", s) }
		clean_numeric <- function(v) { v <- gsub(",", ".", as.character(v), fixed = TRUE); v <- gsub("[^0-9eE+\t\n\r .-]", "", v); suppressWarnings(as.numeric(v)) }
		make_ylab_for_idx <- function(set, idx) {
			name <- if (!is.na(set$meta1[idx]) && nzchar(set$meta1[idx])) set$meta1[idx] else set$header[idx]
			unit <- if (!is.na(set$meta2[idx]) && nzchar(set$meta2[idx])) set$meta2[idx] else ""
			if (nzchar(unit)) paste0(name, " [", unit, "]") else name
		}

		# Zakres wspólnych indeksów (poza 1-szą kolumną etykiet)
		common_idx <- Reduce(intersect, lapply(sets, function(s) seq_len(ncol(s$dane))))
		common_idx <- setdiff(common_idx, 1L)

		# Jeśli użytkownik podał konkretny parametr (y != all), zawężamy do niego
		target_idx <- common_idx
		if (!is.null(y_arg) && tolower(y_arg) != 'all') {
			# próbuj po nazwie nagłówka względem pierwszego zestawu albo indeksie
			a <- y_arg
			if (suppressWarnings(!is.na(as.integer(a)))) {
				i <- as.integer(a); if (i %in% common_idx) target_idx <- i else target_idx <- integer(0)
			} else {
				hit <- which(sets[[1]]$header == a)
				if (length(hit) > 0 && hit[1] %in% common_idx) target_idx <- hit[1] else target_idx <- integer(0)
			}
		}
		if (length(target_idx) == 0) { cat('Brak wspólnych parametrów do narysowania.\n'); return(invisible(TRUE)) }

		# Ustawienia
		plots_dir <- 'plots'; if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
		n_grid <- if (!is.null(n_arg)) { v <- suppressWarnings(as.integer(n_arg)); if (is.na(v) || v < 50) 500 else min(v, 20000) } else 500

		# Kolory serii
		series_cols <- if (exists('hcl.colors', where = asNamespace('grDevices'))) grDevices::hcl.colors(length(sets), palette = 'Dark 3') else grDevices::rainbow(length(sets))

	stats_rows <- list()
	for (yi in target_idx) {
			# Przygotuj fit i predykcje dla każdej serii, aby wyznaczyć wspólne limity
			preds <- list(); data_ranges <- c(); x_ranges <- c(); 
			for (si in seq_along(sets)) {
				s <- sets[[si]]
				if (yi > ncol(s$dane)) next
				x <- seq_len(nrow(s$dane)); y <- clean_numeric(s$dane[[yi]])
				# filtruj skończone pary
				ok <- is.finite(x) & is.finite(y)
				x <- x[ok]; y <- y[ok]
				if (length(x) < 5 || length(unique(x)) < 3) { preds[[si]] <- NULL; next }
				uniq_x <- length(unique(x))
				# wybór df/spar
				if (!is.null(spar_arg)) {
					sp <- suppressWarnings(as.numeric(spar_arg)); if (is.na(sp)) { preds[[si]] <- NULL; next }
					fit <- try(smooth.spline(x, y, spar = sp), silent = TRUE)
				} else {
					if (!is.null(df_arg)) {
						dfv <- suppressWarnings(as.numeric(df_arg)); if (is.na(dfv)) { preds[[si]] <- NULL; next }
						dfv <- min(max(2, dfv), max(2, uniq_x - 1))
						fit <- try(smooth.spline(x, y, df = dfv), silent = TRUE)
					} else {
						dfv <- min(27, max(2, uniq_x - 1))
						fit <- try(smooth.spline(x, y, df = dfv), silent = TRUE)
					}
				}
				if (inherits(fit, 'try-error')) { preds[[si]] <- NULL; next }
				x_seq <- seq(min(x), max(x), length.out = n_grid)
				pr <- predict(fit, x = x_seq)
				preds[[si]] <- list(x = x, y = y, pr = pr)
				data_ranges <- c(data_ranges, y, pr$y)
				x_ranges <- c(x_ranges, x)
			}
			if (length(Filter(Negate(is.null), preds)) == 0) next

			# Limity i opis osi
			x_lim <- grDevices::extendrange(range(x_ranges), f = 0.02)
			y_lim <- grDevices::extendrange(range(data_ranges, finite = TRUE), f = 0.05)
			ylab <- make_ylab_for_idx(sets[[1]], yi)
			axis_labels <- sets[[1]]$x_labels
			axis_labels[is.na(axis_labels)] <- ''
			out_pdf <- file.path(plots_dir, paste0('combined_', safe_name(sets[[1]]$meta1[yi] %||% sets[[1]]$header[yi]), '.pdf'))

			# Wspólna siatka X do statystyk (wspólny zakres wszystkich serii)
			nonnull <- Filter(Negate(is.null), preds)
			min_start <- max(sapply(nonnull, function(p) ceiling(min(p$pr$x))))
			max_end   <- min(sapply(nonnull, function(p) floor(max(p$pr$x))))
			if (is.infinite(min_start) || is.infinite(max_end) || max_end <= min_start) {
				next
			}
			x_common <- seq(min_start, max_end, by = 1)
			if (length(x_common) < 3) next

			# Interpolacja każdej serii na wspólną siatkę
			Y <- matrix(NA_real_, nrow = length(x_common), ncol = length(nonnull))
			for (si in seq_along(nonnull)) {
				pr <- nonnull[[si]]$pr
				Y[, si] <- approx(pr$x, pr$y, xout = x_common, rule = 2)$y
			}
			# Statystyki punktowe
			mean_y <- rowMeans(Y, na.rm = TRUE)
			sd_y <- apply(Y, 1, stats::sd, na.rm = TRUE)
			# Średnia bezwzględna różnica (para-para) na punkt
			pair_abs_mean <- function(v) {
				v <- v[is.finite(v)]
				m <- length(v)
				if (m < 2) return(NA_real_)
				M <- abs(outer(v, v, '-'))
				# średnia z górnego trójkąta bez przekątnej
				mean(M[upper.tri(M)], na.rm = TRUE)
			}
			mad_point <- apply(Y, 1, pair_abs_mean)
			# Agregacja do jednej liczby na parametr
			mean_abs_diff <- mean(mad_point, na.rm = TRUE)
			mean_sd <- mean(sd_y, na.rm = TRUE)
			n_series <- ncol(Y)
			stats_rows[[length(stats_rows) + 1]] <- data.frame(
				parametr = as.character(sets[[1]]$meta1[yi] %||% sets[[1]]$header[yi]),
				n_plikow = n_series,
				n_punktow = length(x_common),
				srednia_bezwzgledna_roznica = mean_abs_diff,
				srednie_odchylenie_std = mean_sd,
				stringsAsFactors = FALSE
			)

			# Rysowanie
			pdf(out_pdf, width = 9, height = 6)
			old_par <- par(no.readonly = TRUE)
			on.exit(par(old_par), add = TRUE)
			par(mar = c(5, 4, 2, 10), xpd = NA)
			plot(NA, NA, xlim = x_lim, ylim = y_lim, main = '', xlab = '', ylab = ylab, xaxt = 'n')
			axis(1, at = seq_along(axis_labels), labels = axis_labels, las = 2, cex.axis = 0.7)
			# Pasmo +/- 1 SD wokół średniej
			band_col <- grDevices::adjustcolor('grey70', alpha.f = 0.35)
			polygon(c(x_common, rev(x_common)), c(mean_y + sd_y, rev(mean_y - sd_y)), col = band_col, border = NA)
			lines(x_common, mean_y, col = 'black', lwd = 2, lty = 2)
			# Serie
			labels <- character(0)
			for (si in seq_along(sets)) {
				pr <- preds[[si]]
				if (is.null(pr)) next
				lines(pr$pr$x, pr$pr$y, col = series_cols[si], lwd = 2)
				points(pr$x, pr$y, pch = 16, col = series_cols[si])
				labels <- c(labels, sets[[si]]$base)
			}
	     # Legenda (poza ramką wykresu, prawa krawędź)
	     legend_labels <- c('Mean ±1 SD', labels)
	     legend_cols <- c('black', series_cols[seq_along(labels)])
	     legend_lty <- c(2, rep(1, length(labels)))
	     usr <- par('usr')
	     legend_x <- usr[2] + 0.03 * diff(usr[1:2])
	     legend_y <- usr[4]
	     legend(legend_x, legend_y, legend = legend_labels, col = legend_cols, lwd = 2, lty = legend_lty,
		     pch = c(NA, rep(16, length(labels))), bty = 'n', cex = 0.8, xjust = 0, yjust = 1)
			dev.off()
			cat('Zapisano wykres:', out_pdf, '\n')
		}
		# Zapis tabeli statystyk
		if (length(stats_rows) > 0) {
			stats_df <- do.call(rbind, stats_rows)
			out_stats <- file.path(plots_dir, 'combined_stats.csv')
			utils::write.csv(stats_df, out_stats, row.names = FALSE, fileEncoding = 'UTF-8')
			cat('Zapisano statystyki:', out_stats, '\n')
		}

		# Macierz korelacji między parametrami: zbuduj jeden wektor na parametr poprzez uśrednienie po plikach na wspólnej siatce
		build_param_vector <- function(idx) {
			preds <- list()
			for (si in seq_along(sets)) {
				s <- sets[[si]]
				if (idx > ncol(s$dane)) next
				x <- seq_len(nrow(s$dane)); y <- clean_numeric(s$dane[[idx]])
				ok <- is.finite(x) & is.finite(y)
				x <- x[ok]; y <- y[ok]
				if (length(x) < 5 || length(unique(x)) < 3) next
				uniq_x <- length(unique(x))
				fit <- try({
					if (!is.null(spar_arg)) {
						sp <- suppressWarnings(as.numeric(spar_arg)); if (is.na(sp)) stop('spar')
						smooth.spline(x, y, spar = sp)
					} else if (!is.null(df_arg)) {
						dfv <- suppressWarnings(as.numeric(df_arg)); if (is.na(dfv)) stop('df')
						dfv <- min(max(2, dfv), max(2, uniq_x - 1))
						smooth.spline(x, y, df = dfv)
					} else {
						smooth.spline(x, y, df = min(27, max(2, uniq_x - 1)))
					}
				}, silent = TRUE)
				if (inherits(fit, 'try-error')) next
				pr <- predict(fit, x = seq(min(x), max(x), length.out = 800))
				preds[[length(preds) + 1]] <- pr
			}
			if (length(preds) == 0) return(NULL)
			min_start <- max(sapply(preds, function(p) ceiling(min(p$x))))
			max_end   <- min(sapply(preds, function(p) floor(max(p$x))))
			if (!is.finite(min_start) || !is.finite(max_end) || max_end <= min_start) return(NULL)
			x_common <- seq(min_start, max_end, by = 1)
			if (length(x_common) < 3) return(NULL)
			Y <- sapply(preds, function(pr) approx(pr$x, pr$y, xout = x_common, rule = 2)$y)
			rowMeans(Y, na.rm = TRUE)
		}
		param_vectors <- lapply(common_idx, build_param_vector)
		keep <- which(vapply(param_vectors, function(v) !is.null(v) && length(v) > 2, logical(1)))
		if (length(keep) >= 2) {
			# Wyrównaj długości (do minimalnej) i zbuduj macierz
			min_len <- min(vapply(param_vectors[keep], length, integer(1)))
			M <- do.call(cbind, lapply(param_vectors[keep], function(v) v[seq_len(min_len)]))
			colnames(M) <- sets[[1]]$meta1[common_idx[keep]]
			# Oblicz korelację
			method <- if (cor_method %in% c('pearson', 'spearman', 'kendall')) cor_method else 'spearman'
			C <- suppressWarnings(stats::cor(M, method = method, use = 'pairwise.complete.obs'))
			# Zapis CSV
			out_cor_csv <- file.path(plots_dir, paste0('correlation_', method, '.csv'))
			utils::write.csv(C, out_cor_csv, fileEncoding = 'UTF-8')
			cat('Zapisano macierz korelacji:', out_cor_csv, '\n')
			# Prosta heatmapa
			out_cor_pdf <- file.path(plots_dir, paste0('correlation_', method, '.pdf'))
			pdf(out_cor_pdf, width = 8, height = 8)
			op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
			par(mar = c(8, 8, 2, 2))
			image(1:ncol(C), 1:ncol(C), t(C[nrow(C):1, ]), axes = FALSE, col = colorRampPalette(c('blue','white','red'))(200), zlim = c(-1,1))
			axis(1, at = 1:ncol(C), labels = colnames(C), las = 2, cex.axis = 0.7)
			axis(2, at = 1:ncol(C), labels = rev(colnames(C)), las = 2, cex.axis = 0.7)
			box()
			title(main = paste('Correlation (', toupper(method), ')', sep=''))
			dev.off()
			cat('Zapisano heatmapę korelacji:', out_cor_pdf, '\n')
		}

		# PCA: cecha = średnia wartości wygładzonej po wspólnej siatce; wiersze = pliki, kolumny = parametry
		if (length(sets) >= 2 && length(common_idx) >= 2) {
			feature_mat <- matrix(NA_real_, nrow = length(sets), ncol = length(common_idx))
			colnames(feature_mat) <- vapply(common_idx, function(i) if (!is.na(sets[[1]]$meta1[i]) && nzchar(sets[[1]]$meta1[i])) sets[[1]]$meta1[i] else sets[[1]]$header[i], character(1))
			rownames(feature_mat) <- vapply(sets, function(s) s$base, character(1))
			for (jj in seq_along(common_idx)) {
				yi <- common_idx[jj]
				# Zbierz predykcje dla każdej serii
				preds_y <- vector('list', length(sets))
				for (si in seq_along(sets)) {
					s <- sets[[si]]
					x <- seq_len(nrow(s$dane)); y <- clean_numeric(s$dane[[yi]])
					ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
					if (length(x) < 5 || length(unique(x)) < 3) { preds_y[[si]] <- NULL; next }
					uniq_x <- length(unique(x))
					fit <- try({
						if (!is.null(spar_arg)) {
							sp <- suppressWarnings(as.numeric(spar_arg)); if (is.na(sp)) stop('spar')
							smooth.spline(x, y, spar = sp)
						} else if (!is.null(df_arg)) {
							dfv <- suppressWarnings(as.numeric(df_arg)); if (is.na(dfv)) stop('df')
							dfv <- min(max(2, dfv), max(2, uniq_x - 1))
							smooth.spline(x, y, df = dfv)
						} else {
							smooth.spline(x, y, df = min(27, max(2, uniq_x - 1)))
						}
					}, silent = TRUE)
					if (inherits(fit, 'try-error')) { preds_y[[si]] <- NULL; next }
					pr <- predict(fit, x = seq(min(x), max(x), length.out = 800))
					preds_y[[si]] <- pr
				}
				nonnull <- Filter(Negate(is.null), preds_y)
				if (length(nonnull) < 2) next
				min_start <- max(sapply(nonnull, function(p) ceiling(min(p$x))))
				max_end   <- min(sapply(nonnull, function(p) floor(max(p$x))))
				if (!is.finite(min_start) || !is.finite(max_end) || max_end <= min_start) next
				x_common <- seq(min_start, max_end, by = 1)
				if (length(x_common) < 3) next
				for (si in seq_along(sets)) {
					pr <- preds_y[[si]]
					if (is.null(pr)) next
					y_interp <- approx(pr$x, pr$y, xout = x_common, rule = 2)$y
					feature_mat[si, jj] <- mean(y_interp, na.rm = TRUE)
				}
			}
			keep_cols <- which(colSums(is.finite(feature_mat)) == nrow(feature_mat))
			if (length(keep_cols) >= 2) {
				Fm <- feature_mat[, keep_cols, drop = FALSE]
				pca <- try(stats::prcomp(Fm, center = TRUE, scale. = TRUE), silent = TRUE)
				if (!inherits(pca, 'try-error')) {
					# Zapis wyników
					scores <- as.data.frame(pca$x)
					scores$file <- rownames(Fm)
					scores <- scores[, c(ncol(scores), seq_len(ncol(scores)-1))]
					loadings <- as.data.frame(pca$rotation)
					loadings$parameter <- rownames(loadings)
					loadings <- loadings[, c(ncol(loadings), seq_len(ncol(loadings)-1))]
					# Dołącz metadane jeśli podane: CSV z kolumnami file,treatment,system (lub dowolnymi innymi)
					meta <- NULL
					if (!is.null(meta_arg) && file.exists(meta_arg)) {
						meta <- try(utils::read.csv(meta_arg, stringsAsFactors = FALSE), silent = TRUE)
						if (!inherits(meta, 'try-error') && 'file' %in% names(meta)) {
							scores <- merge(scores, meta, by = 'file', all.x = TRUE)
						}
					}
					out_scores <- file.path(plots_dir, 'pca_scores.csv')
					out_load <- file.path(plots_dir, 'pca_loadings.csv')
					utils::write.csv(scores, out_scores, row.names = FALSE, fileEncoding = 'UTF-8')
					utils::write.csv(loadings, out_load, row.names = FALSE, fileEncoding = 'UTF-8')
					cat('Zapisano PCA scores:', out_scores, '\n')
					cat('Zapisano PCA loadings:', out_load, '\n')
					# Biplot
					out_pca_pdf <- file.path(plots_dir, 'pca_biplot.pdf')
					pdf(out_pca_pdf, width = 9, height = 7)
					op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
					par(mar = c(5, 5, 2, 12), xpd = NA)
					sc <- pca$x[, 1:2, drop = FALSE]
					ld <- pca$rotation[, 1:2, drop = FALSE]
					var_exp <- summary(pca)$importance[2, 1:2]
					# skala dla strzałek
					mult <- 0.8 * min(
						diff(range(sc[, 1], finite = TRUE)) / diff(range(ld[, 1], finite = TRUE)),
						diff(range(sc[, 2], finite = TRUE)) / diff(range(ld[, 2], finite = TRUE))
					)
					# Kolory/kształty z metadanych, jeśli dostępne
					col_vec <- series_cols[seq_len(nrow(sc))]
					pch_vec <- rep(16, nrow(sc))
					if (!is.null(meta) && !inherits(meta, 'try-error')) {
						# baza nazw plików w tej samej kolejności
						base_order <- rownames(sc)
						meta_ord <- meta[match(base_order, meta$file), , drop = FALSE]
						if ('treatment' %in% names(meta_ord)) {
							# mapowanie do unikalnych kolorów
							tr <- as.factor(meta_ord$treatment)
							pal <- if (exists('hcl.colors', where = asNamespace('grDevices'))) grDevices::hcl.colors(nlevels(tr), palette = 'Set 2') else grDevices::rainbow(nlevels(tr))
							col_vec <- pal[as.integer(tr)]
						}
						if ('system' %in% names(meta_ord)) {
							sys <- as.factor(meta_ord$system)
							pchs <- c(16, 17, 15, 3, 7, 8)
							pch_vec <- pchs[((as.integer(sys) - 1) %% length(pchs)) + 1]
						}
					}
					plot(sc[,1], sc[,2], pch = pch_vec, col = col_vec,
						 xlab = paste0('PC1 (', round(100*var_exp[1], 1), '%)'),
						 ylab = paste0('PC2 (', round(100*var_exp[2], 1), '%)'))
					abline(h = 0, v = 0, col = 'grey80', lty = 3)
					text(sc[,1], sc[,2], labels = rownames(sc), pos = 3, cex = 0.8, col = col_vec)
					arrows(0, 0, ld[,1]*mult, ld[,2]*mult, length = 0.06, col = 'black')
					text(ld[,1]*mult, ld[,2]*mult, labels = rownames(ld), cex = 0.75, pos = 4)
					# Legenda: jeśli są metadane, pokaż legendę treatment/system
					usr <- par('usr')
					if (!is.null(meta) && !inherits(meta, 'try-error')) {
						leg_x <- usr[2] + 0.03*diff(usr[1:2])
						leg_y <- usr[4]
						if ('treatment' %in% names(meta)) {
							tr_all <- as.factor(meta$treatment)
							pal_all <- if (exists('hcl.colors', where = asNamespace('grDevices'))) grDevices::hcl.colors(nlevels(tr_all), palette = 'Set 2') else grDevices::rainbow(nlevels(tr_all))
							legend(leg_x, leg_y, legend = levels(tr_all), col = pal_all, pch = 16, bty = 'n', cex = 0.8, xjust = 0, yjust = 1, title = 'Treatment')
							leg_y <- leg_y - 0.08*diff(usr[3:4])
						}
						if ('system' %in% names(meta)) {
							sys_all <- as.factor(meta$system)
							pchs <- c(16, 17, 15, 3, 7, 8)
							legend(leg_x, leg_y, legend = levels(sys_all), pch = pchs[((seq_along(levels(sys_all)) - 1) %% length(pchs)) + 1], bty = 'n', cex = 0.8, xjust = 0, yjust = 1, title = 'System')
						}
					} else {
						legend(usr[2] + 0.03*diff(usr[1:2]), usr[4], legend = rownames(sc), col = col_vec,
							   pch = pch_vec, bty = 'n', cex = 0.8, xjust = 0, yjust = 1)
					}
					dev.off()
					cat('Zapisano PCA biplot:', out_pca_pdf, '\n')

					# Prosty test wariancji PC1/PC2 vs metadane (jeśli dostępne): ANOVA
					if (!is.null(meta) && !inherits(meta, 'try-error')) {
						if (all(c('file','PC1','PC2') %in% names(scores))) {
							aov_out <- list()
							if ('treatment' %in% names(scores)) {
								aov_out$treatment_PC1 <- summary(aov(PC1 ~ as.factor(treatment), data = scores))
								aov_out$treatment_PC2 <- summary(aov(PC2 ~ as.factor(treatment), data = scores))
							}
							if ('system' %in% names(scores)) {
								aov_out$system_PC1 <- summary(aov(PC1 ~ as.factor(system), data = scores))
								aov_out$system_PC2 <- summary(aov(PC2 ~ as.factor(system), data = scores))
							}
							# Zapisz p-wartości do CSV
							flat <- data.frame(
								factor = character(0), component = character(0), p_value = numeric(0), stringsAsFactors = FALSE
							)
							add_row <- function(fac, comp, summ) {
								if (is.list(summ) && length(summ) >= 1) {
									tab <- summ[[1]]
									if (!is.null(tab) && nrow(tab) >= 1) {
										p <- as.numeric(tab[1, 'Pr(>F)'])
										flat <<- rbind(flat, data.frame(factor = fac, component = comp, p_value = p))
									}
								}
							}
							if (!is.null(aov_out$treatment_PC1)) add_row('treatment', 'PC1', aov_out$treatment_PC1)
							if (!is.null(aov_out$treatment_PC2)) add_row('treatment', 'PC2', aov_out$treatment_PC2)
							if (!is.null(aov_out$system_PC1)) add_row('system', 'PC1', aov_out$system_PC1)
							if (!is.null(aov_out$system_PC2)) add_row('system', 'PC2', aov_out$system_PC2)
							out_aov <- file.path(plots_dir, 'pca_anova.csv')
							utils::write.csv(flat, out_aov, row.names = FALSE, fileEncoding = 'UTF-8')
							cat('Zapisano PCA ANOVA:', out_aov, '\n')
						}
					}
				}
			}
		}
	}
	# Mały operator pomocniczy (A lub B jeśli A jest pusty)
	`%||%` <- function(a, b) { if (!is.null(a) && !is.na(a) && nzchar(a)) a else b }
	run_for_dir(excel_file)
	quit(save = 'no')
} else {
	run_for_file(excel_file)
}