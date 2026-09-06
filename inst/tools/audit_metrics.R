args <- commandArgs(trailingOnly = TRUE)
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
root <- normalizePath(file.path(dirname(script), "../.."), mustWork = TRUE)
output <- if (length(args)) normalizePath(args[1], mustWork = TRUE) else tempdir()
if (identical(output, normalizePath(file.path(root, "inst/benchmarks/numerical-audit"), mustWork = FALSE))) stop("The historical numerical-audit baseline must not be overwritten.")
baseline_file <- file.path(root, "inst/benchmarks/numerical-audit/audit_metrics_results.csv")
baseline_ids <- if (file.exists(baseline_file)) utils::read.csv(baseline_file, stringsAsFactors = FALSE)$id else character()
baseline_hash <- if (file.exists(baseline_file)) tools::md5sum(baseline_file) else NULL
historical_lambda_labels <- sub("^predict_cached_vs_X_response_", "", grep("^predict_cached_vs_X_response_[0-9]", baseline_ids, value = TRUE))
pkgload::load_all(root, quiet = TRUE)
functions <- c("auc", "auc_matrix", "xplus_cv_lognet", "predict.xplus", "assess", "assess.xplus", "get_predictions", "get_auc", "get_auc.xplus", "summary.xplus", "print.summary.xplus", "new_xplus", "validate_xplus", "xplus")
for (name in functions) assign(name, get(name, envir = asNamespace("xplus")), envir = globalenv())
rows <- list()
phase <- "local_source"
brief <- function(value) {
  if (inherits(value, "xplus")) value <- list(class = class(value), n_obs = nrow(value$x), n_features = ncol(value$x), cutoff = value$cutoff, n_iter = value$n_iter)
  if (is.atomic(value) && length(value) > 40) value <- if (is.numeric(value)) list(length = length(value), dim = dim(value), finite_range = if (any(is.finite(value))) range(value[is.finite(value)]) else c(NA_real_, NA_real_), nonfinite = sum(!is.finite(value)), head = head(value)) else list(length = length(value), dim = dim(value), counts = table(value, useNA = "ifany"), head = head(value))
  text <- paste(capture.output(dput(value)), collapse = " ")
  if (nchar(text) > 1800) paste0(substr(text, 1, 1800), " [truncated]") else text
}
no_nan <- function(x) if (is.list(x)) all(vapply(x, no_nan, logical(1))) else !is.numeric(x) || !any(is.nan(x))
equal <- function(x, y) no_nan(x) && isTRUE(all.equal(x, y, tolerance = 1e-10, check.attributes = FALSE))
record <- function(id, api, expected, expr, check = NULL, category = "correctness", location = "", note = "") {
  warnings <- character()
  error <- NULL
  value <- tryCatch(withCallingHandlers(force(expr), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) {
    error <<- conditionMessage(e)
    NULL
  })
  valid <- if (is.null(check)) is.null(error) && equal(value, expected) else isTRUE(check(value, error, warnings))
  if (api == "assess" && is.null(check) && is.list(expected) && isTRUE(is.na(expected$auc))) {
    valid <- valid && undefined(value$auc, error, warnings)
    note <- paste(note, "1.0.0: undefined AUC must be NA_real_ with an explanatory warning, never NaN; other losses remain defined.")
  }
  status <- if (valid) "PASS" else "FINDING"
  if (category == "observation") status <- if (is.null(error)) "OBSERVED" else "ERROR_OBSERVED"
  rows[[length(rows) + 1L]] <<- data.frame(id = id, phase = phase, api = api, status = status, category = category, expected = brief(expected), actual = if (is.null(error)) brief(value) else paste0("ERROR: ", error), warnings = paste(unique(warnings), collapse = " | "), location = location, note = note, stringsAsFactors = FALSE)
  invisible(value)
}
reject <- function(value, error, warnings) !is.null(error)
undefined <- function(value, error, warnings) is.null(error) && identical(unname(value), NA_real_) && any(grepl("AUC.*(mass|class|positive|negative)", warnings, ignore.case = TRUE))
reject_soft <- function(value, error, warnings) !is.null(error) && grepl("use two-column", error, fixed = TRUE)
reject_binary <- function(value, error, warnings) !is.null(error) && grepl("binary|0 and 1", error)
invalid <- function(id, api, expr, location, note = "") record(id, api, "Reject invalid input explicitly", expr, reject, "validation", location, note)
auc_reference <- function(y, p, w = rep(1, length(y))) {
  pos <- y * w
  neg <- (1 - y) * w
  if (sum(pos) == 0 || sum(neg) == 0) return(NA_real_)
  sum(outer(p, p, ">") * outer(pos, neg) + 0.5 * outer(p, p, "==") * outer(pos, neg)) / (sum(pos) * sum(neg))
}
metrics_reference <- function(y, p, w = rep(1, length(y)), cutoff = 0.5) {
  pp <- pmin(pmax(p, 1e-5), 1 - 1e-5)
  entropy <- function(a) ifelse(a == 0, 0, a * log(a))
  list(deviance = weighted.mean(2 * (entropy(y) + entropy(1 - y) - y * log(pp) - (1 - y) * log(1 - pp)), w), class = weighted.mean((1 - y) * (p > cutoff) + y * (p <= cutoff), w), auc = auc_reference(y, p, w), mse = weighted.mean(2 * (y - p)^2, w), mae = weighted.mean(2 * abs(y - p), w))
}
mock <- function(p, cutoff = 0.5) structure(list(pred_y = matrix(p, ncol = 1), cutoff = cutoff), class = "xplus")
y <- rep(c(0, 0, 1, 1), 3)
p <- rep(c(0.1, 0.3, 0.7, 0.9), 3)
w <- rep(c(1, 2, 3, 4), 3)
obj <- mock(p)
soft <- rep(c(0.1, 0.3, 0.7, 0.9), 3)
yt <- cbind(Class0 = 1 - y, Class1 = y)
ys <- cbind(Class0 = 1 - soft, Class1 = soft)
loc_auc <- "R/auc.R:15-20"
loc_matrix <- "R/auc_matrix.R:15-20"
loc_cv <- "R/xplus_cv_lognet.R:12-121; R/assess.xplus.R:28-48"
loc_pred <- "R/predict.xplus.R:22-66"
loc_get <- "R/get_predictions.R:19-35"
loc_valid <- "R/validate_xplus.R:13-166"
for (name in c("perfect", "reverse", "ties", "weighted", "zero_subset")) {
  pp <- switch(name, perfect = p, reverse = 1 - p, ties = rep(0.5, length(y)), weighted = p[c(1:4, 8:5, 9:12)], zero_subset = p)
  ww <- switch(name, weighted = w, zero_subset = replace(w, c(1, 7), 0), rep(1, length(y)))
  record(paste0("auc_", name), "auc", auc_reference(y, pp, ww), auc(y, pp, ww), location = loc_auc)
  record(paste0("auc_matrix_", name), "auc_matrix", auc_reference(y, pp, ww), auc_matrix(yt, pp, ww), location = loc_matrix)
}
invalid("auc_negative_weights_out_of_range", "auc", auc(c(0, 0, 1, 1), c(0.1, 0.9, 0.3, 0.7), c(-5, 0.5, 1, 1)), loc_auc, "Returns 10/9, outside the AUC range [0,1]")
record("auc_explicit_null_weights", "auc", 1, auc(y, p, NULL), location = loc_auc)
record("auc_matrix_omitted_weights", "auc_matrix", 1, auc_matrix(yt, p), location = loc_matrix)
record("auc_matrix_null_weights", "auc_matrix", 1, auc_matrix(yt, p, NULL), location = loc_matrix, note = "1.0.0: explicit NULL and omitted weights are equivalent unit weights.")
for (label in c(0, 1)) {
  record(paste0("auc_one_class_", label), "auc", "Undefined AUC: no positive-negative pairs", auc(rep(label, 12), p), undefined, "mathematically_undefined", loc_auc)
  record(paste0("assess_one_class_", label), "assess", metrics_reference(rep(label, 12), p), assess(obj, newy = rep(label, 12)), location = loc_cv, note = "Only AUC is undefined; deviance, class, mse and mae remain defined")
  record(paste0("assess_one_class_matrix_", label), "assess", metrics_reference(rep(label, 12), p), assess(obj, newy = cbind(1 - rep(label, 12), rep(label, 12))), location = loc_cv)
}
record("auc_zero_all_weights", "auc", "Undefined AUC: zero effective mass", auc(y, p, rep(0, 12)), undefined, "mathematically_undefined", loc_auc)
record("auc_zero_positive_weights", "auc", "Undefined AUC: zero positive mass", auc(y, p, 1 - y), undefined, "mathematically_undefined", loc_auc)
record("assess_zero_all_weights", "assess", "All five weighted metrics are NA_real_ with a warning when sum(weights)=0", assess(obj, newy = y, weights = rep(0, 12)), function(v, e, z) is.null(e) && identical(names(v), c("deviance", "class", "auc", "mse", "mae")) && all(vapply(v, identical, logical(1), NA_real_)) && any(grepl("zero|mass|weight", z)), "mathematically_undefined", loc_cv, "1.0.0: missing results are explicit NA_real_, not NaN; warning is part of the contract.")
set.seed(1910)
for (i in 1:10) {
  yr <- sample(rep(0:1, 12))
  pr <- sample(seq(0.05, 0.95, by = 0.05), 24, replace = TRUE)
  wr <- sample(0:5, 24, replace = TRUE)
  record(paste0("reference_binary_weighted_", i), "assess", metrics_reference(yr, pr, wr, 0.4), assess(mock(pr, 0.4), newy = yr, weights = wr), location = loc_cv)
  sr <- runif(24)
  record(paste0("reference_soft_weighted_", i), "assess", metrics_reference(sr, pr, wr, 0.4), assess(mock(pr, 0.4), newy = cbind(1 - sr, sr), weights = wr), location = loc_cv)
}
record("assess_single_class_fixed_factor_levels", "assess", metrics_reference(rep(0, 12), p), assess(obj, newy = factor(rep(0, 12), levels = 0:1)), location = loc_cv)
record("assess_binary", "assess", metrics_reference(y, p), assess(obj, newy = y), location = loc_cv)
record("assess_weighted", "assess", metrics_reference(y, p, w), assess(obj, newy = y, weights = w), location = loc_cv)
record("assess_zero_subset_weights", "assess", metrics_reference(y, p, replace(w, c(1, 7), 0)), assess(obj, newy = y, weights = replace(w, c(1, 7), 0)), location = loc_cv)
record("assess_soft_matrix", "assess", metrics_reference(soft, p, w), assess(obj, newy = ys, weights = w), location = loc_cv)
record("assess_soft_vector_four_values", "assess", "Reject soft vector with 'use two-column' guidance", assess(obj, newy = soft), reject_soft, "soft_label_contract", loc_cv, "1.0.0 selects the explicit-rejection alternative mentioned, but not actually checked, by the original audit; soft matrices remain supported.")
soft_two <- rep(c(0.2, 0.8), 6)
record("assess_soft_vector_two_values", "assess", "Reject soft vector with 'use two-column' guidance", assess(obj, newy = soft_two), reject_soft, "soft_label_contract", loc_cv, "1.0.0 rejects soft vectors instead of silently factorizing two arbitrary values into hard classes.")
record("auc_soft_matrix", "auc_matrix", auc_reference(soft, p, w), auc_matrix(ys, p, w), location = loc_matrix)
record("auc_soft_vector", "auc", "Reject nonbinary vector explicitly; soft labels require auc_matrix", auc(soft, p), reject_binary, "soft_label_contract", loc_auc, "1.0.0: rank AUC requires binary vector truth; soft masses are represented by two columns, not ordinal ranks.")
record("auc_small_sample_warning", "xplus_cv_lognet", "Keep AUC but do not claim to switch to deviance", xplus_cv_lognet(matrix(qlogis(p[1:4]), ncol = 1), y[1:4], "auc"), function(v, e, z) is.null(e) && !(v$type.measure == "auc" && any(grepl("changed to", z))), "diagnostic", loc_cv)
record("assess_cutoff_boundary", "assess", metrics_reference(y, rep(c(0.2, 0.5, 0.5, 0.8), 3)), assess(mock(rep(c(0.2, 0.5, 0.5, 0.8), 3)), newy = y), location = loc_cv)
for (cut in c(0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9)) {
  boundary_obj <- mock(rep(cut, 12), cut)
  boundary_y <- c(rep(0, 9), rep(1, 3))
  record(paste0("assess_cutoff_exact_", cut), "assess", mean(as.character(predict(boundary_obj, type = "class")) != as.character(boundary_y)), assess(boundary_obj, newy = boundary_y)$class, location = "R/predict.xplus.R:38-40,56; R/xplus_cv_lognet.R:32,68", note = "1.0.0: compare factor labels 0/1, not FALSE/TRUE; direct response thresholding must preserve equality at the cutoff without a logit/sigmoid round trip.")
}
extreme_p <- c(rep(1e-20, 6), rep(1e-18, 6))
extreme_y <- c(rep(0, 6), rep(1, 6))
record("assess_cached_extreme_auc", "assess", auc_reference(extreme_y, extreme_p), assess(mock(extreme_p), newy = extreme_y)$auc, category = "numerical_policy", location = "R/predict.xplus.R:38-40; R/xplus_cv_lognet.R:32,42", note = "1.0.0: valid unequal probabilities below 1e-15 must retain their ranks; destructive cached-link clamping previously converted them to ties.")
for (name in c("negative", "short", "NA", "NaN", "Inf")) {
  ww <- switch(name, negative = replace(w, 1, -1), short = w[1:2], "NA" = replace(w, 1, NA_real_), "NaN" = replace(w, 1, NaN), "Inf" = replace(w, 1, Inf))
  invalid(paste0("auc_weights_", name), "auc", auc(y, p, ww), loc_auc)
  invalid(paste0("matrix_weights_", name), "auc_matrix", auc_matrix(yt, p, ww), loc_matrix)
  invalid(paste0("assess_weights_", name), "assess", assess(obj, newy = y, weights = ww), loc_cv)
}
for (name in c("NA", "NaN", "Inf", "minus_Inf")) {
  bad <- switch(name, "NA" = NA_real_, "NaN" = NaN, "Inf" = Inf, minus_Inf = -Inf)
  pp <- replace(p, 1, bad)
  yy <- replace(y, 1, bad)
  invalid(paste0("auc_probability_", name), "auc", auc(y, pp), loc_auc, "1.0.0: AUC scores must be finite; NA/NaN/Inf are rejected, not omitted, regardless of whether an infinite score can define a rank mathematically.")
  invalid(paste0("auc_truth_", name), "auc", auc(yy, p), loc_auc)
  invalid(paste0("assess_probability_", name), "assess", assess(mock(pp), newy = y), loc_cv, "NA removal and clamping may hide invalid cached predictions")
  invalid(paste0("assess_truth_", name), "assess", assess(obj, newy = yy), loc_cv)
  invalid(paste0("predict_cached_response_", name), "predict", predict(mock(pp)), loc_pred, "1.0.0: invalid cached probabilities are rejected explicitly, rather than treated as an error observation.")
  invalid(paste0("predict_cached_link_", name), "predict", predict(mock(pp), type = "link"), loc_pred, "1.0.0: validate cached probabilities before logit conversion; do not clip nonfinite inputs into apparently valid scores.")
}
invalid("auc_length_mismatch", "auc", auc(y[-1], p), loc_auc)
invalid("auc_labels_outside_binary", "auc", auc(rep(c(-1, 2), 6), p), loc_auc)
record("auc_probability_outside_range", "auc", auc_reference(y, p * 3 - 1), auc(y, p * 3 - 1), location = loc_auc, note = "1.0.0 explicitly accepts finite ranking scores outside [0,1]; the original proposed range restriction was not a mathematical requirement for AUC.")
invalid("assess_length_short", "assess", assess(obj, newy = y[1:2]), loc_cv)
invalid("assess_extra_truth_column", "assess", assess(obj, newy = cbind(yt, third = 0)), loc_cv)
invalid("auc_matrix_one_column", "auc_matrix", auc_matrix(yt[, 1, drop = FALSE], p), loc_matrix)
invalid("auc_matrix_three_columns", "auc_matrix", auc_matrix(cbind(yt, third = 0), p), loc_matrix)
invalid("auc_matrix_negative_mass", "auc_matrix", auc_matrix(replace(ys, 1, -1), p), loc_matrix)
invalid("auc_matrix_mismatched_rows", "auc_matrix", auc_matrix(yt[-1, ], p), loc_matrix)
record("auc_matrix_reversed_named_columns", "auc_matrix", "Observed positional class convention: second column is positive", auc_matrix(yt[, 2:1], p), category = "observation", location = loc_matrix)
record("assess_single_row_matrix", "assess", metrics_reference(1, 0.8), assess(mock(0.8), newy = matrix(c(0, 1), 1, 2)), location = loc_cv, note = "1.0.0: a one-row two-column truth matrix keeps its shape; only AUC is undefined for a hard one-class row.")
record("get_predictions_cutoff_classes", "get_predictions", ifelse(p > 0.5, "Class1", "Class2"), as.character(get_predictions(obj, NULL, y)$predicted), location = loc_get)
record("get_predictions_probability_complements", "get_predictions", rep(1, 12), with(get_predictions(obj, NULL, y), Class1 + Class2), location = loc_get)
invalid("get_predictions_soft_truth", "get_predictions", get_predictions(obj, NULL, soft), loc_get, "Otherwise soft truth is silently converted to Class2")
invalid("get_predictions_short_truth", "get_predictions", get_predictions(obj, NULL, y[1:2]), loc_get, "data.frame recycles truth length when it divides prediction length")
invalid("get_predictions_truth_NA", "get_predictions", get_predictions(obj, NULL, replace(y, 1, NA_real_)), loc_get)
record("predict_class_documented_matrix", "predict", "One-lambda class factor with fixed levels 0/1 matching the cutoff", predict(obj, type = "class"), function(v, e, z) is.null(e) && is.factor(v) && is.null(dim(v)) && identical(levels(v), c("0", "1")) && identical(as.character(v), as.character(as.integer(p > obj$cutoff))), "documentation", loc_pred, "1.0.0: retain the historical ID, but one lambda returns a 0/1 factor; multiple lambdas return a dimension-preserving 0/1 matrix.")
invalid("predict_invalid_s_without_newx", "predict", predict(obj, s = "not_lambda"), loc_pred)
invalid("predict_cache_only_lambda_1se", "predict", predict(obj, s = "lambda.1se"), loc_pred, "1.0.0: cache-only compatibility is restricted to the default lambda.min.")
invalid("predict_cache_only_numeric_lambda", "predict", predict(obj, s = 0.1), loc_pred, "1.0.0: nondefault lambda selection needs a fitted backend and training matrix.")
invalid("get_auc_missing_newx", "get_auc", get_auc(obj, newy = y), "R/get_auc.xplus.R:25-30")
invalid("get_auc_missing_newy", "get_auc", get_auc(obj, newx = matrix(1, 12, 2)), "R/get_auc.xplus.R:25-30")
set.seed(1906)
base_x <- matrix(rnorm(120 * 2), 120, 2, dimnames = list(NULL, c("feature1", "feature2")))
base_y <- as.integer(base_x[, 1] - base_x[, 2] > 0)
base <- xplus(base_x, base_y, nfolds = 4, max_iter = 2, learning_rate = 0.5, seed = 1906)
base_n <- nrow(base_x)
record("validate_structure_only_baseline", "validate_xplus", TRUE, identical(validate_xplus(base), base), location = loc_valid, note = "1.0.0: retain the historical ID but use a genuinely fitted baseline; the former incomplete cv.glmnet mock is invalid and would make every mutation reject for the wrong reason.")
mutations <- list(pred_y_NA = list("pred_y", matrix(NA_real_, base_n, 1)), pred_y_NaN = list("pred_y", matrix(NaN, base_n, 1)), pred_y_Inf = list("pred_y", matrix(Inf, base_n, 1)), pred_y_range = list("pred_y", matrix(2, base_n, 1)), pred_y_rows = list("pred_y", matrix(0.5, 1, 1)), pred_y_character = list("pred_y", matrix("bad", base_n, 1)), cutoff_NA = list("cutoff", NA_real_), cutoff_Inf = list("cutoff", Inf), cutoff_vector = list("cutoff", c(0.2, 0.8)), cutoff_empty = list("cutoff", numeric()), cutoff_range = list("cutoff", 2), pseudo_labels_NA = list("pseudo_labels", rep(NA_real_, base_n)), pseudo_labels_range = list("pseudo_labels", rep(2, base_n)), x_NA = list("x", matrix(NA_real_, base_n, 2)), x_Inf = list("x", matrix(Inf, base_n, 2)), x_character = list("x", matrix("bad", base_n, 2)), y_NA = list("y", rep(NA_real_, base_n)), n_iter_NA = list("n_iter", NA_real_), n_iter_negative = list("n_iter", -1), n_iter_fraction = list("n_iter", 1.5), max_iter_Inf = list("max_iter", Inf), max_iter_negative = list("max_iter", -1))
for (name in names(mutations)) {
  altered <- base
  altered[[mutations[[name]][[1]]]] <- mutations[[name]][[2]]
  invalid(paste0("validate_", name), "validate_xplus", {validate_xplus(altered); "accepted"}, loc_valid, "1.0.0: one mutation at a time on the validated real fit, with matching dimensions except for the deliberate row-mismatch case; rejection must test semantic invariants.")
}
summary_expected <- list(n_obs = nrow(base$x), n_features = ncol(base$x), n_iter = base$n_iter, cutoff = base$cutoff, alpha = base$alpha, learning_rate = base$learning_rate, iterative_path = base$iterative_path, qq = base$qq, lambda_min = base$xplus$lambda.min, lambda_1se = base$xplus$lambda.1se, n_nonzero_coefficients = sum(as.matrix(base$predicted_coefficients) != 0), stop_reason = base$stop_reason, n_original_positive = sum(base_y == 1), n_original_unlabeled = sum(base_y == 0), fallback_used = base$fallback_used, fallback_reason = base$fallback_reason, final_target_range = range(base$y), n_final_soft_labels = sum(base$y > 0 & base$y < 1))
record("summary_metadata", "summary", summary_expected, unclass(summary(base))[names(summary_expected)], location = "R/summary.xplus.R:17-51", note = "1.0.0: compare semantic keys with a real fit and its identifiable attributes, not fixed cutoff/iterations/lambdas. Additional diagnostic metadata is allowed; nonzero coefficients includes the intercept.")
record("summary_print_returns_invisibly", "summary", TRUE, {z <- withVisible(print(summary(base))); !z$visible && inherits(z$value, "summary.xplus")}, location = "R/summary.xplus.R:44-54")
for (name in c("NA", "NaN", "Inf")) {
  xx <- base$x
  xx[1] <- switch(name, "NA" = NA_real_, "NaN" = NaN, "Inf" = Inf)
  invalid(paste0("xplus_input_", name), "xplus", xplus(xx, base_y, max_iter = 1), "R/xplus.R:64-68")
  yy <- base_y
  yy[1] <- switch(name, "NA" = NA_real_, "NaN" = NaN, "Inf" = Inf)
  invalid(paste0("xplus_truth_", name), "xplus", xplus(base$x, yy, max_iter = 1), "R/xplus.R:64-68")
}
invalid("xplus_soft_input_truth", "xplus", xplus(base$x, 0.1 + 0.8 * base_y, max_iter = 1), "R/xplus.R:68", "1.0.0: fitting requires binary original labels; keep the soft-vector length equal to the real baseline training rows.")
deps <- data.frame(package = c("survival", "Matrix", "tibble", "glmnet", "pkgload", "testthat", "xplus"), available = FALSE, version = NA_character_)
for (i in seq_len(nrow(deps))) {
  deps$available[i] <- requireNamespace(deps$package[i], quietly = TRUE)
  if (deps$available[i]) deps$version[i] <- as.character(getNamespaceVersion(deps$package[i]))
}
loaded <- FALSE
if (requireNamespace("glmnet", quietly = TRUE) && requireNamespace("pkgload", quietly = TRUE)) {
  rm(list = c("assess", "auc", "auc_matrix", "get_auc", "get_predictions", "xplus"), envir = globalenv())
  loaded <- record("pkgload_local_package", "dependency", TRUE, {pkgload::load_all(root, quiet = TRUE); TRUE}, location = "DESCRIPTION:20-25", note = "1.0.0 loads the devel namespace from startup, including validation helpers and S3 registration. Historical local_source/pkgload_fitted phase names are retained for comparison.")
  if (isTRUE(loaded)) for (name in c("auc", "auc_matrix", "xplus_cv_lognet", "predict.xplus", "assess", "assess.xplus", "get_predictions", "get_auc", "get_auc.xplus", "summary.xplus", "print.summary.xplus", "new_xplus", "validate_xplus", "xplus")) assign(name, get(name, envir = asNamespace("xplus")), envir = globalenv())
}
if (isTRUE(loaded)) {
  phase <- "pkgload_fitted"
  set.seed(1907)
  xx <- matrix(rnorm(160 * 5), 160, 5, dimnames = list(paste0("row", 1:160), paste0("feature", 1:5)))
  yy <- as.integer(2 * xx[, 1] - 1.5 * xx[, 2] + rnorm(160, sd = 0.3) > 0)
  fitted <- record("fit_xplus_fixture", "xplus", "A locally fitted xplus object", xplus(xx, yy, nfolds = 4, max_iter = 2, learning_rate = 0.5, seed = 1908), function(v, e, z) is.null(e) && inherits(v, "xplus"), location = "R/xplus.R:187-216")
  if (inherits(fitted, "xplus")) {
    pp <- as.numeric(predict(fitted, newx = xx))
    record("fitted_validate", "validate_xplus", TRUE, identical(validate_xplus(fitted), fitted), location = loc_valid)
    record("fitted_assess_binary", "assess", metrics_reference(yy, pp, cutoff = fitted$cutoff), assess(fitted, newx = xx, newy = yy), location = loc_cv)
    ss <- 0.1 + 0.8 * yy
    record("fitted_assess_soft_matrix", "assess", metrics_reference(ss, pp, cutoff = fitted$cutoff), assess(fitted, newx = xx, newy = cbind(1 - ss, ss)), location = loc_cv)
    record("fitted_assess_soft_vector", "assess", "Reject soft vector with 'use two-column' guidance", assess(fitted, newx = xx, newy = ss), reject_soft, "soft_label_contract", loc_cv, "1.0.0 explicitly rejects soft vectors; the adjacent two-column case retains soft-label coverage.")
    record("fitted_get_auc", "get_auc", auc_reference(yy, pp), get_auc(fitted, newx = xx, newy = yy), location = "R/get_auc.xplus.R:25-30")
    ww <- rep(1:4, 40)
    record("fitted_get_auc_weighted", "get_auc", auc_reference(yy, pp, ww), get_auc(fitted, newx = xx, newy = yy, weights = ww), location = "R/get_auc.xplus.R:25-30")
    record("fitted_get_predictions_cutoff", "get_predictions", c(positive_count = sum(pp > fitted$cutoff), mismatches = 0), {z <- as.character(get_predictions(fitted, xx, yy)$predicted); c(positive_count = sum(z == "Class1"), mismatches = sum(z != ifelse(pp > fitted$cutoff, "Class1", "Class2")))}, location = loc_get)
    record("fitted_get_predictions_default", "get_predictions", c(positive_count = sum(pp > 0.5), mismatches = 0), {z <- as.character(get_predictions(fitted, xx, yy, use_cutoff = FALSE)$predicted); c(positive_count = sum(z == "Class1"), mismatches = sum(z != ifelse(pp > 0.5, "Class1", "Class2")))}, location = loc_get)
    record("fitted_backend_class_labels", "get_predictions", "Observe backend class labels for final two-column response", unique(as.vector(predict(fitted$xplus, newx = xx, type = "class", s = "lambda.min"))), category = "observation", location = "R/xplus.R:187-190; R/get_predictions.R:27-30")
    record("fitted_get_auc_soft_vector", "get_auc", "Reject nonbinary vector truth explicitly", get_auc(fitted, newx = xx, newy = ss), reject_binary, "soft_label_contract", "R/get_auc.xplus.R:28-32", "1.0.0: get_auc delegates to binary-vector AUC; use auc_matrix for soft class masses.")
    record("fitted_get_auc_one_class", "get_auc", "Undefined AUC: one class", get_auc(fitted, newx = xx, newy = rep(1, 160)), undefined, "mathematically_undefined", "R/get_auc.xplus.R:28-30")
    record("fitted_get_auc_zero_weights", "get_auc", "Undefined AUC: zero mass", get_auc(fitted, newx = xx, newy = yy, weights = rep(0, 160)), undefined, "mathematically_undefined", "R/get_auc.xplus.R:28-30")
    lambda_big <- max(fitted$xplus$glmnet.fit$lambda)
    for (s in list("lambda.min", "lambda.1se", lambda_big, c(fitted$xplus$lambda.min, lambda_big))) {
      label <- paste(s, collapse = "_")
      if (is.numeric(s) && length(historical_lambda_labels)) {
        historical <- historical_lambda_labels[lengths(strsplit(historical_lambda_labels, "_", fixed = TRUE)) == length(s)]
        if (length(historical) == 1L) label <- historical
      }
      for (type in c("response", "link", "class")) {
        target <- predict(fitted, newx = xx, s = s, type = type)
        record(paste0("predict_cached_vs_X_", type, "_", label), "predict", c(length = length(target), mismatch = 0), {z <- predict(fitted, s = s, type = type); delta <- if (length(z) != length(target)) Inf else if (type == "class") sum(as.character(z) != as.character(target)) else max(abs(z - target)); c(length = length(z), mismatch = delta)}, function(v, e, z) is.null(e) && v[1] == length(target) && v[2] < 1e-10, "correctness", loc_pred, paste0("1.0.0: training-data predictions respect every lambda and agree with explicit newx; no cached link clipping. Historical numeric ID suffixes are retained for comparison when a baseline exists; actual s = ", paste(s, collapse = ", "), "."))
      }
    }
    lambda_pp <- as.numeric(predict(fitted, newx = xx, s = lambda_big))
    record("get_auc_forwards_lambda", "get_auc", auc_reference(yy, lambda_pp), get_auc(fitted, newx = xx, newy = yy, s = lambda_big), location = "R/get_auc.xplus.R:25-30", note = "1.0.0: s must be forwarded through ... to predict, preserving the requested single lambda.")
    record("fitted_assess_lambda", "assess", metrics_reference(yy, lambda_pp, cutoff = fitted$cutoff), assess(fitted, newx = xx, newy = yy, s = lambda_big), location = loc_cv)
    record("fitted_assess_cached_lambda", "assess", metrics_reference(yy, lambda_pp, cutoff = fitted$cutoff), assess(fitted, newy = yy, s = lambda_big), location = loc_pred)
    for (name in c("reordered", "renamed", "duplicate", "missing", "extra")) {
      changed <- switch(name, reordered = xx[, 5:1], renamed = {z <- xx; colnames(z) <- paste0("other", 1:5); z}, duplicate = {z <- xx; colnames(z)[2] <- colnames(z)[1]; z}, missing = xx[, -1], extra = cbind(xx, extra = 0))
      record(paste0("predict_columns_", name), "predict", if (name == "reordered") "Reorder by names or reject" else "Reject incompatible feature schema", {v <- as.numeric(predict(fitted, newx = changed)); c(max_abs_diff = max(abs(v - pp)), n = length(v))}, function(v, e, z) !is.null(e) || (name == "reordered" && v[1] < 1e-10), "schema_safety", loc_pred, "1.0.0: xplus validates the training feature schema before positional glmnet prediction; a permutation may be restored by names or rejected, but incompatible schemas must be rejected.")
    }
    permutation <- rev(seq_len(nrow(xx)))
    record("predict_row_permutation", "predict", pp[permutation], as.numeric(predict(fitted, newx = xx[permutation, ])), location = loc_pred)
    for (name in c("NA", "NaN", "Inf", "minus_Inf")) {
      changed <- xx
      changed[1, 1] <- switch(name, "NA" = NA_real_, "NaN" = NaN, "Inf" = Inf, minus_Inf = -Inf)
      invalid(paste0("predict_newx_", name), "predict", {v <- predict(fitted, newx = changed); c(first_prediction = v[1], nonfinite = sum(!is.finite(v)))}, loc_pred)
      invalid(paste0("assess_newx_", name), "assess", assess(fitted, newx = changed, newy = yy), loc_cv)
    }
    record("predict_positional_newx", "predict", 5L, nrow(predict(fitted, xx[1:5, , drop = FALSE])), category = "argument_safety", location = "R/predict.xplus.R:22,46-49", note = "1.0.0: newx is positional before ...; the requested five observations must be used, not the training cache.")
    record("predict_multiple_lambda_class_dimensions", "predict", c(160L, 2L), {z <- predict(fitted, newx = xx, s = c(fitted$xplus$lambda.min, lambda_big), type = "class"); stopifnot(is.matrix(z), all(z %in% 0:1)); dim(z)}, category = "documentation", location = "R/predict.xplus.R:9-10,63-65", note = "1.0.0: multiple-lambda classes retain the matrix shape and use labels 0/1, unlike the single-lambda factor.")
    invalid("predict_newx_character", "predict", predict(fitted, newx = matrix("bad", 160, 5)), loc_pred)
    invalid("predict_bad_lambda_with_X", "predict", predict(fitted, newx = xx, s = "bad"), loc_pred)
    invalid("predict_NA_lambda_with_X", "predict", predict(fitted, newx = xx, s = NA_real_), loc_pred)
    invalid("predict_NA_lambda_cached", "predict", predict(fitted, s = NA_real_), loc_pred)
    record("fitted_summary", "summary", c(n_obs = 160, n_features = 5, n_nonzero_coefficients = sum(as.matrix(fitted$predicted_coefficients) != 0)), {z <- summary(fitted); unlist(z[c("n_obs", "n_features", "n_nonzero_coefficients")])}, location = "R/summary.xplus.R:17-35")
  }
} else {
  rows[[length(rows) + 1L]] <- data.frame(id = "fitted_suite", phase = "dependency", api = "glmnet", status = "SKIP", category = "dependency", expected = "glmnet and pkgload locally available", actual = "Fitted API tests not run; no installation attempted", warnings = "", location = "DESCRIPTION:20-25", note = "Rerun this script after dependencies finish installing", stringsAsFactors = FALSE)
}
results <- do.call(rbind, rows)
utils::write.csv(results, file.path(output, "audit_metrics_results.csv"), row.names = FALSE)
utils::write.csv(deps, file.path(output, "audit_metrics_dependencies.csv"), row.names = FALSE)
print(deps, row.names = FALSE)
print(table(results$phase, results$status))
if (length(baseline_ids)) {
  missing_ids <- setdiff(baseline_ids, results$id)
  cat("BASELINE_IDS=", length(baseline_ids), " RETAINED=", sum(baseline_ids %in% results$id), " MISSING=", length(missing_ids), " ADDED=", sum(!results$id %in% baseline_ids), "\n", sep = "")
  if (length(missing_ids)) cat("MISSING_IDS=", paste(missing_ids, collapse = ","), "\n", sep = "")
  cat("BASELINE_UNCHANGED=", identical(baseline_hash, tools::md5sum(baseline_file)), "\n", sep = "")
}
for (i in which(results$status %in% c("FINDING", "ERROR_OBSERVED", "SKIP"))) cat(results$id[i], " | ", results$expected[i], " | ", results$actual[i], "\n", sep = "")
cat("RESULTS=", file.path(output, "audit_metrics_results.csv"), "\n", sep = "")
cat("DEPENDENCIES=", file.path(output, "audit_metrics_dependencies.csv"), "\n", sep = "")
cat("R=", R.version.string, "\n", sep = "")
