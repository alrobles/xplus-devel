#' Validate an xplus object
#'
#' @param xplus_object An object of class `"xplus"`.
#'
#' @return The validated `xplus` object.
#' @examples
#' \dontrun{
#' fit <- glmnet::cv.glmnet(matrix(rnorm(50), ncol = 5), c(rep(1, 5), rep(0, 5)), family = "binomial")
#' obj <- new_xplus(fit_xplus = fit, pred_y = matrix(0.5, 10, 1), cutoff = 0.5)
#' validate_xplus(obj)
#' }
#' @keywords internal
validate_xplus <- function(xplus_object) {
  fail <- function(message) stop(message, call. = FALSE)
  text <- function(value, name, choices = NULL) {
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        (!is.null(choices) && !value %in% choices)) {
      fail(sprintf("`%s` must be a valid character scalar.", name))
    }
  }
  probabilities <- function(value, name, n) {
    if (!is.null(dim(value)) || length(value) != n) {
      fail(sprintf("`%s` must be a probability vector with one value per observation.", name))
    }
    .xplus_probabilities(value, n)
  }
  counts <- function(value, name, n, upper = Inf) {
    if (!is.numeric(value) || is.complex(value) || !is.null(dim(value)) ||
        length(value) != n || any(!is.finite(value)) ||
        any(value < 0 | value > upper | value != round(value))) {
      fail(sprintf("`%s` must contain finite nonnegative integer counts.", name))
    }
  }
  if (!inherits(xplus_object, "xplus") || !is.list(xplus_object)) fail("Expected an `xplus` object.")
  values <- unclass(xplus_object)
  required <- c("xplus", "pred_y", "cutoff", "predicted_coefficients", "n_iter", "x", "y",
                "alpha", "learning_rate", "pseudo_labels", "iterative_path", "qq", "call",
                "max_iter", "stop_reason")
  if (anyDuplicated(names(values)) || !all(required %in% names(values))) fail("Missing or duplicated core model fields.")
  .xplus_matrix(values$x)
  n <- nrow(values$x)
  p <- ncol(values$x)
  fit <- values$xplus
  if (!inherits(fit, "cv.glmnet") || !is.list(fit) || !inherits(fit$glmnet.fit, "lognet") ||
      !identical(as.integer(fit$glmnet.fit$nobs), n) || length(fit$glmnet.fit$dim) != 2L ||
      fit$glmnet.fit$dim[1] != p) fail("`xplus` must be a binomial cv.glmnet fit matching the training dimensions.")
  if (!is.numeric(fit$lambda) || !length(fit$lambda) || any(!is.finite(fit$lambda)) || any(fit$lambda < 0)) {
    fail("The fitted lambda path must contain finite nonnegative values.")
  }
  for (name in c("lambda.min", "lambda.1se")) {
    .xplus_scalar(fit[[name]], paste0("xplus$", name), 0)
    if (!fit[[name]] %in% fit$lambda) fail("Selected lambdas must occur in the fitted lambda path.")
  }
  for (name in c("cvm", "cvsd", "nzero")) {
    if (!is.numeric(fit[[name]]) || length(fit[[name]]) != length(fit$lambda) || any(!is.finite(fit[[name]]))) {
      fail("Cross-validation statistics must be finite and match the fitted lambda path.")
    }
  }
  if (any(fit$cvsd < 0)) fail("Cross-validation standard errors cannot be negative.")
  counts(fit$nzero, "xplus$nzero", length(fit$lambda), p)
  if (!is.matrix(values$pred_y) || !identical(dim(values$pred_y), c(n, 1L))) {
    fail("`pred_y` must be an n-observation by one probability matrix.")
  }
  .xplus_probabilities(values$pred_y, n)
  .xplus_scalar(values$cutoff, "cutoff", 0, 1)
  coefficients <- values$predicted_coefficients
  if (!inherits(coefficients, "dgCMatrix") || !identical(dim(coefficients), c(p + 1L, 1L)) ||
      any(!is.finite(coefficients@x))) fail("`predicted_coefficients` must be a finite (p + 1) by one dgCMatrix.")
  methods::validObject(coefficients)
  .xplus_scalar(values$max_iter, "max_iter", 0, integer = TRUE)
  .xplus_scalar(values$n_iter, "n_iter", 0, values$max_iter, integer = TRUE)
  .xplus_scalar(values$alpha, "alpha", 0, 1)
  .xplus_scalar(values$learning_rate, "learning_rate", 0, 1)
  .xplus_scalar(values$qq, "qq", 0, 1)
  probabilities(values$y, "y", n)
  probabilities(values$pseudo_labels, "pseudo_labels", n)
  text(values$iterative_path, "iterative_path", c("current", "continuous_enhancement"))
  text(values$stop_reason, "stop_reason", c("max_iter", "label_stability", "budget_exhausted", "degenerate_labels"))
  if (values$stop_reason == "max_iter" && values$n_iter != values$max_iter) fail("`max_iter` stopping requires the configured iteration limit.")
  if (!is.null(values$final_labels)) {
    probabilities(values$final_labels, "final_labels", n)
    if (!identical(values$y, values$final_labels)) fail("`y` must be identical to the actual `final_labels` used for fitting.")
  }
  if (!is.null(values$original_y)) {
    if (!is.numeric(values$original_y) || is.complex(values$original_y)) fail("`original_y` must be numeric binary labels.")
    .xplus_binary_labels(values$original_y, n, "original_y", require_both = TRUE)
    positive <- values$original_y == 1
    if (any(values$pseudo_labels[positive] != 1) || any(values$y[positive] != 1)) {
      fail("Known original positives must remain one in proposed and final labels.")
    }
  }
  if (!is.null(values$fallback_used)) {
    if (!is.logical(values$fallback_used) || length(values$fallback_used) != 1L || is.na(values$fallback_used)) {
      fail("`fallback_used` must be a nonmissing logical scalar.")
    }
    text(values$fallback_reason, "fallback_reason")
    if (values$fallback_used != nzchar(trimws(values$fallback_reason))) fail("`fallback_reason` must be nonempty exactly when fallback is used.")
    if (!values$fallback_used && !identical(values$y, values$pseudo_labels)) {
      fail("Without fallback the actual targets must equal the proposed pseudo-labels.")
    }
  } else if (!is.null(values$fallback_reason)) {
    fail("`fallback_reason` requires `fallback_used`.")
  }
  if (!is.null(values$cv_measure)) text(values$cv_measure, "cv_measure", c("deviance", "auc"))
  if (!is.null(values$sigmoid_scale)) .xplus_scalar(values$sigmoid_scale, "sigmoid_scale", 0, lower_open = TRUE)
  if (!is.null(values$sampling)) text(values$sampling, "sampling", c("bootstrap", "unique"))
  if (!is.null(values$min_iter)) .xplus_scalar(values$min_iter, "min_iter", 1, integer = TRUE)
  if (!is.null(values$stability_window)) .xplus_scalar(values$stability_window, "stability_window", 1, integer = TRUE)
  if (!is.null(values$min_coverage)) .xplus_scalar(values$min_coverage, "min_coverage", 0, 1)
  if (!is.null(values$sampling_counts)) counts(values$sampling_counts, "sampling_counts", n, values$n_iter)
  if (!is.null(values$draw_counts)) counts(values$draw_counts, "draw_counts", n)
  if (!is.null(values$original_y)) {
    for (name in c("sampling_counts", "draw_counts")) {
      if (!is.null(values[[name]]) && any(values[[name]][positive] != 0)) fail(sprintf("`%s` must be zero for known positives.", name))
    }
  }
  if (!is.null(values$sampling_counts) && !is.null(values$draw_counts)) {
    if (any(values$draw_counts < values$sampling_counts) ||
        any((values$draw_counts == 0) != (values$sampling_counts == 0))) {
      fail("Draw counts must agree with inclusion-round counts.")
    }
  }
  if (!is.null(values$final_foldid)) {
    counts(values$final_foldid, "final_foldid", n, n)
    folds <- sort(unique(values$final_foldid))
    if (length(folds) < 3L || !all(folds == seq_along(folds))) fail("`final_foldid` must identify at least three consecutive, nonempty folds starting at one.")
  }
  if (!is.null(values$history)) {
    history <- values$history
    columns <- c("iteration", "stability", "coverage", "sample_size", "sample_draws", "cv_folds", "cv_measure", "max_label_change")
    if (!is.data.frame(history) || anyDuplicated(names(history)) || !all(columns %in% names(history)) || nrow(history) != values$n_iter) {
      fail("`history` must contain all diagnostic columns and one row per completed iteration.")
    }
    counts(history$iteration, "history$iteration", values$n_iter, values$n_iter)
    if (any(history$iteration != seq_len(values$n_iter))) fail("History iteration numbers must be consecutive starting at one.")
    for (name in c("stability", "coverage", "max_label_change")) probabilities(history[[name]], paste0("history$", name), values$n_iter)
    for (name in c("sample_size", "sample_draws", "cv_folds")) counts(history[[name]], paste0("history$", name), values$n_iter)
    if (any(history$sample_size > n | history$sample_size < 1 | history$sample_draws < history$sample_size |
            history$cv_folds < 3 | history$cv_folds > n)) fail("History sample sizes, draw counts or fold counts are inconsistent.")
    if (!is.character(history$cv_measure) || anyNA(history$cv_measure) || any(!history$cv_measure %in% c("deviance", "auc"))) {
      fail("History CV measures must be deviance or auc.")
    }
    if (identical(values$cv_measure, "deviance") && any(history$cv_measure != "deviance")) fail("Deviance CV cannot record AUC fitting rounds.")
    if (any(diff(history$coverage) < 0)) fail("History coverage must be nondecreasing.")
    if (values$n_iter > 0 && !is.null(values$original_y) && !is.null(values$sampling_counts)) {
      coverage <- mean(values$sampling_counts[!positive] > 0)
      if (abs(utils::tail(history$coverage, 1) - coverage) > 1e-12) fail("Final history coverage must agree with unlabeled inclusion counts.")
    }
    if (!is.null(values$sampling_counts) && sum(history$sample_size) != sum(values$sampling_counts)) fail("History sample sizes must sum to the recorded inclusion-round counts.")
    if (!is.null(values$draw_counts) && sum(history$sample_draws) != sum(values$draw_counts)) fail("History draw totals must sum to the recorded draw counts.")
    if (!is.null(values$original_y)) {
      if (any(history$sample_size > sum(!positive))) fail("History sample sizes cannot exceed the unlabeled population.")
      covered <- history$coverage * sum(!positive)
      if (any(abs(covered - round(covered)) > 1e-10) ||
          any(diff(c(0, covered)) > history$sample_size + 1e-10) ||
          any(covered + 1e-10 < history$sample_size)) fail("History coverage must be consistent with unique unlabeled sampling.")
    }
  }
  if (values$stop_reason == "label_stability") {
    if ((!is.null(values$min_iter) && values$n_iter < values$min_iter) ||
        (!is.null(values$stability_window) && values$n_iter < values$stability_window)) fail("Label-stability stopping precedes its minimum iteration requirements.")
    if (!is.null(values$history) && !is.null(values$min_coverage) &&
        (values$n_iter == 0 || utils::tail(values$history$coverage, 1) < values$min_coverage)) fail("Label-stability stopping requires minimum unlabeled coverage.")
  }
  xplus_object
}
