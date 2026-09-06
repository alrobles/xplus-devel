#' Fit an xplus model
#'
#' Fit a PLUS-derived model for positive-unlabeled learning.
#'
#' @param x Finite numeric feature matrix with at least two columns.
#' @param y Binary vector where `1` indicates known positives and `0` indicates unlabeled samples; factors are interpreted by their labels.
#' @param alpha Elastic-net mixing parameter in `[0, 1]`, not the sigmoid scale in the PLUS paper.
#' @param sample_use_time Unlabeled-sampling budget inherited from the reference implementation:
#'   maximum number of completed sampling rounds containing each unlabeled case,
#'   not the number of bootstrap copies within a round.
#' @param learning_rate Pseudo-label smoothing rate in `(0, 1]`; values below one
#'   retain the package's global-update, hard-thresholded enhancement path.
#' @param qq Quantile used to define the positive-reference cutoff.
#' @param verbose Logical; print iterative progress messages.
#' @param nfolds Requested CV folds, an integer at least three; reduced for small classes.
#' @param max_iter Maximum number of pseudo-labeling iterations.
#' @param convergence_threshold Required stability score in `(0, 1]`, evaluated before learning-rate damping.
#' @param seed Optional nonnegative integer seed, isolated from the caller's RNG stream.
#' @param sigmoid_scale,degenerate_threshold Positive sigmoid scale and nonnegative residual-clamping tolerance.
#' @details
#' Core PLUS behavior alternates between fitting penalized logistic models on known
#' positives plus sampled unlabeled cases, anchoring predictions to a positive
#' quantile cutoff, and iteratively relabeling unlabeled samples.
#' @param min_iter,stability_window Minimum iterations and consecutive stable iterations required before declaring convergence.
#' `xplus` retains two iterative paths, distinct from the paper's pseudocode:
#' `"current"` (`learning_rate = 1`) uses sampled Bernoulli labels, while
#' `"continuous_enhancement"` (`learning_rate < 1`) smooths probabilities and
#' updates all unlabeled hard labels. Both use soft final fitting targets.
#' Stability compares the undamped mapped scores with the current pseudo-labels
#' across all unlabeled cases and requires a full window and sampling coverage.
#' @param min_coverage Minimum fraction of unlabeled cases sampled before convergence, in `[0, 1]`.
#' Bootstrap multiplicities are represented as case weights, keeping duplicate
#' copies together in CV. `sampling = "unique"` retains legacy deduplication.
#' Final-fit fallback is based on effective class mass, not thresholded labels,
#' and is recorded with the actual fitting targets and the iteration history.
#' @param sampling,cv_measure Sampling convention (`"bootstrap"` or `"unique"`) and CV criterion (`"deviance"` or `"auc"`); deviance is the default for both fitting stages.
#' @return An object of class `"xplus"` containing predictions, original and final labels, pseudo-labels, fallback metadata, sampling counts, and history.
#' @references Zhou et al. (2022). doi:10.1371/journal.pcbi.1009956
#' @seealso [predict.xplus()], [summary.xplus()], [assess.xplus()]
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(200 * 10), ncol = 10)
#' y <- c(rep(1, 40), rep(0, 160))
#' fit <- xplus(x, y, max_iter = 20)
#' @export
xplus <- function(x, y, alpha = 1, sample_use_time = 30, learning_rate = 1,
                  qq = 0.1, verbose = FALSE, nfolds = 4, max_iter = 10000,
                  convergence_threshold = 0.9, seed = NULL, sigmoid_scale = 10,
                  min_iter = 5, stability_window = 5, min_coverage = 0.9,
                  sampling = c("bootstrap", "unique"), cv_measure = c("deviance", "auc"),
                  degenerate_threshold = 1e-6) {
  this.call <- match.call()
  x <- .xplus_matrix(x)
  y <- .xplus_binary_labels(y, nrow(x), require_both = TRUE)
  .xplus_scalar(alpha, "alpha", 0, 1)
  .xplus_scalar(sample_use_time, "sample_use_time", 1, .Machine$integer.max, integer = TRUE)
  .xplus_scalar(learning_rate, "learning_rate", 0, 1, lower_open = TRUE)
  .xplus_scalar(qq, "qq", 0, 1)
  .xplus_scalar(nfolds, "nfolds", 3, .Machine$integer.max, integer = TRUE)
  .xplus_scalar(max_iter, "max_iter", 1, .Machine$integer.max, integer = TRUE)
  .xplus_scalar(convergence_threshold, "convergence_threshold", 0, 1, lower_open = TRUE)
  .xplus_scalar(sigmoid_scale, "sigmoid_scale", 0, Inf, lower_open = TRUE)
  .xplus_scalar(min_iter, "min_iter", 1, .Machine$integer.max, integer = TRUE)
  .xplus_scalar(stability_window, "stability_window", 1, .Machine$integer.max, integer = TRUE)
  .xplus_scalar(min_coverage, "min_coverage", 0, 1)
  .xplus_scalar(degenerate_threshold, "degenerate_threshold", 0)
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  sampling <- match.arg(sampling)
  cv_measure <- match.arg(cv_measure)
  n <- nrow(x)
  original_y <- y
  positive_id <- which(y == 1L)
  unlabeled_id <- which(y == 0L)
  if (min(length(positive_id), length(unlabeled_id)) < 3L) {
    stop("At least three positive and three unlabeled observations are required for cross-validation.", call. = FALSE)
  }
  variances <- apply(x, 2, stats::var)
  if (any(!is.finite(variances))) stop("Feature variance is non-finite; rescale `x` before fitting.", call. = FALSE)
  if (!any(variances > 0)) stop("All feature columns have zero variance at numerical precision; rescale or change `x`.", call. = FALSE)
  if (!is.null(seed)) {
    .xplus_scalar(seed, "seed", 0, .Machine$integer.max, integer = TRUE)
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else
      rm(list = ".Random.seed", envir = .GlobalEnv), add = TRUE)
    set.seed(seed)
  }
  pseudo_labels <- as.numeric(y)
  iterative_path <- if (learning_rate < 1) "continuous_enhancement" else "current"
  remaining <- rep(as.numeric(sample_use_time), n)
  remaining[positive_id] <- 0
  sampling_counts <- draw_counts <- integer(n)
  history <- list()
  stability_scores <- numeric()
  # Residual clamping is an explicit application-level tolerance, not machine epsilon.
  pseudo_label_cutoff <- 0.5
  n_iter <- 0L
  # If no early stopping condition triggers, fitting stops at max_iter.
  stop_reason <- "max_iter"
  for (i in seq_len(max_iter)) {
    available <- unlabeled_id[remaining[unlabeled_id] > 0]
    if (!length(available)) {
      stop_reason <- "budget_exhausted"
      break
    }
    valid_sample <- FALSE
    for (attempt in seq_len(20L)) {
      draws <- available[sample.int(length(available), length(positive_id), replace = TRUE, prob = remaining[available])]
      sample_id <- unique(draws)
      fit_id <- c(positive_id, sample_id)
      class_counts <- table(factor(y[fit_id], levels = 0:1))
      if (min(class_counts) >= 3L) {
        valid_sample <- TRUE
        break
      }
    }
    # Stratification keeps both classes in every cross-validation training split.
    # Three unique members per class support three folds with two training members.
    # Bounded retries avoid treating a single unlucky bootstrap draw as collapse.
    if (!valid_sample) {
      stop_reason <- "degenerate_labels"
      if (verbose) message("Stopping: sampled labels cannot support stratified CV after 20 attempts.")
      break
    }
    frequency <- tabulate(draws, nbins = n)
    weights <- if (sampling == "bootstrap") c(rep(1, length(positive_id)), frequency[sample_id]) else rep(1, length(fit_id))
    folds <- as.integer(min(nfolds, min(class_counts)))
    foldid <- stratified_foldid(y[fit_id], folds)
    step <- .xplus_cv_fit(x[fit_id, , drop = FALSE], y[fit_id], foldid, alpha, cv_measure, weights)
    fit_pi <- step$fit
    pred_y <- .xplus_probabilities(stats::predict(fit_pi, newx = x, s = "lambda.min", type = "response"), n)
    cutoff <- stats::quantile(pred_y[positive_id], qq, names = FALSE)
    map_pred_y <- normalize_residuals(pred_y - cutoff, degenerate_threshold)
    mapped <- as.numeric(stats::plogis(sigmoid_scale * map_pred_y))
    update_indices <- if (iterative_path == "continuous_enhancement") unlabeled_id else sample_id
    before <- pseudo_labels
    residuals <- abs(mapped[unlabeled_id] - before[unlabeled_id])
    pseudo_labels[update_indices] <- learning_rate * mapped[update_indices] + (1 - learning_rate) * before[update_indices]
    y[update_indices] <- if (iterative_path == "continuous_enhancement")
      as.integer(pseudo_labels[update_indices] >= pseudo_label_cutoff) else
      stats::rbinom(length(update_indices), 1, pseudo_labels[update_indices])
    # Undamped residuals are in [0, 1] and cannot shrink merely by lowering the learning rate.
    stabilization_score <- 1 - mean(residuals)
    remaining[sample_id] <- remaining[sample_id] - 1
    sampling_counts[sample_id] <- sampling_counts[sample_id] + 1L
    draw_counts <- draw_counts + frequency
    coverage <- mean(sampling_counts[unlabeled_id] > 0)
    stability_scores <- c(stability_scores, stabilization_score)
    n_iter <- as.integer(i)
    history[[i]] <- data.frame(iteration = n_iter, stability = stabilization_score,
      coverage = coverage, sample_size = length(sample_id), sample_draws = length(draws),
      cv_folds = folds, cv_measure = step$measure,
      max_label_change = max(abs(pseudo_labels - before)))
    if (verbose && (i %% 10 == 0 || i == 1)) {
      message(sprintf("Iteration %d: stability=%.3f coverage=%.3f", i, stabilization_score, coverage))
    }
    if (all(remaining[unlabeled_id] <= 0)) {
      stop_reason <- "budget_exhausted"
      break
    }
    if (i >= max(min_iter, stability_window) && coverage >= min_coverage &&
        all(utils::tail(stability_scores, stability_window) > convergence_threshold)) {
      stop_reason <- "label_stability"
      break
    }
  }
  # Soft pseudo-labels are passed as a two-column proportion matrix, the
  # form glmnet supports for non-integer binomial responses. Effective
  # class mass is checked on the full data and every CV training split.
  # Soft labels do not require both thresholded classes to be represented.
  # If mass vanishes, explicitly record reversion to original PU labels.
  # Final folds use original positive/unlabeled strata and keep their identities.
  # The stored y and final_labels always describe the actual final targets.
  final_foldid <- stratified_foldid(original_y, min(nfolds, length(positive_id), length(unlabeled_id)))
  targets <- .xplus_final_targets(pseudo_labels, original_y, final_foldid)
  if (targets$fallback_used) warning(paste("Final fit reverted to original labels:", targets$reason), call. = FALSE)
  final_labels <- targets$labels
  final_step <- .xplus_cv_fit(x, final_labels, final_foldid, alpha, cv_measure)
  fit_pi <- final_step$fit
  pred_y <- .xplus_probabilities(stats::predict(fit_pi, newx = x, s = "lambda.min", type = "response"), n)
  cutoff <- stats::quantile(pred_y[positive_id], qq, names = FALSE)
  pred_coef1 <- stats::coef(fit_pi, s = "lambda.min")
  if (any(!is.finite(as.numeric(pred_coef1)))) stop("Final fitting produced non-finite coefficients.", call. = FALSE)
  history <- if (length(history)) do.call(rbind, history) else
    data.frame(iteration = integer(), stability = numeric(), coverage = numeric(),
      sample_size = integer(), sample_draws = integer(), cv_folds = integer(),
      cv_measure = character(), max_label_change = numeric())
  if (cv_measure == "auc" && (any(history$cv_measure != "auc") || final_step$measure != "auc")) {
    warning("AUC CV requires at least 10 observations per fold; affected fits used deviance, recorded in history.", call. = FALSE)
  }
  xplus_model <- new_xplus(fit_xplus = fit_pi, pred_y = pred_y, cutoff = cutoff,
    predicted_coefficients = pred_coef1, n_iter = n_iter, x = x, y = final_labels,
    alpha = alpha, learning_rate = learning_rate, pseudo_labels = pseudo_labels,
    iterative_path = iterative_path, qq = qq, stop_reason = stop_reason,
    max_iter = max_iter, call = this.call, original_y = original_y,
    final_labels = final_labels, fallback_used = targets$fallback_used,
    fallback_reason = targets$reason, history = history, sampling_counts = sampling_counts,
    draw_counts = draw_counts, final_foldid = final_foldid, cv_measure = cv_measure,
    sigmoid_scale = sigmoid_scale, sampling = sampling, min_iter = min_iter,
    stability_window = stability_window, min_coverage = min_coverage)
  validate_xplus(xplus_model)
}

.xplus_cv_fit <- function(x, labels, foldid, alpha, measure, weights = rep(1, nrow(x))) {
  effective_measure <- if (measure == "auc" && nrow(x) / max(foldid) < 10) "deviance" else measure
  args <- list(x = x, y = cbind("0" = 1 - labels, "1" = labels),
               family = "binomial", alpha = alpha, foldid = foldid,
               type.measure = effective_measure, weights = weights,
               grouped = nrow(x) / max(foldid) >= 3)
  control <- list(thresh = 1e-7, maxit = 100000)
  if (utils::packageVersion("glmnet") >= "5.0") args$control <- control else args <- c(args, control)
  fit <- do.call(glmnet::cv.glmnet, args)
  if (!length(fit$lambda.min) || any(!is.finite(fit$lambda.min)) || !any(is.finite(fit$cvm))) {
    stop("Cross-validation did not produce a finite penalty and score.", call. = FALSE)
  }
  list(fit = fit, measure = effective_measure)
}

.xplus_final_targets <- function(pseudo_labels, original_y, foldid) {
  valid_mass <- function(p) {
    mass <- mean(p)
    is.finite(mass) && mass > 1e-5 && mass < 1 - 1e-5
  }
  partitions <- c(list(seq_along(pseudo_labels)), lapply(sort(unique(foldid)), function(k) which(foldid != k)))
  valid <- function(labels) all(vapply(partitions, function(i) valid_mass(labels[i]), logical(1)))
  if (valid(pseudo_labels)) return(list(labels = pseudo_labels, fallback_used = FALSE, reason = ""))
  if (!valid(original_y)) stop("Original labels have insufficient class mass for binomial CV.", call. = FALSE)
  list(labels = as.numeric(original_y), fallback_used = TRUE,
       reason = "pseudo-label class mass is degenerate in the full data or a CV training split")
}
