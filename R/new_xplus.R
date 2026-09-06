#' Construct a new xplus object
#'
#' @param fit_xplus Fitted [glmnet::cv.glmnet()] object.
#' @param pred_y Predicted probabilities matrix.
#' @param cutoff Numeric classification cutoff.
#' @param predicted_coefficients Sparse coefficient matrix.
#' @param n_iter,history,sampling_counts,draw_counts Number of completed iterations, per-iteration diagnostics, and per-observation unlabeled inclusion-round and draw counts; optional diagnostics default to `NULL`.
#' @param x Training feature matrix used to fit the model.
#' @param y,final_labels Identical actual target probabilities used for final fitting; `final_labels` defaults to `y`.
#' @param alpha Elastic-net alpha used during fitting.
#' @param learning_rate Learning rate used during pseudo-label updates.
#' @param pseudo_labels,original_y,fallback_used,fallback_reason Proposed probabilities before fallback, original numeric binary labels, fallback flag and reason (empty if unused); optional metadata default to `NULL` for legacy bundles.
#' @param iterative_path,final_foldid,cv_measure,sigmoid_scale,sampling Iterative path and optional final cross-validation folds, measure (`deviance` or `auc`), positive sigmoid scale, and sampling mode (`bootstrap` or `unique`); optional controls default to `NULL`.
#' @param qq Quantile parameter used for cutoff calibration.
#' @param call Original function call.
#' @param max_iter,min_iter,stability_window,min_coverage Maximum iterations and optional minimum iterations, consecutive stability rounds, and unlabeled coverage required for stopping; optional controls default to `NULL`.
#' @param stop_reason Reason fitting stopped: `"max_iter"`, `"label_stability"`,
#'   `"budget_exhausted"`, or `"degenerate_labels"` (the pseudo-labels of the
#'   iterative training subset collapsed to a single class).
#'
#' @return An object of class `"xplus"`.
#' @seealso [validate_xplus()]
#' @examples
#' \dontrun{
#' fit <- glmnet::cv.glmnet(matrix(rnorm(50), ncol = 5), c(rep(1, 5), rep(0, 5)), family = "binomial")
#' obj <- new_xplus(fit_xplus = fit, pred_y = matrix(0.5, 10, 1), cutoff = 0.5)
#' }
#' @keywords internal
new_xplus <- function(
  fit_xplus = list(),
  pred_y = matrix(),
  cutoff = numeric(),
  predicted_coefficients = Matrix::Matrix(),
  n_iter = integer(),
  x = matrix(),
  y = numeric(),
  alpha = numeric(),
  learning_rate = numeric(),
  pseudo_labels = numeric(),
  iterative_path = character(),
  qq = numeric(),
  call = character(),
  max_iter = integer(),
  stop_reason = character(),
  original_y = NULL,
  final_labels = y,
  fallback_used = NULL,
  fallback_reason = NULL,
  history = NULL,
  sampling_counts = NULL,
  draw_counts = NULL,
  final_foldid = NULL,
  cv_measure = NULL,
  sigmoid_scale = NULL,
  sampling = NULL,
  min_iter = NULL,
  stability_window = NULL,
  min_coverage = NULL
) {
  predicted_coefficients <- methods::as(
    methods::as(methods::as(Matrix::Matrix(predicted_coefficients, sparse = TRUE), "dMatrix"), "generalMatrix"),
    "CsparseMatrix"
  )

  structure(
    list(
      xplus = fit_xplus,
      pred_y = pred_y,
      cutoff = cutoff,
      predicted_coefficients = predicted_coefficients,
      n_iter = n_iter,
      x = x,
      y = y,
      alpha = alpha,
      learning_rate = learning_rate,
      pseudo_labels = pseudo_labels,
      iterative_path = iterative_path,
      qq = qq,
      call = call,
      max_iter = max_iter,
      stop_reason = stop_reason,
      original_y = original_y,
      final_labels = final_labels,
      fallback_used = fallback_used,
      fallback_reason = fallback_reason,
      history = history,
      sampling_counts = sampling_counts,
      draw_counts = draw_counts,
      final_foldid = final_foldid,
      cv_measure = cv_measure,
      sigmoid_scale = sigmoid_scale,
      sampling = sampling,
      min_iter = min_iter,
      stability_window = stability_window,
      min_coverage = min_coverage
    ),
    class = "xplus"
  )
}
