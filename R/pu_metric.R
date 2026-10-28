#' PU-valid area under the ROC curve for tidymodels
#'
#' `pu_auc()` is a yardstick-compatible probability metric that scores
#' ranked predictions against the *observed* PU labels (labeled positives
#' versus unlabeled), i.e. the positive-vs-unlabeled AUC also available
#' through [pu_score()]. Under SCAR this ranking is a consistent proxy
#' for the true positive-vs-negative ranking, so it can be used for model
#' and hyperparameter selection without negative labels.
#'
#' The positive (labeled) class is the **second** level of `truth`,
#' matching the convention used by [pu_xplus()]; override with
#' `event_level` if your factors are ordered differently.
#'
#' `pu_auc_vec()` is the vector analogue, returning a single number.
#'
#' @param data A data frame with the observed `truth` (a two-level
#'   factor) and the probability columns passed in `...`.
#' @param truth The column with observed labels; unquoted.
#' @param ... The probability column for the positive class; unquoted.
#' @param estimator,na_rm,case_weights,options Forwarded to
#'   [yardstick::roc_auc()].
#' @param event_level Which factor level of `truth` is the positive
#'   event; defaults to `"second"` (the parsnip convention), unlike
#'   yardstick's default of `"first"`.
#' @param estimate A numeric vector of positive-class probabilities in
#'   `pu_auc_vec()`.
#'
#' @return `pu_auc()` returns a tibble with `.metric`, `.estimator` and
#'   `.estimate`; `pu_auc_vec()` returns a number.
#' @examplesIf requireNamespace("yardstick", quietly = TRUE)
#' sim <- simulate_pu_data(n = 200, p = 3, seed = 1)
#' fit <- xplus(sim$x, sim$y, backend = "glmnet", seed = 1)
#' d <- data.frame(y = factor(sim$y, levels = 0:1),
#'                 .pred_1 = as.numeric(predict(fit, newx = sim$x)))
#' pu_auc(d, y, .pred_1)
#' pu_auc_vec(factor(sim$y, levels = 0:1), d$.pred_1)
#' @export
pu_auc <- function(data, truth, ..., estimator = NULL, na_rm = TRUE,
                   event_level = NULL, case_weights = NULL,
                   options = list()) {
  if (!requireNamespace("yardstick", quietly = TRUE)) {
    stop("`pu_auc()` requires the yardstick package.", call. = FALSE)
  }
  if (is.null(event_level)) event_level <- "second"
  case_weights_quo <- rlang::enquo(case_weights)
  yardstick::prob_metric_summarizer(
    name = "pu_auc", fn = pu_auc_vec, data = data,
    truth = !!rlang::enquo(truth), ..., estimator = estimator,
    na_rm = na_rm, event_level = event_level,
    case_weights = !!case_weights_quo
  )
}

#' @rdname pu_auc
#' @export
pu_auc_vec <- function(truth, estimate, estimator = NULL, na_rm = TRUE,
                       event_level = NULL, case_weights = NULL,
                       options = list(), ...) {
  if (!requireNamespace("yardstick", quietly = TRUE)) {
    stop("`pu_auc_vec()` requires the yardstick package.", call. = FALSE)
  }
  if (is.null(event_level)) event_level <- "second"
  yardstick::roc_auc_vec(truth, estimate, estimator = estimator,
                         na_rm = na_rm, event_level = event_level,
                         case_weights = case_weights, options = options,
                         ...)
}

class(pu_auc) <- c("prob_metric", "metric", "function")
attr(pu_auc, "direction") <- "maximize"
attr(pu_auc, "range") <- c(0, 1)
