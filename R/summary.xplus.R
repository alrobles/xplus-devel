#' Summarize an xplus model
#'
#' @param object An `xplus` object.
#' @param ... Additional arguments.
#'
#' @return A list of class `summary.xplus` with model details.
#' @seealso [print.xplus()], [coef.xplus()]
#' @references Zhou et al. (2022). doi:10.1371/journal.pcbi.1009956
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(100 * 5), ncol = 5)
#' y <- c(rep(1, 20), rep(0, 80))
#' fit <- xplus(x, y, max_iter = 5)
#' summary(fit)
#' @method summary xplus
#' @export
summary.xplus <- function(object, ...) {
  n_nonzero <- sum(as.matrix(object$predicted_coefficients) != 0)
  out <- list(
    n_obs = nrow(object$x),
    n_features = ncol(object$x),
    n_iter = object$n_iter,
    cutoff = object$cutoff,
    alpha = object$alpha,
    learning_rate = object$learning_rate,
    iterative_path = object$iterative_path,
    qq = object$qq,
    lambda_min = object$xplus$lambda.min,
    lambda_1se = object$xplus$lambda.1se,
    n_nonzero_coefficients = n_nonzero,
    stop_reason = object$stop_reason
  )
  if (!is.null(object$original_y)) {
    out$n_original_positive <- sum(object$original_y == 1)
    out$n_original_unlabeled <- sum(object$original_y == 0)
  }
  if (!is.null(object$fallback_used)) {
    out$fallback_used <- object$fallback_used
    out$fallback_reason <- object$fallback_reason
    out$final_target_range <- range(object$y)
    out$n_final_soft_labels <- sum(object$y > 0 & object$y < 1)
  }
  for (name in c("cv_measure", "sampling", "sigmoid_scale", "min_iter", "stability_window", "min_coverage")) {
    if (!is.null(object[[name]])) out[[name]] <- object[[name]]
  }
  if (!is.null(object$history) && nrow(object$history) > 0) {
    out$final_coverage <- utils::tail(object$history$coverage, 1)
    out$final_stability <- utils::tail(object$history$stability, 1)
  }
  class(out) <- "summary.xplus"
  out
}

#' Print method for summary.xplus objects
#' @param x A `summary.xplus` object.
#' @param ... Additional arguments.
#' @method print summary.xplus
#' @return The input object \code{x} is returned invisibly (called for side
#' effects).
#' @export
print.summary.xplus <- function(x, ...) {
  cat("Summary of xplus model\n")
  cat("Observations:", x$n_obs, "\n")
  cat("Features:", x$n_features, "\n")
  cat("Iterations:", x$n_iter, "\n")
  cat("Stop reason:", x$stop_reason, "\n")
  if (!is.null(x$n_original_positive)) {
    cat("Original positives:", x$n_original_positive, "\n")
    cat("Original unlabeled:", x$n_original_unlabeled, "\n")
  }
  if (!is.null(x$fallback_used)) {
    cat("Fallback used:", x$fallback_used, "\n")
    if (x$fallback_used) cat("Fallback reason:", x$fallback_reason, "\n")
    cat("Final training target range:", paste(x$final_target_range, collapse = " to "), "\n")
    cat("Final soft targets:", x$n_final_soft_labels, "\n")
  }
  if (!is.null(x$cv_measure)) cat("CV measure:", x$cv_measure, "\n")
  if (!is.null(x$sampling)) cat("Sampling:", x$sampling, "\n")
  if (!is.null(x$final_coverage)) cat("Unlabeled coverage:", x$final_coverage, "\n")
  cat("Cutoff:", x$cutoff, "\n")
  cat("lambda.min:", x$lambda_min, "\n")
  cat("lambda.1se:", x$lambda_1se, "\n")
  cat("Non-zero coefficients:", x$n_nonzero_coefficients, "\n")
  invisible(x)
}
