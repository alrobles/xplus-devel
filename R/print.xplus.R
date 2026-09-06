#' Print an xplus model
#'
#' @param x An `xplus` object.
#' @param digits Number of significant digits.
#' @param ... Additional arguments.
#'
#' @return Invisibly returns `x`.
#' @seealso [summary.xplus()], [coef.xplus()]
#' @references Zhou et al. (2022). doi:10.1371/journal.pcbi.1009956
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(100 * 5), ncol = 5)
#' y <- c(rep(1, 20), rep(0, 80))
#' fit <- xplus(x, y, max_iter = 5)
#' print(fit)
#' @method print xplus
#' @export
print.xplus <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("xplus model (PLUS algorithm)\n")
  cat("Call: ", deparse(x$call), "\n")
  cat("Iterations:", x$n_iter, "\n")
  cat("Stop reason:", x$stop_reason, "\n")
  if (!is.null(x$original_y)) cat("Original positives:", sum(x$original_y == 1), "\n")
  if (!is.null(x$fallback_used)) {
    cat("Fallback used:", x$fallback_used, "\n")
    if (x$fallback_used) cat("Fallback reason:", x$fallback_reason, "\n")
    cat("Final training target range:", paste(format(range(x$y), digits = digits), collapse = " to "), "\n")
    cat("Final soft targets:", sum(x$y > 0 & x$y < 1), "\n")
  }
  if (!is.null(x$cv_measure)) cat("CV measure:", x$cv_measure, "\n")
  cat("Cutoff:", format(x$cutoff, digits = digits), "\n\n")

  opt_lams <- c(x$xplus$lambda.min, x$xplus$lambda.1se)
  which_idx <- match(opt_lams, x$xplus$lambda)
  mat <- cbind(
    Lambda = opt_lams,
    Index = which_idx,
    Measure = x$xplus$cvm[which_idx],
    SE = x$xplus$cvsd[which_idx],
    Nonzero = x$xplus$nzero[which_idx]
  )
  rownames(mat) <- c("min", "1se")
  print(data.frame(mat, check.names = FALSE), digits = digits)
  invisible(x)
}
