#' Assess predictive performance
#'
#' @param object A model object.
#' @param newx Optional feature matrix.
#' @param newy Binary 0/1 labels or a two-column finite nonnegative matrix of negative and positive class masses; soft vectors are not accepted.
#' @param weights Optional finite nonnegative numeric row weights without recycling; NULL means unit weights.
#' @param ... Additional arguments passed to [predict()].
#'
#' @return A named list with `deviance`, `class`, `auc`, `mse`, and `mae`.
#'   For `class` metric, the threshold used is the model's cutoff (from
#'   `object$cutoff`), consistent with `predict(type = "class")`; MSE and MAE sum both class-column losses (twice the scalar loss). Undefined metrics return NA with a warning.
#' @seealso [xplus()], [get_auc()]
#' @references Zhou et al. (2022). doi:10.1371/journal.pcbi.1009956
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(100 * 5), ncol = 5)
#' y <- c(rep(1, 20), rep(0, 80))
#' fit <- xplus(x, y, max_iter = 5)
#' assess(fit, newx = x, newy = y)
#' @export
assess <- function(object, newx = NULL, newy, weights = NULL, ...) {
  UseMethod("assess")
}

#' @rdname assess
#' @method assess xplus
#' @export
assess.xplus <- function(object, newx = NULL, newy, weights = NULL, ...) {
  predmat <- stats::predict(object, newx = newx, type = "response", ...)
  type.measures <- c("deviance", "class", "auc", "mse", "mae")
  outlist <- stats::setNames(vector("list", length(type.measures)), type.measures)
  for (measure in type.measures) {
    raw <- xplus_cv_lognet(predmat, newy, measure, weights, object$cutoff, input = "response")
    if (measure == "deviance" && !any(raw$weights > 0)) {
      undefined <- rep(NA_real_, ncol(raw$cvraw))
      if (length(undefined) > 1L) names(undefined) <- colnames(raw$cvraw)
      return(stats::setNames(rep(list(undefined), length(type.measures)), type.measures))
    }
    out <- vapply(seq_len(ncol(raw$cvraw)), function(j) {
      if (measure == "auc") return(raw$cvraw[1, j])
      if (!any(raw$weights > 0)) return(NA_real_)
      stats::weighted.mean(raw$cvraw[, j], w = raw$weights / sum(raw$weights))
    }, numeric(1))
    if (length(out) > 1L) names(out) <- colnames(raw$cvraw)
    outlist[[measure]] <- out
  }
  outlist
}
