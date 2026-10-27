#' Extract feature scores from an xplus model
#'
#' Return backend-specific feature scores: elastic-net coefficients at
#' `"lambda.min"` for `"glmnet"`, variable importance for `"ranger"`, and
#' smooth-term significance (`-log10` of the mgcv p-value) for `"gam"`.
#' Backends without an importance function return `NULL`.
#'
#' @param object An `xplus` object.
#'
#' @return A named numeric vector of feature scores, or `NULL` when the
#'   backend does not provide one or the fitted model is unavailable.
#' @seealso [xplus()], [xplus_backend()], [coef.xplus()]
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(100 * 5), ncol = 5)
#' y <- c(rep(1, 20), rep(0, 80))
#' fit <- xplus(x, y, max_iter = 5)
#' get_importance(fit)
#' @export
get_importance <- function(object) {
  if (!inherits(object, "xplus")) stop("`object` must be an `xplus` model.", call. = FALSE)
  backend <- object$backend
  if (!inherits(backend, "xplus_backend")) {
    backend <- xplus_backend_glmnet()
  }
  if (is.null(object$xplus) || !length(object$xplus) || is.null(backend$importance)) {
    return(NULL)
  }
  backend$importance(object$xplus)
}
