#' Extract coefficients from an xplus model
#'
#' Only defined for models fitted with the `"glmnet"` backend; for other
#' backends use [get_importance()] instead.
#'
#' @param object An `xplus` object.
#' @param s Penalty value name or numeric lambda.
#' @param ... Additional arguments.
#'
#' @return A sparse coefficient matrix.
#' @seealso [summary.xplus()], [print.xplus()]
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(100 * 5), ncol = 5)
#' y <- c(rep(1, 20), rep(0, 80))
#' fit <- xplus(x, y, max_iter = 5)
#' coef(fit)
#' @method coef xplus
#' @export
coef.xplus <- function(object, s = "lambda.min", ...) {
  backend_name <- .xplus_backend_name(object)
  if (backend_name != "glmnet") {
    stop(sprintf("coef() is only defined for the 'glmnet' backend; '%s' models expose feature scores via get_importance().",
                 backend_name), call. = FALSE)
  }
  glmnet::coef.glmnet(object$xplus, s = s, ...)
}
