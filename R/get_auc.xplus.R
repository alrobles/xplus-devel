#' Compute AUC for predictions from a model
#'
#' @param object A model object.
#' @param newx Feature matrix.
#' @param newy True labels.
#' @param weights Optional sample weights.
#' @param ... Arguments passed to [predict.xplus()], such as `s` selecting a single lambda.
#'
#' @return Numeric AUC value.
#' @seealso [assess()], [auc()]
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(100 * 5), ncol = 5)
#' y <- c(rep(1, 20), rep(0, 80))
#' fit <- xplus(x, y, max_iter = 5)
#' get_auc(fit, newx = x, newy = y)
#' @export
get_auc <- function(object, newx = NULL, newy = NULL, weights = NULL, ...) {
  UseMethod("get_auc")
}

#' @rdname get_auc
#' @method get_auc xplus
#' @export
get_auc.xplus <- function(object, newx = NULL, newy = NULL, weights = NULL, ...) {
  if (is.null(newx)) stop("Provide newx data", call. = FALSE)
  if (is.null(newy)) stop("Provide newy data", call. = FALSE)
  p_response <- stats::predict(object, newx = newx, type = "response", ...)
  if (NCOL(p_response) != 1L) {
    stop("get_auc() requires predictions for a single lambda in `s`.", call. = FALSE)
  }
  auc(newy, as.numeric(p_response), w = weights)
}
