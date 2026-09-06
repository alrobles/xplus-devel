#' Predict from an xplus model
#'
#' @param object An `xplus` object.
#' @param newx Optional finite numeric feature matrix; defaults to training data. Named training features must match and are reordered automatically.
#' @param s Exact penalty name (`"lambda.min"`, `"lambda.1se"`) or finite nonnegative numeric lambda vector. Cache-only objects support only `"lambda.min"`.
#' @param type Prediction type: `"response"`, `"link"`, or `"class"`.
#' @param ... Additional arguments are not supported and cause an error.
#'
#' @return Probabilities (`type = "response"`) or log-odds (`type = "link"`); cached probabilities of 0 and 1 give infinite log-odds.
#'   Classes use a factor with fixed levels `"0"`, `"1"` for one lambda, or a dimension-preserving 0/1 matrix for multiple lambdas.
#' @seealso [xplus()], [print.xplus()]
#' @references Zhou et al. (2022). doi:10.1371/journal.pcbi.1009956
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(100 * 5), ncol = 5)
#' y <- c(rep(1, 20), rep(0, 80))
#' fit <- xplus(x, y, max_iter = 5)
#' predict(fit, newx = x, type = "response")
#' predict(fit, newx = x, type = "link")
#' @method predict xplus
#' @export
predict.xplus <- function(object, newx = NULL, s = "lambda.min", type = "response", ...) {
  if (length(match.call(expand.dots = FALSE)$...)) {
    stop("Unsupported arguments in `...` for predict.xplus().", call. = FALSE)
  }
  type <- match.arg(type, c("response", "link", "class"))
  named_lambda <- is.character(s) && length(s) == 1L && !is.na(s) &&
    s %in% c("lambda.min", "lambda.1se")
  numeric_lambda <- is.numeric(s) && !is.complex(s) && length(s) > 0L &&
    is.null(dim(s)) && all(is.finite(s)) && all(s >= 0)
  if (!named_lambda && !numeric_lambda) {
    stop("`s` must be exactly 'lambda.min' or 'lambda.1se', or a finite nonnegative numeric lambda vector.", call. = FALSE)
  }

  if (is.null(object$xplus$glmnet.fit) || is.null(object[["x"]])) {
    if (!is.null(newx)) {
      stop("Predictions for `newx` require a fitted backend and training matrix.", call. = FALSE)
    }
    if (!named_lambda || s != "lambda.min") {
      stop("Cached predictions without a fitted backend or training matrix support only s = 'lambda.min'.", call. = FALSE)
    }
    prob <- .xplus_probabilities(object$pred_y)
    if (!length(prob) || NCOL(prob) != 1L || length(dim(prob)) > 2L) {
      stop("Cached probabilities must contain one prediction per observation.", call. = FALSE)
    }
    if (type == "link") return(stats::qlogis(prob))
  } else {
    training <- .xplus_matrix(object$x)
    if (is.null(newx)) newx <- training
    newx <- .xplus_matrix(newx, reference = training, name = "newx")
    lambda <- if (named_lambda) .xplus_scalar(object$xplus[[s]], s, lower = 0) else s
    if (type == "link") {
      return(stats::predict(object$xplus$glmnet.fit, newx = newx, s = lambda, type = "link"))
    }
    prob <- .xplus_probabilities(
      stats::predict(object$xplus$glmnet.fit, newx = newx, s = lambda, type = "response"),
      n = nrow(newx)
    )
  }

  if (type == "response") return(prob)
  cutoff <- .xplus_scalar(object$cutoff, "cutoff", lower = 0, upper = 1)
  classes <- (prob > cutoff) * 1L
  if (NCOL(prob) > 1L) return(classes)
  factor(as.integer(classes), levels = c(0, 1))
}
