#' Internal binomial metrics with link or response input
#'
#' @param predmat Finite prediction matrix; `input` selects link (default) or response scale.
#' @param y Binary vector or two-column nonnegative finite class masses.
#' @param type.measure Metric type; MSE and MAE sum losses across both columns.
#' @param weights Optional finite nonnegative row weights, without recycling.
#' @param cutoff Optional cutoff threshold for class metric (defaults to 0.5).
#'
#' @return A list with raw metric values and metadata; undefined metrics warn and return NA.
#' @seealso [assess()]
#' @noRd
xplus_cv_lognet <- function(predmat, y, type.measure = c("deviance", "class", "auc", "mse", "mae"), weights = NULL, cutoff = 0.5, input = c("link", "response")) {
  input <- match.arg(input)
  type.measure <- match.arg(type.measure)
  if (is.null(dim(predmat))) predmat <- matrix(predmat, ncol = 1)
  if (!is.matrix(predmat) || !is.numeric(predmat) || is.complex(predmat) ||
      any(!is.finite(predmat)) || nrow(predmat) < 1L || ncol(predmat) < 1L) {
    stop("Predictions must be a nonempty finite numeric matrix.", call. = FALSE)
  }
  if (input == "link") predmat <- stats::plogis(predmat)
  predmat <- .xplus_probabilities(predmat)
  y <- .xplus_metric_truth(y, nrow(predmat))
  weights <- .xplus_metric_weights(weights, nrow(predmat))
  cutoff <- .xplus_scalar(cutoff, "cutoff", 0, 1)
  nlambda <- ncol(predmat)
  if (type.measure == "auc") {
    cvraw <- matrix(vapply(seq_len(nlambda), function(j) {
      .xplus_mass_auc(y, predmat[, j], weights)
    }, numeric(1)), nrow = 1, dimnames = list(NULL, colnames(predmat)))
    return(list(cvraw = cvraw, weights = 1, N = rep(1L, nlambda), type.measure = type.measure))
  }
  rowmax <- pmax(y[, 1], y[, 2])
  scaled <- y / ifelse(rowmax == 0, 1, rowmax)
  total <- rowSums(scaled)
  y <- scaled / ifelse(total == 0, 1, total)
  weights <- .xplus_metric_product(rowmax, weights, total)
  if (!any(weights > 0)) {
    warning("Metrics are undefined because total effective weight/mass is zero.", call. = FALSE)
    cvraw <- matrix(NA_real_, nrow(predmat), nlambda, dimnames = dimnames(predmat))
  } else {
    cvraw <- switch(
      type.measure,
      mse = (y[, 1] - (1 - predmat))^2 + (y[, 2] - predmat)^2,
      mae = abs(y[, 1] - (1 - predmat)) + abs(y[, 2] - predmat),
      deviance = {
        predmat <- pmin(pmax(predmat, 1e-5), 1 - 1e-5)
        lp <- y[, 1] * log1p(-predmat) + y[, 2] * log(predmat)
        ly <- log(y)
        ly[y == 0] <- 0
        ly <- drop((y * ly) %*% c(1, 1))
        2 * (ly - lp)
      },
      # Misclassification error: 1 when prediction doesn't match true label
      # - For true class 0 (y[,1]=1): error when predmat > cutoff (predict class 1)
      # - For true class 1 (y[,2]=1): error when predmat <= cutoff (predict class 0)
      # This matches predict.xplus() which uses probability > object$cutoff for class 1
      class = y[, 1] * (predmat > cutoff) + y[, 2] * (predmat <= cutoff)
    )
  }
  list(cvraw = cvraw, weights = weights, N = rep(nrow(y), nlambda), type.measure = type.measure)
}

.xplus_metric_weights <- function(weights, n, name = "weights") {
  if (is.null(weights)) return(rep(1, n))
  if (!is.numeric(weights) || is.complex(weights) || !is.null(dim(weights)) ||
      length(weights) != n || any(!is.finite(weights)) || any(weights < 0)) {
    stop(sprintf("`%s` must be a finite nonnegative numeric weight vector with one weight per observation.", name), call. = FALSE)
  }
  weights
}

.xplus_metric_scores <- function(prob) {
  if (!is.numeric(prob) || is.complex(prob) ||
      (!is.null(dim(prob)) && (!is.matrix(prob) || ncol(prob) != 1L)) ||
      length(prob) < 1L || any(!is.finite(prob))) {
    stop("`prob` must contain finite numeric scores, one per observation.", call. = FALSE)
  }
  as.numeric(prob)
}

.xplus_metric_truth <- function(y, n, matrix_only = FALSE) {
  if (is.null(dim(y)) && !matrix_only) {
    if (is.numeric(y) && any(is.finite(y) & !(y %in% c(0, 1)))) {
      stop("Truth must be binary; use two-column class masses for soft labels or counts.", call. = FALSE)
    }
    y <- .xplus_binary_labels(y, n)
    return(cbind(1 - y, y))
  }
  if (!is.matrix(y) || !is.numeric(y) || is.complex(y) || ncol(y) != 2L ||
      nrow(y) != n || any(!is.finite(y)) || any(y < 0)) {
    stop("Truth must be a two-column matrix of finite nonnegative class masses with one row per prediction.", call. = FALSE)
  }
  y
}

.xplus_metric_product <- function(x, w, extra = rep(1, length(x))) {
  result <- x * w * extra
  if (all(is.finite(result)) && is.finite(sum(result)) &&
      !any(result == 0 & x > 0 & w > 0 & extra > 0)) return(result)
  logs <- log(x) + log(w) + log(extra)
  if (!any(is.finite(logs))) return(rep(0, length(x)))
  exp(logs - max(logs))
}

.xplus_mass_auc <- function(y, prob, weights) {
  positive <- .xplus_metric_product(y[, 2], weights)
  negative <- .xplus_metric_product(y[, 1], weights)
  if (!any(positive > 0) || !any(negative > 0)) {
    warning("AUC is undefined: positive or negative effective class mass is zero.", call. = FALSE)
    return(NA_real_)
  }
  positive <- positive / sum(positive)
  negative <- negative / sum(negative)
  index <- order(prob)
  scores <- prob[index]
  group <- cumsum(c(TRUE, scores[-1L] != scores[-length(scores)]))
  masses <- rowsum(cbind(positive[index], negative[index]), group, reorder = FALSE)
  below <- c(0, utils::head(cumsum(masses[, 2]), -1L))
  value <- sum(masses[, 1] * (below + 0.5 * masses[, 2]))
  min(1, max(0, value))
}
