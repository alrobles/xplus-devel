#' PU learning baselines on the xplus backend contract
#'
#' Classical positive-unlabeled algorithms expressed on the same backend
#' contract as [xplus()], for honest head-to-head comparisons.
#'
#' `xplus_elkan_noto()` implements the Elkan & Noto (2008) recipe: train a
#' classifier separating labeled positives (`s = 1`) from the unlabeled pool,
#' estimate the labeling propensity `c = P(s = 1 | y = 1)` from held-out-style
#' predictions on labeled positives, and rescale `p(y = 1 | x) = p(s = 1 | x) / c`
#' clipped to one. `xplus_naive()` is the lower bound: a supervised classifier
#' trained treating all unlabeled instances as negatives.
#'
#' @param x Finite numeric feature matrix with one row per observation.
#' @param y Binary PU labels: `1` labeled positive, `0` unlabeled.
#' @param backend A backend name or [xplus_backend()] object, resolved exactly
#'   as in [xplus()].
#' @param seed Optional integer seed for reproducible resampling.
#' @param nfolds Folds passed to the backend `fit` contract (used by CV
#'   backends for honest predictions).
#' @param estimate How to estimate the labeling propensity `c`: `"mean"`
#'   (Elkan & Noto's validation-set average) or `"median"` over labeled
#'   positives, which is more robust to contaminated positives.
#' @param alpha,cv_measure glmnet backend configuration, as in [xplus()].
#'
#' @return An `xplus_baseline` object with `predict` support, `$cutoff`
#'   (0.5 on the rescaled scale for Elkan-Noto), `$scale` (estimated `c`),
#'   and `$method`.
#'
#' @references
#' Elkan, C. and Noto, K. (2008). Learning classifiers from only positive and
#' unlabeled data. KDD.
#'
#' @examples
#' set.seed(1)
#' sim <- simulate_pu_data(n = 200, p = 5, prevalence = 0.4,
#'                         label_propensity = 0.5)
#' fit <- xplus_elkan_noto(sim$x, sim$y, backend = "ranger")
#' head(predict(fit, newx = sim$x))
#' @export
xplus_elkan_noto <- function(x, y, backend = "glmnet", seed = NULL,
                             nfolds = 10, estimate = c("mean", "median"),
                             alpha = 1, cv_measure = "deviance") {
  estimate <- match.arg(estimate)
  prep <- .xplus_baseline_prep(x, y, backend, seed, nfolds, alpha, cv_measure)
  s_hat <- .xplus_backend_fit_predict(prep$backend, prep$model, prep$x)
  s_hat <- .xplus_probabilities(s_hat, nrow(prep$x))
  c_hat <- min(1, max(0,
    if (estimate == "median") stats::median(s_hat[prep$y == 1])
    else mean(s_hat[prep$y == 1])
  ))
  if (c_hat < 1e-8) {
    stop("Estimated labeling propensity is near zero; check the positive labels.",
         call. = FALSE)
  }
  .xplus_new_baseline(prep, scale = c_hat, cutoff = 0.5, method = "elkan_noto")
}

#' @rdname xplus_elkan_noto
#' @export
xplus_naive <- function(x, y, backend = "glmnet", seed = NULL, nfolds = 10,
                        alpha = 1, cv_measure = "deviance") {
  prep <- .xplus_baseline_prep(x, y, backend, seed, nfolds, alpha, cv_measure)
  .xplus_new_baseline(prep, scale = 1, cutoff = 0.5, method = "naive")
}

# Shared preparation: validate inputs, resolve backend, fit once on the
# observed PU labels (labeled positive vs unlabeled).
.xplus_baseline_prep <- function(x, y, backend, seed, nfolds, alpha, cv_measure) {
  x <- .xplus_matrix(x)
  y <- .xplus_binary_labels(y, nrow(x), require_both = TRUE)
  if (!is.null(seed)) .xplus_local_seed(seed)
  nfolds <- .xplus_scalar(nfolds, "nfolds", 3, nrow(x), integer = TRUE)
  backend <- .xplus_resolve_backend(backend, alpha, cv_measure)
  foldid <- stratified_foldid(y, as.integer(nfolds))
  step <- .xplus_backend_fit(backend, x, y, foldid, rep(1, nrow(x)))
  list(x = x, y = y, backend = backend, model = step$model, foldid = foldid)
}

.xplus_new_baseline <- function(prep, scale, cutoff, method) {
  structure(
    list(model = prep$model, backend = prep$backend, scale = scale,
         cutoff = cutoff, method = method, x = prep$x, y = prep$y,
         foldid = prep$foldid, call = match.call()),
    class = "xplus_baseline"
  )
}

#' Predict from a PU baseline model
#'
#' @param object An `xplus_baseline` object.
#' @param newx Optional numeric feature matrix; defaults to training data.
#' @param ... Unused.
#'
#' @return Rescaled probability of the positive class as a numeric vector.
#' @export
predict.xplus_baseline <- function(object, newx = NULL, ...) {
  if (is.null(newx)) newx <- object$x
  newx <- .xplus_matrix(newx, reference = .xplus_matrix(object$x), name = "newx")
  s <- .xplus_backend_predict(object$backend, object$model, newx, nrow(newx))
  .xplus_probabilities(pmin(1, s / object$scale), nrow(newx))
}

#' @export
print.xplus_baseline <- function(x, ...) {
  cat(sprintf("xplus PU baseline (%s)\n", x$method))
  cat(sprintf("Backend: %s | scale (c-hat): %.4f | cutoff: %.3f\n",
              .xplus_backend_name(x), x$scale, x$cutoff))
  invisible(x)
}
