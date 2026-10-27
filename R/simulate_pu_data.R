#' Simulate a positive-unlabeled dataset
#'
#' Generate a binary dataset with known ground truth for benchmarking
#' [xplus()] backends. Features are independent standard normals; the true
#' positive probability follows a configurable decision boundary. Positive
#' cases are observed under the SCAR assumption (selected completely at
#' random): each true positive is labeled with constant probability
#' `label_propensity`, and everything else remains unlabeled.
#'
#' @param n Number of observations.
#' @param p Number of features.
#' @param boundary Decision boundary driving the positive probability:
#'   `"linear"` (sparse linear predictor), `"interaction"` (pure pairwise
#'   products), or `"nonlinear"` (radial, positives outside a sphere).
#' @param prevalence Target fraction of true positives in `[0, 1]`.
#' @param label_propensity Constant probability that a true positive is
#'   observed as a labeled positive, in `(0, 1]`.
#' @param n_informative Number of features driving the boundary.
#' @param sharpness Positive logistic scale on the centered boundary score;
#'   larger values make the boundary more deterministic.
#' @param seed Optional nonnegative integer seed.
#'
#' @return A list of class `"xplus_pu_data"` with:
#'   `x` (feature matrix), `y` (observed PU labels, `1` = labeled positive,
#'   `0` = unlabeled), `z` (true binary labels), `p_true` (true positive
#'   probability per observation), and the simulation settings.
#' @seealso [xplus()]
#' @examples
#' sim <- simulate_pu_data(n = 300, p = 8, boundary = "interaction", seed = 1)
#' fit <- xplus(sim$x, sim$y, max_iter = 5)
#' auc(sim$z, as.numeric(predict(fit)))
#' @export
simulate_pu_data <- function(n = 500, p = 10,
                             boundary = c("linear", "interaction", "nonlinear"),
                             prevalence = 0.5, label_propensity = 0.5,
                             n_informative = min(5L, p), sharpness = 2,
                             seed = NULL) {
  .xplus_scalar(n, "n", 6, .Machine$integer.max, integer = TRUE)
  .xplus_scalar(p, "p", 2, .Machine$integer.max, integer = TRUE)
  boundary <- match.arg(boundary)
  .xplus_scalar(prevalence, "prevalence", 0, 1, lower_open = TRUE, upper_open = TRUE)
  .xplus_scalar(label_propensity, "label_propensity", 0, 1, lower_open = TRUE)
  .xplus_scalar(n_informative, "n_informative", 1, p, integer = TRUE)
  .xplus_scalar(sharpness, "sharpness", 0, Inf, lower_open = TRUE)
  if (!is.null(seed)) {
    .xplus_scalar(seed, "seed", 0, .Machine$integer.max, integer = TRUE)
    set.seed(seed)
  }
  x <- matrix(stats::rnorm(n * p), nrow = n, ncol = p,
              dimnames = list(NULL, paste0("x", seq_len(p))))
  k <- as.integer(n_informative)
  eta <- switch(boundary,
    linear = {
      beta <- c(stats::rnorm(k), rep(0, p - k))
      as.numeric(x %*% beta)
    },
    interaction = {
      ke <- k - (k %% 2L)
      eta <- if (ke >= 2L) {
        pairs <- matrix(seq_len(ke), nrow = 2)
        rowSums(vapply(seq_len(ncol(pairs)),
                       function(j) 1.5 * x[, pairs[1, j]] * x[, pairs[2, j]],
                       numeric(n)))
      } else 0
      eta + if (k %% 2L == 1L) x[, k] else 0
    },
    nonlinear = rowSums(x[, seq_len(k), drop = FALSE]^2) - k
  )
  threshold <- stats::quantile(eta, 1 - prevalence, names = FALSE)
  p_true <- as.numeric(stats::plogis(sharpness * (eta - threshold)))
  z <- stats::rbinom(n, 1, p_true)
  y <- as.integer(z == 1L & stats::runif(n) < label_propensity)
  if (sum(y) < 3L || sum(y == 0L) < 3L) {
    stop("Simulation produced too few labeled positives or unlabeled cases; adjust `n`, `prevalence`, or `label_propensity`.",
         call. = FALSE)
  }
  structure(
    list(x = x, y = y, z = z, p_true = p_true,
         n = n, p = p, boundary = boundary, prevalence = prevalence,
         observed_prevalence = mean(z), label_propensity = label_propensity,
         n_informative = k, sharpness = sharpness, seed = seed),
    class = "xplus_pu_data"
  )
}

#' Print method for xplus_pu_data objects
#'
#' @param x An `xplus_pu_data` object.
#' @param ... Additional arguments.
#'
#' @return The input object `x`, returned invisibly.
#' @method print xplus_pu_data
#' @export
print.xplus_pu_data <- function(x, ...) {
  cat("Simulated PU dataset (xplus_pu_data)\n")
  cat("Observations:", x$n, "\n")
  cat("Features:", x$p, "\n")
  cat("Boundary:", x$boundary, "\n")
  cat("True positives:", sum(x$z), sprintf("(%.1f%%)", 100 * x$observed_prevalence), "\n")
  cat("Labeled positives:", sum(x$y), "\n")
  cat("Unlabeled:", sum(x$y == 0L),
      sprintf("(%d hidden positives)", sum(x$z == 1L & x$y == 0L)), "\n")
  invisible(x)
}
