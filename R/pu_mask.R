#' Convert true binary labels into PU labels
#'
#' Turn any labeled binary dataset into a positive-unlabeled one under the
#' SCAR assumption: each positive stays labeled with constant probability
#' `label_propensity` and the rest become unlabeled. This is how the PU
#' literature builds benchmarks from open binary datasets — positives of
#' the known class remain `1`, hidden positives and negatives pool as `0`.
#'
#' @param z Binary vector of true labels (`1` positive, `0` negative).
#' @param label_propensity Constant probability that a true positive is
#'   observed as a labeled positive, in `(0, 1]`.
#' @param seed Optional nonnegative integer seed. Use a different seed than
#'   the one that generated `z` — reusing the same stream can correlate the
#'   mask with the simulated labels.
#'
#' @return An integer vector of PU labels (`1` labeled positive, `0`
#'   unlabeled) the same length as `z`.
#' @seealso [simulate_pu_data()], [xplus()]
#' @examples
#' z <- c(rep(1, 80), rep(0, 120))
#' y <- pu_mask(z, label_propensity = 0.5, seed = 1)
#' fit <- xplus(matrix(rnorm(200 * 4), ncol = 4), y, max_iter = 3)
#' @export
pu_mask <- function(z, label_propensity = 0.5, seed = NULL) {
  if (!is.numeric(z) || !all(z %in% c(0, 1))) {
    stop("`z` must be a binary numeric vector.", call. = FALSE)
  }
  .xplus_scalar(label_propensity, "label_propensity", 0, 1, lower_open = TRUE)
  if (!is.null(seed)) {
    .xplus_scalar(seed, "seed", 0, .Machine$integer.max, integer = TRUE)
    set.seed(seed)
  }
  y <- as.integer(z == 1L & stats::runif(length(z)) < label_propensity)
  if (sum(y) < 3L) {
    stop("`pu_mask` produced fewer than three labeled positives; raise `label_propensity`.",
         call. = FALSE)
  }
  y
}
