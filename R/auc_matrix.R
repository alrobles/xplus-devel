#' Compute AUC from matrix labels
#'
#' @param y Two-column finite nonnegative class masses (negative, positive); soft labels, counts, and zero-mass rows are supported.
#' @param prob Finite numeric scores, one per row; values outside `[0, 1]` are allowed.
#' @param weights Optional finite nonnegative numeric row weights without recycling; NULL means unit weights.
#'
#' @return Weighted rank AUC with half credit for ties; NA with a warning if either effective class mass is zero.
#' @seealso [auc()], [assess()]
#' @references Zhou et al. (2022). doi:10.1371/journal.pcbi.1009956
#' @examples
#' y <- cbind(c(1, 1, 0, 0), c(0, 0, 1, 1))
#' p <- c(0.2, 0.3, 0.7, 0.8)
#' auc_matrix(y, p)
#' @export
auc_matrix <- function(y, prob, weights = NULL) {
  prob <- .xplus_metric_scores(prob)
  y <- .xplus_metric_truth(y, length(prob), matrix_only = TRUE)
  weights <- .xplus_metric_weights(weights, length(prob))
  .xplus_mass_auc(y, prob, weights)
}
