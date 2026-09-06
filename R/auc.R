#' Compute area under the ROC curve
#'
#' @param y Binary 0/1 vector, including logical, character, or factor encodings.
#' @param prob Finite numeric scores, one per label; values outside `[0, 1]` are allowed.
#' @param w Optional finite nonnegative numeric sample weights; NULL means unit weights.
#'
#' @return Numeric rank AUC with half credit for ties; NA with a warning if either effective class mass is zero.
#' @seealso [auc_matrix()], [get_auc()]
#' @references Zhou et al. (2022). doi:10.1371/journal.pcbi.1009956
#' @examples
#' y <- c(0, 0, 1, 1)
#' p <- c(0.1, 0.3, 0.7, 0.9)
#' auc(y, p)
#' @export
auc <- function(y, prob, w = NULL) {
  prob <- .xplus_metric_scores(prob)
  y <- .xplus_binary_labels(y, length(prob))
  w <- .xplus_metric_weights(w, length(prob), "w")
  .xplus_mass_auc(cbind(1 - y, y), prob, w)
}
