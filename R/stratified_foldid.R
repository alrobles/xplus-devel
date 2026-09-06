#' Build class-stratified cross-validation fold assignments
#'
#' Assigns each observation to one of `nfolds` folds so that both classes
#' are spread as evenly as possible across folds. This prevents a small
#' class from being concentrated in a single fold, which would leave a
#' cross-validation training split with fewer than the two observations
#' per class that [glmnet::glmnet()] requires for binomial fits.
#'
#' @param y Binary (0/1) vector of class labels.
#' @param nfolds Number of folds.
#'
#' @return Integer vector of fold assignments in `1:nfolds`, the same
#'   length as `y`, suitable for the `foldid` argument of
#'   [glmnet::cv.glmnet()].
#' @keywords internal
stratified_foldid <- function(y, nfolds) {
  y <- .xplus_binary_labels(y, length(y), require_both = TRUE)
  .xplus_scalar(nfolds, "nfolds", 3, .Machine$integer.max, integer = TRUE)
  if (min(table(y)) < nfolds) stop("`nfolds` cannot exceed the smaller class size.", call. = FALSE)
  foldid <- integer(length(y))
  for (cls in 0:1) {
    idx <- which(y == cls)
    assignments <- rep_len(seq_len(nfolds), length(idx))
    foldid[idx] <- assignments[sample.int(length(idx))]
  }
  foldid
}
