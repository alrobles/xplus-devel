#' Build a tidy prediction table
#'
#' @param object An `xplus` model object.
#' @param newx Feature matrix.
#' @param newy True labels.
#' @param use_cutoff Logical; if `TRUE`, classify with the model's cutoff. If `FALSE`,
#' classify probabilities strictly greater than 0.5 as positive.
#'
#' @return A tibble with truth labels, probabilities and predicted classes; `Class1` is the positive label 1, classified by probability strictly greater than the cutoff.
#' @seealso [predict.xplus()], [assess()]
#' @references Zhou et al. (2022). doi:10.1371/journal.pcbi.1009956
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(100 * 5), ncol = 5)
#' y <- c(rep(1, 20), rep(0, 80))
#' fit <- xplus(x, y, max_iter = 5)
#' get_predictions(fit, x, y)
#' @export
get_predictions <- function(object, newx, newy, use_cutoff = TRUE) {
  if (!is.logical(use_cutoff) || length(use_cutoff) != 1L || is.na(use_cutoff)) {
    stop("`use_cutoff` must be TRUE or FALSE.", call. = FALSE)
  }
  class1 <- stats::predict(object, newx = newx, s = "lambda.min", type = "response") |>
    as.numeric()
  class1 <- .xplus_probabilities(class1)
  newy <- .xplus_binary_labels(newy, length(class1), name = "newy")
  truth <- factor(ifelse(newy == 1, "Class1", "Class2"), levels = c("Class1", "Class2"))
  cutoff <- if (use_cutoff) .xplus_scalar(object$cutoff, "cutoff", lower = 0, upper = 1) else 0.5
  predicted <- factor(ifelse(class1 > cutoff, "Class1", "Class2"), levels = c("Class1", "Class2"))
  class2 <- 1 - class1

  tibble::as_tibble(
    data.frame(truth = truth, Class1 = class1, Class2 = class2, predicted)
  )
}
