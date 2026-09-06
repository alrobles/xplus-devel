.xplus_matrix <- function(x, reference = NULL, name = "x", min_rows = 1L, min_cols = 2L) {
  if (!is.matrix(x) || !is.numeric(x) || is.complex(x)) {
    stop(sprintf("`%s` must be a numeric matrix.", name), call. = FALSE)
  }
  if (nrow(x) < min_rows || ncol(x) < min_cols) {
    stop(sprintf("`%s` must have at least %d rows and %d columns.", name, min_rows, min_cols), call. = FALSE)
  }
  if (anyNA(x)) stop(sprintf("`%s` must not contain NA or NaN values.", name), call. = FALSE)
  if (any(!is.finite(x))) stop(sprintf("`%s` must not contain Inf values.", name), call. = FALSE)
  columns <- colnames(x)
  if (!is.null(columns) && (anyNA(columns) || any(!nzchar(trimws(columns))) || anyDuplicated(columns))) {
    stop(sprintf("`%s` column names must be nonempty and unique.", name), call. = FALSE)
  }
  if (!is.null(reference)) {
    if (ncol(x) != ncol(reference)) stop("`newx` must have the same number of features as the training matrix.", call. = FALSE)
    expected <- colnames(reference)
    if (!is.null(expected)) {
      if (is.null(columns) || !setequal(columns, expected)) {
        stop("`newx` column names must match the training features.", call. = FALSE)
      }
      x <- x[, expected, drop = FALSE]
    }
  }
  x
}

.xplus_binary_labels <- function(y, n, name = "y", require_both = FALSE) {
  if (!is.null(dim(y)) || length(y) != n || !(is.numeric(y) || is.logical(y) || is.character(y) || is.factor(y))) {
    stop(sprintf("`%s` must be a binary vector with one label per observation.", name), call. = FALSE)
  }
  if (is.factor(y)) y <- as.character(y)
  if (anyNA(y) || !all(y %in% c(0, 1))) {
    stop(sprintf("`%s` must contain only 0 and 1, without missing values.", name), call. = FALSE)
  }
  y <- as.integer(y)
  if (require_both && !all(0:1 %in% y)) {
    stop(sprintf("`%s` must contain both positive and unlabeled observations.", name), call. = FALSE)
  }
  y
}

.xplus_scalar <- function(value, name, lower = -Inf, upper = Inf, integer = FALSE,
                          lower_open = FALSE, upper_open = FALSE) {
  if (!is.numeric(value) || is.complex(value) || length(value) != 1L || !is.finite(value) ||
      value < lower || value > upper || (lower_open && value == lower) ||
      (upper_open && value == upper) || (integer && value != round(value))) {
    stop(sprintf("`%s` must be a finite %s in %s%s, %s%s.", name,
                 if (integer) "integer" else "number", if (lower_open) "(" else "[",
                 lower, upper, if (upper_open) ")" else "]"), call. = FALSE)
  }
  value
}

.xplus_probabilities <- function(p, n = NULL) {
  if (!is.numeric(p) || is.complex(p) || any(!is.finite(p)) || any(p < 0 | p > 1)) {
    stop("Predictions must be finite probabilities in [0, 1].", call. = FALSE)
  }
  if (!is.null(n) && NROW(p) != n) stop("Prediction rows do not match the observations.", call. = FALSE)
  p
}
