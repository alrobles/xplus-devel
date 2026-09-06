# Internal helper: normalize anchor residuals for the PLUS transformation.
#
# After subtracting the positive-reference quantile cutoff, predicted
# probabilities become signed residuals (`map_pred_y`).  This function
# normalises each side independently so that the maximum positive residual
# maps to +1 and the most-negative residual maps to -1 before the sigmoid
# `1 / (1 + exp(-10 * map_pred_y))` is applied.
#
# Near-degenerate case: if all positive (or all negative) residuals are
# smaller in magnitude than `degenerate_threshold`, those residuals are
# clamped to 0 and a warning is issued.  Clamping to 0 yields a neutral
# prediction of exactly 0.5 from the downstream sigmoid, which is
# deterministic and explicitly documented.
#
# @param map_pred_y Numeric vector of residuals (pred_y - cutoff).
# @param degenerate_threshold Positive scalar.  Residuals whose maximum
#   absolute value falls below this limit are treated as near-degenerate.
#   Defaults to 1e-6.
# @return Numeric vector of the same length as `map_pred_y` with
#   normalised (or clamped) residuals.
# @noRd
normalize_residuals <- function(map_pred_y, degenerate_threshold = 1e-6) {
  if (!is.numeric(map_pred_y) || is.complex(map_pred_y) || any(!is.finite(map_pred_y))) {
    stop("Residuals must be finite numeric values.", call. = FALSE)
  }
  .xplus_scalar(degenerate_threshold, "degenerate_threshold", 0)
  if (any(map_pred_y > 0)) {
    max_pos <- max(map_pred_y[map_pred_y > 0])
    if (max_pos < degenerate_threshold) {
      warning(
        sprintf(
          "Near-degenerate positive scaling detected (max residual=%.2e); positive residuals clamped to 0.",
          max_pos
        ),
        call. = FALSE
      )
      # Clamp to 0: downstream sigmoid yields 0.5 (neutral prediction).
      map_pred_y[map_pred_y > 0] <- 0
    } else {
      map_pred_y[map_pred_y > 0] <- map_pred_y[map_pred_y > 0] / max_pos
    }
  }

  if (any(map_pred_y < 0)) {
    min_neg <- abs(min(map_pred_y[map_pred_y < 0]))
    if (min_neg < degenerate_threshold) {
      warning(
        sprintf(
          "Near-degenerate negative scaling detected (min residual=%.2e); negative residuals clamped to 0.",
          min_neg
        ),
        call. = FALSE
      )
      # Clamp to 0: downstream sigmoid yields 0.5 (neutral prediction).
      map_pred_y[map_pred_y < 0] <- 0
    } else {
      map_pred_y[map_pred_y < 0] <- map_pred_y[map_pred_y < 0] / min_neg
    }
  }

  map_pred_y
}
