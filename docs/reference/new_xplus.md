# Construct a new xplus object

Construct a new xplus object

## Usage

``` r
new_xplus(
  fit_xplus = list(),
  pred_y = matrix(),
  cutoff = numeric(),
  predicted_coefficients = Matrix::Matrix(),
  n_iter = integer(),
  x = matrix(),
  y = numeric(),
  alpha = numeric(),
  learning_rate = numeric(),
  pseudo_labels = numeric(),
  iterative_path = character(),
  qq = numeric(),
  call = character(),
  max_iter = integer(),
  stop_reason = character(),
  original_y = NULL,
  final_labels = y,
  fallback_used = NULL,
  fallback_reason = NULL,
  history = NULL,
  sampling_counts = NULL,
  draw_counts = NULL,
  final_foldid = NULL,
  cv_measure = NULL,
  sigmoid_scale = NULL,
  sampling = NULL,
  min_iter = NULL,
  stability_window = NULL,
  min_coverage = NULL,
  backend = NULL
)
```

## Arguments

- fit_xplus:

  Fitted
  [`glmnet::cv.glmnet()`](https://rdrr.io/pkg/glmnet/man/cv.glmnet.html)
  object.

- pred_y:

  Predicted probabilities matrix.

- cutoff:

  Numeric classification cutoff.

- predicted_coefficients:

  Sparse coefficient matrix.

- n_iter, history, sampling_counts, draw_counts:

  Number of completed iterations, per-iteration diagnostics, and
  per-observation unlabeled inclusion-round and draw counts; optional
  diagnostics default to `NULL`.

- x:

  Training feature matrix used to fit the model.

- y, final_labels:

  Identical actual target probabilities used for final fitting;
  `final_labels` defaults to `y`.

- alpha:

  Elastic-net alpha used during fitting.

- learning_rate:

  Learning rate used during pseudo-label updates.

- pseudo_labels, original_y, fallback_used, fallback_reason:

  Proposed probabilities before fallback, original numeric binary
  labels, fallback flag and reason (empty if unused); optional metadata
  default to `NULL` for legacy bundles.

- iterative_path, final_foldid, cv_measure, sigmoid_scale, sampling:

  Iterative path and optional final cross-validation folds, measure
  (`deviance` or `auc`), positive sigmoid scale, and sampling mode
  (`bootstrap` or `unique`); optional controls default to `NULL`.

- qq:

  Quantile parameter used for cutoff calibration.

- call:

  Original function call.

- max_iter, min_iter, stability_window, min_coverage:

  Maximum iterations and optional minimum iterations, consecutive
  stability rounds, and unlabeled coverage required for stopping;
  optional controls default to `NULL`.

- stop_reason:

  Reason fitting stopped: `"max_iter"`, `"label_stability"`,
  `"budget_exhausted"`, or `"degenerate_labels"` (the pseudo-labels of
  the iterative training subset collapsed to a single class).

- backend:

  [`xplus_backend()`](https://alrobles.github.io/xplus/reference/xplus_backend.md)
  spec used for the fits; `NULL` denotes a legacy object fitted before
  backend support, equivalent to `"glmnet"`.

## Value

An object of class `"xplus"`.

## See also

[`validate_xplus()`](https://alrobles.github.io/xplus/reference/validate_xplus.md)
