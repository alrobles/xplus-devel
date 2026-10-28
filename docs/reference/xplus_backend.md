# Specify a fitting backend for xplus

Describe a learner that
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md) uses
for both the iterative pseudo-labeling fits and the final model.
Built-in backends are
[`xplus_backend_glmnet()`](https://alrobles.github.io/xplus/reference/xplus_backend_glmnet.md),
[`xplus_backend_ranger()`](https://alrobles.github.io/xplus/reference/xplus_backend_ranger.md),
[`xplus_backend_gam()`](https://alrobles.github.io/xplus/reference/xplus_backend_gam.md),
[`xplus_backend_xgboost()`](https://alrobles.github.io/xplus/reference/xplus_backend_xgboost.md),
and
[`xplus_backend_lightgbm()`](https://alrobles.github.io/xplus/reference/xplus_backend_lightgbm.md);
this constructor defines custom learners with the same contract.

## Usage

``` r
xplus_backend(
  name,
  fit,
  predict,
  importance = NULL,
  label = name,
  uses_cv = FALSE,
  parameters = NULL,
  fit_predict = NULL,
  supports_fractional = TRUE,
  supports_weights = TRUE
)
```

## Arguments

- name:

  Nonempty character scalar identifying the backend.

- fit:

  Function with signature `fit(x, y, foldid, weights)` returning a
  fitted model object. `x` is the finite numeric feature matrix, `y` a
  numeric target in `[0, 1]` (binary during iterations, possibly
  fractional in the final fit), `foldid` the stratified cross-validation
  fold assignment, and `weights` nonnegative case weights encoding
  bootstrap multiplicities. To report a cross-validation measure in the
  iteration history, attach `attr(model, "xplus_cv_measure")` with
  `"deviance"` or `"auc"`; otherwise `"none"` is recorded.

- predict:

  Function with signature `predict(model, newx)` returning finite
  probabilities in `[0, 1]`, one per row of `newx`.

- importance:

  Optional function with signature `importance(model)` returning a named
  numeric vector of feature scores, or `NULL`.

- label:

  Human-readable backend label; defaults to `name`.

- uses_cv:

  Logical scalar; whether `fit` performs internal cross-validation whose
  measure is recorded in the iteration history.

- parameters:

  Optional data frame declaring tunable hyperparameters following the
  caret `modelInfo` convention (columns `parameter`, `class`, and
  `label`); informational only, it enables future tuning and AutoML
  adapters without changing the backend contract.

- fit_predict:

  Optional function with signature `fit_predict(model, x)` returning
  honest probabilities for the rows passed to `fit`, in the same order —
  out-of-bag or held-out-fold estimates rather than in-sample
  memorization.
  [`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md) uses
  them after the final fit for cutoff calibration and the stored
  training predictions; non-finite entries fall back to `predict` on
  those rows. When `NULL`, `predict` is reused on training rows,
  appropriate for learners that do not memorize per-observation targets.
  Iterative pseudo-label fits always use `predict`, which keeps
  self-training dynamics uniform across backends.

- supports_fractional:

  Logical scalar; whether `fit` handles fractional targets in `[0, 1]`
  (the final pseudo-label fit). When `FALSE`,
  [`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md) fails
  with a clear error if the final targets are non-binary instead of
  surfacing an engine-specific error deep in the learner.

- supports_weights:

  Logical scalar; whether `fit` honors case weights. When `FALSE`,
  [`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md) warns
  once that `sampling = "bootstrap"` multiplicities will be silently
  ignored by this learner.

## Value

An object of class `"xplus_backend"` accepted by the `backend` argument
of [`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md).

## Details

The contract mirrors the minimal estimator interface shared by caret
`modelInfo` entries, sklearn `fit`/`predict_proba` estimators (the basis
of PU wrappers such as `ElkanotoPuClassifier`), and mlr3 `Learner`
objects: fit weighted `(x, y)` data, then predict probabilities. `xplus`
additionally supplies `foldid` (stratified CV folds for backends that
use them) and soft fractional targets in the final fit, which generic
frameworks do not provide.

## See also

[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md),
[`xplus_backend_ranger()`](https://alrobles.github.io/xplus/reference/xplus_backend_ranger.md),
[`xplus_backend_gam()`](https://alrobles.github.io/xplus/reference/xplus_backend_gam.md)

## Examples

``` r
constant <- xplus_backend(
  name = "constant",
  fit = function(x, y, foldid, weights) stats::weighted.mean(y, weights),
  predict = function(model, newx) rep(model, nrow(newx))
)
fit <- xplus(matrix(rnorm(100 * 5), ncol = 5), c(rep(1, 20), rep(0, 80)),
  backend = constant, max_iter = 3)
```
