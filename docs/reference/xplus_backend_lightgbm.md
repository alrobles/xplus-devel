# LightGBM gradient-boosting backend for xplus

Gradient-boosted trees via
[`lightgbm::lgb.train()`](https://rdrr.io/pkg/lightgbm/man/lgb.train.html).
Binary targets use the `"binary"` objective, fractional pseudo-labels
`"regression"` (clipped to `[0, 1]` on prediction). Hyperparameters use
parsnip-harmonized names. `fit_predict` returns out-of-fold predictions
via `lgb.cv` on the folds
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md)
assigns, so the classification cutoff is calibrated on honest scores
rather than memorized ones.

## Usage

``` r
xplus_backend_lightgbm(
  trees = 100,
  learn_rate = 0.1,
  tree_depth = -1,
  min_n = 20,
  mtry = NULL,
  sample_size = 1,
  loss_reduction = 0,
  num.threads = 1,
  seed = NULL
)
```

## Arguments

- trees:

  Number of boosting rounds (lightgbm `num_iterations`).

- learn_rate:

  Shrinkage step (lightgbm `learning_rate`).

- tree_depth:

  Maximum tree depth (lightgbm `max_depth`; `-1` unbounded).

- min_n:

  Minimum observations per leaf (lightgbm `min_data_in_leaf`).

- mtry:

  Number of features sampled per iteration; `NULL` uses all. Converted
  to `feature_fraction` as a proportion of `ncol(x)`.

- sample_size:

  Row subsampling fraction per iteration (lightgbm `bagging_fraction`;
  enables bagging when below 1).

- loss_reduction:

  Minimum gain to split (lightgbm `min_gain_to_split`).

- num.threads:

  OpenMP threads (lightgbm `num_threads`).

- seed:

  Optional seed passed to each fit; `NULL` draws from R's RNG per fit so
  that `xplus(seed = ...)` remains reproducible.

## Value

An `"xplus_backend"` object.

## See also

[`xplus_backend()`](https://alrobles.github.io/xplus/reference/xplus_backend.md),
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md),
[`xplus_backend_xgboost()`](https://alrobles.github.io/xplus/reference/xplus_backend_xgboost.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- xplus(x, y, backend = xplus_backend_lightgbm(trees = 200))
} # }
```
