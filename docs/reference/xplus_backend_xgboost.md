# XGBoost gradient-boosting backend for xplus

Gradient-boosted trees via
[`xgboost::xgb.train()`](https://rdrr.io/pkg/xgboost/man/xgb.train.html).
Binary targets use `binary:logistic`, fractional pseudo-labels
`reg:squarederror` (clipped to `[0, 1]` on prediction). Hyperparameters
use parsnip-harmonized names so a `boost_tree()` spec can map onto them
later. Boosting memorizes training targets like any high-capacity
learner, so `fit_predict` returns out-of-fold predictions computed with
`xgb.cv` on the folds
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md) already
assigned — the same honest-score role ranger's OOB plays, at the cost of
one extra cross-validation pass per final fit.

## Usage

``` r
xplus_backend_xgboost(
  trees = 100,
  learn_rate = 0.3,
  tree_depth = 6,
  min_n = 1,
  mtry = NULL,
  sample_size = 1,
  loss_reduction = 0,
  num.threads = 1,
  seed = NULL
)
```

## Arguments

- trees:

  Number of boosting rounds (xgboost `nrounds`).

- learn_rate:

  Shrinkage step (xgboost `eta`).

- tree_depth:

  Maximum tree depth (xgboost `max_depth`).

- min_n:

  Minimum child weight (xgboost `min_child_weight`).

- mtry:

  Number of features sampled per split; `NULL` uses all features.
  Converted to `colsample_bynode` as a proportion of `ncol(x)`.

- sample_size:

  Row subsampling fraction per tree (xgboost `subsample`).

- loss_reduction:

  Minimum loss reduction to split (xgboost `gamma`).

- num.threads:

  OpenMP threads (xgboost `nthread`).

- seed:

  Optional seed passed to each fit; `NULL` draws from R's RNG per fit so
  that `xplus(seed = ...)` remains reproducible.

## Value

An `"xplus_backend"` object.

## See also

[`xplus_backend()`](https://alrobles.github.io/xplus/reference/xplus_backend.md),
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md),
[`xplus_backend_ranger()`](https://alrobles.github.io/xplus/reference/xplus_backend_ranger.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- xplus(x, y, backend = xplus_backend_xgboost(trees = 200))
} # }
```
