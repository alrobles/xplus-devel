# Ranger random-forest backend for xplus

Random forests via
[`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md).
Binary targets use a probability forest and fractional pseudo-labels a
regression forest, so soft final-fit targets are supported natively. The
backend performs no internal cross-validation; its hyperparameters are
fixed at construction. Out-of-bag predictions supply `fit_predict`, so
the stored predictions and classification cutoff use honest scores
rather than in-sample memorization.

## Usage

``` r
xplus_backend_ranger(
  trees = 500,
  mtry = NULL,
  min_n = 1,
  max.depth = 0,
  num.threads = 1,
  seed = NULL,
  importance.mode = c("impurity", "permutation", "none")
)
```

## Arguments

- trees:

  Number of trees (ranger `num.trees`); the parsnip-harmonized name is
  used so a parsnip bridge can map `rand_forest(trees = ...)` later.

- mtry:

  Number of variables sampled per split; `NULL` uses the ranger default.

- min_n:

  Minimum node size (ranger `min.node.size`).

- max.depth, num.threads:

  Engine-specific ranger controls forwarded unchanged.

- seed:

  Optional seed passed to each ranger fit. When `NULL`, one is drawn
  from R's RNG per fit so that `xplus(seed = ...)` remains reproducible.

- importance.mode:

  Variable-importance mode (`"impurity"`, `"permutation"`, or `"none"`).

## Value

An `"xplus_backend"` object.

## See also

[`xplus_backend()`](https://alrobles.github.io/xplus/reference/xplus_backend.md),
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- xplus(x, y, backend = xplus_backend_ranger(trees = 1000))
} # }
```
