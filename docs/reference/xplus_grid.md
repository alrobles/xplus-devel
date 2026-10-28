# Compare learners or hyperparameter grids on PU data

The model-selection piece of the multi-backend API: fit
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md) for
every candidate and rank them by a PU-valid
[`pu_score()`](https://alrobles.github.io/xplus/reference/pu_score.md) —
no negative labels required. `learners` accepts a character vector of
built-in backend names, a list mixing names and
[`xplus_backend()`](https://alrobles.github.io/xplus/reference/xplus_backend.md)
objects, or a single constructor function (e.g. `xplus_backend_xgboost`)
combined with `params`, a data frame whose columns are constructor
arguments and whose rows form the tuning grid. One shared `seed` gives
every candidate the same bootstrap draws, so the comparison is paired
rather than noisy. Failures are captured per candidate instead of
aborting the grid.

## Usage

``` r
xplus_grid(x, y, learners, params = NULL, metric = "pu_auc", seed = NULL, ...)
```

## Arguments

- x:

  Feature matrix as in
  [`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md).

- y:

  Binary PU labels: 1 positive, 0 unlabeled.

- learners:

  Character vector of backend names, a list of backend names/specs, or a
  backend constructor function to combine with `params`.

- params:

  Optional data frame of constructor arguments when `learners` is a
  function; each row is one candidate.

- metric:

  Scoring metric passed to
  [`pu_score()`](https://alrobles.github.io/xplus/reference/pu_score.md).

- seed:

  Optional seed shared by every candidate fit.

- ...:

  Additional arguments passed to
  [`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md) for
  every candidate.

## Value

A data frame with one row per candidate: `learner`, any `params`
columns, `score`, `n_iter`, `stop_reason`, `seconds`, and `error` (`NA`
on success). Sorted by descending `score`.

## See also

[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md),
[`pu_score()`](https://alrobles.github.io/xplus/reference/pu_score.md),
[`xplus_backend()`](https://alrobles.github.io/xplus/reference/xplus_backend.md)

## Examples

``` r
sim <- simulate_pu_data(n = 300, p = 6, boundary = "nonlinear", seed = 1,
                        prevalence = 0.4, label_propensity = 0.5)
xplus_grid(sim$x, sim$y, c("glmnet", "ranger"), max_iter = 5, seed = 1)
#>   learner     score n_iter stop_reason seconds error
#> 2  ranger 0.7580417      5    max_iter   0.305  <NA>
#> 1  glmnet 0.5981303      5    max_iter   0.081  <NA>
```
