# PU learning baselines on the xplus backend contract

Classical positive-unlabeled algorithms expressed on the same backend
contract as
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md), for
honest head-to-head comparisons.

## Usage

``` r
xplus_elkan_noto(
  x,
  y,
  backend = "glmnet",
  seed = NULL,
  nfolds = 10,
  estimate = c("mean", "median"),
  alpha = 1,
  cv_measure = "deviance"
)

xplus_naive(
  x,
  y,
  backend = "glmnet",
  seed = NULL,
  nfolds = 10,
  alpha = 1,
  cv_measure = "deviance"
)
```

## Arguments

- x:

  Finite numeric feature matrix with one row per observation.

- y:

  Binary PU labels: `1` labeled positive, `0` unlabeled.

- backend:

  A backend name or
  [`xplus_backend()`](https://alrobles.github.io/xplus/reference/xplus_backend.md)
  object, resolved exactly as in
  [`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md).

- seed:

  Optional integer seed for reproducible resampling.

- nfolds:

  Folds passed to the backend `fit` contract (used by CV backends for
  honest predictions).

- estimate:

  How to estimate the labeling propensity `c`: `"mean"` (Elkan & Noto's
  validation-set average) or `"median"` over labeled positives, which is
  more robust to contaminated positives.

- alpha, cv_measure:

  glmnet backend configuration, as in
  [`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md).

## Value

An `xplus_baseline` object with `predict` support, `$cutoff` (0.5 on the
rescaled scale for Elkan-Noto), `$scale` (estimated `c`), and `$method`.

## Details

`xplus_elkan_noto()` implements the Elkan & Noto (2008) recipe: train a
classifier separating labeled positives (`s = 1`) from the unlabeled
pool, estimate the labeling propensity `c = P(s = 1 | y = 1)` from
held-out-style predictions on labeled positives, and rescale
`p(y = 1 | x) = p(s = 1 | x) / c` clipped to one. `xplus_naive()` is the
lower bound: a supervised classifier trained treating all unlabeled
instances as negatives.

## References

Elkan, C. and Noto, K. (2008). Learning classifiers from only positive
and unlabeled data. KDD.

## Examples

``` r
set.seed(1)
sim <- simulate_pu_data(n = 200, p = 5, prevalence = 0.4,
                        label_propensity = 0.5)
fit <- xplus_elkan_noto(sim$x, sim$y, backend = "ranger")
head(predict(fit, newx = sim$x))
#> [1] 0.27426037 0.35425298 1.00000000 1.00000000 0.08570637 0.38282177
```
