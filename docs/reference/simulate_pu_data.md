# Simulate a positive-unlabeled dataset

Generate a binary dataset with known ground truth for benchmarking
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md)
backends. Features are independent standard normals; the true positive
probability follows a configurable decision boundary. Positive cases are
observed under the SCAR assumption (selected completely at random): each
true positive is labeled with constant probability `label_propensity`,
and everything else remains unlabeled.

## Usage

``` r
simulate_pu_data(
  n = 500,
  p = 10,
  boundary = c("linear", "interaction", "nonlinear"),
  prevalence = 0.5,
  label_propensity = 0.5,
  n_informative = min(5L, p),
  sharpness = 2,
  seed = NULL
)
```

## Arguments

- n:

  Number of observations.

- p:

  Number of features.

- boundary:

  Decision boundary driving the positive probability: `"linear"` (sparse
  linear predictor), `"interaction"` (pure pairwise products), or
  `"nonlinear"` (radial, positives outside a sphere).

- prevalence:

  Target fraction of true positives in `[0, 1]`.

- label_propensity:

  Constant probability that a true positive is observed as a labeled
  positive, in `(0, 1]`.

- n_informative:

  Number of features driving the boundary.

- sharpness:

  Positive logistic scale on the centered boundary score; larger values
  make the boundary more deterministic.

- seed:

  Optional nonnegative integer seed.

## Value

A list of class `"xplus_pu_data"` with: `x` (feature matrix), `y`
(observed PU labels, `1` = labeled positive, `0` = unlabeled), `z` (true
binary labels), `p_true` (true positive probability per observation),
and the simulation settings.

## See also

[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md)

## Examples

``` r
sim <- simulate_pu_data(n = 300, p = 8, boundary = "interaction", seed = 1)
fit <- xplus(sim$x, sim$y, max_iter = 5)
auc(sim$z, as.numeric(predict(fit)))
#> [1] 0.5
```
