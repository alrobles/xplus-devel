# Compute AUC for predictions from a model

Compute AUC for predictions from a model

## Usage

``` r
get_auc(object, newx = NULL, newy = NULL, weights = NULL, ...)

# S3 method for class 'xplus'
get_auc(object, newx = NULL, newy = NULL, weights = NULL, ...)
```

## Arguments

- object:

  A model object.

- newx:

  Feature matrix.

- newy:

  True labels.

- weights:

  Optional sample weights.

- ...:

  Arguments passed to
  [`predict.xplus()`](https://alrobles.github.io/xplus/reference/predict.xplus.md),
  such as `s` selecting a single lambda.

## Value

Numeric AUC value.

## See also

[`assess()`](https://alrobles.github.io/xplus/reference/assess.md),
[`auc()`](https://alrobles.github.io/xplus/reference/auc.md)

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(100 * 5), ncol = 5)
y <- c(rep(1, 20), rep(0, 80))
fit <- xplus(x, y, max_iter = 5)
get_auc(fit, newx = x, newy = y)
#> [1] 0.575
```
