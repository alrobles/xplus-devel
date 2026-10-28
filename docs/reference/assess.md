# Assess predictive performance

Assess predictive performance

## Usage

``` r
assess(object, newx = NULL, newy, weights = NULL, ...)

# S3 method for class 'xplus'
assess(object, newx = NULL, newy, weights = NULL, ...)
```

## Arguments

- object:

  A model object.

- newx:

  Optional feature matrix.

- newy:

  Binary 0/1 labels or a two-column finite nonnegative matrix of
  negative and positive class masses; soft vectors are not accepted.

- weights:

  Optional finite nonnegative numeric row weights without recycling;
  NULL means unit weights.

- ...:

  Additional arguments passed to
  [`predict()`](https://rdrr.io/r/stats/predict.html).

## Value

A named list with `deviance`, `class`, `auc`, `mse`, and `mae`. For
`class` metric, the threshold used is the model's cutoff (from
`object$cutoff`), consistent with `predict(type = "class")`; MSE and MAE
sum both class-column losses (twice the scalar loss). Undefined metrics
return NA with a warning.

## References

Zhou et al. (2022). doi:10.1371/journal.pcbi.1009956

## See also

[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md),
[`get_auc()`](https://alrobles.github.io/xplus/reference/get_auc.md)

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(100 * 5), ncol = 5)
y <- c(rep(1, 20), rep(0, 80))
fit <- xplus(x, y, max_iter = 5)
assess(fit, newx = x, newy = y)
#> $deviance
#> [1] 1.467885
#> 
#> $class
#> [1] 0.65
#> 
#> $auc
#> [1] 0.575
#> 
#> $mse
#> [1] 0.5396069
#> 
#> $mae
#> [1] 1.024311
#> 
```
