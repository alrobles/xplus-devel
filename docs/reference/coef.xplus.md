# Extract coefficients from an xplus model

Only defined for models fitted with the `"glmnet"` backend; for other
backends use
[`get_importance()`](https://alrobles.github.io/xplus/reference/get_importance.md)
instead.

## Usage

``` r
# S3 method for class 'xplus'
coef(object, s = "lambda.min", ...)
```

## Arguments

- object:

  An `xplus` object.

- s:

  Penalty value name or numeric lambda.

- ...:

  Additional arguments.

## Value

A sparse coefficient matrix.

## See also

[`summary.xplus()`](https://alrobles.github.io/xplus/reference/summary.xplus.md),
[`print.xplus()`](https://alrobles.github.io/xplus/reference/print.xplus.md)

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(100 * 5), ncol = 5)
y <- c(rep(1, 20), rep(0, 80))
fit <- xplus(x, y, max_iter = 5)
coef(fit)
#> 6 x 1 sparse Matrix of class "dgCMatrix"
#>             lambda.min
#> (Intercept) 0.09653480
#> V1          0.16658313
#> V2          .         
#> V3          0.04972193
#> V4          0.29994787
#> V5          .         
```
