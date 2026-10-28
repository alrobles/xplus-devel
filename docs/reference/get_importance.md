# Extract feature scores from an xplus model

Return backend-specific feature scores: elastic-net coefficients at
`"lambda.min"` for `"glmnet"`, variable importance for `"ranger"`, and
smooth-term significance (`-log10` of the mgcv p-value) for `"gam"`.
Backends without an importance function return `NULL`.

## Usage

``` r
get_importance(object)
```

## Arguments

- object:

  An `xplus` object.

## Value

A named numeric vector of feature scores, or `NULL` when the backend
does not provide one or the fitted model is unavailable.

## See also

[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md),
[`xplus_backend()`](https://alrobles.github.io/xplus/reference/xplus_backend.md),
[`coef.xplus()`](https://alrobles.github.io/xplus/reference/coef.xplus.md)

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(100 * 5), ncol = 5)
y <- c(rep(1, 20), rep(0, 80))
fit <- xplus(x, y, max_iter = 5)
get_importance(fit)
#>         V1         V2         V3         V4         V5 
#> 0.16658313 0.00000000 0.04972193 0.29994787 0.00000000 
```
