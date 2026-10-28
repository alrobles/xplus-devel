# Elastic-net logistic backend for xplus

The default backend: penalized binomial regression with internal
cross-validation via
[`glmnet::cv.glmnet()`](https://rdrr.io/pkg/glmnet/man/cv.glmnet.html),
reproducing the historical `xplus` behavior.

## Usage

``` r
xplus_backend_glmnet(alpha = 1, cv_measure = c("deviance", "auc"))
```

## Arguments

- alpha:

  Elastic-net mixing parameter in `[0, 1]`, the same as the `alpha`
  argument of
  [`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md).

- cv_measure:

  Cross-validation criterion (`"deviance"` or `"auc"`).

## Value

An `"xplus_backend"` object.

## See also

[`xplus_backend()`](https://alrobles.github.io/xplus/reference/xplus_backend.md),
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md)

## Examples

``` r
backend <- xplus_backend_glmnet(alpha = 0.5)
```
