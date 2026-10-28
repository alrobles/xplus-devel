# GAM logistic backend for xplus

Additive logistic models via
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) with a smoothing
spline per feature. Fractional pseudo-labels are fitted with a
quasibinomial family, binary targets with the binomial family.

## Usage

``` r
xplus_backend_gam(k = NULL, method = "REML", max_features = 100)
```

## Arguments

- k:

  Basis dimension passed to each
  [`mgcv::s()`](https://rdrr.io/pkg/mgcv/man/s.html) term; `NULL` uses
  the mgcv default.

- method:

  Smoothing-parameter estimation method for binary targets (fractional
  targets always use `"GCV.Cp"`, required by the quasibinomial family).

- max_features:

  Maximum number of features accepted; GAM cost grows quickly with
  dimension.

## Value

An `"xplus_backend"` object.

## See also

[`xplus_backend()`](https://alrobles.github.io/xplus/reference/xplus_backend.md),
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md)

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- xplus(x, y, backend = xplus_backend_gam())
} # }
```
