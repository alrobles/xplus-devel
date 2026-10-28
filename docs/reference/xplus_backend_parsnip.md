# Parsnip model-spec backend for xplus

Wrap a
[parsnip::model_spec](https://parsnip.tidymodels.org/reference/model_spec.html)
— e.g.
[`parsnip::rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
combined with
[`parsnip::set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html)
— as an [`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md)
backend, giving the PLUS loop access to the full tidymodels engine zoo.
The spec's mode is set per fit: `"classification"` when the target is
binary (iterative pseudo-label fits), `"regression"` when fractional
(the final fit), so the model must support both modes or
[`parsnip::fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html)
reports the gap. Case weights are passed as
[`hardhat::frequency_weights()`](https://hardhat.tidymodels.org/reference/frequency_weights.html)
when integral and
[`hardhat::importance_weights()`](https://hardhat.tidymodels.org/reference/importance_weights.html)
otherwise; `foldid` is unused because parsnip specs carry no internal
resampling.

## Usage

``` r
xplus_backend_parsnip(spec, supports_fractional = TRUE)
```

## Arguments

- spec:

  A `parsnip` model specification produced by a model function such as
  [`parsnip::rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html),
  [`parsnip::boost_tree()`](https://parsnip.tidymodels.org/reference/boost_tree.html)
  or
  [`parsnip::logistic_reg()`](https://parsnip.tidymodels.org/reference/logistic_reg.html),
  usually piped through
  [`parsnip::set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html).
  The engine must be set and its package installed.

- supports_fractional:

  Logical scalar; declare `FALSE` for specs whose model lacks a
  regression mode (e.g. `logistic_reg`) so
  [`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md) fails
  early with a clear message instead of at the fractional final fit.

## Value

An `"xplus_backend"` object named `parsnip:<engine>`.

## Details

`fit_predict` returns out-of-bag scores when the underlying engine
stores them on the fitted object (ranger engines) and falls back to
in-sample `predict` otherwise, so honest cutoff calibration is preserved
where the engine makes it possible. Engines that consume a `seed`
argument (ranger, xgboost, lightgbm) get one drawn from R's RNG when the
spec does not set it explicitly, keeping `xplus(seed = ...)`
reproducible. Feature importance is read from the fitted engine object
when present; declare it in the spec, e.g.
`set_engine("ranger", importance = "impurity")`.

## See also

[`xplus_backend()`](https://alrobles.github.io/xplus/reference/xplus_backend.md),
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md),
[`xplus_backend_ranger()`](https://alrobles.github.io/xplus/reference/xplus_backend_ranger.md)

## Examples

``` r
if (FALSE) { # \dontrun{
backend <- parsnip::rand_forest(trees = 500) |>
  parsnip::set_engine("ranger") |>
  xplus_backend_parsnip()
fit <- xplus(x, y, backend = backend)
} # }
```
