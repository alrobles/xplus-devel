# Engine fit and prediction entry points for `pu_xplus`

These wrappers are registered with parsnip and invoked when a `pu_xplus`
spec is fitted or predicted on; they are not meant to be called
directly. `xplus_parsnip_fit()` resolves the backend argument, folds
extra constructor arguments into the matching `xplus_backend_*()`
constructor (ignoring arguments the backend does not accept, with a
warning), and calls
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md). The
prediction adapters return parsnip-shaped outputs: `.pred_0`/`.pred_1`
columns for probabilities, `.pred` for numeric, and a `0/1` factor for
class.

## Usage

``` r
xplus_parsnip_fit(
  x,
  y,
  backend = "glmnet",
  seed = NULL,
  max_iter = 100,
  nfolds = 10,
  ...
)

xplus_parsnip_prob(object, new_data)

xplus_parsnip_num(object, new_data)

xplus_parsnip_class(object, new_data)
```

## Arguments

- x:

  Predictor data (data frame or matrix).

- y:

  Outcome (factor `0/1`, or coercible vector).

- backend, seed, max_iter, nfolds:

  `pu_xplus` arguments, see
  [`pu_xplus()`](https://alrobles.github.io/xplus/reference/pu_xplus.md).

- ...:

  Backend constructor arguments forwarded from the spec.

- object:

  The fitted `xplus` object (parsnip passes `object$fit`).

- new_data:

  New predictor data for prediction.

## Value

`xplus_parsnip_fit()` returns an
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md) object;
prediction helpers return tibbles or factors in parsnip shape.
