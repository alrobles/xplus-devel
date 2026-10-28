# xplus as a parsnip model

Register
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md) as a
parsnip model (`pu_xplus` with engine `"xplus"`) so PU learning composes
with tidymodels:
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html),
[`finetune::tune_sim_anneal()`](https://finetune.tidymodels.org/reference/tune_sim_anneal.html),
and `rsample` resamples all work against the backend contract. The
`backend` argument selects the learner — a built-in name, an
[`xplus_backend()`](https://alrobles.github.io/xplus/reference/xplus_backend.md)
spec, or a constructor name — and backend-constructor arguments
(`trees`, `learn_rate`, `tree_depth`, `min_n`, `mtry`, `sample_size`,
`loss_reduction`) are tunable through `tune()` placeholders.
Registration happens automatically when parsnip is installed; call this
function to re-register after loading parsnip manually.

## Usage

``` r
xplus_parsnip_register()
```

## Value

Invisibly, `TRUE` when the model was registered.

## Examples

``` r
if (FALSE) { # \dontrun{
library(parsnip)
spec <- pu_xplus(backend = "xgboost", trees = 300, learn_rate = 0.05) |>
  set_engine("xplus")
fit <- fit_xy(spec, x = data.frame(x), y = factor(y, levels = 0:1))
} # }
```
