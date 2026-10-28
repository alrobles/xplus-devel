# parsnip model spec for PU learning with xplus

Create a parsnip spec driving
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md); use
with
[`parsnip::fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html)/`fit()`,
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html),
or
[`finetune::tune_sim_anneal()`](https://finetune.tidymodels.org/reference/tune_sim_anneal.html).

## Usage

``` r
pu_xplus(
  mode = "classification",
  engine = "xplus",
  backend = "glmnet",
  seed = NULL,
  max_iter = 100,
  nfolds = 10,
  trees = NULL,
  learn_rate = NULL,
  tree_depth = NULL,
  min_n = NULL,
  mtry = NULL,
  sample_size = NULL,
  loss_reduction = NULL,
  ...
)
```

## Arguments

- mode:

  Model mode; `"classification"` is the only supported value.

- engine:

  Fitting engine; `"xplus"`.

- backend:

  Backend selector: a built-in name such as `"glmnet"` or `"xgboost"`,
  an
  [`xplus_backend()`](https://alrobles.github.io/xplus/reference/xplus_backend.md)
  object, or a constructor function. When a name is given, the boost
  hyperparameter arguments below map onto the matching
  `xplus_backend_*()` constructor and accept `tune()`.

- seed, max_iter, nfolds:

  [`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md)
  controls forwarded verbatim.

- trees, learn_rate, tree_depth, min_n, mtry, sample_size,
  loss_reduction:

  Backend constructor arguments; passed through to the matching
  `xplus_backend_*()` constructor when `backend` is a name.

- ...:

  Reserved.

## Value

A parsnip `model_spec`.
