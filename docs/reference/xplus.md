# Fit an xplus model

Fit a PLUS-derived model for positive-unlabeled learning.

## Usage

``` r
xplus(
  x,
  y,
  alpha = 1,
  sample_use_time = 30,
  learning_rate = 1,
  qq = 0.1,
  verbose = FALSE,
  nfolds = 4,
  max_iter = 10000,
  convergence_threshold = 0.9,
  seed = NULL,
  sigmoid_scale = 10,
  min_iter = 5,
  stability_window = 5,
  min_coverage = 0.9,
  sampling = c("bootstrap", "unique"),
  cv_measure = c("deviance", "auc"),
  degenerate_threshold = 1e-06,
  backend = "glmnet"
)
```

## Arguments

- x:

  Finite numeric feature matrix with at least two columns.

- y:

  Binary vector where `1` indicates known positives and `0` indicates
  unlabeled samples; factors are interpreted by their labels.

- alpha:

  Elastic-net mixing parameter in `[0, 1]`, not the sigmoid scale in the
  PLUS paper.

- sample_use_time:

  Unlabeled-sampling budget inherited from the reference implementation:
  maximum number of completed sampling rounds containing each unlabeled
  case, not the number of bootstrap copies within a round.

- learning_rate:

  Pseudo-label smoothing rate in `(0, 1]`; values below one retain the
  package's global-update, hard-thresholded enhancement path.

- qq:

  Quantile used to define the positive-reference cutoff.

- verbose:

  Logical; print iterative progress messages.

- nfolds:

  Requested CV folds, an integer at least three; reduced for small
  classes.

- max_iter:

  Maximum number of pseudo-labeling iterations.

- convergence_threshold:

  Required stability score in `(0, 1]`, evaluated before learning-rate
  damping.

- seed:

  Integer or `NULL`. Random seed for reproducibility, applied via
  [`set.seed()`](https://rdrr.io/r/base/Random.html). Default `NULL` (no
  seed).

- sigmoid_scale, degenerate_threshold:

  Positive sigmoid scale and nonnegative residual-clamping tolerance.

- min_iter, stability_window:

  Minimum iterations and consecutive stable iterations required before
  declaring convergence. `xplus` retains two iterative paths, distinct
  from the paper's pseudocode: `"current"` (`learning_rate = 1`) uses
  sampled Bernoulli labels, while `"continuous_enhancement"`
  (`learning_rate < 1`) smooths probabilities and updates all unlabeled
  hard labels. Both use soft final fitting targets. Stability compares
  the undamped mapped scores with the current pseudo-labels across all
  unlabeled cases and requires a full window and sampling coverage.

- min_coverage:

  Minimum fraction of unlabeled cases sampled before convergence, in
  `[0, 1]`. Bootstrap multiplicities are represented as case weights,
  keeping duplicate copies together in CV. `sampling = "unique"` retains
  legacy deduplication. Final-fit fallback is based on effective class
  mass, not thresholded labels, and is recorded with the actual fitting
  targets and the iteration history.

- sampling, cv_measure:

  Sampling convention (`"bootstrap"` or `"unique"`) and CV criterion
  (`"deviance"` or `"auc"`); deviance is the default for both fitting
  stages.

- backend:

  Learner for the iterative and final fits: one of `"glmnet"`,
  `"ranger"`, `"gam"`, `"xgboost"`, `"lightgbm"`, or an
  [`xplus_backend()`](https://alrobles.github.io/xplus/reference/xplus_backend.md)
  object for custom backends. `alpha` and `cv_measure` only configure
  the `"glmnet"` backend.

## Value

An object of class `"xplus"` containing predictions, original and final
labels, pseudo-labels, fallback metadata, sampling counts, and history.

## Details

Core PLUS behavior alternates between fitting penalized logistic models
on known positives plus sampled unlabeled cases, anchoring predictions
to a positive quantile cutoff, and iteratively relabeling unlabeled
samples.

## References

Zhou et al. (2022). doi:10.1371/journal.pcbi.1009956

## See also

[`predict.xplus()`](https://alrobles.github.io/xplus/reference/predict.xplus.md),
[`summary.xplus()`](https://alrobles.github.io/xplus/reference/summary.xplus.md),
[`assess.xplus()`](https://alrobles.github.io/xplus/reference/assess.md),
[`xplus_backend()`](https://alrobles.github.io/xplus/reference/xplus_backend.md)

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(200 * 10), ncol = 10)
y <- c(rep(1, 40), rep(0, 160))
fit <- xplus(x, y, max_iter = 20)
```
