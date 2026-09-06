# xplus

`xplus` provides **PLUS-derived extensions** for Positive and Unlabeled Learning from Unbalanced Cases and Sparse Structures (Zhou et al., 2022). This development checkout targets **1.0.0**, a major release with breaking numerical and interface contracts. It is not a reproduction of the paper's experiments and does not establish predictive superiority or guarantee calibrated probabilities.

## Installation and development origin

The package name remains `xplus`. The private development repository is <https://github.com/alrobles/xplus-devel>; access must already be configured for your account. No credentials should be embedded in installation commands or committed files.

```r
# install.packages("remotes")
remotes::install_github("alrobles/xplus-devel")
```

The production repository <https://github.com/alrobles/xplus> is separate and unchanged by this development work. Do not use its release results as verification of this candidate.

R >= 4.1.0 is required (the source uses the native pipe), and the declared minimum glmnet version is 4.1-8. The verification environment uses **R 4.5.2 and glmnet 5.0**; the dependency minimum is not a claim that other versions have been tested.

## What is PU learning?

Only known positives are labeled `1`; `0` means unlabeled, not a confirmed negative. Unlabeled cases may contain both negatives and hidden positives. The package iteratively constructs pseudo-labels and fits penalized logistic models. Its response-scale outputs are model scores in `[0, 1]`, not externally validated probabilities of the true PU class.

## Basic workflow

```r
library(xplus)
set.seed(1)
x <- matrix(rnorm(200 * 10), ncol = 10)
colnames(x) <- paste0("feature", seq_len(ncol(x)))
y <- c(rep(1, 40), rep(0, 160))

fit <- xplus(x, y, max_iter = 20, seed = 42)
head(predict(fit, x, type = "response"))  # positional newx is supported
levels(predict(fit, x, type = "class"))   # always "0", "1"
summary(fit)
fit$stop_reason
fit$fallback_used

# Illustration on training PU labels, not held-out true-class performance:
assess(fit, newx = x, newy = y, s = "lambda.1se")
get_auc(fit, newx = x, newy = y, s = "lambda.1se")
```

Use held-out, independently established truth to assess true-class performance. The example's random features are not a predictive-quality fixture. A finite fitted model or reaching `max_iter` is not evidence of convergence.

## Migrating from 0.1.x

### Inputs and prediction

- `learning_rate` must be in `(0, 1]`: **zero is rejected** rather than allowing a frozen update to appear stable.
- Fitting requires a finite numeric feature matrix with at least two columns and at least **three known positives and three unlabeled observations** for cross-validation. Labels must be binary; factors are interpreted by their `"0"`/`"1"` labels, not internal level codes.
- `predict(fit, newx, s = ..., type = ...)` accepts the new matrix positionally. Named training features require an exact set of nonempty, unique feature names in `newx`, which is reordered into training order. Missing, extra, duplicate or unnamed features are errors when training features are named. With unnamed training features, the column count and positional order are the caller's responsibility. Non-finite features and unsupported `...` arguments are rejected.
- Single-lambda class predictions are factors with fixed levels `c("0", "1")`, even when only one class is predicted. Multiple numeric lambdas return a dimension-preserving 0/1 class matrix. Classification uses `probability > fit$cutoff`.
- `s` is respected for both training and new-data predictions: choose exactly `"lambda.min"`, `"lambda.1se"`, or finite nonnegative numeric lambdas. Cache-only legacy objects support only `"lambda.min"` without `newx`. `get_auc()` forwards prediction arguments, including `s`, and requires a single lambda; `assess()` supports per-lambda results.

### Strict metrics

- Truth must be finite binary 0/1 labels, or a finite nonnegative two-column matrix of **negative and positive class masses**, such as `cbind(1 - p, p)` for soft truth. Soft probability vectors are not accepted as binary truth. `auc_matrix()` uses the two-column representation; `get_auc()` uses binary truth through `auc()`.
- Optional row weights must be numeric, finite, nonnegative and have exactly one entry per observation; there is no recycling. Invalid observations are errors, not silently dropped rows, including when their weights are zero.
- `auc()` and `auc_matrix()` rank finite scores, including scores outside `[0, 1]`; assessment uses response probabilities. Undefined AUC (zero positive or negative effective class mass) returns `NA` with a warning. Zero total effective weight/mass likewise makes assessment metrics undefined. Tied scores use half credit in AUC; constant scores with both classes present have AUC 0.5.
- MSE and MAE sum the losses for **both class columns**, so they are **twice the scalar binary loss**. For class masses, normalize each row to proportions and weight by its total mass as well as its supplied row weight. Deviance clips predictions to `[1e-5, 1 - 1e-5]`; this clipping is specific to the deviance calculation, not a global modification of predictions.

## Algorithm and stopping contract

The paper's algorithm, its authors' reference implementation, and this package's extensions are distinct. The reference implementation already uses replacement sampling followed by deduplication, sampling budgets and learning-rate smoothing; these are not all new inventions of this package.

1. Sample unlabeled cases with replacement and fit penalized binomial models alongside known positives. The new default `sampling = "bootstrap"` preserves draw multiplicities as **case weights** on unique rows. Each identity is assigned to only one CV fold, preventing copies of one observation leaking between training and validation. `sampling = "unique"` opts into legacy deduplication; it does not restore every 0.1.x default.
2. `sample_use_time` limits the number of **completed inclusion rounds per unlabeled identity**, not the number of bootstrap draws. An identity appearing several times in one round spends one budget unit. `sampling_counts` records inclusion rounds; `draw_counts` records multiplicities.
3. `cv_measure = "deviance"` is the default in **both iterative and final fits**. `"auc"` is opt-in and explicitly falls back to deviance for small samples (fewer than 10 observations per fold on average), with a warning and effective iterative measures in `history`. Requested folds are reduced to available class counts. Sampled iterative labels must support at least three unique observations per class; up to **20 retries** precede `stop_reason = "degenerate_labels"`.
4. Anchor predictions at the positive-reference `qq` quantile, normalize residuals, and apply a sigmoid. `sigmoid_scale = 10` is now independently configurable; public `alpha` remains glmnet's elastic-net mixture and is **not** the sigmoid scale from the paper.
5. `learning_rate = 1` retains the `"current"` path, updating sampled unlabeled cases with Bernoulli training labels. `0 < learning_rate < 1` still selects the package's `"continuous_enhancement"` path: it smooths probabilities globally and hard-thresholds all unlabeled **iterative training labels**. Both paths use soft targets for the final fit.
6. Stability uses `1 - mean(abs(mapped_score - previous_pseudo_label))` across **all unlabeled cases before learning-rate damping**. Defaults `min_iter = 5`, `stability_window = 5`, and `min_coverage = 0.9` require at least five iterations, a full window of five consecutive scores **strictly above** `convergence_threshold`, and 90% inclusion coverage. A small learning rate alone cannot manufacture convergence.

Inspect `stop_reason`, `n_iter`, `history`, and sampling counts. `"label_stability"` denotes the stated heuristic; `"budget_exhausted"`, `"max_iter"` and `"degenerate_labels"` describe different stopping conditions, not equivalent successes.

### Final targets, fallback and random seeds

Fractional binomial responses `cbind(1 - p, p)` are valid. Soft targets may all exceed 0.5 without being degenerate: final fallback is **not** decided by hard-label counts. Effective positive and negative mass is checked on the full data and **every CV training split**, using a mean positive proportion strictly between `1e-5` and `1 - 1e-5`.

If proposed soft targets fail that guard, final fitting falls back to the original PU labels with a warning and explicit `fallback_used`/`fallback_reason`. `original_y` stores input labels, `pseudo_labels` stores the proposed pre-fallback probabilities, and `final_labels` and `y` agree with the actual final fitting targets. The object also stores `history`, `sampling_counts`, `draw_counts` and `final_foldid` for diagnostics. If original labels cannot support the required class mass either, fitting errors.

Supplying `seed` creates a local reproducible run and restores the caller's previous RNG state (including absence of `.Random.seed`). It does not reset the external RNG stream. With `seed = NULL`, normal caller-controlled RNG behavior applies.

## Verification and diagnostic harnesses

Run package tests from this development repository and inspect the reported **FAIL/WARN/SKIP counts**; an exit code alone is not a pass criterion. For an explicit failure gate:

```bash
Rscript -e 'devtools::test(stop_on_failure = TRUE)'
```

The historical numerical-audit baseline in `inst/benchmarks/numerical-audit` is preserved. Write new candidate results separately under `inst/benchmarks/v1.0.0`, never over the baseline:

```bash
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 Rscript inst/tools/audit_core.R inst/benchmarks/v1.0.0
Rscript inst/tools/audit_metrics.R inst/benchmarks/v1.0.0
```

Run the core harness first: it creates the output directory required by the metrics harness. Its optional second argument is a local checkout of the original PLUS source (`xiaoyulu95/PLUS`, also available as `zcslab/PLUS`). Audit scripts are **diagnostics, not CI pass/fail gates**: exit status 0 does not mean no findings. Inspect CSV findings, recorded source revisions and dependency versions; repeated variants and proposed validation contracts are not one distinct bug per row. The preserved audit is not a rerun or validation of the paper's experiments.

`inst/tools/benchmark_paths.R` compares iterative paths and reports AUC, class error, support, runtime and stability; it does not establish predictive superiority. Its default input is a deterministic synthetic fixture; metrics against its training labels are diagnostics, not estimates of true predictive quality. Required scripts in `inst/tools` are packaged so installed-package benchmark tests can resolve them with `system.file()`. `.devin` and `inst/benchmarks` are excluded from the source package; results remain development artifacts.

To keep build/check artifacts out of the checkout, use a temporary directory (the repository path is captured before changing directory):

```bash
repo="$PWD"
check_dir="$(mktemp -d)"
(cd "$check_dir" && R CMD build "$repo" && R CMD check xplus_1.0.0.tar.gz)
```

Review the complete check log, including warnings and notes. Do not commit tarballs, check directories, credentials or generated audit outputs as package source by accident.

## Reference

Zhou et al. (2022), *PLoS Computational Biology*, doi:[10.1371/journal.pcbi.1009956](https://doi.org/10.1371/journal.pcbi.1009956).
