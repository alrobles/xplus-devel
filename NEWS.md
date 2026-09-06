# xplus 1.0.0

This is a major, breaking upgrade of the PLUS-derived package extensions, not
an exact reproduction of the paper or evidence of predictive superiority.
Response-scale model scores are not guaranteed to be calibrated probabilities.
The package remains `xplus`; the production repository is
<https://github.com/alrobles/xplus-devel>. Development, audit harnesses and
historical baselines live in the development repository
<https://github.com/alrobles/xplus-develeopment>.

## Breaking interfaces and metrics

* Require R >= 4.1.0 for native-pipe syntax and declare glmnet >= 4.1-8.
  Verification uses R 4.5.2 / glmnet 5.0; other supported versions have not
  thereby been verified.
* Reject `learning_rate = 0`; the accepted interval is `(0, 1]`.
* Interpret binary factors by labels, not level codes. Single-lambda class
  predictions now have fixed factor levels `"0"`, `"1"`; multiple lambdas retain
  a 0/1 matrix. Classification uses a strict `> object$cutoff` comparison.
* Accept positional `newx` in `predict()`. Validate finite numeric features;
  named training features require an exact set of nonempty unique prediction
  names and are automatically aligned. Reject mismatches and unsupported dots.
* Respect `s` for training and new-data prediction, including numeric lambdas.
  Cache-only objects support only `"lambda.min"` without new data. `get_auc()`
  forwards prediction arguments and requires one lambda; `assess()` can return
  per-lambda metrics.
* Require finite binary truth, or finite nonnegative two-column negative/positive
  masses for soft truth or counts (`assess()`/`auc_matrix()`). Soft vectors are
  not binary truth. Weights must be finite, nonnegative and row-aligned, without
  recycling. Invalid rows are errors, not silently dropped observations.
* Undefined AUC returns `NA` with a warning when either effective class mass is
  zero. Zero effective total mass/weight makes assessment undefined. Ties receive
  half credit; constant scores with both classes have AUC 0.5.
* MSE and MAE retain the two-class-column loss convention: twice scalar binary
  loss. Class-mass totals contribute to effective row weights. Deviance clips
  probabilities to `[1e-5, 1 - 1e-5]`; other metrics do not inherit that clipping.

## Sampling, CV and stopping

* Default `sampling = "bootstrap"` preserves replacement multiplicities as case
  weights on unique rows. Duplicate identities never cross CV folds.
  `sampling = "unique"` retains legacy deduplication, not all legacy behavior.
* `sample_use_time` is a budget of completed inclusion **rounds per identity**,
  not bootstrap draws. Record both `sampling_counts` and `draw_counts`.
* Default `cv_measure = "deviance"` in both iterative and final fitting stages.
  AUC remains opt-in, with an explicit warning and deviance fallback for fewer
  than 10 observations per fold on average; iterative effective measures appear
  in `history`.
* Require at least three positive and three unlabeled input observations.
  Reduce requested folds to available class counts; require three unique members
  of each iterative training class and retry degenerate samples at most 20 times
  before reporting `stop_reason = "degenerate_labels"`.
* Preserve the selected global-update, hard-thresholded iterative training path
  when `learning_rate < 1`; `learning_rate = 1` uses sampled Bernoulli updates.
  Final targets remain soft in both paths. Reference sampling budgets and
  smoothing should not be mistaken for newly invented package extensions.
* Expose `sigmoid_scale = 10` separately from glmnet elastic-net `alpha`.
* Default `min_iter = 5`, `stability_window = 5`, `min_coverage = 0.9`.
  Stability compares undamped mapped scores against previous pseudo-labels over
  all unlabeled cases. A full window of consecutive scores strictly above
  `convergence_threshold` and minimum coverage are required; damping alone cannot
  create convergence. Record `max_iter`, `budget_exhausted`, `degenerate_labels`
  and `label_stability` distinctly rather than implying every stop is success.

## Final fitting and reproducibility

* Validate effective class mass on the full final dataset and every CV training
  split (mean positive proportion strictly within `1e-5` and `1 - 1e-5`). Soft
  labels all above 0.5 remain valid when mass is sufficient; hard-class counts
  do not trigger final fallback.
* Warn when reverting to original PU labels and record `fallback_used` and
  `fallback_reason`. Preserve `original_y`, proposed `pseudo_labels`, actual
  `final_labels`/`y`, iteration `history`, sampling/draw counts and `final_foldid`.
  Insufficient original-label mass is an error, not a hidden recovery.
* Isolate an explicit `seed` from the caller's RNG stream and restore its prior
  state, including when no `.Random.seed` existed.

## Documentation and verification

* Regenerate reference manuals from current roxygen contracts. Installation
  examples target `alrobles/xplus-devel` without embedded credentials.
* Diagnostic harnesses, the historical numerical-audit baseline and candidate
  diagnostics are maintained in the development repository
  <https://github.com/alrobles/xplus-develeopment>; they are not part of the
  production source tree.
* Document `Rscript -e 'devtools::test(stop_on_failure = TRUE)'` and require
  inspection of FAIL/WARN/SKIP counts. Keep build/check output outside the
  tracked checkout.

# xplus 0.1.1

* Fixed a crash when the pseudo-labels of the iterative training subset
  collapse to a single class: fitting now stops cleanly with
  `stop_reason = "degenerate_labels"` instead of failing inside
  `glmnet::cv.glmnet()`.
* Sampling probabilities are now clamped at zero when the unlabeled-sampling
  budget is decremented, and sampling is skipped once the budget is fully
  exhausted (avoids an uninformative `sample()` error).

# xplus 0.1.0

* Initial release.
* Implements the PLUS algorithm for positive-unlabeled learning
  (Zhou et al., 2022, doi:10.1371/journal.pcbi.1009956).
* Provides `xplus()` fitting function with iterative pseudo-label updates.
* S3 methods: `predict()`, `coef()`, `summary()`, `print()`, `assess()`,
  `get_auc()`, and `get_predictions()`.
* Ships bundled datasets: `lacs`, `lacsSample`, and `binexample`.
