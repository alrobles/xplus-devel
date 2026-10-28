# Introduction to xplus

## What is PU learning?

Positive and Unlabeled (PU) learning addresses settings where only
positives are labeled, and the remaining data are unlabeled (not
guaranteed negatives).

## When to use xplus

`xplus` provides PLUS-derived extensions for
confirmed-positive/unlabeled data and sparse feature models. The 1.0.0
development candidate does not guarantee calibrated probabilities or
establish predictive superiority. Fitting requires finite numeric
features (at least two columns) and at least three known positives and
three unlabeled observations. `learning_rate` must be in `(0, 1]`.

## Basic workflow

[`library`](https://rdrr.io/r/base/library.html)`(`[`xplus`](https://alrobles.github.io/xplus/)`)`` ``#> Package 'xplus' version 1.0.2.9000`` ``#> Type 'citation("xplus")' for citing this R package in publications.`` `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``1``)`` ``x`` ``<-`` `[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``200`` ``*`` ``10``)``, ncol ``=`` ``10``)`` ``y`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(`[`rep`](https://rdrr.io/r/base/rep.html)`(``1``, ``40``)``, `[`rep`](https://rdrr.io/r/base/rep.html)`(``0``, ``160``)``)`` `` ``fit`` ``<-`` `[`xplus`](https://alrobles.github.io/xplus/reference/xplus.md)`(``x``, ``y``, max_iter ``=`` ``20``, seed ``=`` ``42``)`` `[`summary`](https://rdrr.io/r/base/summary.html)`(``fit``)`` ``#> Summary of xplus model`` ``#> Observations: 200 `` ``#> Features: 10 `` ``#> Iterations: 20 `` ``#> Stop reason: max_iter `` ``#> Original positives: 40 `` ``#> Original unlabeled: 160 `` ``#> Fallback used: FALSE `` ``#> Final training target range: 0 to 1 `` ``#> Final soft targets: 159 `` ``#> CV measure: deviance `` ``#> Sampling: bootstrap `` ``#> Unlabeled coverage: 0.99375 `` ``#> Backend: glmnet `` ``#> Cutoff: 0.6542115 `` ``#> lambda.min: 0.008950157 `` ``#> lambda.1se: 0.03613194 `` ``#> Non-zero coefficients: 10`` ``fit``$``stop_reason`` ``#> [1] "max_iter"`` ``fit``$``fallback_used`` ``#> [1] FALSE`` `[`head`](https://rdrr.io/r/utils/head.html)`(`[`predict`](https://rdrr.io/r/stats/predict.html)`(``fit``, ``x``, type ``=`` ``"response"``)``)`` ``#> s=0.008950157`` ``#> [1,] 0.9669237`` ``#> [2,] 0.9076893`` ``#> [3,] 0.8130480`` ``#> [4,] 0.6220108`` ``#> [5,] 0.7130373`` ``#> [6,] 0.7939128`` `[`levels`](https://rdrr.io/r/base/levels.html)`(`[`predict`](https://rdrr.io/r/stats/predict.html)`(``fit``, ``x``, type ``=`` ``"class"``)``)`` ``#> [1] "0" "1"`

The default uses weighted bootstrap multiplicities on unique CV rows and
`cv_measure = "deviance"` in both fitting stages. `sampling = "unique"`
opts into legacy deduplication. An explicit `seed` restores the caller’s
RNG state. `max_iter` is a limit, not a convergence claim: stability
requires a full consecutive window and sampling coverage. Inspect
stopping and fallback metadata.

## Model assessment

These are training PU-label diagnostics on random features, not held-out
true-class performance. Use independently established test labels for
the latter.
[`get_auc()`](https://alrobles.github.io/xplus/reference/get_auc.md)
forwards a single penalty selection to prediction.

`metrics`` ``<-`` `[`assess`](https://alrobles.github.io/xplus/reference/assess.md)`(``fit``, newx ``=`` ``x``, newy ``=`` ``y``, s ``=`` ``"lambda.1se"``)`` ``metrics``$``auc`` ``#> [1] 0.6057813`` `[`get_auc`](https://alrobles.github.io/xplus/reference/get_auc.md)`(``fit``, newx ``=`` ``x``, newy ``=`` ``y``, s ``=`` ``"lambda.1se"``)`` ``#> [1] 0.6057813`

Assessment accepts finite binary truth or two-column negative/positive
masses for soft truth; soft vectors are not binary truth. Weights must
be finite, nonnegative and exactly row-aligned. Invalid rows are errors,
not silently dropped data. Undefined AUC warns and returns `NA`. MSE and
MAE sum both class columns (twice scalar binary loss); deviance clips
probabilities to `[1e-5, 1 - 1e-5]`.
