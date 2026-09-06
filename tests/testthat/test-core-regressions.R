core_fixture <- function() {
  set.seed(20260906)
  list(x = matrix(rnorm(100 * 6), 100), y = c(rep(1, 20), rep(0, 80)))
}

test_that("factor labels preserve positive identity and seed behavior", {
  d <- core_fixture()
  a <- suppressWarnings(xplus(d$x, d$y, seed = 17, max_iter = 6))
  b <- suppressWarnings(xplus(d$x, factor(d$y, levels = 0:1), seed = 17, max_iter = 6))
  expect_equal(a$pred_y, b$pred_y)
  expect_equal(b$original_y, as.integer(d$y))
  expect_true(all(b$pseudo_labels[d$y == 1] == 1))
})

test_that("small learning rates cannot claim one-step convergence", {
  d <- core_fixture()
  fit <- suppressWarnings(xplus(d$x, d$y, learning_rate = 0.05, max_iter = 10, seed = 17))
  expect_gte(fit$n_iter, 5)
  expect_equal(nrow(fit$history), fit$n_iter)
  if (fit$stop_reason == "label_stability") {
    expect_true(all(tail(fit$history$stability, fit$stability_window) > 0.9))
    expect_gte(tail(fit$history$coverage, 1), fit$min_coverage)
  }
})

test_that("explicit seeds do not reset the caller RNG stream", {
  d <- core_fixture()
  set.seed(452)
  before <- .Random.seed
  suppressWarnings(xplus(d$x, d$y, max_iter = 2, seed = 17))
  expect_identical(.Random.seed, before)
})

test_that("final soft targets and fallback provenance are truthful", {
  d <- core_fixture()
  fit <- suppressWarnings(xplus(d$x, d$y, max_iter = 8, seed = 17))
  expect_identical(fit$y, fit$final_labels)
  expect_equal(fit$original_y, as.integer(d$y))
  expect_type(fit$fallback_used, "logical")
  expect_length(fit$fallback_used, 1)
  if (!fit$fallback_used) expect_identical(fit$final_labels, fit$pseudo_labels)
  expect_true(all(is.finite(fit$pred_y)))
  expect_true(all(is.finite(as.numeric(coef(fit)))))
})

test_that("soft targets above one half do not imply a degenerate class", {
  y <- c(rep(1, 12), rep(0, 24))
  foldid <- rep(1:3, 12)
  candidate <- c(rep(1, 12), rep(0.7, 24))
  out <- .xplus_final_targets(candidate, y, foldid)
  expect_false(out$fallback_used)
  expect_identical(out$labels, candidate)
  collapsed <- .xplus_final_targets(rep(1, 36), y, foldid)
  expect_true(collapsed$fallback_used)
  expect_equal(collapsed$labels, y)
  expect_match(collapsed$reason, "mass")
})

test_that("sampling history distinguishes draws from distinct rows", {
  d <- core_fixture()
  fit <- suppressWarnings(xplus(d$x, d$y, max_iter = 5, seed = 17, sampling = "bootstrap"))
  expect_true(all(fit$history$sample_draws == 20))
  expect_true(all(fit$history$sample_size <= fit$history$sample_draws))
  expect_equal(sum(fit$draw_counts), sum(fit$history$sample_draws))
  expect_equal(sum(fit$sampling_counts), sum(fit$history$sample_size))
  expect_true(all(fit$sampling_counts[d$y == 1] == 0))
  expect_true(all(fit$draw_counts[d$y == 1] == 0))
  expect_true(all(fit$sampling_counts <= 30))
  expect_equal(fit$cv_measure, "deviance")
  expect_true(all(fit$history$cv_measure == "deviance"))
})

test_that("CV folds use every class and adapt to small valid samples", {
  set.seed(3)
  folds <- stratified_foldid(c(rep(0, 3), rep(1, 6)), 3)
  expect_equal(sort(unique(folds)), 1:3)
  expect_true(all(table(c(rep(0, 3), rep(1, 6)), folds) >= 1))
  expect_error(stratified_foldid(c(0, 1), 2), "nfolds")
  expect_error(stratified_foldid(c(rep(0, 4), rep(1, 4)), 3.5), "nfolds")
  d <- core_fixture()
  fit <- suppressWarnings(xplus(d$x, as.integer(seq_len(100) <= 3), max_iter = 2, seed = 17))
  expect_equal(max(fit$final_foldid), 3)
})

test_that("core parameters reject unsupported and nonfinite values", {
  d <- core_fixture()
  for (name in c("alpha", "sample_use_time", "learning_rate", "qq", "nfolds", "max_iter", "convergence_threshold", "sigmoid_scale", "min_iter", "stability_window", "min_coverage")) {
    for (value in list(NA_real_, NaN, Inf, c(1, 2))) {
      expect_error(do.call(xplus, c(list(x = d$x, y = d$y), setNames(list(value), name))), name)
    }
  }
  expect_error(xplus(d$x, d$y, learning_rate = 0), "learning_rate")
  expect_error(xplus(d$x, d$y, alpha = 2), "alpha")
  expect_error(xplus(d$x, d$y, nfolds = 2), "nfolds")
  expect_error(xplus(d$x, d$y, nfolds = 3.5), "nfolds")
  expect_error(xplus(d$x, d$y, max_iter = 2.5), "max_iter")
  expect_error(xplus(d$x, d$y, verbose = NA), "verbose")
  expect_error(xplus(d$x, as.integer(seq_len(100) <= 2)), "three")
})

test_that("feature validation rejects unsafe geometry and names", {
  d <- core_fixture()
  colnames(d$x) <- rep("same", 6)
  expect_error(xplus(d$x, d$y), "unique")
  colnames(d$x) <- NULL
  expect_error(xplus(d$x * 0, d$y), "variance")
  expect_error(xplus(d$x * 1e200, d$y), "rescale")
  expect_error(xplus(d$x * 1e-200, d$y), "variance")
  expect_error(normalize_residuals(c(0, NaN)), "finite")
  expect_error(normalize_residuals(c(0, 1), NA_real_), "degenerate_threshold")
})

test_that("an actual final fallback is exposed without falsifying targets", {
  set.seed(125)
  x <- matrix(rnorm(103 * 4), 103)
  y <- c(rep(1, 100), rep(0, 3))
  testthat::local_mocked_bindings(normalize_residuals = function(map_pred_y, degenerate_threshold) {
    rep(1, length(map_pred_y))
  }, .package = "xplus")
  expect_warning(fit <- xplus(x, y, sigmoid_scale = 700, max_iter = 1, seed = 3), "reverted to original labels")
  expect_true(fit$fallback_used)
  expect_match(fit$fallback_reason, "mass")
  expect_equal(fit$final_labels, y)
  expect_identical(fit$y, fit$final_labels)
  expect_true(all(fit$pseudo_labels == 1))
  expect_s3_class(validate_xplus(fit), "xplus")
})

test_that("small-sample AUC CV substitutions are recorded", {
  d <- core_fixture()
  expect_warning(fit <- xplus(d$x[1:24, ], rep(0:1, each = 12), cv_measure = "auc", max_iter = 2, seed = 3), "AUC CV")
  expect_equal(fit$cv_measure, "auc")
  expect_true(all(fit$history$cv_measure == "deviance"))
  expect_match(fit$xplus$name, "Deviance")
})
