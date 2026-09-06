test_that("xplus runs and returns xplus class", {
  skip_if_not_installed("glmnet")
  set.seed(123)
  x <- matrix(rnorm(100 * 5), ncol = 5)
  y <- c(rep(1, 25), rep(0, 75))

  fit <- xplus(x, y, max_iter = 5)

  expect_s3_class(fit, "xplus")
  expect_true(inherits(fit$xplus, "cv.glmnet"))
  expect_equal(nrow(fit$pred_y), nrow(x))
  expect_equal(length(fit$pseudo_labels), nrow(x))
  expect_equal(fit$iterative_path, "current")
  expect_true(fit$stop_reason %in% c("max_iter", "label_stability", "budget_exhausted"))
})

test_that("xplus records max_iter stop reason when no early stopping condition triggers", {
  skip_if_not_installed("glmnet")
  set.seed(123)
  x <- matrix(rnorm(80 * 4), ncol = 4)
  y <- c(rep(1, 20), rep(0, 60))

  fit <- xplus(x, y, max_iter = 2, convergence_threshold = 1)
  out <- summary(fit)

  expect_equal(fit$stop_reason, "max_iter")
  expect_equal(out$stop_reason, "max_iter")
  expect_false("convergence_reached" %in% names(out))
})

test_that("xplus rejects a disabled learning rate", {
  skip_if_not_installed("glmnet")
  set.seed(123)
  x <- matrix(rnorm(100 * 5), ncol = 5)
  y <- c(rep(1, 25), rep(0, 75))

  expect_error(xplus(x, y, learning_rate = 0, max_iter = 10), "learning_rate")
})

test_that("xplus stabilization uses undamped global residuals", {
  skip_if_not_installed("glmnet")
  set.seed(234)
  x <- matrix(rnorm(100 * 5), ncol = 5)
  y <- c(rep(1, 25), rep(0, 75))

  fit <- xplus(
    x,
    y,
    learning_rate = 1e-4,
    sample_use_time = 1e6,
    convergence_threshold = 0.99,
    max_iter = 10,
    seed = 234
  )

  expect_equal(fit$iterative_path, "continuous_enhancement")
  expect_equal(fit$n_iter, 10)
  expect_equal(fit$stop_reason, "max_iter")
})

test_that("continuous enhancement path keeps pseudo-labels continuous", {
  skip_if_not_installed("glmnet")
  set.seed(321)
  x <- matrix(rnorm(100 * 5), ncol = 5)
  y <- c(rep(1, 25), rep(0, 75))

  fit <- xplus(x, y, learning_rate = 0.5, max_iter = 3, seed = 321)

  expect_equal(fit$iterative_path, "continuous_enhancement")
  expect_true(all(fit$pseudo_labels >= 0 & fit$pseudo_labels <= 1))
  expect_true(any(fit$pseudo_labels[y == 0] > 0 & fit$pseudo_labels[y == 0] < 1))
})

test_that("continuous enhancement extensions update all unlabeled pseudo-labels", {
  skip_if_not_installed("glmnet")
  set.seed(777)
  x <- matrix(rnorm(100 * 5), ncol = 5)
  y <- c(rep(1, 25), rep(0, 75))
  unlabeled <- y == 0

  fit_current <- xplus(x, y, learning_rate = 1, max_iter = 1, convergence_threshold = 1, seed = 777)
  fit_enhanced <- xplus(x, y, learning_rate = 0.5, max_iter = 1, convergence_threshold = 1, seed = 777)

  expect_equal(fit_current$iterative_path, "current")
  expect_equal(fit_enhanced$iterative_path, "continuous_enhancement")
  expect_true(any(fit_current$pseudo_labels[unlabeled] == 0))
  expect_true(all(fit_enhanced$pseudo_labels[unlabeled] > 0))
})

test_that("continuous enhancement path is reproducible with fixed seed", {
  skip_if_not_installed("glmnet")
  set.seed(654)
  x <- matrix(rnorm(100 * 5), ncol = 5)
  y <- c(rep(1, 25), rep(0, 75))

  fit_a <- xplus(x, y, learning_rate = 0.5, max_iter = 3, seed = 999)
  fit_b <- xplus(x, y, learning_rate = 0.5, max_iter = 3, seed = 999)

  expect_equal(fit_a$iterative_path, "continuous_enhancement")
  expect_equal(fit_a$pseudo_labels, fit_b$pseudo_labels)
  expect_equal(fit_a$y, fit_b$y)
  expect_equal(fit_a$n_iter, fit_b$n_iter)
})

test_that("final refit passes alpha = 0 (ridge) to cv.glmnet", {
  skip_if_not_installed("glmnet")
  set.seed(42)
  x <- matrix(rnorm(100 * 5), ncol = 5)
  y <- c(rep(1, 25), rep(0, 75))

  fit <- xplus(x, y, alpha = 0, max_iter = 2, seed = 42)

  expect_equal(fit$alpha, 0)
  # Ridge (alpha=0) never produces exact zeros; all non-intercept coefs should be non-zero
  coefs <- as.numeric(coef(fit))[-1]
  expect_true(all(coefs != 0))
})

test_that("final refit passes alpha = 1 (lasso) to cv.glmnet", {
  skip_if_not_installed("glmnet")
  set.seed(42)
  x <- matrix(rnorm(100 * 5), ncol = 5)
  y <- c(rep(1, 25), rep(0, 75))

  fit <- xplus(x, y, alpha = 1, max_iter = 2, seed = 42)

  expect_equal(fit$alpha, 1)
  # Lasso (alpha=1) should produce a sparse solution (at least one zero coef)
  coefs <- as.numeric(coef(fit))[-1]
  expect_true(any(coefs == 0))
})

test_that("final refit alpha is stored and consistent for alpha = 0.5", {
  skip_if_not_installed("glmnet")
  set.seed(42)
  x <- matrix(rnorm(100 * 5), ncol = 5)
  y <- c(rep(1, 25), rep(0, 75))

  fit <- xplus(x, y, alpha = 0.5, max_iter = 2, seed = 42)

  expect_equal(fit$alpha, 0.5)
  expect_s3_class(fit, "xplus")
  expect_true(inherits(fit$xplus, "cv.glmnet"))
})

# Input validation tests

test_that("xplus rejects x with NA values", {
  skip_if_not_installed("glmnet")
  x <- matrix(rnorm(100 * 5), ncol = 5)
  x[1, 1] <- NA
  y <- c(rep(1, 25), rep(0, 75))

  expect_error(
    xplus(x, y, max_iter = 2),
    "`x` must not contain NA or NaN values.",
    fixed = TRUE
  )
})

test_that("xplus rejects x with Inf values", {
  skip_if_not_installed("glmnet")
  x <- matrix(rnorm(100 * 5), ncol = 5)
  x[2, 3] <- Inf
  y <- c(rep(1, 25), rep(0, 75))

  expect_error(
    xplus(x, y, max_iter = 2),
    "`x` must not contain Inf values.",
    fixed = TRUE
  )
})

test_that("xplus rejects x with -Inf values", {
  skip_if_not_installed("glmnet")
  x <- matrix(rnorm(100 * 5), ncol = 5)
  x[5, 2] <- -Inf
  y <- c(rep(1, 25), rep(0, 75))

  expect_error(
    xplus(x, y, max_iter = 2),
    "`x` must not contain Inf values.",
    fixed = TRUE
  )
})

test_that("xplus rejects non-integer-like sample_use_time", {
  skip_if_not_installed("glmnet")
  x <- matrix(rnorm(100 * 5), ncol = 5)
  y <- c(rep(1, 25), rep(0, 75))

  expect_error(
    xplus(x, y, sample_use_time = 30.5, max_iter = 2),
    "sample_use_time.*integer"
  )
})

test_that("xplus accepts integer-like sample_use_time as numeric", {
  skip_if_not_installed("glmnet")
  set.seed(42)
  x <- matrix(rnorm(100 * 5), ncol = 5)
  y <- c(rep(1, 25), rep(0, 75))

  # Should work with integer-like numeric values
  fit <- xplus(x, y, sample_use_time = 30.0, max_iter = 2, seed = 42)
  expect_s3_class(fit, "xplus")
})

# --- normalize_residuals() unit tests ---

test_that("normalize_residuals normalizes positive residuals to [0, 1]", {
  v <- c(-0.5, 0, 0.2, 0.4, 0.8)
  out <- xplus:::normalize_residuals(v)
  # Positive side scaled so max == 1
  expect_equal(max(out[out > 0]), 1)
  # Negative side scaled so min == -1
  expect_equal(min(out[out < 0]), -1)
  # Zero unchanged
  expect_equal(out[v == 0], 0)
})

test_that("normalize_residuals normalizes negative residuals to [-1, 0]", {
  v <- c(-0.9, -0.3, 0, 0.1)
  out <- xplus:::normalize_residuals(v)
  expect_equal(min(out[out < 0]), -1)
  expect_equal(max(out[out > 0]), 1)
})

test_that("normalize_residuals: near-degenerate positive residuals are clamped to 0 with a warning", {
  # All positive residuals are below 1e-6
  v <- c(-0.5, 5e-8, 9e-7)
  expect_warning(
    out <- xplus:::normalize_residuals(v),
    regexp = "Near-degenerate positive scaling detected"
  )
  # Clamped-to-0 residuals yield 0 (sigmoid -> 0.5 neutral prediction)
  expect_equal(out[v > 0], c(0, 0))
  # Negative side still normalised normally
  expect_equal(min(out[out < 0]), -1)
})

test_that("normalize_residuals: near-degenerate negative residuals are clamped to 0 with a warning", {
  # All negative residuals are below 1e-6 in magnitude
  v <- c(-9e-7, -5e-8, 0.5)
  expect_warning(
    out <- xplus:::normalize_residuals(v),
    regexp = "Near-degenerate negative scaling detected"
  )
  # Clamped-to-0
  expect_equal(out[v < 0], c(0, 0))
  # Positive side still normalised normally
  expect_equal(max(out[out > 0]), 1)
})

test_that("normalize_residuals warning message includes the residual value", {
  v <- c(-0.5, 3e-8)
  expect_warning(
    xplus:::normalize_residuals(v),
    regexp = "3.00e-08"
  )
})

test_that("normalize_residuals: all-zero input is returned unchanged without warning", {
  v <- c(0, 0, 0)
  expect_no_warning(out <- xplus:::normalize_residuals(v))
  expect_equal(out, v)
})

test_that("normalize_residuals: all-positive input normalised so max == 1", {
  v <- c(0.1, 0.3, 0.6)
  out <- xplus:::normalize_residuals(v)
  expect_equal(max(out), 1)
  expect_true(all(out >= 0))
})

test_that("normalize_residuals: all-negative input normalised so min == -1", {
  v <- c(-0.8, -0.4, -0.1)
  out <- xplus:::normalize_residuals(v)
  expect_equal(min(out), -1)
  expect_true(all(out <= 0))
})

test_that("normalize_residuals: degenerate positive clamping is deterministic (same output for same input)", {
  v <- c(-0.3, 1e-8, 2e-8)
  # Suppress the warning and check determinism
  out1 <- suppressWarnings(xplus:::normalize_residuals(v))
  out2 <- suppressWarnings(xplus:::normalize_residuals(v))
  expect_equal(out1, out2)
  # Clamped values are exactly 0
  expect_equal(out1[v > 0], c(0, 0))
})

# --- integration tests: xplus() with near-degenerate data ---

test_that("xplus completes on near-degenerate positive threshold", {
  skip_if_not_installed("glmnet")
  set.seed(123)
  x <- matrix(runif(100 * 5, min = -1e-8, max = 1e-8), ncol = 5)
  y <- c(rep(1, 25), rep(0, 75))

  # Depending on the glmnet version the residuals are either exactly zero
  # (no warning) or near-degenerate (clamping warning); both must complete.
  fit <- suppressWarnings(xplus(x, y, max_iter = 1, seed = 123))
  expect_s3_class(fit, "xplus")
  expect_true(all(fit$pseudo_labels >= 0 & fit$pseudo_labels <= 1))
})

test_that("xplus completes on near-degenerate negative threshold", {
  skip_if_not_installed("glmnet")
  set.seed(456)
  x <- matrix(runif(100 * 5, min = -1e-8, max = 1e-8), ncol = 5)
  y <- c(rep(1, 25), rep(0, 75))

  fit <- suppressWarnings(xplus(x, y, max_iter = 1, seed = 456))
  expect_s3_class(fit, "xplus")
  expect_true(all(fit$pseudo_labels >= 0 & fit$pseudo_labels <= 1))
})

test_that("final fit uses stabilized continuous pseudo-labels with unchanged interfaces", {
  skip_if_not_installed("glmnet")
  set.seed(999)
  x <- matrix(rnorm(120 * 4), ncol = 4)
  y <- c(rep(1, 30), rep(0, 90))

  fit <- xplus(x, y, learning_rate = 1, max_iter = 1, seed = 999)

  expect_true(any(fit$y > 0 & fit$y < 1))

  pred_response <- predict(fit, newx = x, type = "response")
  pred_class <- predict(fit, newx = x, type = "class")
  out_summary <- summary(fit)
  out_assess <- assess(fit, newx = x, newy = y)
  out_coef <- coef(fit)

  expect_equal(nrow(pred_response), nrow(x))
  expect_equal(length(pred_class), nrow(x))
  expect_equal(out_summary$n_obs, nrow(x))
  expect_named(out_assess, c("deviance", "class", "auc", "mse", "mae"))
  expect_s4_class(out_coef, "dgCMatrix")
})

test_that("xplus stops cleanly when pseudo-labels collapse to one class", {
  set.seed(42)
  # Strongly separable data pushes sampled unlabeled pseudo-labels toward 1,
  # which used to crash cv.glmnet with a single-class training subset.
  n_pos <- 60
  n_unl <- 90
  x <- rbind(
    matrix(rnorm(n_pos * 5, mean = 3), ncol = 5),
    matrix(rnorm(n_unl * 5, mean = 0), ncol = 5)
  )
  y <- c(rep(1, n_pos), rep(0, n_unl))
  fit <- xplus(x, y, max_iter = 200, seed = 1)
  expect_s3_class(fit, "xplus")
  expect_true(fit$stop_reason %in%
                c("max_iter", "label_stability", "budget_exhausted", "degenerate_labels"))
})

test_that("xplus survives near-single-class training subsets and final fits", {
  set.seed(7)
  # Rare-positive, high-dimensional setting where iterative relabeling can
  # leave a class with fewer than two observations; used to crash lognet.
  n <- 300
  x <- matrix(rnorm(n * 12), ncol = 12)
  y <- c(rep(1, 4), rep(0, n - 4))
  x[y == 1, ] <- x[y == 1, ] + 4
  fit <- xplus(x, y, max_iter = 500, seed = 3)
  expect_s3_class(fit, "xplus")
  expect_equal(nrow(fit$pred_y), n)
})
