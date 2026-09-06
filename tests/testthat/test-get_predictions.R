test_that("get_predictions returns correct structure with use_cutoff = TRUE", {
  skip_if_not_installed("glmnet")
  set.seed(42)
  x <- matrix(rnorm(120 * 4), ncol = 4)
  y <- c(rep(1, 30), rep(0, 90))

  fit <- xplus(x, y, max_iter = 5)
  out <- get_predictions(fit, newx = x, newy = y, use_cutoff = TRUE)

  expect_identical(levels(out$predicted), c("Class1", "Class2"))
  expect_identical(as.character(out$predicted), ifelse(out$Class1 > fit$cutoff, "Class1", "Class2"))
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), nrow(x))
  expect_named(out, c("truth", "Class1", "Class2", "predicted"))
  expect_s3_class(out$truth, "factor")
  expect_s3_class(out$predicted, "factor")
  expect_true(all(out$Class1 >= 0 & out$Class1 <= 1))
  expect_true(all(out$Class2 >= 0 & out$Class2 <= 1))
  expect_equal(out$Class1 + out$Class2, rep(1, nrow(x)), tolerance = 1e-10)
})

test_that("get_predictions returns correct structure with use_cutoff = FALSE", {
  skip_if_not_installed("glmnet")
  set.seed(42)
  x <- matrix(rnorm(120 * 4), ncol = 4)
  y <- c(rep(1, 30), rep(0, 90))

  fit <- xplus(x, y, max_iter = 5)
  out <- get_predictions(fit, newx = x, newy = y, use_cutoff = FALSE)

  expect_identical(levels(out$predicted), c("Class1", "Class2"))
  expect_identical(as.character(out$predicted), ifelse(out$Class1 > 0.5, "Class1", "Class2"))
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), nrow(x))
  expect_named(out, c("truth", "Class1", "Class2", "predicted"))
  expect_s3_class(out$truth, "factor")
  expect_s3_class(out$predicted, "factor")
  expect_true(all(out$Class1 >= 0 & out$Class1 <= 1))
  expect_true(all(out$Class2 >= 0 & out$Class2 <= 1))
  expect_equal(out$Class1 + out$Class2, rep(1, nrow(x)), tolerance = 1e-10)
})

test_that("get_predictions use_cutoff = FALSE returns factor predictions with correct length and no NAs", {
  skip_if_not_installed("glmnet")
  set.seed(7)
  x <- matrix(rnorm(100 * 5), ncol = 5)
  y <- c(rep(1, 20), rep(0, 80))

  fit <- xplus(x, y, max_iter = 5)
  out <- get_predictions(fit, newx = x, newy = y, use_cutoff = FALSE)

  # predicted must be a factor with no NAs and exactly nrow(x) elements
  expect_equal(length(out$predicted), nrow(x))
  expect_false(anyNA(out$predicted))
  expect_true(all(levels(out$predicted) %in% c("Class1", "Class2")))
})

test_that("get_predictions use_cutoff = FALSE Class1 probabilities have correct length with no NAs", {
  skip_if_not_installed("glmnet")
  set.seed(7)
  x <- matrix(rnorm(100 * 5), ncol = 5)
  y <- c(rep(1, 20), rep(0, 80))

  fit <- xplus(x, y, max_iter = 5)
  out <- get_predictions(fit, newx = x, newy = y, use_cutoff = FALSE)

  expect_equal(length(out$Class1), nrow(x))
  expect_false(anyNA(out$Class1))
})
