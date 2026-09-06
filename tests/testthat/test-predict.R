test_that("predict returns expected dimensions", {
  skip_if_not_installed("glmnet")
  set.seed(123)
  x <- matrix(rnorm(120 * 4), ncol = 4)
  y <- c(rep(1, 30), rep(0, 90))

  fit <- xplus(x, y, max_iter = 5)

  pred_response <- predict(fit, newx = x, type = "response")
  pred_class <- predict(fit, newx = x, type = "class")
  pred_link <- predict(fit, newx = x, type = "link")

  expect_equal(nrow(pred_response), nrow(x))
  expect_equal(length(pred_class), nrow(x))
  expect_equal(nrow(pred_link), nrow(x))
})

test_that("predict link matches glmnet linear predictor", {
  skip_if_not_installed("glmnet")
  set.seed(123)
  x <- matrix(rnorm(120 * 4), ncol = 4)
  y <- c(rep(1, 30), rep(0, 90))

  fit <- xplus(x, y, max_iter = 5)
  pred_link <- predict(fit, newx = x, type = "link")
  expected <- stats::predict(fit$xplus$glmnet.fit, x, s = fit$xplus$lambda.min, type = "link")

  expect_equal(pred_link, expected)
})

test_that("predict type='link' returns the backend log-odds", {
  skip_if_not_installed("glmnet")
  set.seed(123)
  x <- matrix(rnorm(120 * 4), ncol = 4)
  y <- c(rep(1, 30), rep(0, 90))

  fit <- xplus(x, y, max_iter = 5)

  pred_link <- predict(fit, newx = x, type = "link")

  expect_equal(nrow(pred_link), nrow(x))
  # log-odds match glmnet without requiring random data to produce any particular range
  expected <- stats::predict(fit$xplus$glmnet.fit, x, s = fit$xplus$lambda.min, type = "link")
  expect_equal(pred_link, expected)

  # link and response are consistent: sigmoid(link) == response
  pred_response <- predict(fit, newx = x, type = "response")
  expect_equal(1 / (1 + exp(-pred_link)), pred_response, tolerance = 1e-6)
})

test_that("predict type='link' works without newx (uses training data)", {
  skip_if_not_installed("glmnet")
  set.seed(123)
  x <- matrix(rnorm(120 * 4), ncol = 4)
  y <- c(rep(1, 30), rep(0, 90))

  fit <- xplus(x, y, max_iter = 5)

  pred_link_null <- predict(fit, type = "link")
  pred_response_null <- predict(fit, type = "response")

  expect_equal(nrow(pred_link_null), nrow(x))
  expect_equal(1 / (1 + exp(-pred_link_null)), pred_response_null, tolerance = 1e-6)
})

test_that("predict type='link' returns log-odds (linear predictor)", {
  skip_if_not_installed("glmnet")
  set.seed(123)
  x <- matrix(rnorm(120 * 4), ncol = 4)
  y <- c(rep(1, 30), rep(0, 90))

  fit <- xplus(x, y, max_iter = 5)

  pred_link <- predict(fit, newx = x, type = "link")
  pred_response <- predict(fit, newx = x, type = "response")

  expect_equal(nrow(pred_link), nrow(x))
  # Link values match the backend linear predictor, regardless of their observed range
  expected <- stats::predict(fit$xplus$glmnet.fit, x, s = fit$xplus$lambda.min, type = "link")
  expect_equal(pred_link, expected)
  # sigmoid(link) should equal response
  expect_equal(1 / (1 + exp(-pred_link)), pred_response, tolerance = 1e-6)
})

test_that("predict type='link' without newx returns backend training log-odds", {
  skip_if_not_installed("glmnet")
  set.seed(123)
  x <- matrix(rnorm(120 * 4), ncol = 4)
  y <- c(rep(1, 30), rep(0, 90))

  fit <- xplus(x, y, max_iter = 5)

  pred_link_null <- predict(fit, newx = NULL, type = "link")
  expected_link <- stats::predict(fit$xplus$glmnet.fit, x, s = fit$xplus$lambda.min, type = "link")

  expect_equal(pred_link_null, expected_link)
})
