test_that("assess returns all requested metrics", {
  skip_if_not_installed("glmnet")
  set.seed(123)
  x <- matrix(rnorm(120 * 4), ncol = 4)
  y <- c(rep(1, 30), rep(0, 90))

  fit <- xplus(x, y, max_iter = 5)
  out <- assess(fit, newx = x, newy = y)

  expect_named(out, c("deviance", "class", "auc", "mse", "mae"))
})

test_that("assess AUC matches direct response scores on noise", {
  skip_if_not_installed("glmnet")
  set.seed(123)
  x <- matrix(rnorm(120 * 4), ncol = 4)
  y <- c(rep(1, 30), rep(0, 90))

  fit <- xplus(x, y, max_iter = 5)
  out <- assess(fit, newx = x, newy = y)

  # AUC should be a valid probability in [0, 1], even on noise.
  # Strictly monotone score transforms preserve ranks, unlike destructive clipping.
  auc_val <- out$auc
  expect_true(auc_val >= 0 && auc_val <= 1)
  # Noise does not guarantee discrimination; compare directly with response AUC.
  expect_equal(auc_val, auc(y, predict(fit, newx = x, type = "response")))
})

test_that("assess class metric matches manual and predict type='class' thresholding", {
  fit <- structure(
    list(
      pred_y = matrix(c(0.2, 0.5, 0.51, 0.8, 0.49, 0.75, 0.1, 0.95, 0.65, 0.35, 0.55, 0.45), ncol = 1),
      cutoff = 0.5,
      stop_reason = "max_iter"
    ),
    class = "xplus"
  )
  y <- c(0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0)

  pred_prob <- predict(fit, type = "response")
  # With cutoff = 0.5, values > 0.5 map to class 1 and <= 0.5 to class 0.
  manual_class <- as.integer(pred_prob > fit$cutoff)
  # predict(type = "class") returns factor labels 0/1; decode the labels.
  pred_class <- as.integer(as.character(predict(fit, type = "class")))
  manual_error <- mean(manual_class != y)

  expect_equal(pred_class, manual_class)
  expect_equal(assess(fit, newy = y)$class, manual_error, tolerance = 1e-10)
})

test_that("assess preserves signal, lambda selection and multiple-column shape", {
  skip_if_not_installed("glmnet")
  set.seed(1908)
  x <- matrix(rnorm(160 * 5), 160, 5)
  y <- as.integer(2 * x[, 1] - 1.5 * x[, 2] > 0)
  fit <- xplus(x, y, max_iter = 2, nfolds = 4, seed = 1908)
  expect_gt(assess(fit, newx = x, newy = y)$auc, 0.8)
  s <- c(fit$xplus$lambda.min, max(fit$xplus$glmnet.fit$lambda))
  p <- predict(fit, newx = x, s = s, type = "response")
  out <- assess(fit, newx = x, newy = y, s = s)
  expect_named(out, c("deviance", "class", "auc", "mse", "mae"))
  expect_true(all(vapply(out, function(z) is.numeric(z) && is.null(dim(z)) && length(z) == 2L, logical(1))))
  expect_equal(unname(out$auc), vapply(seq_len(2), function(j) auc(y, p[, j]), numeric(1)))
  for (j in seq_len(2)) {
    single <- assess(fit, newx = x, newy = y, s = s[j])
    for (measure in names(out)) expect_equal(unname(out[[measure]][j]), single[[measure]])
  }
  one <- assess(fit, newx = x[1, , drop = FALSE], newy = matrix(c(2, 3), 1, 2), s = s)
  expect_true(all(lengths(one) == 2L))
  expect_equal(unname(one$auc), c(0.5, 0.5))
  for (bad in c(NA_real_, NaN, Inf, -Inf)) {
    xx <- x
    xx[1] <- bad
    expect_error(assess(fit, newx = xx, newy = y), "NA|NaN|Inf|finite")
  }
})
