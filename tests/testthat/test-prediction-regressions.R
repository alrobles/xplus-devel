prediction_regression_fit <- function(named = TRUE) {
  set.seed(813)
  x <- matrix(rnorm(80 * 3), ncol = 3)
  if (named) colnames(x) <- c("signal", "noise1", "noise2")
  y <- as.integer(x[, 1] + 0.3 * x[, 2] > 0)
  final_labels <- 0.1 + 0.8 * y
  backend <- glmnet::cv.glmnet(
    x, cbind(1 - final_labels, final_labels), family = "binomial",
    foldid = rep(1:4, length.out = nrow(x)), lambda = c(1, 0.1, 0.01)
  )
  backend$lambda.min <- 0.01
  backend$lambda.1se <- 0.1
  structure(list(xplus = backend, x = x, y = y, cutoff = 0.4,
                 pred_y = matrix(rep(0.2, nrow(x)), ncol = 1)), class = "xplus")
}

test_that("positional newx and training predictions honor every lambda", {
  skip_if_not_installed("glmnet")
  fit <- prediction_regression_fit()
  expect_equal(predict(fit, fit$x[1:3, , drop = FALSE]),
               predict(fit, newx = fit$x[1:3, , drop = FALSE]))
  for (s in list("lambda.min", "lambda.1se", 0.1, c(1, 0.01))) {
    lambda <- if (is.character(s)) fit$xplus[[s]] else s
    expect_equal(predict(fit, s = s, type = "class"),
                 predict(fit, newx = fit$x, s = s, type = "class"))
    for (type in c("response", "link")) {
      expected <- stats::predict(fit$xplus$glmnet.fit, fit$x, s = lambda, type = type)
      expect_equal(predict(fit, s = s, type = type), expected)
      expect_equal(predict(fit, newx = fit$x, s = s, type = type), expected)
    }
  }
})

test_that("class predictions have fixed binary levels and preserve lambda dimensions", {
  skip_if_not_installed("glmnet")
  fit <- prediction_regression_fit()
  for (cutoff in c(0, 0.4, 1)) {
    fit$cutoff <- cutoff
    p <- predict(fit, newx = fit$x)
    cl <- predict(fit, newx = fit$x, type = "class")
    expect_identical(levels(cl), c("0", "1"))
    expect_identical(as.character(cl), as.character(as.integer(p > cutoff)))
  }
  for (rows in list(1L, 1:4)) {
    x <- fit$x[rows, , drop = FALSE]
    p <- predict(fit, newx = x, s = c(1, 0.01))
    cl <- predict(fit, newx = x, s = c(1, 0.01), type = "class")
    expect_true(is.matrix(cl))
    expect_identical(dim(cl), dim(p))
    expect_identical(dimnames(cl), dimnames(p))
    expect_equal(cl, (p > fit$cutoff) * 1L)
    single <- predict(fit, newx = x, type = "class")
    expect_identical(levels(single), c("0", "1"))
    expect_length(single, length(rows))
  }
  for (bad in list(NA_real_, Inf, -0.1, 1.1, c(0.4, 0.5))) {
    fit$cutoff <- bad
    expect_error(predict(fit, type = "class"), "cutoff")
  }
})

test_that("cached probabilities retain exact log odds and reject invalid requests", {
  p <- matrix(c(0, 1e-20, 1e-17, 0.5, 1), ncol = 1)
  fit <- structure(list(pred_y = p, cutoff = 0.5), class = "xplus")
  expect_equal(predict(fit), p)
  expect_equal(predict(fit, type = "link"), qlogis(p))
  expect_equal(plogis(predict(fit, type = "link")), p)
  expect_identical(as.character(predict(fit, type = "class")), c("0", "0", "0", "0", "1"))
  expect_identical(levels(predict(fit, type = "class")), c("0", "1"))
  expect_error(predict(fit, s = "lambda.1se"), "cache|training|lambda.min")
  expect_error(predict(fit, s = 0.1), "cache|training|lambda.min")
  expect_error(predict(fit, newx = matrix(0, 5, 2)), "training|backend")
  expect_error(predict(fit, unused = TRUE), "[Uu]nsupported|unused")
  for (s in list(NA_real_, Inf, -1, numeric(), c(0.1, NA), "lambda", "bad", TRUE)) {
    expect_error(predict(fit, s = s), "s|lambda")
  }
  for (bad in c(NA_real_, NaN, Inf, -0.1, 1.1)) {
    fit$pred_y[1] <- bad
    expect_error(predict(fit), "probabilit")
    expect_error(predict(fit, type = "link"), "probabilit")
  }
})

test_that("prediction validates finite inputs, exact lambdas, and feature schema", {
  skip_if_not_installed("glmnet")
  fit <- prediction_regression_fit()
  expect_equal(predict(fit, newx = fit$x[, c(3, 1, 2)]), predict(fit, newx = fit$x))
  expect_error(predict(fit, newx = unname(fit$x)), "names|features")
  duplicate <- fit$x
  colnames(duplicate) <- c("signal", "signal", "noise2")
  expect_error(predict(fit, newx = duplicate), "unique|duplicate")
  mismatch <- fit$x
  colnames(mismatch)[1] <- "different"
  expect_error(predict(fit, newx = mismatch), "names|features")
  expect_error(predict(fit, newx = fit$x[, 1:2]), "features|columns")
  expect_error(predict(fit, newx = fit$x[FALSE, ]), "rows")
  for (bad in c(NA_real_, NaN, Inf, -Inf)) {
    invalid <- fit$x
    invalid[1, 1] <- bad
    expect_error(predict(fit, newx = invalid), "NA|NaN|Inf|finite")
  }
  expect_error(predict(fit, newx = as.data.frame(fit$x)), "matrix")
  expect_error(predict(fit, unused = TRUE), "[Uu]nsupported|unused")
  for (s in list(NA_real_, Inf, -1, numeric(), c(0.1, NA), "lambda", "bad", TRUE)) {
    expect_error(predict(fit, newx = fit$x, s = s), "s|lambda")
  }
  without_training <- fit
  without_training$x <- NULL
  expect_equal(predict(without_training), fit$pred_y)
  expect_error(predict(without_training, s = "lambda.1se"), "cache|training|lambda.min")
  without_backend <- fit
  without_backend$xplus <- NULL
  expect_equal(predict(without_backend), fit$pred_y)
  fit <- prediction_regression_fit(named = FALSE)
  expect_equal(predict(fit, newx = fit$x), predict(fit))
  named_newx <- fit$x
  colnames(named_newx) <- c("a", "b", "c")
  expect_equal(predict(fit, newx = named_newx), predict(fit, newx = fit$x))
})

test_that("prediction tables derive Class1 from probabilities rather than backend labels", {
  skip_if_not_installed("glmnet")
  fit <- prediction_regression_fit()
  for (use_cutoff in c(TRUE, FALSE)) {
    p <- as.numeric(predict(fit, newx = fit$x))
    cutoff <- if (use_cutoff) fit$cutoff else 0.5
    expected <- ifelse(p > cutoff, "Class1", "Class2")
    expect_true(all(c("Class1", "Class2") %in% expected))
    out <- get_predictions(fit, fit$x, fit$y, use_cutoff = use_cutoff)
    expect_equal(out$Class1, p)
    expect_equal(out$Class2, 1 - p)
    expect_identical(as.character(out$predicted), expected)
    expect_identical(levels(out$predicted), c("Class1", "Class2"))
    expect_identical(as.character(out$truth), ifelse(fit$y == 1, "Class1", "Class2"))
  }
  fit$cutoff <- 1
  out <- get_predictions(fit, fit$x, rep(1, nrow(fit$x)))
  expect_identical(levels(out$truth), c("Class1", "Class2"))
  expect_identical(levels(out$predicted), c("Class1", "Class2"))
  expect_error(get_predictions(fit, fit$x, fit$y, use_cutoff = NA), "use_cutoff")
  expect_error(get_predictions(fit, fit$x, fit$y[-1]), "newy|label")
})

test_that("get_auc forwards lambda and rejects unused prediction arguments", {
  skip_if_not_installed("glmnet")
  fit <- prediction_regression_fit()
  w <- seq_len(nrow(fit$x))
  for (s in list("lambda.1se", 1, 0.01)) {
    p <- as.numeric(predict(fit, newx = fit$x, s = s))
    expect_equal(get_auc(fit, fit$x, fit$y, weights = w, s = s), auc(fit$y, p, w = w))
  }
  expect_error(get_auc(fit, fit$x, fit$y, unused = TRUE), "[Uu]nsupported|unused")
  expect_error(get_auc(fit, fit$x, fit$y, s = "bad"), "s|lambda")
  expect_error(get_auc(fit, fit$x, fit$y, s = c(1, 0.01)), "single lambda")
})

test_that("prediction table thresholds are strict at model and default cutoffs", {
  fit <- structure(list(pred_y = matrix(c(0.4, 0.5, 0.6), ncol = 1), cutoff = 0.4), class = "xplus")
  truth <- c(0, 1, 1)
  out <- get_predictions(fit, NULL, truth)
  expect_identical(as.character(out$predicted), c("Class2", "Class1", "Class1"))
  out <- get_predictions(fit, NULL, truth, use_cutoff = FALSE)
  expect_identical(as.character(out$predicted), c("Class2", "Class2", "Class1"))
})
