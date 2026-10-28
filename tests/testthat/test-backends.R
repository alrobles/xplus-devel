test_that("xplus_backend validates its contract", {
  fit_fun <- function(x, y, foldid, weights) 1
  predict_fun <- function(model, newx) rep(0.5, nrow(newx))
  expect_error(xplus_backend("", fit_fun, predict_fun), "name")
  expect_error(xplus_backend("b", "not a function", predict_fun), "fit")
  expect_error(xplus_backend("b", function(x) x, predict_fun), "signature")
  expect_error(xplus_backend("b", fit_fun, "nope"), "predict")
  expect_error(xplus_backend("b", fit_fun, predict_fun, importance = 3), "importance")
  expect_error(xplus_backend("b", fit_fun, predict_fun, label = ""), "label")
  expect_error(xplus_backend("b", fit_fun, predict_fun, uses_cv = NA), "uses_cv")
  expect_error(xplus_backend("b", fit_fun, predict_fun, fit_predict = "x"), "fit_predict")
  expect_error(xplus_backend("b", fit_fun, predict_fun,
                             fit_predict = function(m) m), "signature")

  backend <- xplus_backend("b", fit_fun, predict_fun)
  expect_s3_class(backend, "xplus_backend")
  expect_equal(backend$name, "b")
  expect_equal(backend$label, "b")
  expect_false(backend$uses_cv)
  expect_output(print(backend), "xplus backend")
})

test_that("xplus resolves backend names and rejects invalid values", {
  skip_if_not_installed("glmnet")
  set.seed(1)
  x <- matrix(rnorm(60 * 4), ncol = 4)
  y <- c(rep(1, 15), rep(0, 45))

  expect_error(xplus(x, y, backend = "svm", max_iter = 2), "backend|arg")
  expect_error(xplus(x, y, backend = 42, max_iter = 2), "backend")

  fit <- xplus(x, y, backend = "glmnet", max_iter = 2)
  expect_equal(fit$backend$name, "glmnet")
  expect_s3_class(fit$backend, "xplus_backend")
})

test_that("a custom backend spec drives the iterative and final fits", {
  skip_if_not_installed("glmnet")
  set.seed(11)
  x <- matrix(rnorm(90 * 4), ncol = 4)
  y <- c(rep(1, 25), rep(0, 65))
  constant <- xplus_backend(
    name = "constant",
    fit = function(x, y, foldid, weights) stats::weighted.mean(y, weights),
    predict = function(model, newx) rep(model, nrow(newx))
  )

  fit <- xplus(x, y, backend = constant, max_iter = 4)

  expect_s3_class(fit, "xplus")
  expect_equal(fit$backend$name, "constant")
  expect_true(all(fit$history$cv_measure == "none"))
  expect_equal(length(unique(as.numeric(fit$pred_y))), 1)
  probs <- predict(fit)
  expect_true(all(probs >= 0 & probs <= 1))
})

test_that("the ranger backend fits, predicts and records importance", {
  skip_if_not_installed("ranger")
  sim <- simulate_pu_data(n = 250, p = 6, boundary = "linear", seed = 7,
                          label_propensity = 0.7)

  fit <- xplus(sim$x, sim$y, backend = "ranger", max_iter = 8, seed = 7)

  expect_s3_class(fit, "xplus")
  expect_equal(fit$backend$name, "ranger")
  expect_true(all(fit$history$cv_measure == "none"))
  probs <- predict(fit)
  expect_true(is.matrix(probs) && ncol(probs) == 1)
  expect_true(all(probs >= 0 & probs <= 1))
  classes <- predict(fit, type = "class")
  expect_true(is.factor(classes) && all(levels(classes) == c("0", "1")))
  imp <- get_importance(fit)
  expect_true(is.numeric(imp) && length(imp) == ncol(sim$x))
  expect_identical(names(imp), colnames(sim$x))
  expect_error(coef(fit), "glmnet")
})

test_that("ranger predictions are reproducible under xplus(seed)", {
  skip_if_not_installed("ranger")
  sim <- simulate_pu_data(n = 200, p = 5, seed = 3, label_propensity = 0.7)

  a <- xplus(sim$x, sim$y, backend = "ranger", max_iter = 5, seed = 9)
  b <- xplus(sim$x, sim$y, backend = "ranger", max_iter = 5, seed = 9)

  expect_identical(as.numeric(a$pred_y), as.numeric(b$pred_y))
})

test_that("the gam backend fits and predicts probabilities", {
  skip_if_not_installed("mgcv")
  sim <- simulate_pu_data(n = 250, p = 6, boundary = "linear", seed = 5,
                          label_propensity = 0.7)

  fit <- xplus(sim$x, sim$y, backend = "gam", max_iter = 5, seed = 5)

  expect_s3_class(fit, "xplus")
  expect_equal(fit$backend$name, "gam")
  probs <- predict(fit)
  expect_true(all(probs >= 0 & probs <= 1))
  expect_error(coef(fit), "glmnet")
})

test_that("backend objects carry their own hyperparameters", {
  skip_if_not_installed("ranger")
  sim <- simulate_pu_data(n = 200, p = 5, seed = 4, label_propensity = 0.7)
  backend <- xplus_backend_ranger(trees = 50, importance.mode = "none")

  fit <- xplus(sim$x, sim$y, backend = backend, max_iter = 4, seed = 4)

  expect_equal(fit$xplus$model$num.trees, 50)
  expect_true(all(c("trees", "mtry", "min_n") %in% fit$backend$parameters$parameter))
  expect_null(get_importance(fit))
})

test_that("non-glmnet backends reject lambda arguments", {
  skip_if_not_installed("ranger")
  sim <- simulate_pu_data(n = 200, p = 5, seed = 6, label_propensity = 0.7)
  fit <- xplus(sim$x, sim$y, backend = "ranger", max_iter = 3, seed = 6)

  expect_error(predict(fit, s = "lambda.1se"), "glmnet")
  expect_error(predict(fit, s = 0.1), "glmnet")
  expect_silent(predict(fit, s = "lambda.min"))
  link <- predict(fit, type = "link")
  expect_equal(dim(link), c(nrow(sim$x), 1L))
})

test_that("summary and print handle non-glmnet backends", {
  skip_if_not_installed("ranger")
  sim <- simulate_pu_data(n = 200, p = 5, seed = 8, label_propensity = 0.7)
  fit <- xplus(sim$x, sim$y, backend = "ranger", max_iter = 4, seed = 8)

  out <- summary(fit)
  expect_equal(out$backend, "ranger")
  expect_null(out$lambda_min)
  expect_output(print(out), "Backend: ranger")
  expect_output(print(fit), "Backend: ranger")
})

test_that("fit_predict drives honest cutoff calibration and stored predictions", {
  skip_if_not_installed("glmnet")
  set.seed(3)
  x <- matrix(rnorm(80 * 4), ncol = 4)
  y <- c(rep(1, 20), rep(0, 60))
  backend <- xplus_backend(
    name = "honest-constant",
    fit = function(x, y, foldid, weights) stats::weighted.mean(y, weights),
    predict = function(model, newx) rep(model, nrow(newx)),
    fit_predict = function(model, x) rep(0.3, nrow(x))
  )

  fit <- xplus(x, y, backend = backend, max_iter = 3)

  # Every row is a fit row in the final stage, so stored predictions and the
  # positive quantile cutoff come from the honest 0.3 predictions.
  expect_true(all(as.numeric(fit$pred_y) == 0.3))
  expect_equal(fit$cutoff, 0.3)
  expect_equal(as.numeric(predict(fit)), rep(0.3, nrow(x)))
})

test_that("non-finite fit_predict entries fall back to predict on those rows", {
  skip_if_not_installed("glmnet")
  set.seed(4)
  x <- matrix(rnorm(80 * 4), ncol = 4)
  y <- c(rep(1, 20), rep(0, 60))
  backend <- xplus_backend(
    name = "sparse-honest",
    fit = function(x, y, foldid, weights) stats::weighted.mean(y, weights),
    predict = function(model, newx) rep(model, nrow(newx)),
    fit_predict = function(model, x) replace(rep(NaN, nrow(x)), 1L, 0.4)
  )

  fit <- xplus(x, y, backend = backend, max_iter = 3)

  expect_true(all(is.finite(as.numeric(fit$pred_y))))
  # The first fit row is the first positive (row 1 of x); its honest value is
  # kept while the remaining rows fall back to the weighted-mean prediction.
  expect_equal(as.numeric(fit$pred_y)[1], 0.4)
})

test_that("ranger uses out-of-bag predictions for training rows", {
  skip_if_not_installed("ranger")
  sim <- simulate_pu_data(n = 400, p = 8, boundary = "nonlinear", seed = 21,
                          prevalence = 0.4, label_propensity = 0.5)

  fit <- xplus(sim$x, sim$y, backend = "ranger", max_iter = 100, seed = 21)

  pos <- fit$original_y == 1
  unl <- !pos
  pred <- as.numeric(fit$pred_y)
  # Honest positive predictions are far below the memorized in-sample scores
  # a probability forest produces on its own training rows.
  insample <- as.numeric(.xplus_backend_predict(fit$backend, fit$xplus, sim$x))
  expect_true(median(pred[pos]) < median(insample[pos]))
  # The cutoff tracks the honest positive quantile and stays reachable;
  # before honest predictions, in-sample memorization pushed the cutoff above
  # every unlabeled score and collapsed F1 to zero on hidden positives.
  expect_equal(fit$cutoff, unname(stats::quantile(pred[pos], fit$qq)))
  expect_true(mean(pred[unl] > fit$cutoff) > 0.05)
  expect_equal(as.numeric(predict(fit)), pred)
})

test_that("the xgboost backend fits and calibrates on out-of-fold scores", {
  skip_if_not_installed("xgboost")
  sim <- simulate_pu_data(n = 300, p = 6, boundary = "linear", seed = 17,
                          label_propensity = 0.7)

  fit <- xplus(sim$x, sim$y, backend = "xgboost", max_iter = 5, seed = 17)

  expect_s3_class(fit, "xplus")
  expect_equal(fit$backend$name, "xgboost")
  expect_true(all(fit$history$cv_measure == "none"))
  probs <- predict(fit)
  expect_true(all(probs >= 0 & probs <= 1))
  # Stored training predictions are out-of-fold scores from xgb.cv, so the
  # cutoff stays reachable for unlabeled cases.
  pos <- fit$original_y == 1
  pred <- as.numeric(fit$pred_y)
  expect_equal(fit$cutoff, unname(stats::quantile(pred[pos], fit$qq)))
  insample <- as.numeric(.xplus_backend_predict(fit$backend, fit$xplus, sim$x))
  expect_true(median(pred[pos]) <= median(insample[pos]))
  imp <- get_importance(fit)
  expect_true(is.numeric(imp) && length(imp) == ncol(sim$x))
  expect_error(coef(fit), "glmnet")
})

test_that("xgboost predictions are reproducible under xplus(seed)", {
  skip_if_not_installed("xgboost")
  sim <- simulate_pu_data(n = 200, p = 5, seed = 19, label_propensity = 0.7)

  a <- xplus(sim$x, sim$y, backend = "xgboost", max_iter = 4, seed = 23)
  b <- xplus(sim$x, sim$y, backend = "xgboost", max_iter = 4, seed = 23)

  expect_identical(as.numeric(a$pred_y), as.numeric(b$pred_y))
})

test_that("the lightgbm backend fits, predicts and records importance", {
  skip_if_not_installed("lightgbm")
  sim <- simulate_pu_data(n = 300, p = 6, boundary = "linear", seed = 29,
                          label_propensity = 0.7)

  fit <- xplus(sim$x, sim$y, backend = "lightgbm", max_iter = 5, seed = 29)

  expect_s3_class(fit, "xplus")
  expect_equal(fit$backend$name, "lightgbm")
  probs <- predict(fit)
  expect_true(all(probs >= 0 & probs <= 1))
  pos <- fit$original_y == 1
  expect_equal(fit$cutoff,
               unname(stats::quantile(as.numeric(fit$pred_y)[pos], fit$qq)))
  imp <- get_importance(fit)
  expect_true(is.numeric(imp) && length(imp) == ncol(sim$x))
  expect_error(coef(fit), "glmnet")
})

test_that("backend name resolution covers the boosting engines", {
  skip_if_not_installed("glmnet")
  expect_equal(.xplus_resolve_backend("xgboost")$name, "xgboost")
  expect_equal(.xplus_resolve_backend("lightgbm")$name, "lightgbm")
  expect_error(xplus(matrix(rnorm(60 * 4), ncol = 4),
                     c(rep(1, 15), rep(0, 45)), backend = "svm",
                     max_iter = 2), "backend|arg")
})

test_that("get_importance falls back to glmnet coefficients for legacy objects", {
  skip_if_not_installed("glmnet")
  set.seed(2)
  x <- matrix(rnorm(80 * 4), ncol = 4)
  y <- c(rep(1, 20), rep(0, 60))
  fit <- xplus(x, y, max_iter = 3)

  imp <- get_importance(fit)
  expect_true(is.numeric(imp) && length(imp) == ncol(x))
})

test_that("xplus_backend_parsnip validates its spec", {
  skip_if_not_installed("parsnip")
  expect_error(xplus_backend_parsnip(list()), "model_spec")
  spec <- parsnip::rand_forest(trees = 50)
  spec$engine <- ""
  expect_error(xplus_backend_parsnip(spec), "set_engine")
})

test_that("the parsnip backend drives xplus with a ranger engine", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("ranger")
  sim <- simulate_pu_data(n = 300, p = 6, boundary = "nonlinear", seed = 31,
                          prevalence = 0.4, label_propensity = 0.5)
  backend <- parsnip::rand_forest(trees = 100) |>
    parsnip::set_engine("ranger", importance = "impurity") |>
    xplus_backend_parsnip()

  fit <- xplus(sim$x, sim$y, backend = backend, max_iter = 6, seed = 31)

  expect_s3_class(fit, "xplus")
  expect_equal(fit$backend$name, "parsnip:ranger")
  expect_true(all(fit$history$cv_measure == "none"))
  probs <- predict(fit)
  expect_true(all(probs >= 0 & probs <= 1))
  # OOB honest scores sit below memorized in-sample ones.
  pos <- fit$original_y == 1
  pred <- as.numeric(fit$pred_y)
  insample <- as.numeric(.xplus_backend_predict(fit$backend, fit$xplus, sim$x))
  expect_true(median(pred[pos]) < median(insample[pos]))
  imp <- get_importance(fit)
  expect_true(is.numeric(imp) && length(imp) == ncol(sim$x))
})

test_that("parsnip backend predictions are reproducible under xplus(seed)", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("ranger")
  sim <- simulate_pu_data(n = 200, p = 5, seed = 33, label_propensity = 0.7)
  backend <- function() {
    parsnip::rand_forest(trees = 50) |>
      parsnip::set_engine("ranger") |>
      xplus_backend_parsnip()
  }

  a <- xplus(sim$x, sim$y, backend = backend(), max_iter = 4, seed = 37)
  b <- xplus(sim$x, sim$y, backend = backend(), max_iter = 4, seed = 37)

  expect_identical(as.numeric(a$pred_y), as.numeric(b$pred_y))
})

test_that("capability flags validate and gate the final fit", {
  skip_if_not_installed("glmnet")
  fit_fun <- function(x, y, foldid, weights) stats::weighted.mean(y, weights)
  predict_fun <- function(model, newx) rep(model, nrow(newx))
  expect_error(xplus_backend("b", fit_fun, predict_fun,
                             supports_fractional = NA), "supports_fractional")
  expect_error(xplus_backend("b", fit_fun, predict_fun,
                             supports_weights = "yes"), "supports_weights")

  set.seed(5)
  x <- matrix(rnorm(80 * 4), ncol = 4)
  y <- c(rep(1, 20), rep(0, 60))
  binary_only <- xplus_backend("binary-only", fit_fun, predict_fun,
                               supports_fractional = FALSE)
  expect_output(print(binary_only), "Fractional targets: FALSE")
  # Soft final pseudo-labels make every final target fractional.
  expect_error(xplus(x, y, backend = binary_only, max_iter = 2),
               "supports_fractional")
})

test_that("supports_weights warns under bootstrap sampling", {
  skip_if_not_installed("glmnet")
  set.seed(6)
  x <- matrix(rnorm(80 * 4), ncol = 4)
  y <- c(rep(1, 20), rep(0, 60))
  unweighted <- xplus_backend(
    "unweighted",
    fit = function(x, y, foldid, weights) stats::weighted.mean(y, weights),
    predict = function(model, newx) rep(model, nrow(newx)),
    supports_weights = FALSE
  )
  expect_warning(xplus(x, y, backend = unweighted, max_iter = 2),
                 "supports_weights")
  expect_silent(xplus(x, y, backend = unweighted, max_iter = 2,
                      sampling = "unique"))
})

test_that("classification-only parsnip specs fail clearly on the final fit", {
  skip_if_not_installed("parsnip")
  sim <- simulate_pu_data(n = 150, p = 4, seed = 41, label_propensity = 0.7)
  backend <- parsnip::logistic_reg() |>
    parsnip::set_engine("glm") |>
    xplus_backend_parsnip()

  expect_error(xplus(sim$x, sim$y, backend = backend, max_iter = 2),
               "regression")
})

test_that("pu_score ranks learners without negative labels", {
  skip_if_not_installed("ranger")
  sim <- simulate_pu_data(n = 300, p = 6, boundary = "nonlinear", seed = 51,
                          prevalence = 0.4, label_propensity = 0.5)
  fit_g <- xplus(sim$x, sim$y, backend = "glmnet", max_iter = 5, seed = 51)
  fit_r <- xplus(sim$x, sim$y, backend = "ranger", max_iter = 5, seed = 51)
  expect_true(pu_score(fit_r) > pu_score(fit_g))
  expect_true(is.finite(pu_score(fit_r, "stability")))
  expect_true(is.finite(pu_score(fit_r, "pseudo_auc")))
})

test_that("xplus_grid compares names, specs and constructor grids", {
  skip_if_not_installed("glmnet")
  sim <- simulate_pu_data(n = 200, p = 4, seed = 53, label_propensity = 0.7)
  lb <- xplus_grid(sim$x, sim$y, c("glmnet", "ranger"), max_iter = 3, seed = 53)
  expect_true(is.data.frame(lb) && nrow(lb) == 2)
  expect_true(all(c("learner", "score", "n_iter", "stop_reason", "error") %in% names(lb)))
  expect_true(all(is.na(lb$error)))

  grid <- data.frame(alpha = c(0.5, 1))
  lg <- xplus_grid(sim$x, sim$y, xplus_backend_glmnet, params = grid,
                   max_iter = 3, seed = 53)
  expect_equal(sort(lg$alpha), c(0.5, 1))

  broken <- xplus_backend("broken",
    fit = function(x, y, foldid, weights) stop("boom"),
    predict = function(model, newx) rep(0.5, nrow(newx)))
  lf <- xplus_grid(sim$x, sim$y, list(broken, "glmnet"), max_iter = 3, seed = 53)
  expect_true(any(!is.na(lf$error)))
})
