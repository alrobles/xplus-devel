test_that("pu_xplus registers idempotently and fits via parsnip", {
  skip_if_not_installed("parsnip")
  library(parsnip)
  expect_true(xplus_parsnip_register())
  expect_true(xplus_parsnip_register())

  sim <- simulate_pu_data(n = 120, p = 4, boundary = "nonlinear", seed = 11,
                          prevalence = 0.4, label_propensity = 0.5)
  spec <- pu_xplus(backend = "ranger", seed = 7, max_iter = 10, trees = 50)
  fit <- fit_xy(spec, x = data.frame(sim$x), y = factor(sim$y, levels = c(0, 1)))
  expect_s3_class(fit, "model_fit")
  expect_s3_class(fit$fit, "xplus")

  pr <- predict(fit, new_data = data.frame(sim$x), type = "prob")
  expect_named(pr, c(".pred_0", ".pred_1"))
  expect_equal(nrow(pr), nrow(sim$x))
  expect_true(all(pr$.pred_0 + pr$.pred_1 == 1))

  cls <- predict(fit, new_data = data.frame(sim$x), type = "class")
  expect_s3_class(cls$.pred_class, "factor")
  expect_setequal(levels(cls$.pred_class), c("0", "1"))
})

test_that("pu_xplus spec updates and drops foreign backend args", {
  skip_if_not_installed("parsnip")
  library(parsnip)
  spec <- pu_xplus(backend = "ranger", seed = 1, max_iter = 5, trees = 10)
  spec2 <- update(spec, trees = 25)
  expect_s3_class(spec2, "model_spec")
  expect_equal(rlang::eval_tidy(spec2$args$trees), 25)
})

test_that("pu_xplus fits convert factor and data-frame outcomes", {
  skip_if_not_installed("parsnip")
  sim <- simulate_pu_data(n = 80, p = 3, seed = 5, prevalence = 0.4,
                          label_propensity = 0.5)
  f1 <- xplus_parsnip_fit(x = data.frame(sim$x),
                          y = factor(sim$y, levels = c(0, 1)),
                          backend = "ranger", seed = 1, max_iter = 5,
                          trees = 20)
  f2 <- xplus_parsnip_fit(x = data.frame(sim$x),
                          y = data.frame(y = sim$y),
                          backend = "ranger", seed = 1, max_iter = 5,
                          trees = 20)
  expect_s3_class(f1, "xplus")
  expect_s3_class(f2, "xplus")
  expect_equal(predict(f1), predict(f2))
})

test_that("tune_grid ranks pu_xplus candidates", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("tune")
  skip_if_not_installed("workflows")
  skip_if_not_installed("rsample")
  skip_if_not_installed("yardstick")
  library(parsnip)
  sim <- simulate_pu_data(n = 120, p = 4, boundary = "nonlinear", seed = 11,
                          prevalence = 0.4, label_propensity = 0.5)
  d <- data.frame(sim$x); d$y <- factor(sim$y, levels = c(0, 1))
  wf <- workflows::workflow(
    y ~ .,
    spec = pu_xplus(backend = "ranger", seed = 7, max_iter = 10,
                    trees = tune::tune())
  )
  set.seed(3)
  res <- tune::tune_grid(
    wf, resamples = rsample::vfold_cv(d, v = 2, strata = y),
    grid = data.frame(trees = c(20, 60)),
    metrics = yardstick::metric_set(yardstick::roc_auc)
  )
  met <- tune::collect_metrics(res)
  expect_equal(nrow(met), 2)
  expect_true(all(met$.metric == "roc_auc"))
  expect_true(all(met$mean > 0 & met$mean <= 1))
})

test_that("parsnip bridge preserves arbitrary class labels", {
  skip_if_not_installed("parsnip")
  library(parsnip)
  sim <- simulate_pu_data(n = 120, p = 2, seed = 5, label_propensity = 0.5)
  d <- data.frame(sim$x)
  d$y <- factor(ifelse(sim$y == 1, "pos", "neg"), levels = c("neg", "pos"))
  spec <- pu_xplus(backend = "glmnet") |> set_engine("xplus")
  f <- fit_xy(spec, x = d[, c("x1", "x2")], y = d$y)
  pr <- predict(f, d[, c("x1", "x2")], type = "prob")
  expect_named(pr, c(".pred_neg", ".pred_pos"))
  pc <- predict(f, d[, c("x1", "x2")], type = "class")
  expect_identical(levels(pc[[1]]), c("neg", "pos"))
})

test_that("parsnip bridge expands non-numeric predictors", {
  skip_if_not_installed("parsnip")
  library(parsnip)
  sim <- simulate_pu_data(n = 120, p = 2, seed = 6, label_propensity = 0.5)
  d <- data.frame(sim$x)
  d$y <- factor(sim$y, levels = c(0, 1))
  d$grp <- factor(sample(letters[1:3], nrow(d), TRUE))
  spec <- pu_xplus(backend = "glmnet") |> set_engine("xplus")
  f <- fit(spec, y ~ ., data = d)
  expect_s3_class(f$fit, "xplus")
  pr <- predict(f, d, type = "prob")
  expect_true(all(c(".pred_0", ".pred_1") %in% names(pr)))
})

test_that("numeric prediction is rejected for classification spec", {
  skip_if_not_installed("parsnip")
  library(parsnip)
  sim <- simulate_pu_data(n = 80, p = 2, seed = 7, label_propensity = 0.5)
  d <- data.frame(sim$x); d$y <- factor(sim$y, levels = c(0, 1))
  f <- fit(pu_xplus(backend = "glmnet") |> set_engine("xplus"), y ~ ., data = d)
  expect_error(predict(f, d, type = "numeric"))
})

test_that("pu_auc metric matches internal labeled-vs-unlabeled AUC", {
  skip_if_not_installed("yardstick")
  library(yardstick)
  sim <- simulate_pu_data(n = 200, p = 3, seed = 1, label_propensity = 0.5)
  fit <- xplus(sim$x, sim$y, backend = "glmnet", seed = 1)
  p <- as.numeric(predict(fit))
  d <- data.frame(y = factor(sim$y, levels = 0:1), .pred_1 = p)
  out <- pu_auc(d, y, .pred_1)
  expect_equal(out$.metric, "pu_auc")
  expect_equal(attr(pu_auc, "direction"), "maximize")
  expect_equal(pu_auc_vec(d$y, p), auc(sim$y, p), tolerance = 1e-6)
})

test_that("workflow_set ranks backends by pu_auc", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("workflows")
  skip_if_not_installed("workflowsets")
  skip_if_not_installed("yardstick")
  skip_if_not_installed("rsample")
  library(parsnip); library(workflows); library(workflowsets)
  library(yardstick); library(rsample)
  sim <- simulate_pu_data(n = 150, p = 4, boundary = "nonlinear", seed = 11,
                          prevalence = 0.4, label_propensity = 0.5)
  d <- data.frame(sim$x); d$y <- factor(sim$y, levels = 0:1)
  wfs <- workflow_set(
    preproc = list(base = y ~ .),
    models = list(
      glmnet = pu_xplus(backend = "glmnet", seed = 7, max_iter = 5),
      ranger = pu_xplus(backend = "ranger", seed = 7, max_iter = 5, trees = 40)
    ),
    cross = FALSE
  )
  set.seed(1)
  res <- workflow_map(wfs, "fit_resamples",
                      resamples = vfold_cv(d, v = 2, strata = y),
                      metrics = metric_set(pu_auc))
  rk <- rank_results(res, rank_metric = "pu_auc", select_best = TRUE)
  expect_equal(nrow(rk), 2)
  expect_true(all(rk$.metric == "pu_auc"))
})
