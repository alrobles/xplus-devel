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
