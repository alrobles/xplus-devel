test_that("xplus_elkan_noto and xplus_naive fit and predict", {
  sim <- simulate_pu_data(n = 150, p = 4, boundary = "nonlinear", seed = 7,
                          prevalence = 0.4, label_propensity = 0.5)
  for (m in c("xplus_elkan_noto", "xplus_naive")) {
    fit <- get(m)(sim$x, sim$y, backend = "ranger", seed = 3, nfolds = 5)
    expect_s3_class(fit, "xplus_baseline")
    expect_true(fit$scale > 0 && fit$scale <= 1)
    expect_equal(fit$cutoff, 0.5)
    p <- predict(fit, newx = sim$x)
    expect_true(is.numeric(p) && all(p >= 0 & p <= 1))
    expect_equal(length(p), nrow(sim$x))
    unl <- sim$y == 0
    expect_gt(xplus::auc(sim$z[unl], p[unl]), 0.6)
  }
})

test_that("elkan_noto estimates labeling propensity near truth", {
  sim <- simulate_pu_data(n = 400, p = 4, seed = 2, prevalence = 0.4,
                          label_propensity = 0.5)
  fit <- xplus_elkan_noto(sim$x, sim$y, backend = "ranger", seed = 1, nfolds = 5)
  expect_gt(fit$scale, 0.1)
  expect_lt(fit$scale, 0.9)
  expect_equal(fit$method, "elkan_noto")
})

test_that("baselines are reproducible and validate inputs", {
  sim <- simulate_pu_data(n = 100, p = 3, seed = 4, prevalence = 0.4,
                          label_propensity = 0.5)
  f1 <- xplus_elkan_noto(sim$x, sim$y, backend = "ranger", seed = 9, nfolds = 5)
  f2 <- xplus_elkan_noto(sim$x, sim$y, backend = "ranger", seed = 9, nfolds = 5)
  expect_equal(predict(f1, newx = sim$x), predict(f2, newx = sim$x))
  expect_error(xplus_elkan_noto(sim$x, rep(1, nrow(sim$x))),
               "must contain both")
  expect_error(xplus_elkan_noto(sim$x, c(sim$y[-1], NA)),
               "0 and 1")
})
