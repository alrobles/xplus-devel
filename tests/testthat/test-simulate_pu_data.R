test_that("simulate_pu_data returns a coherent PU dataset", {
  sim <- simulate_pu_data(n = 400, p = 8, seed = 10)

  expect_s3_class(sim, "xplus_pu_data")
  expect_equal(dim(sim$x), c(400, 8))
  expect_true(all(sim$y %in% c(0, 1)))
  expect_true(all(sim$z %in% c(0, 1)))
  expect_true(all(sim$p_true > 0 & sim$p_true < 1))
  # Labeled positives are always a subset of true positives.
  expect_true(all(sim$z[sim$y == 1] == 1))
  # SCAR: some true positives remain hidden in the unlabeled pool.
  expect_gt(sum(sim$z == 1 & sim$y == 0), 0)
  expect_output(print(sim), "Simulated PU dataset")
})

test_that("simulate_pu_data is reproducible and respects its settings", {
  a <- simulate_pu_data(n = 300, p = 6, seed = 42)
  b <- simulate_pu_data(n = 300, p = 6, seed = 42)
  expect_identical(a$x, b$x)
  expect_identical(a$y, b$y)

  sim <- simulate_pu_data(n = 2000, p = 6, label_propensity = 0.25, seed = 42)
  rate <- sum(sim$y) / sum(sim$z)
  expect_true(abs(rate - 0.25) < 0.05)

  sparse <- simulate_pu_data(n = 300, p = 10, n_informative = 2, seed = 1)
  expect_equal(sparse$n_informative, 2)
})

test_that("each boundary produces learnable signal", {
  for (boundary in c("linear", "interaction", "nonlinear")) {
    sim <- simulate_pu_data(n = 500, p = 8, boundary = boundary, seed = 2,
                            prevalence = 0.4)
    # An oracle with access to p_true should rank well above chance.
    expect_gt(auc(sim$z, sim$p_true), 0.9)
  }
})

test_that("simulate_pu_data validates arguments", {
  expect_error(simulate_pu_data(n = 5), "n")
  expect_error(simulate_pu_data(p = 1), "p")
  expect_error(simulate_pu_data(boundary = "quadratic"), "arg|boundary")
  expect_error(simulate_pu_data(prevalence = 0), "prevalence")
  expect_error(simulate_pu_data(prevalence = 1), "prevalence")
  expect_error(simulate_pu_data(label_propensity = 0), "label_propensity")
  expect_error(simulate_pu_data(n_informative = 0), "n_informative")
  expect_error(simulate_pu_data(p = 5, n_informative = 6), "n_informative")
  expect_error(simulate_pu_data(sharpness = 0), "sharpness")
})
