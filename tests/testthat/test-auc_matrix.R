test_that("auc_matrix unweighted returns high AUC for separable data", {
  # Perfectly separable: first 4 are class 0, last 4 are class 1
  y <- cbind(c(1, 1, 1, 1, 0, 0, 0, 0), c(0, 0, 0, 0, 1, 1, 1, 1))
  p <- c(0.1, 0.15, 0.2, 0.25, 0.75, 0.8, 0.85, 0.9)
  result <- auc_matrix(y, p)
  expect_gte(result, 0.9)
})

test_that("auc_matrix unweighted matches weighted with unit weights", {
  y <- cbind(c(1, 1, 0, 0), c(0, 0, 1, 1))
  p <- c(0.2, 0.3, 0.7, 0.8)
  w <- rep(1, 4)
  expect_equal(auc_matrix(y, p), auc_matrix(y, p, weights = w))
})

test_that("auc_matrix unweighted is not ~0.5 for separable data", {
  y <- cbind(c(1, 1, 1, 0, 0, 0), c(0, 0, 0, 1, 1, 1))
  p <- c(0.1, 0.2, 0.3, 0.7, 0.8, 0.9)
  result <- auc_matrix(y, p)
  expect_gt(result, 0.5)
})

test_that("mass AUC agrees with binary augmentation and survives extreme finite weights", {
  set.seed(1910)
  for (i in seq_len(30)) {
    y <- matrix(runif(40), 20, 2)
    p <- sample(-3:3, 20, replace = TRUE)
    w <- runif(20)
    labels <- rep(0:1, each = 20)
    scores <- rep(p, 2)
    mass <- as.vector(y * w)
    reference <- survival::concordance(labels ~ scores, weights = mass)$concordance
    expect_equal(auc_matrix(y, p, w), reference, tolerance = 1e-12)
  }
  y <- cbind(c(1, 0, 1), c(0, 1, 1))
  p <- c(-1, 1, 0)
  expected <- auc_matrix(y, p)
  for (scale in c(1e-300, 1e300)) {
    expect_equal(auc_matrix(y * scale, p, rep(scale, 3)), expected, tolerance = 1e-12)
  }
  expect_equal(auc_matrix(matrix(1e308, 1, 2), 2, 1e308), 0.5)
})
