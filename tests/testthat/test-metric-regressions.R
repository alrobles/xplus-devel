metric_fixture <- function(p, cutoff = 0.5) {
  structure(list(pred_y = if (is.matrix(p)) p else matrix(p, ncol = 1), cutoff = cutoff), class = "xplus")
}

metric_auc_reference <- function(y, p, w = rep(1, length(p))) {
  positive <- y[, 2] * w
  negative <- y[, 1] * w
  sum((outer(p, p, ">") + 0.5 * outer(p, p, "==")) *
        outer(positive, negative)) / (sum(positive) * sum(negative))
}

metric_loss_reference <- function(y, p, w, cutoff = 0.5) {
  mass <- rowSums(y)
  q <- y[, 2] / ifelse(mass == 0, 1, mass)
  w <- w * mass
  clipped <- pmin(pmax(p, 1e-5), 1 - 1e-5)
  entropy <- function(z) ifelse(z == 0, 0, z * log(z))
  list(deviance = weighted.mean(2 * (entropy(q) + entropy(1 - q) -
         q * log(clipped) - (1 - q) * log1p(-clipped)), w),
       class = weighted.mean((1 - q) * (p > cutoff) + q * (p <= cutoff), w),
       auc = metric_auc_reference(y, p, w / ifelse(mass == 0, 1, mass)),
       mse = weighted.mean(2 * (q - p)^2, w),
       mae = weighted.mean(2 * abs(q - p), w))
}

test_that("binary AUC validates encodings, finite scores and weights", {
  p <- c(-3, 2, 2, 8)
  y <- c(0, 0, 1, 1)
  for (labels in list(y, as.logical(y), as.character(y), factor(y, levels = 1:0))) {
    expect_equal(auc(labels, p), 0.875)
    expect_equal(auc(labels, p, NULL), 0.875)
  }
  for (bad in list(c(0.2, 0.2, 0.8, 0.8), c(-1, -1, 2, 2), c(0, NA, 1, 1))) {
    expect_error(auc(bad, p), "binary|0 and 1")
  }
  expect_error(auc(y[-1], p), "label|length|observation")
  for (bad in list(c(NA, 2, 2, 8), c(NaN, 2, 2, 8), c(Inf, 2, 2, 8), as.character(p))) {
    expect_error(auc(y, bad), "numeric|finite")
  }
  for (bad in list(1, c(1, 2), c(-1, 1, 1, 1), c(NA, 1, 1, 1),
                   c(NaN, 1, 1, 1), c(Inf, 1, 1, 1), rep("1", 4))) {
    expect_error(auc(y, p, bad), "weight|`w`")
    expect_error(auc_matrix(cbind(1 - y, y), p, bad), "weight")
    expect_error(assess(metric_fixture(plogis(p)), newy = y, weights = bad), "weight")
  }
})

test_that("undefined AUC is warned NA_real_ and does not abort other losses", {
  for (y in list(rep(0, 4), rep(1, 4), factor(rep(0, 4), levels = 0:1))) {
    expect_warning(value <- auc(y, 1:4), "AUC.*(positive|negative|class|mass)")
    expect_identical(value, NA_real_)
    expect_warning(out <- assess(metric_fixture(c(0.1, 0.3, 0.7, 0.9)), newy = y), "AUC")
    expect_identical(out$auc, NA_real_)
    expect_true(all(is.finite(unlist(out[-3]))))
  }
  for (w in list(rep(0, 4), c(1, 1, 0, 0))) {
    expect_warning(value <- auc(c(0, 0, 1, 1), 1:4, w), "AUC")
    expect_identical(value, NA_real_)
  }
  expect_warning(out <- assess(metric_fixture(c(0.1, 0.3, 0.7, 0.9)),
                               newy = c(0, 0, 1, 1), weights = rep(0, 4)), "zero|mass|weight")
  expect_true(all(vapply(out, identical, logical(1), NA_real_)))
})

test_that("two-column truth is strict and one-row or zero-mass labels retain shape", {
  y <- cbind(c(1, 0), c(0, 1))
  p <- c(0.1, 0.9)
  expect_equal(auc_matrix(y, p, NULL), 1)
  for (bad in list(y[, 1, drop = FALSE], cbind(y, 0), y[1, , drop = FALSE],
                   replace(y, 1, -1), replace(y, 1, Inf), replace(y, 1, NA_real_))) {
    expect_error(auc_matrix(bad, p), "two.column|mass|finite|row")
    expect_error(assess(metric_fixture(p), newy = bad), "two.column|mass|finite|row")
  }
  expect_error(assess(metric_fixture(p), newy = c(0.2, 0.8)), "use two-column")
  expect_warning(out <- assess(metric_fixture(0.8), newy = matrix(c(0, 1), 1, 2)), "AUC")
  expect_identical(out$auc, NA_real_)
  expect_equal(out$mse, 0.08)
  expect_equal(out$mae, 0.4)
  expect_equal(assess(metric_fixture(0.8), newy = matrix(c(2, 3), 1, 2))$auc, 0.5)
  yz <- rbind(y, c(0, 0))
  expect_equal(assess(metric_fixture(c(p, 0.99)), newy = yz), assess(metric_fixture(p), newy = y))
  expect_warning(out <- assess(metric_fixture(p), newy = y * 0), "mass|weight")
  expect_true(all(vapply(out, identical, logical(1), NA_real_)))
})

test_that("assess preserves direct probabilities and cutoff boundaries", {
  for (cutoff in c(0, 0.1, 0.2, 0.3, 0.4, 0.6, 0.9, 1)) {
    expect_equal(assess(metric_fixture(rep(cutoff, 4), cutoff), newy = c(0, 0, 0, 1))$class, 0.25)
  }
  expect_equal(assess(metric_fixture(c(1e-20, 1e-18)), newy = 0:1)$auc, 1)
  for (bad in c(NA_real_, NaN, Inf, -Inf, -0.1, 1.1)) {
    expect_error(assess(metric_fixture(c(bad, 0.8)), newy = 0:1), "finite|probabilit")
    expect_error(assess(metric_fixture(c(0.2, 0.8)), newy = c(bad, 1)), "binary|0 and 1")
  }
})

test_that("weighted mass metrics match independent references and replication", {
  set.seed(1907)
  for (i in seq_len(30)) {
    y <- matrix(runif(48, 0, 5), 24, 2)
    y[1, ] <- 0
    p <- sample(seq(0, 1, 0.1), 24, replace = TRUE)
    w <- sample(0:5, 24, replace = TRUE)
    expect_equal(auc_matrix(y, p, w), metric_auc_reference(y, p, w), tolerance = 1e-12)
    expect_equal(assess(metric_fixture(p, 0.4), newy = y, weights = w),
                 metric_loss_reference(y, p, w, 0.4), tolerance = 1e-12)
    index <- rep(seq_along(p), w)
    expect_equal(auc_matrix(y, p, w), auc_matrix(y[index, , drop = FALSE], p[index]), tolerance = 1e-12)
    expect_equal(assess(metric_fixture(p), newy = y, weights = w),
                 assess(metric_fixture(p[index]), newy = y[index, , drop = FALSE]), tolerance = 1e-12)
  }
})

test_that("metric response input and multiple prediction columns have stable shape", {
  p <- cbind(first = c(0.1, 0.8), second = c(0.7, 0.2))
  y <- 0:1
  for (measure in c("deviance", "class", "auc", "mse", "mae")) {
    expect_no_warning(response <- xplus:::xplus_cv_lognet(p, y, measure, input = "response"))
    expect_no_warning(link <- xplus:::xplus_cv_lognet(qlogis(p), y, measure))
    expect_equal(response$cvraw, link$cvraw, tolerance = 1e-12)
  }
})

test_that("finite extreme weights do not overflow or quantize averaged losses", {
  fit <- metric_fixture(c(0.9, 0.1))
  expected <- assess(fit, newy = 0:1)
  for (scale in c(5e307, 5e-324)) {
    expect_equal(assess(fit, newy = 0:1, weights = rep(scale, 2)), expected, tolerance = 1e-12)
  }
})
