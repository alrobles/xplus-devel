benchmark_harness_path <- function() {
  harness_path <- system.file("tools", "benchmark_paths.R", package = "xplus")
  if (identical(harness_path, "")) {
    harness_path <- testthat::test_path("../../inst/tools/benchmark_paths.R")
  }
  harness_path
}

test_that("benchmark harness returns required comparison metrics", {
  skip_if_not_installed("glmnet")

  harness <- new.env(parent = baseenv())
  sys.source(benchmark_harness_path(), envir = harness)

  set.seed(123)
  x <- matrix(rnorm(80 * 4), ncol = 4)
  y <- c(rep(1, 20), rep(0, 60))

  path_specs <- harness$default_path_specs(max_iter = 3, nfolds = 4)
  output_dir <- file.path(tempdir(), "xplus-benchmark-output")
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  result <- harness$run_xplus_path_benchmark(
    x = x,
    y = y,
    seeds = c(11, 19),
    path_specs = path_specs,
    output_dir = output_dir
  )

  expect_named(result, c("runs", "summary", "outputs"))
  expect_true(all(c(
    "path",
    "seed",
    "auc",
    "class_error",
    "support_size",
    "sparsity",
    "n_iter",
    "runtime_seconds",
    "support"
  ) %in% names(result$runs)))
  expect_true(all(c(
    "path",
    "auc_mean",
    "class_error_mean",
    "support_size_mean",
    "sparsity_mean",
    "n_iter_mean",
    "runtime_seconds_mean",
    "support_jaccard_mean"
  ) %in% names(result$summary)))

  expect_true(file.exists(result$outputs$runs_csv))
  expect_true(file.exists(result$outputs$summary_csv))
  expect_true(file.exists(result$outputs$table_md))
})

test_that("benchmark harness is stable under fixed seeds for decision metrics", {
  skip_if_not_installed("glmnet")

  harness <- new.env(parent = baseenv())
  sys.source(benchmark_harness_path(), envir = harness)

  set.seed(99)
  x <- matrix(rnorm(80 * 4), ncol = 4)
  y <- c(rep(1, 20), rep(0, 60))
  path_specs <- harness$default_path_specs(max_iter = 2, nfolds = 4)

  run_a <- harness$run_xplus_path_benchmark(
    x = x,
    y = y,
    seeds = c(3, 7),
    path_specs = path_specs
  )$runs
  run_b <- harness$run_xplus_path_benchmark(
    x = x,
    y = y,
    seeds = c(3, 7),
    path_specs = path_specs
  )$runs

  compare_columns <- c(
    "path",
    "seed",
    "auc",
    "class_error",
    "support_size",
    "sparsity",
    "n_iter",
    "support"
  )
  run_a <- run_a[order(run_a$path, run_a$seed), compare_columns]
  run_b <- run_b[order(run_b$path, run_b$seed), compare_columns]

  expect_equal(run_a, run_b)
})

test_that("benchmark default inputs are reproducible and usable", {
  harness <- new.env(parent = baseenv())
  sys.source(benchmark_harness_path(), envir = harness)
  inputs <- harness$default_benchmark_inputs()
  expect_true(is.matrix(inputs$x))
  expect_true(all(is.finite(inputs$x)))
  expect_equal(length(inputs$y), nrow(inputs$x))
  expect_true(all(table(inputs$y) >= 3))
  expect_identical(inputs, harness$default_benchmark_inputs())
  result <- harness$run_xplus_path_benchmark(seeds = 2, path_specs = harness$default_path_specs(max_iter = 2))
  expect_equal(nrow(result$runs), 2)
})
