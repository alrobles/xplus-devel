model_contract_fixture <- function(extended = FALSE) {
  set.seed(601)
  x <- matrix(rnorm(60 * 3), 60, 3, dimnames = list(NULL, paste0("v", 1:3)))
  y <- c(rep(1, 20), rep(0, 40))
  foldid <- rep(1:5, 12)
  fit <- suppressWarnings(glmnet::cv.glmnet(x, y, family = "binomial", foldid = foldid, type.measure = "deviance"))
  args <- list(
    fit_xplus = fit, pred_y = predict(fit, x, s = "lambda.min", type = "response"),
    cutoff = 0.5, predicted_coefficients = coef(fit, s = "lambda.min"),
    n_iter = 1L, x = x, y = y, alpha = 1, learning_rate = 1,
    pseudo_labels = y, iterative_path = "current", qq = 0.05,
    call = quote(xplus(x, y)), max_iter = 1L, stop_reason = "max_iter"
  )
  if (extended) {
    args <- c(args, list(
      original_y = y, final_labels = y, fallback_used = FALSE, fallback_reason = "",
      history = data.frame(iteration = 1L, stability = 1, coverage = 0.5,
                           sample_size = 20L, sample_draws = 20L, cv_folds = 5L,
                           cv_measure = "deviance", max_label_change = 0),
      sampling_counts = c(rep(0L, 20), rep(1L, 20), rep(0L, 20)),
      draw_counts = c(rep(0L, 20), rep(1L, 20), rep(0L, 20)),
      final_foldid = foldid, cv_measure = "deviance", sigmoid_scale = 1,
      sampling = "unique", min_iter = 1L, stability_window = 1L, min_coverage = 0
    ))
  }
  do.call(xplus:::new_xplus, args, quote = TRUE)
}

test_that("legacy model bundles retain their valid interface", {
  object <- model_contract_fixture()
  expect_identical(xplus:::validate_xplus(object), object)
  expect_named(summary(object), c("n_obs", "n_features", "n_iter", "cutoff", "alpha",
    "learning_rate", "iterative_path", "qq", "lambda_min", "lambda_1se",
    "n_nonzero_coefficients", "stop_reason"))
})

test_that("model validation rejects malformed core fields", {
  object <- model_contract_fixture()
  mutations <- list(
    function(z) { z$x[1, 1] <- Inf; z },
    function(z) { z$x[1, 1] <- NA_real_; z },
    function(z) { storage.mode(z$x) <- "character"; z },
    function(z) { colnames(z$x) <- rep("duplicate", 3); z },
    function(z) { z$pred_y <- matrix(0.5, 59, 1); z },
    function(z) { z$pred_y <- matrix(0.5, 60, 2); z },
    function(z) { z$pred_y[1, 1] <- Inf; z },
    function(z) { z$pred_y[1, 1] <- 1.1; z },
    function(z) { z$cutoff <- numeric(); z },
    function(z) { z$cutoff <- c(0.3, 0.5); z },
    function(z) { z$cutoff <- NA_real_; z },
    function(z) { z$cutoff <- -0.1; z },
    function(z) { z$predicted_coefficients[1, 1] <- Inf; z },
    function(z) { z$predicted_coefficients <- z$predicted_coefficients[-1, , drop = FALSE]; z },
    function(z) { z$n_iter <- -1; z },
    function(z) { z$n_iter <- 0.5; z },
    function(z) { z$n_iter <- 2L; z },
    function(z) { z$max_iter <- Inf; z },
    function(z) { z$y <- as.character(z$y); z },
    function(z) { z$pseudo_labels[1] <- -1; z },
    function(z) { z$alpha <- 2; z },
    function(z) { z$learning_rate <- NaN; z },
    function(z) { z$qq <- 1.1; z },
    function(z) { z$stop_reason <- "unknown"; z },
    function(z) { z$xplus <- structure(list(), class = "cv.glmnet"); z },
    function(z) { z$xplus$lambda.min <- Inf; z },
    function(z) { z$xplus$cvm <- numeric(); z },
    function(z) { z$xplus$glmnet.fit$nobs <- 59L; z }
  )
  for (i in seq_along(mutations)) {
    expect_error(xplus:::validate_xplus(mutations[[i]](object)), info = paste("mutation", i))
  }
})

test_that("extended model metadata is structurally and semantically checked", {
  object <- model_contract_fixture(TRUE)
  expect_identical(xplus:::validate_xplus(object), object)
  mutations <- list(
    function(z) { z$original_y[1] <- 0.5; z },
    function(z) { z$original_y <- as.character(z$original_y); z },
    function(z) { z$pseudo_labels[1] <- 0; z },
    function(z) { z$final_labels[1] <- 0; z },
    function(z) { z$y[21] <- 0.2; z },
    function(z) { z$fallback_used <- NA; z },
    function(z) { z$fallback_used <- 1; z },
    function(z) { z$fallback_reason <- "unexpected"; z },
    function(z) { z$history <- z$history[FALSE, ]; z },
    function(z) { z$history$iteration <- 2L; z },
    function(z) { z$history$stability <- 1.1; z },
    function(z) { z$history$coverage <- -0.1; z },
    function(z) { z$history$sample_draws <- 1L; z },
    function(z) { z$history$cv_folds <- 1L; z },
    function(z) { z$history$cv_measure <- "auc"; z },
    function(z) { z$history$max_label_change <- Inf; z },
    function(z) { z$sampling_counts[1] <- 1L; z },
    function(z) { z$sampling_counts[21] <- 2L; z },
    function(z) { z$draw_counts[21] <- 0L; z },
    function(z) { z$draw_counts <- z$draw_counts[-1]; z },
    function(z) { z$final_foldid[1] <- 0L; z },
    function(z) { z$final_foldid <- rep(1L, 60); z },
    function(z) { z$cv_measure <- "class"; z },
    function(z) { z$sigmoid_scale <- 0; z },
    function(z) { z$sampling <- "replacement"; z },
    function(z) { z$min_iter <- 0L; z },
    function(z) { z$stability_window <- 0L; z },
    function(z) { z$min_coverage <- 2; z },
    function(z) { z$history$sample_size <- 19L; z },
    function(z) { z$history$sample_draws <- 21L; z },
    function(z) { z$history$coverage <- 0.4; z },
    function(z) { z$fallback_used <- TRUE; z },
    function(z) { z$stop_reason <- "label_stability"; z$min_iter <- 2L; z },
    function(z) { z$stop_reason <- "label_stability"; z$min_coverage <- 1; z }
  )
  for (i in seq_along(mutations)) {
    expect_error(xplus:::validate_xplus(mutations[[i]](object)), info = paste("metadata mutation", i))
  }
})

test_that("fallback distinguishes proposed labels from final training targets", {
  object <- model_contract_fixture(TRUE)
  object$pseudo_labels[] <- 1
  object$fallback_used <- TRUE
  object$fallback_reason <- "degenerate_labels"
  expect_identical(xplus:::validate_xplus(object), object)
  out <- summary(object)
  expect_true(out$fallback_used)
  expect_equal(out$fallback_reason, "degenerate_labels")
  expect_equal(out$n_original_positive, 20)
  expect_equal(out$final_target_range, c(0, 1))
  expect_equal(out$n_final_soft_labels, 0)
  expect_output(print(object), "Fallback used: TRUE")
  expect_output(print(out), "Fallback used: TRUE")
  expect_output(print(out), "Original positives: 20")
})

test_that("zero completed iterations retain a typed empty diagnostic history", {
  object <- model_contract_fixture(TRUE)
  object$n_iter <- 0L
  object$stop_reason <- "degenerate_labels"
  object$history <- object$history[FALSE, ]
  object$sampling_counts[] <- 0L
  object$draw_counts[] <- 0L
  expect_identical(xplus:::validate_xplus(object), object)
})

test_that("sampling draws and actual CV measures preserve diagnostic meaning", {
  object <- model_contract_fixture(TRUE)
  object$draw_counts[21] <- 2L
  object$history$sample_draws <- 21L
  object$cv_measure <- "auc"
  expect_identical(xplus:::validate_xplus(object), object)
  object$sampling <- "bootstrap"
  expect_identical(xplus:::validate_xplus(object), object)
})

test_that("constructor preserves invalid scalar metadata for validation", {
  object <- model_contract_fixture()
  object$stop_reason <- c("max_iter", "label_stability")
  expect_error(xplus:::validate_xplus(object))
  expect_identical(xplus:::new_xplus(stop_reason = c("max_iter", "label_stability"))$stop_reason,
                   c("max_iter", "label_stability"))
})
