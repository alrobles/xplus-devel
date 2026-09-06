audit_core <- function(repo = ".", output_dir = file.path("inst", "benchmarks", "v1.0.0"), original_repo = NULL) {
  repo <- normalizePath(repo, mustWork = TRUE)
  pkgload::load_all(repo, quiet = TRUE)
  baseline <- normalizePath(file.path(repo, "inst", "benchmarks", "numerical-audit"), mustWork = FALSE)
  if (identical(normalizePath(output_dir, mustWork = FALSE), baseline)) stop("The archived audit baseline must not be overwritten.")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  trace_state <- new.env(parent = emptyenv())
  trace_state$calls <- list()
  assign(".xplus_core_audit_trace", trace_state, envir = .GlobalEnv)
  trace("cv.glmnet", where = asNamespace("glmnet"), print = FALSE, tracer = quote({
    state <- get(".xplus_core_audit_trace", envir = .GlobalEnv)
    state$calls[[length(state$calls) + 1L]] <- list(
      y = y, n = nrow(x), ids = rownames(x),
      metric = if (missing(type.measure)) "deviance (default)" else type.measure,
      folds = if (missing(foldid)) NA_integer_ else max(foldid)
    )
  }))
  on.exit({
    untrace("cv.glmnet", where = asNamespace("glmnet"))
    rm(".xplus_core_audit_trace", envir = .GlobalEnv)
  }, add = TRUE)
  rows <- list()
  checks <- list()
  auc_rank <- function(scores, truth) {
    n1 <- sum(truth == 1)
    n0 <- sum(truth == 0)
    if (!n1 || !n0 || any(!is.finite(scores))) return(NA_real_)
    (sum(rank(scores)[truth == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  }
  record_check <- function(name, passed, detail) {
    checks[[length(checks) + 1L]] <<- data.frame(check = name, passed = passed, detail = detail)
  }
  run_case <- function(name, x, y, args = list(), truth = NULL, fit_fun = xplus::xplus) {
    trace_state$calls <- list()
    warnings <- character()
    elapsed <- system.time({
      fit <- tryCatch(withCallingHandlers(
        do.call(fit_fun, c(list(x = x, y = y), args)),
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ), error = function(e) e)
    })[["elapsed"]]
    failed <- inherits(fit, "error")
    calls <- trace_state$calls
    soft_call <- if (length(calls) && is.matrix(tail(calls, 1)[[1]]$y)) tail(calls, 1)[[1]] else NULL
    finite <- if (failed) NA else all(is.finite(fit$pred_y)) && all(is.finite(as.numeric(stats::coef(fit))))
    pred <- if (failed) numeric() else as.numeric(fit$pred_y)
    row <- data.frame(
      case = name, seed = if (is.null(args$seed)) NA_integer_ else args$seed,
      n = nrow(x), p = ncol(x), known_positives = sum(y == 1),
      learning_rate = if (is.null(args$learning_rate)) 1 else args$learning_rate,
      status = if (failed) "error" else "fit", error = if (failed) conditionMessage(fit) else "",
      n_iter = if (failed) NA_integer_ else fit$n_iter,
      stop_reason = if (failed) "" else fit$stop_reason,
      cv_calls = length(calls), finite_predictions_and_coef = finite,
      distinct_predictions = if (failed) NA_integer_ else length(unique(pred)),
      untouched_unlabeled = if (failed) NA_integer_ else sum(fit$pseudo_labels[y == 0] == 0),
      positive_labels_preserved = if (failed) NA else all(fit$pseudo_labels[y == 1] == 1),
      final_target_discrepancy = if (failed || is.null(soft_call)) NA_real_ else max(abs(soft_call$y[, 2] - fit$y)),
      fallback_used = if (failed) NA else isTRUE(fit$fallback_used),
      first_fit_rows = if (!length(calls)) NA_integer_ else calls[[1]]$n,
      first_cv_metric = if (!length(calls)) "" else calls[[1]]$metric,
      final_cv_metric = if (!length(calls)) "" else tail(calls, 1)[[1]]$metric,
      cvm_finite = if (failed) NA else all(is.finite(fit$xplus$cvm)),
      auc_unlabeled_truth = if (failed || is.null(truth)) NA_real_ else auc_rank(pred[y == 0], truth[y == 0]),
      warnings = length(warnings), warning_messages = paste(unique(warnings), collapse = " | "),
      elapsed_seconds = elapsed, stringsAsFactors = FALSE
    )
    rows[[length(rows) + 1L]] <<- row
    cat(name, row$status, row$stop_reason, "iterations", row$n_iter, "\n")
    list(fit = fit, row = row, calls = calls)
  }
  set.seed(20260906)
  x <- matrix(rnorm(100 * 6), 100, dimnames = list(paste0("s", 1:100), paste0("x", 1:6)))
  y <- c(rep(1L, 20), rep(0L, 80))
  basic <- list(max_iter = 30, seed = 17)
  a <- run_case("finite_noise", x, y, basic)
  b <- run_case("finite_noise_repeat", x, y, basic)
  record_check("fixed_seed_predictions", identical(a$fit$pred_y, b$fit$pred_y), "Repeated identical inputs and seed")
  for (value in c(NA_real_, NaN, Inf, -Inf)) {
    bad <- x
    bad[1, 1] <- value
    out <- run_case(paste0("invalid_x_", as.character(value)), bad, y, basic)
    record_check(paste0("reject_x_", as.character(value)), out$row$status == "error" && out$row$cv_calls == 0, out$row$error)
  }
  run_case("constant_features", x * 0 + 1, y, basic)
  run_case("constant_column", cbind(x, constant = 1), y, basic)
  run_case("duplicate_columns", cbind(x, x[, 1:2]), y, basic)
  run_case("tiny_finite_features", x * 1e-200, y, basic)
  run_case("huge_finite_features", x * 1e200, y, basic)
  run_case("single_feature", x[, 1, drop = FALSE], y, basic)
  run_case("two_folds", x, y, c(basic, list(nfolds = 2)))
  run_case("fractional_folds", x, y, c(basic, list(nfolds = 3.5)))
  run_case("factor_labels", x, factor(y, levels = 0:1), basic)
  run_case("all_positive", x, rep(1, 100), basic)
  run_case("all_unlabeled", x, rep(0, 100), basic)
  for (k in c(1, 2, 3, 4, 5, 8, 10, 15, 30)) {
    run_case(paste0("positive_count_", k), x, as.integer(seq_len(100) <= k), basic)
  }
  for (k in c(1, 2, 3, 4, 5)) {
    run_case(paste0("unlabeled_count_", k), x[seq_len(20 + k), ], c(rep(1, 20), rep(0, k)), basic)
  }
  for (lr in c(0, 0.01, 0.05, 0.1, 0.5, 0.99, 1)) {
    run_case(paste0("learning_rate_", lr), x, y, c(basic, list(learning_rate = lr)))
  }
  for (q in c(0, 0.1, 0.5, 1)) {
    run_case(paste0("anchor_quantile_", q), x, y, c(basic, list(qq = q)))
  }
  for (threshold in c(0.9, 0.99, 1)) {
    run_case(paste0("stopping_threshold_", threshold), x, y, c(basic, list(convergence_threshold = threshold)))
  }
  run_case("budget_one", x, y, c(basic, list(sample_use_time = 1, convergence_threshold = 1)))
  for (scenario in c("null", "linear", "separable")) {
    for (seed in 1:10) {
      set.seed(1000 + seed)
      xx <- matrix(rnorm(200 * 8), 200, dimnames = list(paste0("s", 1:200), paste0("x", 1:8)))
      probability <- if (scenario == "null") rep(0.5, 200) else plogis(xx[, 1] * 2 - xx[, 2] * 1.5 + xx[, 3] * 0.7)
      truth <- if (scenario == "separable") as.integer(xx[, 1] > 0) else rbinom(200, 1, probability)
      known <- sample(which(truth == 1), 20)
      yy <- as.integer(seq_len(200) %in% known)
      for (lr in c(1, 0.5, 0.05)) {
        run_case(paste0("simulation_", scenario, "_", seed, "_", lr), xx, yy,
                 list(max_iter = 40, learning_rate = lr, seed = seed), truth)
      }
    }
  }
  set.seed(141)
  soft_x <- matrix(rnorm(120 * 5), 120)
  soft_y <- c(rep(1, 20), plogis(soft_x[21:120, 1] - 0.8))
  fractional <- glmnet::glmnet(soft_x, cbind(1 - soft_y, soft_y), family = "binomial",
                              lambda = 0.05, standardize = FALSE, control = list(thresh = 1e-10))
  augmented <- glmnet::glmnet(rbind(soft_x, soft_x), c(rep(0, 120), rep(1, 120)),
                             weights = c(1 - soft_y, soft_y), family = "binomial", lambda = 0.05,
                             standardize = FALSE, control = list(thresh = 1e-10))
  diff_coef <- max(abs(as.numeric(stats::coef(fractional)) - as.numeric(stats::coef(augmented))))
  record_check("soft_labels_equal_weighted_binary_augmentation", diff_coef < 1e-6,
               paste("max coefficient difference", format(diff_coef, digits = 12)))
  record_check("soft_label_predictions_finite", all(is.finite(stats::predict(fractional, soft_x, type = "response"))),
               "Fractional binomial targets with fixed positive labels")
  for (v in list(c(-0.4, 0, 0.2), c(0, 0, 0), c(1e-12, -1e-12), c(-1, 0, 1))) {
    z <- suppressWarnings(xplus:::normalize_residuals(v))
    record_check(paste0("normalize_", paste(v, collapse = "_")),
                 all(is.finite(z)) && all(abs(z) <= 1) && all(is.finite(plogis(10 * z))),
                 paste(z, collapse = ","))
  }
  sibling <- dirname(repo)
  set.seed(55)
  locations <- matrix(rnorm(80 * 12), 80)
  distance <- as.matrix(dist(locations))
  rownames(distance) <- colnames(distance) <- paste0("host", 1:80)
  interactions <- data.frame(parasite = "focal", host = rownames(distance)[1:12])
  for (package in c("phylopred", "phylopred-devel")) {
    path <- file.path(sibling, package, "R", "fit_phylopred_pu.R")
    if (!file.exists(path)) next
    env <- new.env(parent = .GlobalEnv)
    sys.source(path, env)
    wrapper <- local({
      fun <- env$fit_phylopred_pu
      function(x, y, ...) fun(interactions, distance, "focal", n_eigen = 5, ...)$fit
    })
    result <- run_case(paste0("integration_", package), locations, as.integer(seq_len(80) <= 12), basic, fit_fun = wrapper)
    record_check(paste0("integration_finite_", package), result$row$status == "fit" && isTRUE(result$row$finite_predictions_and_coef), result$row$error)
  }
  metrics <- new.env(parent = .GlobalEnv)
  sys.source(file.path(sibling, "phylopred-devel", "R", "host_threshold_metric.R"), metrics)
  sys.source(file.path(sibling, "phylopred-devel", "R", "phylo_partial_roc.R"), metrics)
  constant_roc <- metrics$phylo_partial_roc(rep(0.4, 100), y, error_rate = 0.95, n_boot = 10, seed = 1)
  record_check("constant_score_partial_ROC_is_explicit_NA", is.na(constant_roc$auc_ratio) && !is.nan(constant_roc$auc_ratio),
               "One distinct threshold cannot define a nonzero integration interval; not NaN predictions")
  if (!is.null(original_repo)) {
    original_env <- new.env(parent = asNamespace("glmnet"))
    sys.source(file.path(original_repo, "R", "PLUS.R"), original_env)
    for (seed in 1:3) {
      set.seed(seed)
      original <- tryCatch(suppressWarnings(original_env$PLUS(train_data = x, Label.obs = y)), error = function(e) e)
      record_check(paste0("original_PLUS_noise_seed_", seed),
                   !inherits(original, "error") && all(is.finite(original$pred.y)),
                   if (inherits(original, "error")) conditionMessage(original) else paste("range", paste(range(original$pred.y), collapse = ",")))
    }
  }
  results <- do.call(rbind, rows)
  check_results <- do.call(rbind, checks)
  write.csv(results, file.path(output_dir, "core_cases.csv"), row.names = FALSE)
  write.csv(check_results, file.path(output_dir, "core_checks.csv"), row.names = FALSE)
  writeLines(capture.output(sessionInfo()), file.path(output_dir, "session-info.txt"))
  write_audit_comparison(repo, output_dir)
  print(with(results, table(status, useNA = "ifany")))
  print(check_results, row.names = FALSE)
  invisible(list(cases = results, checks = check_results))
}

write_audit_comparison <- function(repo, output_dir) {
  current <- read.csv(file.path(output_dir, "core_cases.csv"))
  baseline_file <- file.path(repo, "inst", "benchmarks", "numerical-audit", "core_cases.csv")
  if (file.exists(baseline_file)) {
    previous <- read.csv(baseline_file)
    both <- merge(previous, current, by = "case", suffixes = c("_old", "_new"))
    write.csv(both, file.path(output_dir, "core_comparison.csv"), row.names = FALSE)
    simulation <- both[startsWith(both$case, "simulation_"), ]
    simulation$scenario <- sub("simulation_([^_]+).*", "\\1", simulation$case)
    summary <- aggregate(cbind(auc_unlabeled_truth_old, auc_unlabeled_truth_new, n_iter_old, n_iter_new) ~
                           scenario + learning_rate_new, simulation, mean)
    paired <- aggregate(case ~ scenario + learning_rate_new, simulation, length)
    names(paired)[3] <- "n_cases"
    summary <- merge(summary, paired, by = c("scenario", "learning_rate_new"))
    write.csv(summary, file.path(output_dir, "simulation_comparison.csv"), row.names = FALSE)
  }
  files <- c(list.files(file.path(repo, "R"), pattern = "\\.R$", full.names = TRUE),
             file.path(repo, c("DESCRIPTION", "NAMESPACE")))
  hashes <- tools::md5sum(files)
  write.csv(data.frame(file = sub(paste0(normalizePath(repo), "/"), "", normalizePath(files), fixed = TRUE),
                       md5 = unname(hashes)), file.path(output_dir, "source_fingerprints.csv"), row.names = FALSE)
  invisible(current)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  audit_core(output_dir = if (length(args)) args[1] else file.path("inst", "benchmarks", "v1.0.0"),
             original_repo = if (length(args) >= 2) args[2] else NULL)
}
