#!/usr/bin/env Rscript

default_path_specs <- function(
  max_iter = 30,
  nfolds = 4,
  enhanced_learning_rate = 0.5,
  enhanced_sample_use_budget = 60
) {
  list(
    current = list(
      label = "current",
      args = list(max_iter = max_iter, nfolds = nfolds)
    ),
    continuous_enhancement = list(
      label = "continuous_enhancement",
      args = list(
        max_iter = max_iter,
        nfolds = nfolds,
        learning_rate = enhanced_learning_rate,
        sample_use_time = enhanced_sample_use_budget
      )
    )
  )
}

default_benchmark_inputs <- function() {
  index <- seq_len(120)
  x <- cbind(x1 = sin(index), x2 = cos(index / 3),
             x3 = sin(index / 7), x4 = cos(index / 11))
  y <- as.integer(x[, 1] > 0.6 & index %% 2 == 0)
  list(x = x, y = y)
}

extract_support <- function(fit) {
  coefficients <- as.numeric(stats::coef(fit))[-1]
  which(coefficients != 0)
}

calculate_sparsity <- function(support_size, n_features) {
  if (n_features <= 0 || support_size < 0 || support_size > n_features) {
    return(NA_real_)
  }
  1 - (support_size / n_features)
}

encode_support <- function(support) {
  paste(support, collapse = ";")
}

decode_support <- function(value) {
  if (identical(value, "")) {
    return(integer(0))
  }
  as.integer(strsplit(value, ";", fixed = TRUE)[[1]])
}

support_stability_score <- function(support_sets) {
  n <- length(support_sets)
  if (n < 2) {
    return(1)
  }

  pair_indices <- utils::combn(n, 2)
  scores <- apply(pair_indices, 2, function(pair) {
    a <- support_sets[[pair[1]]]
    b <- support_sets[[pair[2]]]
    union <- union(a, b)
    if (length(union) == 0) {
      return(1)
    }
    length(intersect(a, b)) / length(union)
  })
  mean(scores)
}

run_single_path_seed <- function(x, y, path_name, seed, args) {
  fit_time <- system.time({
    fit <- do.call(
      xplus::xplus,
      c(
        list(x = x, y = y, seed = seed, verbose = FALSE),
        args
      )
    )
  })

  metrics <- xplus::assess(fit, newx = x, newy = y)
  support <- extract_support(fit)
  n_features <- ncol(x)
  support_size <- length(support)
  sparsity <- calculate_sparsity(support_size, n_features)

  data.frame(
    path = path_name,
    seed = seed,
    auc = as.numeric(metrics$auc),
    class_error = as.numeric(metrics$class),
    support_size = support_size,
    sparsity = sparsity,
    n_iter = fit$n_iter,
    runtime_seconds = unname(fit_time[["elapsed"]]),
    support = encode_support(support),
    stringsAsFactors = FALSE
  )
}

summarize_runs <- function(run_df) {
  paths <- unique(run_df$path)
  rows <- lapply(paths, function(path_name) {
    path_runs <- run_df[run_df$path == path_name, , drop = FALSE]
    support_sets <- lapply(path_runs$support, decode_support)

    data.frame(
      path = path_name,
      auc_mean = mean(path_runs$auc),
      auc_sd = stats::sd(path_runs$auc),
      class_error_mean = mean(path_runs$class_error),
      class_error_sd = stats::sd(path_runs$class_error),
      support_size_mean = mean(path_runs$support_size),
      support_size_sd = stats::sd(path_runs$support_size),
      sparsity_mean = mean(path_runs$sparsity),
      sparsity_sd = stats::sd(path_runs$sparsity),
      n_iter_mean = mean(path_runs$n_iter),
      n_iter_sd = stats::sd(path_runs$n_iter),
      runtime_seconds_mean = mean(path_runs$runtime_seconds),
      runtime_seconds_sd = stats::sd(path_runs$runtime_seconds),
      support_jaccard_mean = support_stability_score(support_sets),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

write_markdown_table <- function(summary_df, seeds, output_file) {
  digits <- 3

  lines <- c(
    "# xplus path comparison",
    "",
    paste0("Seeds: ", paste(seeds, collapse = ", ")),
    "",
    "| path | auc (mean+/-sd) | class error (mean+/-sd) | support size (mean+/-sd) | sparsity (mean+/-sd) | iterations (mean+/-sd) | runtime seconds (mean+/-sd) | support jaccard |",
    "| --- | --- | --- | --- | --- | --- | --- | --- |"
  )

  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, , drop = FALSE]
    fmt <- function(mean_col, sd_col) {
      paste0(
        format(round(row[[mean_col]], digits), nsmall = digits, trim = TRUE),
        " +/- ",
        format(round(row[[sd_col]], digits), nsmall = digits, trim = TRUE)
      )
    }
    lines <- c(
      lines,
      paste0(
        "| ", row$path,
        " | ", fmt("auc_mean", "auc_sd"),
        " | ", fmt("class_error_mean", "class_error_sd"),
        " | ", fmt("support_size_mean", "support_size_sd"),
        " | ", fmt("sparsity_mean", "sparsity_sd"),
        " | ", fmt("n_iter_mean", "n_iter_sd"),
        " | ", fmt("runtime_seconds_mean", "runtime_seconds_sd"),
        " | ", format(round(row[["support_jaccard_mean"]], digits), nsmall = digits, trim = TRUE),
        " |"
      )
    )
  }

  lines <- c(
    lines,
    "",
    "Stability under fixed seeds is summarized by AUC/class error standard deviations and support_jaccard_mean."
  )

  # useBytes=TRUE keeps the markdown table encoding stable for non-ASCII content (e.g. "+/-")
  # across locales and platforms.
  writeLines(lines, con = output_file, useBytes = TRUE)
}

run_xplus_path_benchmark <- function(
  x = NULL,
  y = NULL,
  seeds = c(11, 29, 47),
  path_specs = default_path_specs(),
  output_dir = NULL
) {
  if (is.null(x) || is.null(y)) {
    inputs <- default_benchmark_inputs()
    x <- inputs$x
    y <- inputs$y
  }

  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }

  run_rows <- lapply(names(path_specs), function(path_name) {
    spec <- path_specs[[path_name]]
    lapply(seeds, function(seed) {
      run_single_path_seed(x, y, spec$label, seed, spec$args)
    })
  })

  run_df <- do.call(rbind, unlist(run_rows, recursive = FALSE))
  summary_df <- summarize_runs(run_df)

  outputs <- list()
  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    run_file <- file.path(output_dir, "path_comparison_runs.csv")
    summary_file <- file.path(output_dir, "path_comparison_summary.csv")
    markdown_file <- file.path(output_dir, "path_comparison_table.md")

    utils::write.csv(run_df, run_file, row.names = FALSE)
    utils::write.csv(summary_df, summary_file, row.names = FALSE)
    write_markdown_table(summary_df, seeds = seeds, output_file = markdown_file)

    outputs <- list(
      runs_csv = run_file,
      summary_csv = summary_file,
      table_md = markdown_file
    )
  }

  list(runs = run_df, summary = summary_df, outputs = outputs)
}

main <- function() {
  output_dir <- file.path("inst", "benchmarks", "latest")
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) >= 1 && nzchar(args[1])) {
    output_dir <- args[1]
  }

  if (!requireNamespace("xplus", quietly = TRUE)) {
    stop("Package 'xplus' must be installed to run this script.", call. = FALSE)
  }

  result <- run_xplus_path_benchmark(output_dir = output_dir)

  cat("Benchmark complete.\n")
  cat("Rows:", nrow(result$runs), "\n")
  if (length(result$outputs) > 0) {
    cat("Outputs:\n")
    cat(" - ", result$outputs$runs_csv, "\n", sep = "")
    cat(" - ", result$outputs$summary_csv, "\n", sep = "")
    cat(" - ", result$outputs$table_md, "\n", sep = "")
  }
}

if (sys.nframe() == 0) {
  main()
}
