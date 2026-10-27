#!/usr/bin/env Rscript
# Backend comparison benchmark for xplus.
#
# Fits xplus with each requested backend on synthetic PU datasets from
# simulate_pu_data() and scores probability predictions against the hidden
# ground-truth labels, both over all observations and restricted to the
# unlabeled pool (the cases whose labels were never observed).
#
# Usage:
#   Rscript inst/benchmarks/backends/compare_backends.R [outdir] [reps] [quick]
#
#   outdir  Directory for the results CSV (default: this directory).
#   reps    Replicates per backend x boundary cell (default 10).
#   quick   Any value: single replicate, small data, few iterations.

args <- commandArgs(trailingOnly = TRUE)
script_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
pkg_root <- normalizePath(file.path(script_dir, "..", "..", ".."))

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(pkg_root, quiet = TRUE)
} else {
  library(xplus)
}

outdir <- if (length(args) >= 1) args[1] else file.path(script_dir, "results")
reps <- if (length(args) >= 2) as.integer(args[2]) else 10L
quick <- length(args) >= 3
if (quick) {
  reps <- 2L
  message("Quick mode: 2 replicates on small data.")
}
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

n_obs <- if (quick) 400 else 1000
n_feat <- if (quick) 8 else 12
max_iter <- if (quick) 30 else 100

backends <- c("glmnet", "ranger", "gam")
boundaries <- c("linear", "interaction", "nonlinear")

f1_at_cutoff <- function(z, prob, cutoff) {
  pred <- as.integer(prob > cutoff)
  tp <- sum(pred == 1 & z == 1)
  fp <- sum(pred == 1 & z == 0)
  fn <- sum(pred == 0 & z == 1)
  if (tp + fp + fn == 0) return(NA_real_)
  2 * tp / (2 * tp + fp + fn)
}

rows <- list()
i <- 0L
for (boundary in boundaries) {
  for (rep in seq_len(reps)) {
    sim <- simulate_pu_data(
      n = n_obs, p = n_feat, boundary = boundary,
      prevalence = 0.4, label_propensity = 0.5,
      seed = 1000 * rep + match(boundary, boundaries)
    )
    unl <- sim$y == 0
    for (backend in backends) {
      i <- i + 1L
      t0 <- proc.time()["elapsed"]
      fit <- tryCatch(
        xplus(sim$x, sim$y, backend = backend, max_iter = max_iter,
              seed = 77 * rep, learning_rate = 1),
        error = function(e) e
      )
      elapsed <- proc.time()["elapsed"] - t0
      if (inherits(fit, "error")) {
        warning(sprintf("[%s/%s rep %d] failed: %s", backend, boundary, rep,
                        conditionMessage(fit)))
        rows[[i]] <- data.frame(backend = backend, boundary = boundary, rep = rep,
                                auc_all = NA, auc_unlabeled = NA, f1_unlabeled = NA,
                                n_iter = NA, stop_reason = "error", seconds = elapsed,
                                error = conditionMessage(fit))
        next
      }
      prob <- as.numeric(predict(fit))
      rows[[i]] <- data.frame(
        backend = backend, boundary = boundary, rep = rep,
        auc_all = auc(sim$z, prob),
        auc_unlabeled = auc(sim$z[unl], prob[unl]),
        f1_unlabeled = f1_at_cutoff(sim$z[unl], prob[unl], fit$cutoff),
        n_iter = fit$n_iter,
        stop_reason = fit$stop_reason,
        seconds = elapsed,
        error = NA_character_
      )
      message(sprintf("[%d] %-6s x %-12s rep %2d  AUC=%.3f  AUC_unl=%.3f  F1_unl=%.3f  iters=%d  %ss",
                      i, backend, boundary, rep, rows[[i]]$auc_all,
                      rows[[i]]$auc_unlabeled, rows[[i]]$f1_unlabeled,
                      fit$n_iter, round(elapsed, 1)))
    }
  }
}

results <- do.call(rbind, rows)
stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
outfile <- file.path(outdir, sprintf("backends_%s.csv", stamp))
write.csv(results, outfile, row.names = FALSE)
message("Results written to ", outfile)

summary_stats <- aggregate(
  cbind(auc_all, auc_unlabeled, f1_unlabeled, n_iter, seconds) ~ backend + boundary,
  data = results,
  FUN = function(v) c(mean = mean(v, na.rm = TRUE), sd = sd(v, na.rm = TRUE))
)
print(summary_stats)
message("Convergence rates:")
print(with(results, table(backend, boundary, stop_reason)))
