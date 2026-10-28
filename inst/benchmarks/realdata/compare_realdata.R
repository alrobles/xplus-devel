#!/usr/bin/env Rscript
# Benchmark xplus backends on open binary datasets converted to PU problems
# via pu_mask(). Datasets come from the UCI ML repository; each is cached
# under inst/benchmarks/realdata/data/.
#
# Usage: Rscript inst/benchmarks/realdata/compare_realdata.R [reps]

pkgload_available <- requireNamespace("pkgload", quietly = TRUE)
if (pkgload_available) pkgload::load_all(".", quiet = TRUE) else library(xplus)

reps <- as.integer(commandArgs(trailingOnly = TRUE)[1])
if (is.na(reps)) reps <- 2L
data_dir <- file.path("inst", "benchmarks", "realdata", "data")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

fetch <- function(url, dest) {
  if (!file.exists(dest)) download.file(url, dest, quiet = TRUE)
  dest
}

load_spambase <- function() {
  f <- fetch("https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data",
             file.path(data_dir, "spambase.data"))
  d <- utils::read.csv(f, header = FALSE)
  list(name = "spambase", x = as.matrix(d[, -ncol(d)]), z = d[[ncol(d)]])
}

load_mushroom <- function() {
  f <- fetch("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data",
             file.path(data_dir, "mushroom.data"))
  d <- utils::read.csv(f, header = FALSE, stringsAsFactors = TRUE)
  z <- as.integer(d[[1]] == "e")             # edible = positive class
  feats <- d[, -1]
  feats <- feats[vapply(feats, function(c) length(unique(c)) > 1, logical(1))]
  x <- stats::model.matrix(~ . - 1, data = feats)
  list(name = "mushroom", x = as.matrix(x), z = z)
}

backends <- c("glmnet", "ranger", "lightgbm", "xgboost")
rows <- list()
for (ds in list(load_spambase(), load_mushroom())) {
  for (rep in seq_len(reps)) {
    y <- pu_mask(ds$z, label_propensity = 0.4, seed = 100 + rep)
    lb <- xplus_grid(ds$x, y, backends, max_iter = 15, seed = 200 + rep,
                     nfolds = 5)
    lb$dataset <- ds$name
    lb$rep <- rep
    # held-out-truth AUC for reference: hidden z is known here
    rows[[length(rows) + 1L]] <- lb
    cat(sprintf("[%s rep %d] done\n", ds$name, rep))
  }
}
out <- do.call(rbind, rows)
print(out[order(out$dataset, -out$score),
          c("dataset", "learner", "score", "n_iter", "stop_reason", "seconds", "error")])
res <- file.path("inst", "benchmarks", "backends", "results")
dir.create(res, recursive = TRUE, showWarnings = FALSE)
f <- file.path(res, sprintf("realdata_%s.csv", format(Sys.time(), "%Y%m%d-%H%M%S")))
utils::write.csv(out, f, row.names = FALSE)
cat("Results written to", f, "\n")
