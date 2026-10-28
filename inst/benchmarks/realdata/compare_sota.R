# Held-out benchmark: xplus backends vs published PU SOTA
# =========================================================
#
# Protocol comparable to published leaderboards (PU ET, nnPU, uPU, TEDn):
#   * split the fully-labeled data into train/test stratified on z
#   * mask train positives with pu_mask(label_propensity = 0.5)
#   * fit xplus on the PU training data; evaluate on the test set with
#     TRUE labels -> accuracy / AUC / F1 comparable to published numbers
#
# Published SOTA (test accuracy, hidden labels):
#   mushroom : PU ET 99.70, Self-PU 99.25, nnPU 99.04, uPU 98.09
#   covtype  : PU ET 76.51, uPU 72.38, nnPU 71.64, Self-PU 54.67
#   spambase : TEDn 89.4, PvU* 89.2, nnPU 88.5, CVuO 88.1
#
# NOTE: published numbers come from supervised-held-out protocols on
# binarized versions of the same datasets; ours uses pu_mask(p=0.5) masking.

pkgload::load_all("/home/reumanlab/GitHub/xplus-devel", quiet = TRUE)

data_dir <- here::here("inst/benchmarks/realdata/data")
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

fetch <- function(url, dest) {
  if (!file.exists(dest)) {
    message("Downloading ", url)
    utils::download.file(url, dest, mode = "wb", quiet = TRUE)
  }
  dest
}

load_spambase <- function() {
  f <- fetch(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data",
    file.path(data_dir, "spambase.data")
  )
  d <- utils::read.csv(f, header = FALSE)
  list(name = "spambase", x = as.matrix(d[, -58]), z = as.integer(d[[58]]))
}

load_mushroom <- function() {
  f <- fetch(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data",
    file.path(data_dir, "mushroom.data")
  )
  d <- utils::read.csv(f, header = FALSE, stringsAsFactors = TRUE)
  z <- as.integer(d[[1]] == "e")
  feats <- d[, -1]
  feats <- feats[vapply(feats, function(c) length(unique(c)) > 1, logical(1))]
  x <- stats::model.matrix(~ . - 1, data = feats)
  list(name = "mushroom", x = as.matrix(x), z = z)
}

load_covtype <- function(n_sub = 40000) {
  f <- fetch(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz",
    file.path(data_dir, "covtype.data.gz")
  )
  d <- utils::read.csv(gzfile(f), header = FALSE)
  z <- as.integer(d[[ncol(d)]] == 2)
  set.seed(99)
  keep <- sample.int(nrow(d), n_sub)
  list(name = "covtype", x = as.matrix(d[keep, -ncol(d)]), z = z[keep])
}

split_pu <- function(x, z, train_frac = 0.7, seed) {
  set.seed(seed)
  pos <- which(z == 1); neg <- which(z == 0)
  tr <- c(sample(pos, floor(train_frac * length(pos))),
          sample(neg, floor(train_frac * length(neg))))
  te <- setdiff(seq_along(z), tr)
  list(xtr = x[tr, ], ztr = z[tr], xte = x[te, ], zte = z[te])
}

eval_fit <- function(fit, xte, zte) {
  p <- as.numeric(predict(fit, newx = xte))
  cls <- as.integer(p >= fit$cutoff)
  auc <- xplus::auc(zte, p)
  acc <- mean(cls == zte)
  tp <- sum(cls == 1 & zte == 1); fp <- sum(cls == 1 & zte == 0)
  fn <- sum(cls == 0 & zte == 1)
  f1 <- 2 * tp / max(1, 2 * tp + fp + fn)
  # oracle threshold: best achievable accuracy on this test set (upper bound;
  # diagnoses whether the gap is discrimination vs threshold calibration)
  ts <- sort(unique(c(0, p, 1)))
  acc_oracle <- max(vapply(ts, function(t) mean(as.integer(p >= t) == zte), numeric(1)))
  list(auc = auc, acc = acc, f1 = f1, acc_oracle = acc_oracle)
}

published <- list(
  mushroom = c("PU ET" = 0.997, "Self-PU" = 0.9925, "nnPU" = 0.9904, "uPU" = 0.9809),
  covtype  = c("PU ET" = 0.7651, "uPU" = 0.7238, "nnPU" = 0.7164, "Self-PU" = 0.5467),
  spambase = c("TEDn" = 0.894, "PvU*" = 0.892, "nnPU" = 0.885, "CVuO" = 0.881)
)

methods <- c("xplus", "elkan_noto", "naive")
backends <- c("glmnet", "ranger", "lightgbm", "xgboost")
nrep <- 3

fit_method <- function(method, xtr, y, bk, rep) {
  if (method == "xplus") {
    xplus(xtr, y, backend = bk, max_iter = 15, seed = 300 + rep, nfolds = 5)
  } else {
    get(paste0("xplus_", method))(xtr, y, backend = bk, seed = 300 + rep, nfolds = 5)
  }
}

out <- list()
for (ds in list(load_spambase(), load_mushroom(), load_covtype())) {
  for (rep in seq_len(nrep)) {
    sp <- split_pu(ds$x, ds$z, seed = 100 + rep)
    y <- pu_mask(sp$ztr, label_propensity = 0.5, seed = 200 + rep)
    for (method in methods) {
      for (bk in backends) {
        t0 <- proc.time()[3]
        fit <- tryCatch(
          fit_method(method, sp$xtr, y, bk, rep),
          error = function(e) e
        )
        secs <- proc.time()[3] - t0
        if (inherits(fit, "error")) {
          out[[length(out) + 1]] <- data.frame(dataset = ds$name, method = method,
                                               backend = bk, rep = rep,
                                               error = conditionMessage(fit))
          next
        }
        ev <- eval_fit(fit, sp$xte, sp$zte)
        out[[length(out) + 1]] <- data.frame(
          dataset = ds$name, method = method, backend = bk, rep = rep,
          auc = ev$auc, acc = ev$acc, f1 = ev$f1,
          acc_oracle = ev$acc_oracle, seconds = secs
        )
      }
    }
    cat(sprintf("[%s rep %d] done\n", ds$name, rep))
  }
}

res <- do.call(rbind, out)
res <- res[order(res$dataset, res$method, -res$acc), ]
print(res, row.names = FALSE)

agg <- aggregate(cbind(acc, auc, f1, acc_oracle) ~ dataset + method + backend, res, mean)
agg <- agg[order(agg$dataset, agg$method, -agg$acc), ]
cat("\n=== mean over reps ===\n")
print(agg, row.names = FALSE)

cat("\n=== vs published SOTA (test accuracy) ===\n")
for (nm in names(published)) {
  cat("--", nm, "-- published:",
      paste(sprintf("%s %.4f", names(published[[nm]]), published[[nm]]), collapse = ", "), "\n")
  ours <- agg[agg$dataset == nm, ]
  for (i in seq_len(nrow(ours))) {
    cat(sprintf("   %-11s-%-9s acc=%.4f auc=%.4f f1=%.4f oracle=%.4f\n",
                ours$method[i], ours$backend[i], ours$acc[i], ours$auc[i],
                ours$f1[i], ours$acc_oracle[i]))
  }
}

write.csv(res, file.path(data_dir, "sota_comparison.csv"), row.names = FALSE)
