#' Specify a fitting backend for xplus
#'
#' Describe a learner that [xplus()] uses for both the iterative pseudo-labeling
#' fits and the final model. Built-in backends are [xplus_backend_glmnet()],
#' [xplus_backend_ranger()], [xplus_backend_gam()], [xplus_backend_xgboost()],
#' and [xplus_backend_lightgbm()]; this constructor defines
#' custom learners with the same contract.
#'
#' @param name Nonempty character scalar identifying the backend.
#' @param fit Function with signature `fit(x, y, foldid, weights)` returning a
#'   fitted model object. `x` is the finite numeric feature matrix, `y` a
#'   numeric target in `[0, 1]` (binary during iterations, possibly fractional
#'   in the final fit), `foldid` the stratified cross-validation fold
#'   assignment, and `weights` nonnegative case weights encoding bootstrap
#'   multiplicities. To report a cross-validation measure in the iteration
#'   history, attach `attr(model, "xplus_cv_measure")` with `"deviance"` or
#'   `"auc"`; otherwise `"none"` is recorded.
#' @param predict Function with signature `predict(model, newx)` returning
#'   finite probabilities in `[0, 1]`, one per row of `newx`.
#' @param importance Optional function with signature `importance(model)`
#'   returning a named numeric vector of feature scores, or `NULL`.
#' @param label Human-readable backend label; defaults to `name`.
#' @param uses_cv Logical scalar; whether `fit` performs internal
#'   cross-validation whose measure is recorded in the iteration history.
#' @param parameters Optional data frame declaring tunable hyperparameters
#'   following the caret `modelInfo` convention (columns `parameter`,
#'   `class`, and `label`); informational only, it enables future tuning
#'   and AutoML adapters without changing the backend contract.
#' @param fit_predict Optional function with signature `fit_predict(model, x)`
#'   returning honest probabilities for the rows passed to `fit`, in the same
#'   order — out-of-bag or held-out-fold estimates rather than in-sample
#'   memorization. `xplus()` uses them after the final fit for cutoff
#'   calibration and the stored training predictions; non-finite entries
#'   fall back to `predict` on those rows. When `NULL`, `predict` is reused
#'   on training rows, appropriate for learners that do not memorize
#'   per-observation targets. Iterative pseudo-label fits always use
#'   `predict`, which keeps self-training dynamics uniform across backends.
#' @param supports_fractional Logical scalar; whether `fit` handles fractional
#'   targets in `[0, 1]` (the final pseudo-label fit). When `FALSE`, `xplus()`
#'   fails with a clear error if the final targets are non-binary instead of
#'   surfacing an engine-specific error deep in the learner.
#' @param supports_weights Logical scalar; whether `fit` honors case weights.
#'   When `FALSE`, `xplus()` warns once that `sampling = "bootstrap"`
#'   multiplicities will be silently ignored by this learner.
#' @details The contract mirrors the minimal estimator interface shared by
#'   caret `modelInfo` entries, sklearn `fit`/`predict_proba` estimators
#'   (the basis of PU wrappers such as `ElkanotoPuClassifier`), and mlr3
#'   `Learner` objects: fit weighted `(x, y)` data, then predict
#'   probabilities. `xplus` additionally supplies `foldid` (stratified CV
#'   folds for backends that use them) and soft fractional targets in the
#'   final fit, which generic frameworks do not provide.
#'
#' @return An object of class `"xplus_backend"` accepted by the `backend`
#'   argument of [xplus()].
#' @seealso [xplus()], [xplus_backend_ranger()], [xplus_backend_gam()]
#' @examples
#' constant <- xplus_backend(
#'   name = "constant",
#'   fit = function(x, y, foldid, weights) stats::weighted.mean(y, weights),
#'   predict = function(model, newx) rep(model, nrow(newx))
#' )
#' fit <- xplus(matrix(rnorm(100 * 5), ncol = 5), c(rep(1, 20), rep(0, 80)),
#'   backend = constant, max_iter = 3)
#' @export
xplus_backend <- function(name, fit, predict, importance = NULL, label = name,
                          uses_cv = FALSE, parameters = NULL, fit_predict = NULL,
                          supports_fractional = TRUE, supports_weights = TRUE) {
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(trimws(name))) {
    stop("`name` must be a nonempty character scalar.", call. = FALSE)
  }
  if (!is.function(fit) || length(formals(fit)) < 4L) {
    stop("`fit` must be a function with signature `fit(x, y, foldid, weights)`.", call. = FALSE)
  }
  if (!is.function(predict) || length(formals(predict)) < 2L) {
    stop("`predict` must be a function with signature `predict(model, newx)`.", call. = FALSE)
  }
  if (!is.null(importance) && !is.function(importance)) {
    stop("`importance` must be NULL or a function with signature `importance(model)`.", call. = FALSE)
  }
  if (!is.character(label) || length(label) != 1L || is.na(label) || !nzchar(trimws(label))) {
    stop("`label` must be a nonempty character scalar.", call. = FALSE)
  }
  if (!is.logical(uses_cv) || length(uses_cv) != 1L || is.na(uses_cv)) {
    stop("`uses_cv` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(fit_predict) &&
      (!is.function(fit_predict) || length(formals(fit_predict)) < 2L)) {
    stop("`fit_predict` must be NULL or a function with signature `fit_predict(model, x)`.",
         call. = FALSE)
  }
  if (!is.null(parameters)) {
    if (!is.data.frame(parameters) || !all(c("parameter", "class", "label") %in% names(parameters)) ||
        any(!nzchar(parameters$parameter))) {
      stop("`parameters` must be a data frame with nonempty `parameter`, `class`, and `label` columns.",
           call. = FALSE)
    }
  }
  if (!is.logical(supports_fractional) || length(supports_fractional) != 1L ||
      is.na(supports_fractional)) {
    stop("`supports_fractional` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(supports_weights) || length(supports_weights) != 1L ||
      is.na(supports_weights)) {
    stop("`supports_weights` must be TRUE or FALSE.", call. = FALSE)
  }
  structure(
    list(name = name, label = label, fit = fit, predict = predict,
         importance = importance, uses_cv = uses_cv,
         parameters = parameters, fit_predict = fit_predict,
         supports_fractional = supports_fractional,
         supports_weights = supports_weights),
    class = "xplus_backend"
  )
}

#' Print method for xplus_backend objects
#'
#' @param x An `xplus_backend` object.
#' @param ... Additional arguments.
#'
#' @return The input object `x`, returned invisibly.
#' @method print xplus_backend
#' @export
print.xplus_backend <- function(x, ...) {
  cat("xplus backend:", x$label, "\n")
  cat("Name:", x$name, "\n")
  cat("Internal cross-validation:", x$uses_cv, "\n")
  cat("Feature importance:", !is.null(x$importance), "\n")
  cat("Honest training predictions:", !is.null(x$fit_predict), "\n")
  cat("Fractional targets:", isTRUE(x$supports_fractional), "\n")
  cat("Case weights:", isTRUE(x$supports_weights), "\n")
  if (!is.null(x$parameters) && nrow(x$parameters)) {
    cat("Tunable parameters:", paste(x$parameters$parameter, collapse = ", "), "\n")
  }
  invisible(x)
}

#' Elastic-net logistic backend for xplus
#'
#' The default backend: penalized binomial regression with internal
#' cross-validation via [glmnet::cv.glmnet()], reproducing the historical
#' `xplus` behavior.
#'
#' @param alpha Elastic-net mixing parameter in `[0, 1]`, the same as the
#'   `alpha` argument of [xplus()].
#' @param cv_measure Cross-validation criterion (`"deviance"` or `"auc"`).
#'
#' @return An `"xplus_backend"` object.
#' @seealso [xplus_backend()], [xplus()]
#' @examples
#' backend <- xplus_backend_glmnet(alpha = 0.5)
#' @export
xplus_backend_glmnet <- function(alpha = 1, cv_measure = c("deviance", "auc")) {
  .xplus_scalar(alpha, "alpha", 0, 1)
  cv_measure <- match.arg(cv_measure)
  xplus_backend(
    name = "glmnet",
    label = "elastic-net logistic regression (cv.glmnet)",
    uses_cv = TRUE,
    parameters = data.frame(
      parameter = "alpha", class = "numeric",
      label = "Elastic-net mixture", stringsAsFactors = FALSE
    ),
    fit = function(x, y, foldid, weights) {
      step <- .xplus_cv_fit(x, y, foldid, alpha, cv_measure, weights)
      attr(step$fit, "xplus_cv_measure") <- step$measure
      step$fit
    },
    predict = function(model, newx) {
      as.numeric(stats::predict(model, newx = newx, s = "lambda.min", type = "response"))
    },
    importance = function(model) {
      beta <- as.numeric(stats::coef(model, s = "lambda.min"))[-1]
      stats::setNames(beta, rownames(stats::coef(model, s = "lambda.min"))[-1])
    }
  )
}

#' Ranger random-forest backend for xplus
#'
#' Random forests via [ranger::ranger()]. Binary targets use a probability
#' forest and fractional pseudo-labels a regression forest, so soft final-fit
#' targets are supported natively. The backend performs no internal
#' cross-validation; its hyperparameters are fixed at construction.
#' Out-of-bag predictions supply `fit_predict`, so the stored predictions and
#' classification cutoff use honest scores rather than in-sample memorization.
#'
#' @param trees Number of trees (ranger `num.trees`); the parsnip-harmonized
#'   name is used so a parsnip bridge can map `rand_forest(trees = ...)` later.
#' @param mtry Number of variables sampled per split; `NULL` uses the ranger
#'   default.
#' @param min_n Minimum node size (ranger `min.node.size`).
#' @param max.depth,num.threads Engine-specific ranger controls forwarded
#'   unchanged.
#' @param seed Optional seed passed to each ranger fit. When `NULL`, one is
#'   drawn from R's RNG per fit so that `xplus(seed = ...)` remains
#'   reproducible.
#' @param importance.mode Variable-importance mode (`"impurity"`,
#'   `"permutation"`, or `"none"`).
#'
#' @return An `"xplus_backend"` object.
#' @seealso [xplus_backend()], [xplus()]
#' @examples
#' \dontrun{
#' fit <- xplus(x, y, backend = xplus_backend_ranger(trees = 1000))
#' }
#' @export
xplus_backend_ranger <- function(trees = 500, mtry = NULL, min_n = 1,
                                 max.depth = 0, num.threads = 1, seed = NULL,
                                 importance.mode = c("impurity", "permutation", "none")) {
  .xplus_scalar(trees, "trees", 1, .Machine$integer.max, integer = TRUE)
  if (!is.null(mtry)) .xplus_scalar(mtry, "mtry", 1, .Machine$integer.max, integer = TRUE)
  .xplus_scalar(min_n, "min_n", 0, .Machine$integer.max, integer = TRUE)
  .xplus_scalar(max.depth, "max.depth", 0, .Machine$integer.max, integer = TRUE)
  if (!is.null(num.threads)) .xplus_scalar(num.threads, "num.threads", 1, .Machine$integer.max, integer = TRUE)
  if (!is.null(seed)) .xplus_scalar(seed, "seed", 0, .Machine$integer.max, integer = TRUE)
  importance.mode <- match.arg(importance.mode)
  xplus_backend(
    name = "ranger",
    label = "random forest (ranger)",
    uses_cv = FALSE,
    parameters = data.frame(
      parameter = c("trees", "mtry", "min_n"),
      class = "integer",
      label = c("Number of trees", "Randomly selected predictors",
                "Minimal node size"),
      stringsAsFactors = FALSE
    ),
    fit = function(x, y, foldid, weights) {
      if (!requireNamespace("ranger", quietly = TRUE)) {
        stop("The 'ranger' backend requires the ranger package to be installed.", call. = FALSE)
      }
      frame <- .xplus_backend_frame(x)
      features <- colnames(x)
      rf_seed <- if (is.null(seed)) sample.int(.Machine$integer.max, 1L) else seed
      args <- list(x = frame, num.trees = trees, mtry = mtry,
                   min.node.size = min_n, max.depth = max.depth,
                   case.weights = weights, num.threads = num.threads,
                   seed = rf_seed, importance = importance.mode,
                   keep.inbag = FALSE, verbose = FALSE)
      if (all(y %in% c(0, 1))) {
        args$y <- factor(y, levels = c(0, 1))
        args$probability <- TRUE
        mode <- "probability"
      } else {
        args$y <- y
        mode <- "regression"
      }
      model <- do.call(ranger::ranger, args)
      structure(list(model = model, mode = mode, features = features),
                class = "xplus_ranger_fit")
    },
    predict = function(model, newx) {
      frame <- .xplus_backend_frame(newx)
      prob <- stats::predict(model$model, data = frame, verbose = FALSE)$predictions
      if (model$mode == "probability") prob <- prob[, "1"]
      pmin(pmax(as.numeric(prob), 0), 1)
    },
    fit_predict = function(model, x) {
      # ranger stores out-of-bag predictions on the training rows in
      # `predictions`; rows never out-of-bag get NaN and fall back to
      # in-sample predictions upstream.
      prob <- model$model$predictions
      if (model$mode == "probability") prob <- prob[, "1"]
      as.numeric(prob)
    },
    importance = function(model) {
      imp <- model$model$variable.importance
      if (!length(imp)) return(NULL)
      if (!is.null(model$features) && length(model$features) == length(imp)) {
        names(imp) <- model$features
      }
      imp
    }
  )
}

#' XGBoost gradient-boosting backend for xplus
#'
#' Gradient-boosted trees via [xgboost::xgb.train()]. Binary targets use
#' `binary:logistic`, fractional pseudo-labels `reg:squarederror` (clipped to
#' `[0, 1]` on prediction). Hyperparameters use parsnip-harmonized names so a
#' `boost_tree()` spec can map onto them later. Boosting memorizes training
#' targets like any high-capacity learner, so `fit_predict` returns
#' out-of-fold predictions computed with `xgb.cv` on the folds `xplus()`
#' already assigned — the same honest-score role ranger's OOB plays, at the
#' cost of one extra cross-validation pass per final fit.
#'
#' @param trees Number of boosting rounds (xgboost `nrounds`).
#' @param learn_rate Shrinkage step (xgboost `eta`).
#' @param tree_depth Maximum tree depth (xgboost `max_depth`).
#' @param min_n Minimum child weight (xgboost `min_child_weight`).
#' @param mtry Number of features sampled per split; `NULL` uses all features.
#'   Converted to `colsample_bynode` as a proportion of `ncol(x)`.
#' @param sample_size Row subsampling fraction per tree (xgboost `subsample`).
#' @param loss_reduction Minimum loss reduction to split (xgboost `gamma`).
#' @param num.threads OpenMP threads (xgboost `nthread`).
#' @param seed Optional seed passed to each fit; `NULL` draws from R's RNG per
#'   fit so that `xplus(seed = ...)` remains reproducible.
#'
#' @return An `"xplus_backend"` object.
#' @seealso [xplus_backend()], [xplus()], [xplus_backend_ranger()]
#' @examples
#' \dontrun{
#' fit <- xplus(x, y, backend = xplus_backend_xgboost(trees = 200))
#' }
#' @export
xplus_backend_xgboost <- function(trees = 100, learn_rate = 0.3, tree_depth = 6,
                                  min_n = 1, mtry = NULL, sample_size = 1,
                                  loss_reduction = 0, num.threads = 1,
                                  seed = NULL) {
  .xplus_scalar(trees, "trees", 1, .Machine$integer.max, integer = TRUE)
  .xplus_scalar(learn_rate, "learn_rate", 0, 1)
  .xplus_scalar(tree_depth, "tree_depth", 1, .Machine$integer.max, integer = TRUE)
  .xplus_scalar(min_n, "min_n", 0, Inf)
  if (!is.null(mtry)) .xplus_scalar(mtry, "mtry", 1, .Machine$integer.max, integer = TRUE)
  .xplus_scalar(sample_size, "sample_size", 0, 1, lower_open = TRUE)
  .xplus_scalar(loss_reduction, "loss_reduction", 0, Inf)
  if (!is.null(num.threads)) .xplus_scalar(num.threads, "num.threads", 1, .Machine$integer.max, integer = TRUE)
  if (!is.null(seed)) .xplus_scalar(seed, "seed", 0, .Machine$integer.max, integer = TRUE)
  xplus_backend(
    name = "xgboost",
    label = "gradient-boosted trees (xgboost)",
    uses_cv = FALSE,
    parameters = data.frame(
      parameter = c("trees", "learn_rate", "tree_depth", "min_n", "mtry",
                    "sample_size", "loss_reduction"),
      class = c("integer", "numeric", "integer", "numeric", "integer",
                "numeric", "numeric"),
      label = c("Number of trees", "Learning rate", "Tree depth",
                "Minimal node size", "Randomly selected predictors",
                "Proportion of rows sampled", "Minimum loss reduction"),
      stringsAsFactors = FALSE
    ),
    fit = function(x, y, foldid, weights) {
      if (!requireNamespace("xgboost", quietly = TRUE)) {
        stop("The 'xgboost' backend requires the xgboost package to be installed.", call. = FALSE)
      }
      features <- colnames(x)
      if (is.null(features)) features <- paste0("V", seq_len(ncol(x)))
      binary <- all(y %in% c(0, 1))
      colnames(x) <- features
      params <- list(
        objective = if (binary) "binary:logistic" else "reg:squarederror",
        max_depth = tree_depth, eta = learn_rate, gamma = loss_reduction,
        min_child_weight = min_n, subsample = sample_size,
        colsample_bynode = if (is.null(mtry)) 1 else min(1, mtry / ncol(x)),
        nthread = if (is.null(num.threads)) 1 else num.threads,
        seed = if (is.null(seed)) sample.int(.Machine$integer.max, 1L) else seed
      )
      dtrain <- xgboost::xgb.DMatrix(x, label = y, weight = weights)
      booster <- xgboost::xgb.train(params, dtrain, nrounds = trees, verbose = 0)
      structure(list(model = booster, binary = binary, features = features,
                     foldid = foldid, weights = weights, y = y,
                     params = params, trees = trees),
                class = "xplus_xgb_fit")
    },
    predict = function(model, newx) {
      colnames(newx) <- model$features
      prob <- stats::predict(model$model, xgboost::xgb.DMatrix(newx))
      pmin(pmax(as.numeric(prob), 0), 1)
    },
    fit_predict = function(model, x) {
      # Out-of-fold predictions on the xplus CV folds: each row is scored by
      # the booster that never saw it, the honest analogue of ranger's OOB.
      colnames(x) <- model$features
      dtrain <- xgboost::xgb.DMatrix(x, label = model$y, weight = model$weights)
      folds <- lapply(sort(unique(model$foldid)), function(k) which(model$foldid == k))
      cv <- xgboost::xgb.cv(model$params, dtrain, nrounds = model$trees,
                            folds = folds, prediction = TRUE, verbose = 0)
      prob <- if (!is.null(cv$cv_predict)) cv$cv_predict$pred else cv$pred
      pmin(pmax(as.numeric(prob), 0), 1)
    },
    importance = function(model) {
      imp <- xgboost::xgb.importance(feature_names = model$features,
                                   model = model$model)
      scores <- imp$Gain
      full <- stats::setNames(rep(0, length(model$features)), model$features)
      full[match(imp$Feature, model$features)] <- scores
      full
    }
  )
}

#' LightGBM gradient-boosting backend for xplus
#'
#' Gradient-boosted trees via [lightgbm::lgb.train()]. Binary targets use the
#' `"binary"` objective, fractional pseudo-labels `"regression"` (clipped to
#' `[0, 1]` on prediction). Hyperparameters use parsnip-harmonized names.
#' `fit_predict` returns out-of-fold predictions via `lgb.cv` on the folds
#' `xplus()` assigns, so the classification cutoff is calibrated on honest
#' scores rather than memorized ones.
#'
#' @param trees Number of boosting rounds (lightgbm `num_iterations`).
#' @param learn_rate Shrinkage step (lightgbm `learning_rate`).
#' @param tree_depth Maximum tree depth (lightgbm `max_depth`; `-1` unbounded).
#' @param min_n Minimum observations per leaf (lightgbm `min_data_in_leaf`).
#' @param mtry Number of features sampled per iteration; `NULL` uses all.
#'   Converted to `feature_fraction` as a proportion of `ncol(x)`.
#' @param sample_size Row subsampling fraction per iteration (lightgbm
#'   `bagging_fraction`; enables bagging when below 1).
#' @param loss_reduction Minimum gain to split (lightgbm `min_gain_to_split`).
#' @param num.threads OpenMP threads (lightgbm `num_threads`).
#' @param seed Optional seed passed to each fit; `NULL` draws from R's RNG per
#'   fit so that `xplus(seed = ...)` remains reproducible.
#'
#' @return An `"xplus_backend"` object.
#' @seealso [xplus_backend()], [xplus()], [xplus_backend_xgboost()]
#' @examples
#' \dontrun{
#' fit <- xplus(x, y, backend = xplus_backend_lightgbm(trees = 200))
#' }
#' @export
xplus_backend_lightgbm <- function(trees = 100, learn_rate = 0.1, tree_depth = -1,
                                   min_n = 20, mtry = NULL, sample_size = 1,
                                   loss_reduction = 0, num.threads = 1,
                                   seed = NULL) {
  .xplus_scalar(trees, "trees", 1, .Machine$integer.max, integer = TRUE)
  .xplus_scalar(learn_rate, "learn_rate", 0, 1)
  .xplus_scalar(tree_depth, "tree_depth", -1, .Machine$integer.max, integer = TRUE)
  .xplus_scalar(min_n, "min_n", 1, .Machine$integer.max, integer = TRUE)
  if (!is.null(mtry)) .xplus_scalar(mtry, "mtry", 1, .Machine$integer.max, integer = TRUE)
  .xplus_scalar(sample_size, "sample_size", 0, 1, lower_open = TRUE)
  .xplus_scalar(loss_reduction, "loss_reduction", 0, Inf)
  if (!is.null(num.threads)) .xplus_scalar(num.threads, "num.threads", 1, .Machine$integer.max, integer = TRUE)
  if (!is.null(seed)) .xplus_scalar(seed, "seed", 0, .Machine$integer.max, integer = TRUE)
  xplus_backend(
    name = "lightgbm",
    label = "gradient-boosted trees (lightgbm)",
    uses_cv = FALSE,
    parameters = data.frame(
      parameter = c("trees", "learn_rate", "tree_depth", "min_n", "mtry",
                    "sample_size", "loss_reduction"),
      class = c("integer", "numeric", "integer", "integer", "integer",
                "numeric", "numeric"),
      label = c("Number of trees", "Learning rate", "Tree depth",
                "Minimal data in leaf", "Randomly selected predictors",
                "Proportion of rows sampled", "Minimum gain to split"),
      stringsAsFactors = FALSE
    ),
    fit = function(x, y, foldid, weights) {
      if (!requireNamespace("lightgbm", quietly = TRUE)) {
        stop("The 'lightgbm' backend requires the lightgbm package to be installed.", call. = FALSE)
      }
      features <- colnames(x)
      if (is.null(features)) features <- paste0("V", seq_len(ncol(x)))
      binary <- all(y %in% c(0, 1))
      colnames(x) <- features
      params <- list(
        objective = if (binary) "binary" else "regression",
        num_iterations = trees, learning_rate = learn_rate,
        max_depth = tree_depth, min_data_in_leaf = min_n,
        feature_fraction = if (is.null(mtry)) 1 else min(1, mtry / ncol(x)),
        bagging_fraction = sample_size,
        bagging_freq = if (sample_size < 1) 1L else 0L,
        min_gain_to_split = loss_reduction,
        num_threads = if (is.null(num.threads)) 1 else num.threads,
        seed = if (is.null(seed)) sample.int(.Machine$integer.max, 1L) else seed,
        verbose = -1
      )
      dtrain <- lightgbm::lgb.Dataset(x, label = y, weight = weights)
      booster <- lightgbm::lgb.train(params, dtrain, verbose = -1)
      structure(list(model = booster, binary = binary, features = features,
                     foldid = foldid, weights = weights, y = y,
                     params = params, trees = trees),
                class = "xplus_lgb_fit")
    },
    predict = function(model, newx) {
      colnames(newx) <- model$features
      prob <- stats::predict(model$model, newx)
      pmin(pmax(as.numeric(prob), 0), 1)
    },
    fit_predict = function(model, x) {
      # lgb.cv has no stored out-of-fold predictions; score each row with the
      # booster of the fold that held it out.
      colnames(x) <- model$features
      dtrain <- lightgbm::lgb.Dataset(x, label = model$y, weight = model$weights)
      folds <- lapply(sort(unique(model$foldid)), function(k) which(model$foldid == k))
      cv <- lightgbm::lgb.cv(model$params, dtrain, folds = folds,
                             verbose = -1, showsd = FALSE, stratified = FALSE)
      oof <- numeric(nrow(x))
      for (k in seq_along(folds)) {
        oof[folds[[k]]] <- stats::predict(cv$boosters[[k]]$booster,
                                         x[folds[[k]], , drop = FALSE])
      }
      pmin(pmax(as.numeric(oof), 0), 1)
    },
    importance = function(model) {
      imp <- lightgbm::lgb.importance(model$model, percentage = TRUE)
      full <- stats::setNames(rep(0, length(model$features)), model$features)
      full[match(imp$Feature, model$features)] <- imp$Gain
      full
    }
  )
}

#' GAM logistic backend for xplus
#'
#' Additive logistic models via [mgcv::gam()] with a smoothing spline per
#' feature. Fractional pseudo-labels are fitted with a quasibinomial family,
#' binary targets with the binomial family.
#'
#' @param k Basis dimension passed to each [mgcv::s()] term; `NULL` uses the
#'   mgcv default.
#' @param method Smoothing-parameter estimation method for binary targets
#'   (fractional targets always use `"GCV.Cp"`, required by the quasibinomial
#'   family).
#' @param max_features Maximum number of features accepted; GAM cost grows
#'   quickly with dimension.
#'
#' @return An `"xplus_backend"` object.
#' @seealso [xplus_backend()], [xplus()]
#' @examples
#' \dontrun{
#' fit <- xplus(x, y, backend = xplus_backend_gam())
#' }
#' @export
xplus_backend_gam <- function(k = NULL, method = "REML", max_features = 100) {
  if (!is.null(k)) .xplus_scalar(k, "k", 3, .Machine$integer.max, integer = TRUE)
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    stop("`method` must be a nonempty character scalar.", call. = FALSE)
  }
  .xplus_scalar(max_features, "max_features", 1, .Machine$integer.max, integer = TRUE)
  xplus_backend(
    name = "gam",
    label = "generalized additive model (mgcv::gam)",
    uses_cv = FALSE,
    parameters = data.frame(
      parameter = "k", class = "integer",
      label = "Smooth basis dimension", stringsAsFactors = FALSE
    ),
    fit = function(x, y, foldid, weights) {
      if (!requireNamespace("mgcv", quietly = TRUE)) {
        stop("The 'gam' backend requires the mgcv package to be installed.", call. = FALSE)
      }
      p <- ncol(x)
      if (p > max_features) {
        stop(sprintf("The 'gam' backend accepts at most %d features; got %d.", max_features, p),
             call. = FALSE)
      }
      frame <- .xplus_backend_frame(x)
      smooth <- function(v) if (is.null(k)) sprintf("s(%s)", v) else sprintf("s(%s, k = %d)", v, k)
      formula <- stats::as.formula(
        paste("y ~", paste(vapply(names(frame), smooth, character(1)), collapse = " + "))
      )
      binary <- all(y %in% c(0, 1))
      data <- cbind(frame, y = y)
      model <- mgcv::gam(formula, data = data, weights = weights,
                         family = if (binary) stats::binomial() else stats::quasibinomial(),
                         method = if (binary) method else "GCV.Cp")
      structure(list(model = model, features = colnames(x)),
                class = "xplus_gam_fit")
    },
    predict = function(model, newx) {
      prob <- stats::predict(model$model, newdata = .xplus_backend_frame(newx),
                             type = "response")
      pmin(pmax(as.numeric(prob), 0), 1)
    },
    importance = function(model) {
      s_table <- summary(model$model)$s.table
      if (is.null(s_table) || !nrow(s_table)) return(NULL)
      scores <- -log10(pmax(s_table[, ncol(s_table)], .Machine$double.xmin))
      imp <- stats::setNames(scores, model$features)
      imp
    }
  )
}

#' Parsnip model-spec backend for xplus
#'
#' Wrap a [parsnip::model_spec] — e.g. `parsnip::rand_forest()` combined with
#' [parsnip::set_engine()] — as an [xplus()] backend, giving the PLUS loop
#' access to the full tidymodels engine zoo. The spec's mode is set per fit:
#' `"classification"` when the target is binary (iterative pseudo-label fits),
#' `"regression"` when fractional (the final fit), so the model must support
#' both modes or `parsnip::fit_xy()` reports the gap. Case weights are passed
#' as [hardhat::frequency_weights()] when integral and
#' [hardhat::importance_weights()] otherwise; `foldid` is unused because
#' parsnip specs carry no internal resampling.
#'
#' `fit_predict` returns out-of-bag scores when the underlying engine stores
#' them on the fitted object (ranger engines) and falls back to in-sample
#' `predict` otherwise, so honest cutoff calibration is preserved where the
#' engine makes it possible. Engines that consume a `seed` argument
#' (ranger, xgboost, lightgbm) get one drawn from R's RNG when the spec does
#' not set it explicitly, keeping `xplus(seed = ...)` reproducible.
#' Feature importance is read from the fitted engine object when present;
#' declare it in the spec, e.g. `set_engine("ranger", importance = "impurity")`.
#'
#' @param spec A `parsnip` model specification produced by a model function
#'   such as [parsnip::rand_forest()], [parsnip::boost_tree()] or
#'   [parsnip::logistic_reg()], usually piped through [parsnip::set_engine()].
#'   The engine must be set and its package installed.
#' @param supports_fractional Logical scalar; declare `FALSE` for specs whose
#'   model lacks a regression mode (e.g. `logistic_reg`) so `xplus()` fails
#'   early with a clear message instead of at the fractional final fit.
#'
#' @return An `"xplus_backend"` object named `parsnip:<engine>`.
#' @seealso [xplus_backend()], [xplus()], [xplus_backend_ranger()]
#' @examples
#' \dontrun{
#' backend <- parsnip::rand_forest(trees = 500) |>
#'   parsnip::set_engine("ranger") |>
#'   xplus_backend_parsnip()
#' fit <- xplus(x, y, backend = backend)
#' }
#' @export
xplus_backend_parsnip <- function(spec, supports_fractional = TRUE) {
  if (!inherits(spec, "model_spec")) {
    stop("`spec` must be a parsnip model specification (class `model_spec`).",
         call. = FALSE)
  }
  if (is.null(spec$engine) || !nzchar(spec$engine)) {
    stop("`spec` must declare an engine; use `parsnip::set_engine()`.",
         call. = FALSE)
  }
  engine <- spec$engine
  xplus_backend(
    name = paste0("parsnip:", engine),
    label = sprintf("parsnip %s spec (engine '%s')", class(spec)[1], engine),
    uses_cv = FALSE,
    supports_fractional = supports_fractional,
    fit = function(x, y, foldid, weights) {
      if (!requireNamespace("parsnip", quietly = TRUE)) {
        stop("The parsnip backend requires the parsnip package to be installed.",
             call. = FALSE)
      }
      frame <- .xplus_backend_frame(x)
      binary <- all(y %in% c(0, 1))
      fit_spec <- spec
      fit_spec$mode <- if (binary) "classification" else "regression"
      if (engine %in% c("ranger", "xgboost", "lightgbm") &&
          !"seed" %in% names(fit_spec$eng_args)) {
        fit_spec$eng_args$seed <- rlang::new_quosure(sample.int(.Machine$integer.max, 1L))
      }
      cw <- NULL
      if (!is.null(weights)) {
        cw <- if (all(weights == round(weights))) {
          hardhat::frequency_weights(weights)
        } else {
          hardhat::importance_weights(weights)
        }
      }
      target <- if (binary) factor(y, levels = c(0, 1)) else y
      fit_obj <- tryCatch(
        parsnip::fit_xy(fit_spec, x = frame, y = target,
                        case_weights = cw,
                        control = parsnip::control_parsnip(verbosity = 0)),
        error = function(e) {
          stop(sprintf("parsnip %s spec (engine '%s') failed in '%s' mode: %s",
                       class(spec)[1], engine, fit_spec$mode,
                       conditionMessage(e)), call. = FALSE)
        }
      )
      structure(list(model = fit_obj,
                     mode = fit_spec$mode,
                     features = colnames(x)),
                class = "xplus_parsnip_fit")
    },
    predict = function(model, newx) {
      frame <- .xplus_backend_frame(newx)
      if (model$mode == "classification") {
        prob <- stats::predict(model$model, new_data = frame, type = "prob")
        p <- prob[[".pred_1"]]
      } else {
        p <- stats::predict(model$model, new_data = frame,
                            type = "numeric")[[".pred"]]
      }
      pmin(pmax(as.numeric(p), 0), 1)
    },
    fit_predict = function(model, x) {
      eng <- model$model$fit
      if (inherits(eng, "ranger")) {
        prob <- eng$predictions
        if (model$mode == "classification") prob <- prob[, "1"]
        return(as.numeric(prob))
      }
      rep(NA_real_, nrow(x))
    },
    importance = function(model) {
      eng <- model$model$fit
      imp <- if (inherits(eng, "ranger")) eng$variable.importance else NULL
      if (!length(imp)) return(NULL)
      if (length(model$features) == length(imp)) names(imp) <- model$features
      imp
    }
  )
}

# Resolve the `backend` argument of xplus() into a validated xplus_backend.
# String names call the built-in constructors so the `alpha` and `cv_measure`
# arguments apply to the default glmnet backend only.
.xplus_resolve_backend <- function(backend, alpha = 1, cv_measure = "deviance") {
  if (is.character(backend) && length(backend) == 1L && !is.na(backend)) {
    return(switch(match.arg(backend, c("glmnet", "ranger", "gam", "xgboost", "lightgbm")),
      glmnet = xplus_backend_glmnet(alpha = alpha, cv_measure = cv_measure),
      ranger = xplus_backend_ranger(),
      gam = xplus_backend_gam(),
      xgboost = xplus_backend_xgboost(),
      lightgbm = xplus_backend_lightgbm()
    ))
  }
  if (inherits(backend, "xplus_backend")) return(backend)
  stop("`backend` must be one of \"glmnet\", \"ranger\", \"gam\", \"xgboost\", \"lightgbm\", or an `xplus_backend` object.",
       call. = FALSE)
}

# Run a backend fit, normalize the optional CV-measure attribute, and wrap
# failures with the backend name for context.
.xplus_backend_fit <- function(backend, x, y, foldid, weights) {
  model <- tryCatch(
    backend$fit(x, y, foldid, weights),
    error = function(e) {
      stop(sprintf("Backend '%s' failed to fit: %s", backend$name, conditionMessage(e)),
           call. = FALSE)
    }
  )
  measure <- attr(model, "xplus_cv_measure")
  if (is.null(measure)) measure <- "none"
  if (!measure %in% c("deviance", "auc", "none")) {
    stop(sprintf("Backend '%s' reported an invalid cross-validation measure '%s'.",
                 backend$name, measure), call. = FALSE)
  }
  list(model = model, measure = measure)
}

# Backend predictions are validated to the same contract as glmnet responses
# and returned as a one-column matrix, matching the glmnet prediction shape.
.xplus_backend_predict <- function(backend, model, newx, n = NULL) {
  prob <- tryCatch(
    backend$predict(model, newx),
    error = function(e) {
      stop(sprintf("Backend '%s' failed to predict: %s", backend$name, conditionMessage(e)),
           call. = FALSE)
    }
  )
  matrix(.xplus_probabilities(prob, n), ncol = 1L)
}

# Honest predictions on the rows a backend was fitted on, returned in the
# same order. Backends without a `fit_predict` hook reuse `predict`, matching
# learners that do not memorize per-observation targets. Non-finite honest
# estimates (e.g. rows in-bag in every ranger tree) fall back to in-sample
# predictions for those rows only.
.xplus_backend_fit_predict <- function(backend, model, x) {
  if (is.null(backend$fit_predict)) {
    return(.xplus_backend_predict(backend, model, x, nrow(x)))
  }
  prob <- tryCatch(
    backend$fit_predict(model, x),
    error = function(e) {
      stop(sprintf("Backend '%s' failed to compute honest training predictions: %s",
                   backend$name, conditionMessage(e)), call. = FALSE)
    }
  )
  if (!is.numeric(prob) || is.complex(prob) || length(prob) != nrow(x)) {
    stop(sprintf("Backend '%s' `fit_predict` must return one prediction per fitted row.",
                 backend$name), call. = FALSE)
  }
  prob <- as.numeric(prob)
  missing <- !is.finite(prob)
  if (any(missing)) {
    replacement <- .xplus_backend_predict(backend, model, x[missing, , drop = FALSE],
                                          sum(missing))
    prob[missing] <- as.numeric(replacement)
  }
  matrix(.xplus_probabilities(prob, nrow(x)), ncol = 1L)
}

# Feature frame with synthetic column names; positional ordering is preserved,
# so the same names apply to any newx passed to the backend predict function.
.xplus_backend_frame <- function(x) {
  stats::setNames(as.data.frame(unname(x)), paste0("V", seq_len(ncol(x))))
}

# Name of the backend stored in an xplus object; NULL means a legacy object
# constructed before the backend field existed, i.e. glmnet.
.xplus_backend_name <- function(object) {
  if (inherits(object$backend, "xplus_backend")) object$backend$name else "glmnet"
}
