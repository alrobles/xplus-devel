#' Specify a fitting backend for xplus
#'
#' Describe a learner that [xplus()] uses for both the iterative pseudo-labeling
#' fits and the final model. Built-in backends are [xplus_backend_glmnet()],
#' [xplus_backend_ranger()], and [xplus_backend_gam()]; this constructor defines
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
                          uses_cv = FALSE, parameters = NULL) {
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
  if (!is.null(parameters)) {
    if (!is.data.frame(parameters) || !all(c("parameter", "class", "label") %in% names(parameters)) ||
        any(!nzchar(parameters$parameter))) {
      stop("`parameters` must be a data frame with nonempty `parameter`, `class`, and `label` columns.",
           call. = FALSE)
    }
  }
  structure(
    list(name = name, label = label, fit = fit, predict = predict,
         importance = importance, uses_cv = uses_cv, parameters = parameters),
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

# Resolve the `backend` argument of xplus() into a validated xplus_backend.
# String names call the built-in constructors so the `alpha` and `cv_measure`
# arguments apply to the default glmnet backend only.
.xplus_resolve_backend <- function(backend, alpha = 1, cv_measure = "deviance") {
  if (is.character(backend) && length(backend) == 1L && !is.na(backend)) {
    return(switch(match.arg(backend, c("glmnet", "ranger", "gam")),
      glmnet = xplus_backend_glmnet(alpha = alpha, cv_measure = cv_measure),
      ranger = xplus_backend_ranger(),
      gam = xplus_backend_gam()
    ))
  }
  if (inherits(backend, "xplus_backend")) return(backend)
  stop("`backend` must be one of \"glmnet\", \"ranger\", \"gam\", or an `xplus_backend` object.",
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
