#' PU-valid score for a fitted xplus object
#'
#' Rank learners without negative labels. The only honest supervision in
#' positive-unlabeled data is the labeled-positive vs unlabeled contrast,
#' so the primary score is the AUC of the stored honest predictions
#' (`object$pred_y`) with unlabeled cases treated as negatives. Under SCAR
#' labeling this proxy is biased toward conservatism but ranks learners
#' consistently — the standard model-selection trick of the PU literature.
#'
#' @param object A fitted `"xplus"` object.
#' @param metric `"pu_auc"` (positive-vs-unlabeled AUC on honest scores),
#'   `"stability"` (mean pseudo-label stability score across iterations),
#'   or `"pseudo_auc"` (AUC of honest scores against the soft final labels).
#'
#' @return A numeric scalar; higher is better.
#' @seealso [xplus()], [xplus_grid()]
#' @examples
#' fit <- xplus(matrix(rnorm(100 * 4), ncol = 4),
#'              c(rep(1, 20), rep(0, 80)), max_iter = 3)
#' pu_score(fit)
#' @export
pu_score <- function(object, metric = c("pu_auc", "stability", "pseudo_auc")) {
  metric <- match.arg(metric)
  pred <- as.numeric(object$pred_y)
  truth <- as.numeric(object$original_y)
  if (metric == "pu_auc") {
    return(auc(truth, pred))
  }
  if (metric == "stability") {
    scores <- object$history$stability
    if (is.null(scores) || !length(scores)) return(NA_real_)
    return(mean(unlist(scores), na.rm = TRUE))
  }
  auc(as.numeric(object$final_labels) >= 0.5, pred)
}

#' Compare learners or hyperparameter grids on PU data
#'
#' The model-selection piece of the multi-backend API: fit `xplus()` for
#' every candidate and rank them by a PU-valid [pu_score()] — no negative
#' labels required. `learners` accepts a character vector of built-in
#' backend names, a list mixing names and [xplus_backend()] objects, or a
#' single constructor function (e.g. `xplus_backend_xgboost`) combined with
#' `params`, a data frame whose columns are constructor arguments and whose
#' rows form the tuning grid. One shared `seed` gives every candidate the
#' same bootstrap draws, so the comparison is paired rather than noisy.
#' Failures are captured per candidate instead of aborting the grid.
#'
#' @param x Feature matrix as in [xplus()].
#' @param y Binary PU labels: 1 positive, 0 unlabeled.
#' @param learners Character vector of backend names, a list of backend
#'   names/specs, or a backend constructor function to combine with `params`.
#' @param params Optional data frame of constructor arguments when `learners`
#'   is a function; each row is one candidate.
#' @param metric Scoring metric passed to [pu_score()].
#' @param seed Optional seed shared by every candidate fit.
#' @param ... Additional arguments passed to [xplus()] for every candidate.
#'
#' @return A data frame with one row per candidate: `learner`, any `params`
#'   columns, `score`, `n_iter`, `stop_reason`, `seconds`, and `error`
#'   (`NA` on success). Sorted by descending `score`.
#' @seealso [xplus()], [pu_score()], [xplus_backend()]
#' @examples
#' sim <- simulate_pu_data(n = 300, p = 6, boundary = "nonlinear", seed = 1,
#'                         prevalence = 0.4, label_propensity = 0.5)
#' xplus_grid(sim$x, sim$y, c("glmnet", "ranger"), max_iter = 5, seed = 1)
#' @export
xplus_grid <- function(x, y, learners, params = NULL, metric = "pu_auc",
                       seed = NULL, ...) {
  candidates <- .xplus_grid_candidates(learners, params)
  rows <- vector("list", length(candidates))
  for (i in seq_along(candidates)) {
    cand <- candidates[[i]]
    seconds <- system.time({
      fit <- tryCatch(
        xplus(x, y, backend = cand$backend, seed = seed, ...),
        error = function(e) e
      )
    })[["elapsed"]]
    if (inherits(fit, "xplus")) {
      rows[[i]] <- c(cand$meta, list(
        learner = cand$label, score = pu_score(fit, metric),
        n_iter = fit$n_iter, stop_reason = fit$stop_reason,
        seconds = unname(seconds), error = NA_character_
      ))
    } else {
      rows[[i]] <- c(cand$meta, list(
        learner = cand$label, score = NA_real_, n_iter = NA_integer_,
        stop_reason = NA_character_, seconds = unname(seconds),
        error = conditionMessage(fit)
      ))
    }
  }
  out <- do.call(rbind, lapply(rows, function(r) {
    as.data.frame(r, stringsAsFactors = FALSE, check.names = FALSE)
  }))
  out[order(-out$score), , drop = FALSE]
}

# Expand `learners` + `params` into a uniform list of candidates with
# display labels and grid metadata.
.xplus_grid_candidates <- function(learners, params) {
  if (is.function(learners)) {
    if (is.null(params)) params <- data.frame(.dummy = 0)
    if (!is.data.frame(params)) stop("`params` must be a data frame.", call. = FALSE)
    ctor <- learners
    return(lapply(seq_len(nrow(params)), function(i) {
      args <- as.list(params[i, , drop = FALSE])
      args <- args[names(args) != ".dummy"]
      backend <- do.call(ctor, args)
      if (!inherits(backend, "xplus_backend")) {
        stop("The `learners` function must return an `xplus_backend` object.", call. = FALSE)
      }
      list(backend = backend, label = backend$name, meta = args)
    }))
  }
  if (is.character(learners)) learners <- as.list(learners)
  if (!is.list(learners) || !length(learners)) {
    stop("`learners` must be backend names, a list of backends, or a constructor function.",
         call. = FALSE)
  }
  if (!is.null(params)) {
    stop("`params` only applies when `learners` is a constructor function.", call. = FALSE)
  }
  lapply(learners, function(b) {
    label <- if (is.character(b)) b else b$name
    list(backend = b, label = label, meta = list())
  })
}
