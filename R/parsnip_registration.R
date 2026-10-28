#' xplus as a parsnip model
#'
#' Register `xplus()` as a parsnip model (`pu_xplus` with engine `"xplus"`)
#' so PU learning composes with tidymodels: `tune::tune_grid()`,
#' `finetune::tune_sim_anneal()`, and `rsample` resamples all work against
#' the backend contract. The `backend` argument selects the learner — a
#' built-in name, an [xplus_backend()] spec, or a constructor name —
#' and backend-constructor arguments (`trees`, `learn_rate`, `tree_depth`,
#' `min_n`, `mtry`, `sample_size`, `loss_reduction`) are tunable through
#' `tune()` placeholders. Registration happens automatically when parsnip
#' is installed; call this function to re-register after loading parsnip
#' manually.
#'
#' @return Invisibly, `TRUE` when the model was registered.
#' @examples
#' \dontrun{
#' library(parsnip)
#' spec <- pu_xplus(backend = "xgboost", trees = 300, learn_rate = 0.05) |>
#'   set_engine("xplus")
#' fit <- fit_xy(spec, x = data.frame(x), y = factor(y, levels = 0:1))
#' }
#' @export
xplus_parsnip_register <- function() {
  if (!requireNamespace("parsnip", quietly = TRUE)) {
    stop("xplus_parsnip_register() requires the parsnip package.", call. = FALSE)
  }
  try(parsnip::set_new_model("pu_xplus"), silent = TRUE)
  parsnip::set_model_mode("pu_xplus", "classification")
  parsnip::set_model_engine("pu_xplus", "classification", "xplus")
  parsnip::set_dependency("pu_xplus", "xplus", "xplus", mode = "classification")
  args <- list(
    list(parsnip = "trees", original = "trees",
         func = list(pkg = "dials", fun = "trees")),
    list(parsnip = "learn_rate", original = "learn_rate",
         func = list(pkg = "dials", fun = "learn_rate", range = c(-3, -1/2))),
    list(parsnip = "tree_depth", original = "tree_depth",
         func = list(pkg = "dials", fun = "tree_depth")),
    list(parsnip = "min_n", original = "min_n",
         func = list(pkg = "dials", fun = "min_n")),
    list(parsnip = "mtry", original = "mtry",
         func = list(pkg = "dials", fun = "mtry")),
    list(parsnip = "sample_size", original = "sample_size",
         func = list(pkg = "dials", fun = "sample_prop", range = c(0.5, 1))),
    list(parsnip = "loss_reduction", original = "loss_reduction",
         func = list(pkg = "dials", fun = "loss_reduction"))
  )
  for (a in args) {
    parsnip::set_model_arg(
      model = "pu_xplus", eng = "xplus", parsnip = a$parsnip,
      original = a$original, func = a$func, has_submodel = FALSE
    )
  }
  parsnip::set_fit(
    model = "pu_xplus", eng = "xplus", mode = "classification",
    value = list(
      interface = "data.frame", protect = c("x", "y"),
      func = c(pkg = "xplus", fun = "xplus_parsnip_fit"),
      defaults = list()
    )
  )
  parsnip::set_encoding(
    model = "pu_xplus", eng = "xplus", mode = "classification",
    options = list(
      predictor_indicators = "traditional", compute_intercept = FALSE,
      remove_intercept = FALSE, allow_sparse_x = TRUE
    )
  )
  parsnip::set_pred(
    model = "pu_xplus", eng = "xplus", mode = "classification",
    type = "prob",
    value = list(
      pre = NULL, post = NULL, func = c(pkg = "xplus", fun = "xplus_parsnip_prob"),
      args = list(object = quote(object), new_data = quote(new_data))
    )
  )
  parsnip::set_pred(
    model = "pu_xplus", eng = "xplus", mode = "classification",
    type = "class",
    value = list(
      pre = NULL, post = NULL, func = c(pkg = "xplus", fun = "xplus_parsnip_class"),
      args = list(object = quote(object), new_data = quote(new_data))
    )
  )
  invisible(TRUE)
}

#' parsnip model spec for PU learning with xplus
#'
#' Create a parsnip spec driving [xplus()]; use with
#' `parsnip::fit_xy()`/`fit()`, `tune::tune_grid()`, or
#' `finetune::tune_sim_anneal()`.
#'
#' @param mode Model mode; `"classification"` is the only supported value.
#' @param engine Fitting engine; `"xplus"`.
#' @param backend Backend selector: a built-in name such as `"glmnet"` or
#'   `"xgboost"`, an [xplus_backend()] object, or a constructor function.
#'   When a name is given, the boost hyperparameter arguments below map onto
#'   the matching `xplus_backend_*()` constructor and accept `tune()`.
#' @param seed,max_iter,nfolds `xplus()` controls forwarded verbatim.
#' @param trees,learn_rate,tree_depth,min_n,mtry,sample_size,loss_reduction
#'   Backend constructor arguments; passed through to the matching
#'   `xplus_backend_*()` constructor when `backend` is a name.
#' @param ... Reserved.
#'
#' @return A parsnip `model_spec`.
#' @export
pu_xplus <- function(mode = "classification", engine = "xplus",
                     backend = "glmnet", seed = NULL, max_iter = 100,
                     nfolds = 10, trees = NULL, learn_rate = NULL,
                     tree_depth = NULL, min_n = NULL, mtry = NULL,
                     sample_size = NULL, loss_reduction = NULL, ...) {
  xplus_parsnip_register()
  args <- list(
    backend = rlang::enquo(backend), seed = rlang::enquo(seed),
    max_iter = rlang::enquo(max_iter), nfolds = rlang::enquo(nfolds),
    trees = rlang::enquo(trees), learn_rate = rlang::enquo(learn_rate),
    tree_depth = rlang::enquo(tree_depth), min_n = rlang::enquo(min_n),
    mtry = rlang::enquo(mtry), sample_size = rlang::enquo(sample_size),
    loss_reduction = rlang::enquo(loss_reduction)
  )
  args <- args[!vapply(args, rlang::quo_is_missing, logical(1))]
  parsnip::new_model_spec(
    "pu_xplus", args = args, eng_args = NULL,
    mode = mode, method = NULL, engine = engine
  )
}

#' @method update pu_xplus
#' @keywords internal
#' @export
update.pu_xplus <- function(object, parameters = NULL, backend = NULL,
                            seed = NULL, max_iter = NULL, nfolds = NULL,
                            trees = NULL, learn_rate = NULL, tree_depth = NULL,
                            min_n = NULL, mtry = NULL, sample_size = NULL,
                            loss_reduction = NULL, fresh = FALSE, ...) {
  args <- list(
    backend = rlang::enquo(backend), seed = rlang::enquo(seed),
    max_iter = rlang::enquo(max_iter), nfolds = rlang::enquo(nfolds),
    trees = rlang::enquo(trees), learn_rate = rlang::enquo(learn_rate),
    tree_depth = rlang::enquo(tree_depth), min_n = rlang::enquo(min_n),
    mtry = rlang::enquo(mtry), sample_size = rlang::enquo(sample_size),
    loss_reduction = rlang::enquo(loss_reduction)
  )
  parsnip::update_spec(
    object = object, parameters = parameters, args_enquo_list = args,
    fresh = fresh, cls = "pu_xplus", ...
  )
}

#' Engine fit and prediction entry points for `pu_xplus`
#'
#' These wrappers are registered with parsnip and invoked when a
#' `pu_xplus` spec is fitted or predicted on; they are not meant to be
#' called directly. `xplus_parsnip_fit()` resolves the backend argument,
#' folds extra constructor arguments into the matching
#' `xplus_backend_*()` constructor (ignoring arguments the backend does
#' not accept, with a warning), and calls [xplus()]. The prediction
#' adapters return parsnip-shaped outputs: `.pred_<level>` columns for
#' probabilities and a factor with the observed class levels for class
#' predictions. The second outcome level is treated as the positive
#' class, matching the parsnip convention for binary classification.
#'
#' @param x Predictor data (data frame or matrix).
#' @param y Outcome (factor `0/1`, or coercible vector).
#' @param new_data New predictor data for prediction.
#' @param object The fitted `xplus` object (parsnip passes `object$fit`).
#' @param backend,seed,max_iter,nfolds `pu_xplus` arguments, see [pu_xplus()].
#' @param ... Backend constructor arguments forwarded from the spec.
#'
#' @return `xplus_parsnip_fit()` returns an [xplus()] object; prediction
#'   helpers return tibbles or factors in parsnip shape.
#' @keywords internal
#' @export
xplus_parsnip_fit <- function(x, y, backend = "glmnet", seed = NULL,
                               max_iter = 100, nfolds = 10, ...) {
  ctors <- c(
    glmnet = "xplus_backend_glmnet", ranger = "xplus_backend_ranger",
    gam = "xplus_backend_gam", xgboost = "xplus_backend_xgboost",
    lightgbm = "xplus_backend_lightgbm"
  )
  ctor_args <- Filter(Negate(is.null), list(...))
  backend_spec <- backend
  if (is.character(backend) && length(ctor_args)) {
    if (!backend %in% names(ctors)) {
      stop(sprintf("Backend '%s' has no tunable constructor; pass an `xplus_backend()` object instead.",
                   backend), call. = FALSE)
    }
    ctor <- get(ctors[[backend]])
    keep <- names(ctor_args) %in% names(formals(ctor))
    dropped <- names(ctor_args)[!keep]
    if (length(dropped)) {
      warning(sprintf("Ignoring arguments not accepted by %s: %s",
                      ctors[[backend]], paste(dropped, collapse = ", ")),
              call. = FALSE)
    }
    backend_spec <- do.call(ctor, ctor_args[keep])
  }
  if (is.data.frame(y)) y <- y[[1]]
  if (is.factor(y)) {
    if (nlevels(y) != 2L) stop("`pu_xplus` requires a binary outcome.", call. = FALSE)
    y <- as.integer(y) - 1L
  } else {
    y <- as.integer(y)
  }
  xplus(.xplus_parsnip_matrix(x), y, backend = backend_spec, seed = seed,
        max_iter = max_iter, nfolds = nfolds)
}

#' @rdname xplus_parsnip_fit
#' @export
xplus_parsnip_prob <- function(object, new_data) {
  lvl <- object$lvl %||% c("0", "1")
  p <- as.numeric(stats::predict(object$fit, .xplus_parsnip_matrix(new_data)))
  out <- tibble::tibble(1 - p, p)
  names(out) <- paste0(".pred_", lvl[1:2])
  out
}

#' @rdname xplus_parsnip_fit
#' @export
xplus_parsnip_num <- function(object, new_data) {
  tibble::tibble(.pred = as.numeric(
    stats::predict(object, .xplus_parsnip_matrix(new_data))))
}

#' @rdname xplus_parsnip_fit
#' @export
xplus_parsnip_class <- function(object, new_data) {
  lvl <- object$lvl %||% c("0", "1")
  p <- as.numeric(stats::predict(object$fit, .xplus_parsnip_matrix(new_data)))
  factor(ifelse(p > object$fit$cutoff, lvl[2], lvl[1]), levels = lvl)
}

.xplus_parsnip_matrix <- function(d) {
  if (is.matrix(d)) return(d)
  if (all(vapply(d, is.numeric, logical(1)))) return(as.matrix(d))
  stats::model.matrix(~ . - 1, d)
}
