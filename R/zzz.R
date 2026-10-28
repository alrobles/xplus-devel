# Register the pu_xplus parsnip model whenever parsnip is available so
# tune/finetune machinery sees it without a manual registration call.
.onLoad <- function(libname, pkgname) {
  if (requireNamespace("parsnip", quietly = TRUE)) {
    try(xplus_parsnip_register(), silent = TRUE)
  }
}
