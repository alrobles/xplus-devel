## R CMD check results

0 errors | 0 warnings | 1 note

* This is a resubmission following review by Konstanze Lauseker.

## Resubmission

In response to Konstanze Lauseker's review of the previous submission:

* Removed the `\examples` sections from the unexported internal functions
  `new_xplus()` and `validate_xplus()` (documented with `\keyword{internal}`).
* With those examples removed, the package no longer uses `\dontrun{}`
  anywhere; all remaining examples are executable.
* `xplus()` no longer reads or writes `.GlobalEnv` directly. The optional
  `seed` argument now uses a plain `set.seed()` call, consistent with
  standard RNG practice in R packages.

## Test environments

* local: Ubuntu 22.04, R 4.1.2
