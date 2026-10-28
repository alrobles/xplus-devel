# Build class-stratified cross-validation fold assignments

Assigns each observation to one of `nfolds` folds so that both classes
are spread as evenly as possible across folds. This prevents a small
class from being concentrated in a single fold, which would leave a
cross-validation training split with fewer than the two observations per
class that
[`glmnet::glmnet()`](https://rdrr.io/pkg/glmnet/man/glmnet.html)
requires for binomial fits.

## Usage

``` r
stratified_foldid(y, nfolds)
```

## Arguments

- y:

  Binary (0/1) vector of class labels.

- nfolds:

  Number of folds.

## Value

Integer vector of fold assignments in `1:nfolds`, the same length as
`y`, suitable for the `foldid` argument of
[`glmnet::cv.glmnet()`](https://rdrr.io/pkg/glmnet/man/cv.glmnet.html).
