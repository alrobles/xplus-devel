# Summarize an xplus model

Summarize an xplus model

## Usage

``` r
# S3 method for class 'xplus'
summary(object, ...)
```

## Arguments

- object:

  An `xplus` object.

- ...:

  Additional arguments.

## Value

A list of class `summary.xplus` with model details.

## References

Zhou et al. (2022). doi:10.1371/journal.pcbi.1009956

## See also

[`print.xplus()`](https://alrobles.github.io/xplus/reference/print.xplus.md),
[`coef.xplus()`](https://alrobles.github.io/xplus/reference/coef.xplus.md)

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(100 * 5), ncol = 5)
y <- c(rep(1, 20), rep(0, 80))
fit <- xplus(x, y, max_iter = 5)
summary(fit)
#> Summary of xplus model
#> Observations: 100 
#> Features: 5 
#> Iterations: 5 
#> Stop reason: max_iter 
#> Original positives: 20 
#> Original unlabeled: 80 
#> Fallback used: FALSE 
#> Final training target range: 0 to 1 
#> Final soft targets: 59 
#> CV measure: deviance 
#> Sampling: bootstrap 
#> Unlabeled coverage: 0.7375 
#> Backend: glmnet 
#> Cutoff: 0.4683134 
#> lambda.min: 0.02089059 
#> lambda.1se: 0.0925583 
#> Non-zero coefficients: 4 
```
