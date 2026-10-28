# Print an xplus model

Print an xplus model

## Usage

``` r
# S3 method for class 'xplus'
print(x, digits = max(3, getOption("digits") - 3), ...)
```

## Arguments

- x:

  An `xplus` object.

- digits:

  Number of significant digits.

- ...:

  Additional arguments.

## Value

Invisibly returns `x`.

## References

Zhou et al. (2022). doi:10.1371/journal.pcbi.1009956

## See also

[`summary.xplus()`](https://alrobles.github.io/xplus/reference/summary.xplus.md),
[`coef.xplus()`](https://alrobles.github.io/xplus/reference/coef.xplus.md)

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(100 * 5), ncol = 5)
y <- c(rep(1, 20), rep(0, 80))
fit <- xplus(x, y, max_iter = 5)
print(fit)
#> xplus model (PLUS algorithm)
#> Call:  xplus(x = x, y = y, max_iter = 5) 
#> Iterations: 5 
#> Stop reason: max_iter 
#> Original positives: 20 
#> Fallback used: FALSE 
#> Final training target range: 0 to 1 
#> Final soft targets: 59 
#> CV measure: deviance 
#> Backend: glmnet 
#> Cutoff: 0.4683 
#> 
#>      Lambda Index Measure      SE Nonzero
#> min 0.02089    17  0.7757 0.04789       3
#> 1se 0.09256     1  0.7817 0.07399       0
```
