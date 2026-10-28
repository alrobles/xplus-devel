# Build a tidy prediction table

Build a tidy prediction table

## Usage

``` r
get_predictions(object, newx, newy, use_cutoff = TRUE)
```

## Arguments

- object:

  An `xplus` model object.

- newx:

  Feature matrix.

- newy:

  True labels.

- use_cutoff:

  Logical; if `TRUE`, classify with the model's cutoff. If `FALSE`,
  classify probabilities strictly greater than 0.5 as positive.

## Value

A tibble with truth labels, probabilities and predicted classes;
`Class1` is the positive label 1, classified by probability strictly
greater than the cutoff.

## References

Zhou et al. (2022). doi:10.1371/journal.pcbi.1009956

## See also

[`predict.xplus()`](https://alrobles.github.io/xplus/reference/predict.xplus.md),
[`assess()`](https://alrobles.github.io/xplus/reference/assess.md)

## Examples

``` r
set.seed(1)
x <- matrix(rnorm(100 * 5), ncol = 5)
y <- c(rep(1, 20), rep(0, 80))
fit <- xplus(x, y, max_iter = 5)
get_predictions(fit, x, y)
#> # A tibble: 100 × 4
#>    truth  Class1 Class2 predicted
#>    <fct>   <dbl>  <dbl> <fct>    
#>  1 Class1  0.570  0.430 Class1   
#>  2 Class1  0.474  0.526 Class1   
#>  3 Class1  0.652  0.348 Class1   
#>  4 Class1  0.557  0.443 Class1   
#>  5 Class1  0.630  0.370 Class1   
#>  6 Class1  0.631  0.369 Class1   
#>  7 Class1  0.559  0.441 Class1   
#>  8 Class1  0.603  0.397 Class1   
#>  9 Class1  0.471  0.529 Class1   
#> 10 Class1  0.542  0.458 Class1   
#> # ℹ 90 more rows
```
