# Compute AUC from matrix labels

Compute AUC from matrix labels

## Usage

``` r
auc_matrix(y, prob, weights = NULL)
```

## Arguments

- y:

  Two-column finite nonnegative class masses (negative, positive); soft
  labels, counts, and zero-mass rows are supported.

- prob:

  Finite numeric scores, one per row; values outside `[0, 1]` are
  allowed.

- weights:

  Optional finite nonnegative numeric row weights without recycling;
  NULL means unit weights.

## Value

Weighted rank AUC with half credit for ties; NA with a warning if either
effective class mass is zero.

## References

Zhou et al. (2022). doi:10.1371/journal.pcbi.1009956

## See also

[`auc()`](https://alrobles.github.io/xplus/reference/auc.md),
[`assess()`](https://alrobles.github.io/xplus/reference/assess.md)

## Examples

``` r
y <- cbind(c(1, 1, 0, 0), c(0, 0, 1, 1))
p <- c(0.2, 0.3, 0.7, 0.8)
auc_matrix(y, p)
#> [1] 1
```
