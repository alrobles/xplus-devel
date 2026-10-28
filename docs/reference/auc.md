# Compute area under the ROC curve

Compute area under the ROC curve

## Usage

``` r
auc(y, prob, w = NULL)
```

## Arguments

- y:

  Binary 0/1 vector, including logical, character, or factor encodings.

- prob:

  Finite numeric scores, one per label; values outside `[0, 1]` are
  allowed.

- w:

  Optional finite nonnegative numeric sample weights; NULL means unit
  weights.

## Value

Numeric rank AUC with half credit for ties; NA with a warning if either
effective class mass is zero.

## References

Zhou et al. (2022). doi:10.1371/journal.pcbi.1009956

## See also

[`auc_matrix()`](https://alrobles.github.io/xplus/reference/auc_matrix.md),
[`get_auc()`](https://alrobles.github.io/xplus/reference/get_auc.md)

## Examples

``` r
y <- c(0, 0, 1, 1)
p <- c(0.1, 0.3, 0.7, 0.9)
auc(y, p)
#> [1] 1
```
