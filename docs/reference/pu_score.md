# PU-valid score for a fitted xplus object

Rank learners without negative labels. The only honest supervision in
positive-unlabeled data is the labeled-positive vs unlabeled contrast,
so the primary score is the AUC of the stored honest predictions
(`object$pred_y`) with unlabeled cases treated as negatives. Under SCAR
labeling this proxy is biased toward conservatism but ranks learners
consistently — the standard model-selection trick of the PU literature.

## Usage

``` r
pu_score(object, metric = c("pu_auc", "stability", "pseudo_auc"))
```

## Arguments

- object:

  A fitted `"xplus"` object.

- metric:

  `"pu_auc"` (positive-vs-unlabeled AUC on honest scores), `"stability"`
  (mean pseudo-label stability score across iterations), or
  `"pseudo_auc"` (AUC of honest scores against the soft final labels).

## Value

A numeric scalar; higher is better.

## See also

[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md),
[`xplus_grid()`](https://alrobles.github.io/xplus/reference/xplus_grid.md)

## Examples

``` r
fit <- xplus(matrix(rnorm(100 * 4), ncol = 4),
             c(rep(1, 20), rep(0, 80)), max_iter = 3)
pu_score(fit)
#> [1] 0.5
```
