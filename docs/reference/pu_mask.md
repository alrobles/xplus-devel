# Convert true binary labels into PU labels

Turn any labeled binary dataset into a positive-unlabeled one under the
SCAR assumption: each positive stays labeled with constant probability
`label_propensity` and the rest become unlabeled. This is how the PU
literature builds benchmarks from open binary datasets — positives of
the known class remain `1`, hidden positives and negatives pool as `0`.

## Usage

``` r
pu_mask(z, label_propensity = 0.5, seed = NULL)
```

## Arguments

- z:

  Binary vector of true labels (`1` positive, `0` negative).

- label_propensity:

  Constant probability that a true positive is observed as a labeled
  positive, in `(0, 1]`.

- seed:

  Optional nonnegative integer seed. Use a different seed than the one
  that generated `z` — reusing the same stream can correlate the mask
  with the simulated labels.

## Value

An integer vector of PU labels (`1` labeled positive, `0` unlabeled) the
same length as `z`.

## See also

[`simulate_pu_data()`](https://alrobles.github.io/xplus/reference/simulate_pu_data.md),
[`xplus()`](https://alrobles.github.io/xplus/reference/xplus.md)

## Examples

``` r
z <- c(rep(1, 80), rep(0, 120))
y <- pu_mask(z, label_propensity = 0.5, seed = 1)
fit <- xplus(matrix(rnorm(200 * 4), ncol = 4), y, max_iter = 3)
```
