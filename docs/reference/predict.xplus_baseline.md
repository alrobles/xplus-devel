# Predict from a PU baseline model

Predict from a PU baseline model

## Usage

``` r
# S3 method for class 'xplus_baseline'
predict(object, newx = NULL, ...)
```

## Arguments

- object:

  An `xplus_baseline` object.

- newx:

  Optional numeric feature matrix; defaults to training data.

- ...:

  Unused.

## Value

Rescaled probability of the positive class as a numeric vector.
