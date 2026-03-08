# Mediation Analysis for cSEM Models

Mediation Analysis for cSEM Models

## Usage

``` r
# S3 method for class 'cSEMResults'
rmedsem(
  mod,
  indep,
  med,
  dep,
  approach = c("bk", "zlc"),
  p.threshold = 0.05,
  effect.size = c("RIT", "RID", "upsilon"),
  nbootstrap = 1000,
  ci.two.tailed = 0.95,
  ...
)
```

## Arguments

- mod:

  A fitted SEM model (cSEM).

- indep:

  A string indicating the name of the independent variable in the model.

- med:

  A string indicating the name of the mediator variable in the model.

- dep:

  A string indicating the name of the dependent variable in the model.

- approach:

  either 'bk' or 'zlc' or both c("bk", "zlc") (default)

- p.threshold:

  A double giving the p-value for determining whether a path is
  significant or not

- effect.size:

  calculate different effect-sizes; one or more of "RIT", "RID"

- nbootstrap:

  number of bootstrap samples, default=1000

- ci.two.tailed:

  A double giving the confidence level for two-tailed confidence
  intervals (default 0.95)

- ...:

  additional arguments (currently unused)

## Value

A `rmedsem` structure containing the results from the analysis
