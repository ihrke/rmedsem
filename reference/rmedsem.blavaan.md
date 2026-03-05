# Mediation Analysis for Blavaan Models

Mediation Analysis for Blavaan Models

## Usage

``` r
# S3 method for class 'blavaan'
rmedsem(
  mod,
  indep,
  med,
  dep,
  approach = c("bk", "zlc"),
  p.threshold = 0.05,
  effect.size = c("RIT", "RID"),
  ...
)
```

## Arguments

- mod:

  A fitted SEM model (blavaan). Note that the model has to be fit using
  `save.lvs=T` if the mediation model contains latent variables.

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

- ...:

  additional arguments (currently unused)

## Value

A `rmedsem` structure containing the results from the analysis

## Examples

``` r
if (FALSE) { # \dontrun{
model02 <- "
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
"
if (requireNamespace("blavaan", quietly = TRUE)) {
  mod <- blavaan::bsem(model02, data=lavaan::PoliticalDemocracy, std.lv=TRUE,
              meanstructure=TRUE, n.chains=1,
              save.lvs=TRUE, burnin=500, sample=500)
  out <- rmedsem(mod,  indep="ind60", med="dem60", dep="dem65")
  print(out)
}
} # }
```
