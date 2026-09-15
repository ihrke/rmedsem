# rmedsem 1.0.0.9000 (development)

## Breaking changes

* `summary()` returns a `summary.rmedsem` object 
* `as.data.frame()` returns a plain data frame 
* `rmedsem()` stops with an error for models that previously ran but gave
  wrong or empty results
* `ci.two.tailed` now determines all intervals
* blavaan: the direct effect's `pval` is now the posterior probability of the
  opposite sign
* `mcreps` defaults to 5000 and is no longer raised to the sample size
* RID is not reported (and `RID()` warns) if the direct effect is not
  significant; `RIT()` warns if the total effect is too small

## New features

* `summary()` method
* New methods `coef()`, `confint()` (with arguments `parm`, `level` and
  `method`) and `nobs()` for `rmedsem` objects.
* `rmedsem.blavaan()` gains `ci.two.tailed` and `hdi` (highest-density intervals)
* Printing is now an extensible S3 method system
* Result objects gain the elements `zlc.method`
* All exported functions check their arguments and stop with informative
  messages
* lavaan: a warning is given if the model did not converge.
* cSEM: new `seed` argument; results are reproducible with `set.seed()`

## Bug fixes

* lavaan and modsem models with labelled regression paths (e.g.,
  `read ~ a*math`) now work 
* blavaan: the rows of the printed table were shifted relative to their
  labels 
* modsem: the moderation effects are printed in causal direction
* cSEM: the bootstrap interval of the indirect effect was taken from the incorrect row
* `digits` and `indent` are passed correctly to the effect-size output.
* `plot_coef()` no longer emits a `position_dodge()` warning
* Very small p-values are no longer printed as `0`; tables use consistent
  decimal places and printed lines fit into 80 characters
* `plot_effect()` title and caption are wrapped to fit small figures

## Documentation

* Help pages were merged into a compact set: `?rmedsem`, `?rmedsem-methods`, `?effect-sizes` and `?plot.rmedsem` 
* Examples for all backends
* Corrected and completed the documentation of the `hsbdemo` and `mchoice`
  datasets.
* New article "Working with the results"
* Added CRAN badge to README

# rmedsem 1.0.0

* Initial CRAN release.
* Support for mediation analysis with lavaan (CB-SEM), cSEM (PLS-SEM),
  blavaan (Bayesian SEM), and modsem (moderated mediation).
* Implements Baron and Kenny (1986) and Zhao, Lynch & Chen (2010) approaches.
* Reports indirect effects via Sobel, Delta, and Monte-Carlo methods.
* Effect size measures: RIT and RID.
* Summary and plot methods for results.
* Fix standard error calculation for standardized coefficients in
  rmedsem.lavaan and rmedsem.modsem (contributed by Kjell Slupphaug).
* New `ci.two.tailed` parameter for configurable confidence interval width.
