# Changelog

## rmedsem 1.0.0.9000 (development)

### Breaking changes

- [`summary()`](https://rdrr.io/r/base/summary.html) returns a
  `summary.rmedsem` object
- [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
  a plain data frame
- [`rmedsem()`](https://ihrke.github.io/rmedsem/reference/rmedsem.md)
  stops with an error for models that previously ran but gave wrong or
  empty results
- `ci.two.tailed` now determines all intervals
- blavaan: the direct effect’s `pval` is now the posterior probability
  of the opposite sign
- `mcreps` defaults to 5000 and is no longer raised to the sample size
- RID is not reported if the direct effect is not significant;
  [`RIT()`](https://ihrke.github.io/rmedsem/reference/effect-sizes.md)
  warns if the total effect is too small

### New features

- [`summary()`](https://rdrr.io/r/base/summary.html) method
- New methods [`coef()`](https://rdrr.io/r/stats/coef.html),
  [`confint()`](https://rdrr.io/r/stats/confint.html) and
  [`nobs()`](https://rdrr.io/r/stats/nobs.html) for `rmedsem` objects.
- [`rmedsem.blavaan()`](https://ihrke.github.io/rmedsem/reference/rmedsem.md)
  gains `ci.two.tailed` and `hdi` (highest-density intervals)
- Printing is now an extensible S3 method system
- Result objects gain the elements `zlc.method`
- All exported functions check their arguments and stop with informative
  messages
- lavaan: a warning is given if the model did not converge.
- cSEM: new `seed` argument; results are reproducible with
  [`set.seed()`](https://rdrr.io/r/base/Random.html)

### Bug fixes

- lavaan and modsem models with labelled regression paths (e.g.,
  `read ~ a*math`) now work
- blavaan: the rows of the printed table were shifted relative to their
  labels
- modsem: the moderation effects are printed in causal direction
- cSEM: the bootstrap interval of the indirect effect was taken from the
  incorrect row
- `digits` and `indent` are passed correctly to the effect-size output.
- [`plot_coef()`](https://ihrke.github.io/rmedsem/reference/plot.rmedsem.md)
  no longer emits a `position_dodge()` warning
- Very small p-values are no longer printed as `0`
- [`plot_effect()`](https://ihrke.github.io/rmedsem/reference/plot.rmedsem.md)
  title and caption are wrapped to fit small figures

### Documentation

- Help pages were merged into a compact set:
  [`?rmedsem`](https://ihrke.github.io/rmedsem/reference/rmedsem.md),
  `?rmedsem-methods`, `?effect-sizes` and
  [`?plot.rmedsem`](https://ihrke.github.io/rmedsem/reference/plot.rmedsem.md)
- Examples for all backends
- Corrected and completed the documentation of the `hsbdemo` and
  `mchoice` datasets.
- New article “Working with the results”
- Added CRAN badge to README

## rmedsem 1.0.0

CRAN release: 2026-03-16

- Initial CRAN release.
- Support for mediation analysis with lavaan (CB-SEM), cSEM (PLS-SEM),
  blavaan (Bayesian SEM), and modsem (moderated mediation).
- Implements Baron and Kenny (1986) and Zhao, Lynch & Chen (2010)
  approaches.
- Reports indirect effects via Sobel, Delta, and Monte-Carlo methods.
- Effect size measures: RIT and RID.
- Summary and plot methods for results.
- Fix standard error calculation for standardized coefficients in
  rmedsem.lavaan and rmedsem.modsem (contributed by Kjell Slupphaug).
- New `ci.two.tailed` parameter for configurable confidence interval
  width.
