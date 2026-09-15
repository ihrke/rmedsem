## Update

This is an update of rmedsem (1.0.0 -> 1.1.0). The main changes are:

- An extensible S3 method system for printing, a `summary()` method that
  returns an object, and new `coef()`, `confint()` and `nobs()` methods.
- Informative argument and model checks in all exported functions.
- Bug fixes (labelled model parameters, Bayesian output table, bootstrap
  intervals and reproducibility with cSEM, confidence levels of
  simulation-based intervals).
- Merged, more compact help pages and examples for all supported backends.

See NEWS.md for details.

The examples for the `cSEM`, `modsem` and `blavaan` backends in `?rmedsem`
are wrapped in `\donttest{}` (with a `requireNamespace()` guard) because
they depend on suggested packages and take more than a few seconds (MCMC
sampling, bootstrapping, LMS estimation).

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Ubuntu 25.04, R 4.5.3
* GitHub Actions: Ubuntu (R release, R devel), macOS (R release),
  Windows (R release)
* win-builder (R devel)

## Reverse dependencies

There are currently no reverse dependencies on CRAN.
