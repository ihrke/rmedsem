## Update

This is an update of rmedsem (1.0.0 -> 1.1.0). The main changes are:

- An extensible S3 method system for printing, a `summary()` method that
  returns an object, and new `coef()`, `confint()` and `nobs()` methods.
- Informative argument and model checks in all exported functions.
- Bug fixes.
- Merged, more compact help pages and examples for all supported backends.

See NEWS.md for details.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Ubuntu 25.04, R 4.5.3 (`R CMD check --as-cran`, also with
  `_R_CHECK_DEPENDS_ONLY_=true`)
* GitHub Actions: Ubuntu (R release, R devel), macOS (R release),
  Windows (R release)
* win-builder (R devel)
* macOS builder (R release)

## Reverse dependencies

There are currently no reverse dependencies on CRAN.
