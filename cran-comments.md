## Resubmission

This is a resubmission. In this version I have:

- Added `\value` tags to all exported function documentation (RIT, RID, Upsilon)
- Added a new effect-size estimate "upsilon" to the package
- Replaced `T`/`F` with `TRUE`/`FALSE` throughout
- The `rmedsem.blavaan`, `rmedsem.cSEMResults` and `rmedsem.modsem` examples
  use `\donttest{}` (with a `requireNamespace()` guard) because they depend
  on suggested packages and take more than a few seconds (MCMC sampling,
  bootstrapping, LMS estimation)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Ubuntu 24.04, R 4.4.3
* GitHub Actions: Ubuntu (R release, R devel), macOS (R release),
  Windows (R release)
