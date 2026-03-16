# rmedsem 1.0.0.9000 (development)

* Added CRAN badge to README.

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
