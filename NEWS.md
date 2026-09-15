# rmedsem 1.0.0.9000 (development)

* Added CRAN badge to README.
* Printing is now an extensible S3 method system: `print.rmedsem()` handles
  all frequentist backends generically (one table column per entry of
  `est.methods`), `print.rmedsem_blavaan()` and `print.rmedsem_modsem()` are
  registered methods. New backends need no changes to the printing code; the
  required fields of an `rmedsem` object are documented in `?rmedsem`.
* New result field `zlc.method` naming the method whose p-value is used for
  the Zhao, Lynch & Chen approach.
* `print(<modsem result>, ci_moderation = TRUE)` now works.
* Fixed the blavaan output table, whose rows were shifted relative to the
  labels (e.g., `P(z>0)` and `P(z<0)` were swapped and the interval was
  wrong). The interval is now labelled `CI` (it is an equal-tailed quantile
  interval, not an HDI).
* The Baron and Kenny and Zhao, Lynch & Chen conclusions are now printed in
  all cases; a p-value equal to `p.threshold` counts as not significant.
* `digits` and `indent` are now passed correctly to the effect-size output.

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
