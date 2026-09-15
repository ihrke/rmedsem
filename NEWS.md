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
* modsem: the moderation effects are now printed in causal direction
  (e.g., `OwnLook -> SelfEst` instead of `SelfEst -> OwnLook`), the indirect
  moderation effect is labelled `X -> M -> Y` and the total moderation effect
  `X -> Y` (previously it reused the label of the indirect effect). The
  `lhs`/`rhs` elements of `$moderation` now hold the unpadded variable names
  (lavaan convention `lhs ~ rhs`); `$moderation$indirect.effect` gains `med`.
* Fixed the blavaan output table, whose rows were shifted relative to the
  labels (e.g., `P(z>0)` and `P(z<0)` were swapped and the interval was
  wrong). The interval is now labelled `CI` (it is an equal-tailed quantile
  interval, not an HDI).
* The Baron and Kenny and Zhao, Lynch & Chen conclusions are now printed in
  all cases; a p-value equal to `p.threshold` counts as not significant.
* `digits` and `indent` are now passed correctly to the effect-size output.
* All exported functions now check their arguments and stop with
  informative messages:
  * `rmedsem()` checks that `indep`, `med` and `dep` are distinct variables
    in the model and that the paths X -> M, M -> Y and X -> Y are estimated
    (lavaan, blavaan, cSEM and modsem). Multi-group/multilevel lavaan models
    and multi-group cSEM models are rejected.
  * `standardized`, `mcreps`, `nbootstrap`, `ci.two.tailed` and `moderator`
    are validated. A `mcreps` value below the sample size is raised to the
    sample size with a message (previously silently).
  * `print()` checks `digits` and `indent`, `plot_effect()` checks
    `description`, `Upsilon()` checks `adjusted`.
  * New `rmedsem.default()`, `RIT.default()`, `RID.default()` and
    `Upsilon.default()` methods give informative errors for unsupported
    objects.
  * modsem: an unknown moderator, or a moderator without interaction term,
    is now an error (previously silently ignored).
* lavaan and modsem models with labelled regression paths (e.g.,
  `read ~ a*math`) now work (previously "subscript out of bounds").
* lavaan: a warning is given if the model did not converge.
* `summary()` now returns a `summary.rmedsem` object (with a compact
  `print` method) containing a table of indirect, direct and total effects,
  the type of mediation according to Baron & Kenny and Zhao, Lynch & Chen, and
  the effect sizes. Previously it only printed the `rmedsem` object.
* New methods `coef()`, `confint()` (with arguments `method` and `level`) and
  `nobs()` for `rmedsem` objects. Results now store `nobs` and `ci.level`.
* `as.data.frame()` now returns a plain data frame instead of a tibble.
* `ci.two.tailed` now also determines the Monte-Carlo intervals of the
  indirect and total effects (lavaan, modsem); previously these were always
  95% intervals. `rmedsem.blavaan()` gains `ci.two.tailed` (previously fixed
  at 95%).
* blavaan: the direct effect's `pval` is now the posterior probability of the
  opposite sign, consistent with the indirect effect (previously
  `P(direct <= 0)`, which is wrong for negative effects).
* Examples: `rmedsem.blavaan()` now has a runnable example (`\donttest{}`
  instead of `\dontrun{}`), `rmedsem.cSEMResults()` has a new example, and
  the datasets `hsbdemo`, `mchoice` and `workout` have examples.
* Corrected the documentation of the `hsbdemo` (200 rows; all columns now
  described) and `mchoice` (1067 rows, 15 columns) datasets.
* `plot_coef()` no longer emits a `position_dodge()` warning.
* cSEM: the bootstrap confidence intervals of the indirect and total effects
  now respect `ci.two.tailed` (previously always 95%), and the correct row is
  selected in models with several indirect effects.

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
