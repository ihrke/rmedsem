# Mediation Analysis for Structural Equation Models

Tests the indirect effect of an independent variable X on a dependent
variable Y through a mediator M in a fitted structural equation model
(SEM), and determines the type of mediation using the Baron and Kenny
(1986) and/or Zhao, Lynch & Chen (2010) approaches. Models estimated
with lavaan (covariance-based SEM), cSEM and plssem (PLS-SEM), blavaan
(Bayesian SEM) and modsem (models with latent interactions) are
supported. The model must contain the regression paths X -\> M, M -\> Y
and X -\> Y.

## Usage

``` r
rmedsem(
  mod,
  indep,
  med,
  dep,
  approach = c("bk", "zlc"),
  p.threshold = 0.05,
  effect.size = c("RIT", "RID", "upsilon"),
  ...
)

# Default S3 method
rmedsem(mod, indep, med, dep, ...)

# S3 method for class 'blavaan'
rmedsem(
  mod,
  indep,
  med,
  dep,
  approach = c("bk", "zlc"),
  p.threshold = 0.05,
  effect.size = c("RIT", "RID", "upsilon"),
  ci.two.tailed = 0.95,
  hdi = FALSE,
  ...
)

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
  seed = NULL,
  ...
)

# S3 method for class 'lavaan'
rmedsem(
  mod,
  indep,
  med,
  dep,
  approach = c("bk", "zlc"),
  p.threshold = 0.05,
  effect.size = c("RIT", "RID", "upsilon"),
  standardized = TRUE,
  mcreps = 5000,
  ci.two.tailed = 0.95,
  ...
)

# S3 method for class 'modsem'
rmedsem(
  mod,
  indep,
  med,
  dep,
  approach = c("bk", "zlc"),
  p.threshold = 0.05,
  effect.size = c("RIT", "RID", "upsilon"),
  moderator = NULL,
  standardized = TRUE,
  mcreps = 5000,
  ci.two.tailed = 0.95,
  ...
)

# S3 method for class 'PlsModel'
rmedsem(
  mod,
  indep,
  med,
  dep,
  approach = c("bk", "zlc"),
  p.threshold = 0.05,
  effect.size = c("RIT", "RID", "upsilon"),
  ci.two.tailed = 0.95,
  ...
)
```

## Arguments

- mod:

  a fitted SEM: an object of class `lavaan`, `cSEMResults`, `blavaan`,
  `modsem` or `PlsModel` (plssem). `blavaan` models containing latent
  variables must be fitted with `save.lvs = TRUE`; `PlsModel` objects
  must be fitted with `bootstrap = TRUE`.

- indep:

  a string, the name of the independent variable (X). For `modsem` and
  `PlsModel` models, this can be an interaction term such as `"W:X"`.

- med:

  a string, the name of the mediator (M)

- dep:

  a string, the name of the dependent variable (Y)

- approach:

  approach(es) to determine the type of mediation: `"bk"` (Baron and
  Kenny), `"zlc"` (Zhao, Lynch & Chen), or both (default). Ignored for
  `blavaan` models.

- p.threshold:

  a number between 0 and 1, the p-value threshold for significance
  (default 0.05). A p-value equal to the threshold counts as not
  significant.

- effect.size:

  character vector with the effect sizes to compute; one or more of
  `"RIT"` (ratio of indirect to total effect), `"RID"` (ratio of
  indirect to direct effect) and `"upsilon"` (Lachowicz et al., 2018);
  see
  [effect-sizes](https://ihrke.github.io/rmedsem/reference/effect-sizes.md).

- ...:

  additional arguments passed to methods (currently unused)

- ci.two.tailed:

  a number between 0 and 1, the level of all confidence (or, for
  `blavaan`, credible) intervals (default 0.95)

- hdi:

  (`blavaan`) a logical. If `FALSE` (default), equal-tailed credible
  intervals are computed; if `TRUE`, highest density intervals (requires
  the HDInterval package). Applies to the indirect, direct and total
  effects and to Upsilon.

- nbootstrap:

  (`cSEM`) the number of bootstrap samples (default 1000)

- seed:

  (`cSEM`) `NULL` (default) or a non-negative integer, the seed for the
  bootstrap. If `NULL`, the seed is drawn from R's random number
  generator, so that results can be reproduced with
  [`set.seed()`](https://rdrr.io/r/base/Random.html).

- standardized:

  (`lavaan`, `modsem`) a logical, whether to use standardized
  coefficients (default `TRUE`). `cSEM` and `blavaan` results are always
  standardized.

- mcreps:

  (`lavaan`, `modsem`) the number of Monte-Carlo samples, a positive
  integer (default 5000)

- moderator:

  (`modsem`) `NULL` (default) or a string, the name of the moderator W
  for moderated mediation. The model must contain an interaction of the
  moderator with `indep` and/or `med`.

## Value

an object of class `c("rmedsem_<pkg>", "rmedsem")`, where `<pkg>`
identifies the backend (`lavaan`, `cSEM`, `blavaan`, `modsem` or
`plssem`). See
[rmedsem-methods](https://ihrke.github.io/rmedsem/reference/rmedsem-methods.md)
for functions to print, summarize and extract results,
[effect-sizes](https://ihrke.github.io/rmedsem/reference/effect-sizes.md)
for effect sizes and
[`plot.rmedsem()`](https://ihrke.github.io/rmedsem/reference/plot.rmedsem.md)
for plots. The structure of the object is described in section 'Adding a
backend'.

## Backends

- `lavaan`:

  The indirect effect is tested with the Sobel, Delta and Monte-Carlo
  methods. The Zhao, Lynch & Chen approach is based on the Monte-Carlo
  test.

- `cSEMResults`:

  The model is re-estimated with `nbootstrap` bootstrap samples. The
  indirect effect is tested with the Sobel, Delta and bootstrap methods,
  and the Zhao, Lynch & Chen approach is based on the bootstrap test.
  Only single-group, first-order models are supported.

- `blavaan`:

  Estimates are based on the (standardized) posterior samples. The
  output reports posterior means, standard deviations, posterior
  probabilities of a positive and negative indirect effect, evidence
  ratios and credible intervals; the Baron and Kenny and Zhao, Lynch &
  Chen approaches are not applied.

- `modsem`:

  As for `lavaan`. In addition, moderated mediation (via `moderator`)
  and mediated moderation (an interaction term as `indep`) are
  supported.

- `PlsModel`:

  Models estimated with
  [`plssem::pls()`](https://kss2k.github.io/plssem/reference/pls.html)
  (PLS-SEM and consistent PLSc-SEM, including models with interaction
  terms and ordinal indicators). The model must be estimated with
  `bootstrap = TRUE`; the number of bootstrap samples is set in
  [`plssem::pls()`](https://kss2k.github.io/plssem/reference/pls.html)
  (`boot.R`), and results are reproducible with its `boot.iseed`
  argument. The indirect effect is tested with the Sobel, Delta and
  bootstrap methods, where the bootstrap test uses the bootstrap samples
  of plssem. The Zhao, Lynch & Chen approach is based on the bootstrap
  test. Mediated moderation (an interaction term as `indep`) is
  supported.

Multi-group and multilevel models are not supported.

## Adding a backend

Support for further model classes is added by writing a method
`rmedsem.<class>()` that returns a list of class
`c("rmedsem_<pkg>", "rmedsem")`. For the default
[`print.rmedsem()`](https://ihrke.github.io/rmedsem/reference/rmedsem-methods.md),
[`plot.rmedsem()`](https://ihrke.github.io/rmedsem/reference/plot.rmedsem.md)
and
[`as.data.frame.rmedsem()`](https://ihrke.github.io/rmedsem/reference/rmedsem-methods.md)
methods to work, the list must contain the following elements:

- `package`:

  name of the estimating package (character).

- `standardized`:

  whether the coefficients are standardized (logical).

- `vars`:

  list with elements `indep`, `med` and `dep`.

- `est.methods`:

  character vector naming the estimation methods for the indirect
  effect, e.g. `c("sobel", "delta", "montc")`.

- one element per entry in `est.methods`:

  a named numeric vector with elements `coef`, `se`, `zval`, `pval`,
  `lower` and `upper`. The Baron and Kenny approach requires the element
  `sobel`.

- `zlc.method`:

  (optional) the entry of `est.methods` whose p-value is used for the
  Zhao, Lynch & Chen approach; defaults to the last entry of
  `est.methods`.

- `direct.effect`:

  named numeric vector with elements `coef`, `se`, `pval`, `lower` and
  `upper`.

- `total.effect`:

  named numeric vector with elements `coef`, `se`, `lower` and `upper`.

- `med.approach`:

  character vector, a subset of `c("bk", "zlc")`.

- `med.data`:

  list with elements `sig_thresh` (the p-value threshold), `coefs` and
  `pvals`; the latter two are lists with elements `moi` (X -\> M), `dom`
  (M -\> Y) and `doi` (X -\> Y).

- `effect.size`:

  list with (a subset of) elements `RIT`, `RID` and `upsilon`, as
  returned by the built-in backends.

- `nobs`:

  (optional) number of observations, used by
  [`stats::nobs()`](https://rdrr.io/r/stats/nobs.html).

- `ci.level`:

  (optional) level of the stored intervals, used by
  [`stats::confint()`](https://rdrr.io/r/stats/confint.html) and
  [`summary()`](https://rdrr.io/r/base/summary.html); defaults to 0.95.

- `ci.type`:

  (optional) label of the stored intervals, `"CI"` (default) or `"HDI"`.

A backend whose output does not fit this scheme can provide its own
`print.rmedsem_<pkg>()` method, either replacing the default output (as
for `blavaan`) or extending it via
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) (as for
`modsem`).

## References

Baron, R. M., & Kenny, D. A. (1986). The moderator-mediator variable
distinction in social psychological research: Conceptual, strategic, and
statistical considerations. *Journal of Personality and Social
Psychology*, 51(6), 1173–1182.
[doi:10.1037/0022-3514.51.6.1173](https://doi.org/10.1037/0022-3514.51.6.1173)

Lachowicz, M. J., Preacher, K. J., & Kelley, K. (2018). A novel measure
of effect size for mediation analysis. *Psychological Methods*, 23(2),
244–261. [doi:10.1037/met0000165](https://doi.org/10.1037/met0000165)

Zhao, X., Lynch, J. G., & Chen, Q. (2010). Reconsidering Baron and
Kenny: Myths and truths about mediation analysis. *Journal of Consumer
Research*, 37(2), 197–206.
[doi:10.1086/651257](https://doi.org/10.1086/651257)

## Examples

``` r
## lavaan: observed variables
mod.txt <- "
read ~ math
science ~ read + math
"
mod <- lavaan::sem(mod.txt, data = rmedsem::hsbdemo)
out <- rmedsem(mod, indep = "math", med = "read", dep = "science")
out
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'math' -> 'read' -> 'science'
#> 
#>                          Sobel          Delta    Monte-Carlo
#> Indirect effect          0.251          0.251          0.251
#> Std. Err.                0.046          0.046          0.046
#> z-value                  5.501          5.446          5.453
#> p-value               3.79e-08       5.15e-08       4.95e-08
#> CI              [0.161, 0.340] [0.160, 0.341] [0.164, 0.342]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'math' -> 'read' (X -> M) with B=0.662 and p<0.001
#>    STEP 2 - 'read' -> 'science' (M -> Y) with B=0.378 and p<0.001
#>    STEP 3 - 'math' -> 'science' (X -> Y) with B=0.380 and p<0.001
#>             As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above
#>             are significant the mediation is partial.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'math' -> 'science' (X -> Y) with B=0.380 and p<0.001
#>             As the Monte-Carlo test above is significant, STEP 1 is
#>             significant and their coefficients point in same direction,
#>             there is complementary mediation (partial mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.251/0.631) = 0.397
#>          Meaning that about 40% of the effect of 'math'
#>          on 'science' is mediated by 'read'
#>    RID = (Indirect effect / Direct effect)
#>          (0.251/0.380) = 0.659
#>          That is, the mediated effect is about 0.7 times as
#>          large as the direct effect of 'math' on 'science'
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.063, v(adj) = 0.061
#> 

# Zhao, Lynch & Chen approach only, unstandardized coefficients
rmedsem(mod, indep = "math", med = "read", dep = "science",
        approach = "zlc", standardized = FALSE, mcreps = 5000)
#> Significance testing of indirect effect (unstandardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'math' -> 'read' -> 'science'
#> 
#>                          Sobel          Delta    Monte-Carlo
#> Indirect effect          0.265          0.265          0.265
#> Std. Err.                0.052          0.052          0.053
#> z-value                  5.073          5.073          5.050
#> p-value               3.91e-07       3.91e-07       4.41e-07
#> CI              [0.163, 0.367] [0.163, 0.367] [0.163, 0.373]
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'math' -> 'science' (X -> Y) with B=0.402 and p<0.001
#>             As the Monte-Carlo test above is significant, STEP 1 is
#>             significant and their coefficients point in same direction,
#>             there is complementary mediation (partial mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.265/0.667) = 0.397
#>          Meaning that about 40% of the effect of 'math'
#>          on 'science' is mediated by 'read'
#>    RID = (Indirect effect / Direct effect)
#>          (0.265/0.402) = 0.659
#>          That is, the mediated effect is about 0.7 times as
#>          large as the direct effect of 'math' on 'science'
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.063, v(adj) = 0.061
#> 

# \donttest{
## cSEM
if (requireNamespace("cSEM", quietly = TRUE)) {
  model <- "
    OwnLook  =~ smv_attr_face + smv_attr_body + smv_sexy
    SelfEst  =~ ses_satis + ses_qualities + ses_able_todo
    MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
    SelfEst  ~ OwnLook
    MentWell ~ OwnLook + SelfEst
  "
  mod <- cSEM::csem(rmedsem::mchoice, model)
  # small number of bootstrap samples to keep the example fast
  rmedsem(mod, indep = "OwnLook", med = "SelfEst", dep = "MentWell",
          nbootstrap = 200)
}
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'cSEM'
#> Mediation effect: 'OwnLook' -> 'SelfEst' -> 'MentWell'
#> 
#>                          Sobel          Delta      Bootstrap
#> Indirect effect          0.316          0.316          0.316
#> Std. Err.                0.032          0.033          0.034
#> z-value                  9.969          9.475          9.370
#> p-value                 <2e-16         <2e-16         <2e-16
#> CI              [0.254, 0.378] [0.250, 0.381] [0.262, 0.391]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'OwnLook' -> 'SelfEst' (X -> M) with B=0.578 and p<0.001
#>    STEP 2 - 'SelfEst' -> 'MentWell' (M -> Y) with B=0.546 and p<0.001
#>    STEP 3 - 'OwnLook' -> 'MentWell' (X -> Y) with B=0.088 and p=0.075
#>             As STEP 1, STEP 2 and the Sobel's test above are significant
#>             and STEP 3 is not significant the mediation is complete.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Bootstrap
#>   STEP 1 - 'OwnLook' -> 'MentWell' (X -> Y) with B=0.088 and p=0.075
#>             As the Bootstrap test above is significant and STEP 1 is not
#>             significant there is indirect-only mediation (full mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.316/0.404) = 0.781
#>          Meaning that about 78% of the effect of 'OwnLook'
#>          on 'MentWell' is mediated by 'SelfEst'
#>    RID = (Indirect effect / Direct effect)
#>          RID is not reported: direct effect 0.088 is not significant (p = 0.075)
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.100, v(adj) = 0.099
#> 

## modsem: mediated moderation and moderated mediation
if (requireNamespace("modsem", quietly = TRUE)) {
  m <- "
    OwnLook =~ smv_attr_face + smv_attr_body + smv_sexy
    SelfEst =~ ses_satis + ses_qualities + ses_able_todo
    MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
    smv =~ smv_kind + smv_caring + smv_understanding +
      smv_make_laughh + smv_funny + smv_sociable
    SelfEst ~ OwnLook + smv + smv:OwnLook
    MentWell ~ OwnLook + SelfEst + smv + smv:OwnLook
  "
  est <- modsem::modsem(m, data = rmedsem::mchoice, method = "lms")

  # mediated moderation
  rmedsem(est, indep = "smv:OwnLook", med = "SelfEst", dep = "MentWell")

  # moderated mediation
  rmedsem(est, indep = "OwnLook", med = "SelfEst", dep = "MentWell",
          moderator = "smv")
}
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'modsem'
#> Mediation effect: 'OwnLook' -> 'SelfEst' -> 'MentWell'
#> 
#>                          Sobel          Delta    Monte-Carlo
#> Indirect effect          0.253          0.253          0.253
#> Std. Err.                0.029          0.029          0.029
#> z-value                  8.847          8.836          8.715
#> p-value                 <2e-16         <2e-16         <2e-16
#> CI              [0.197, 0.309] [0.197, 0.309] [0.199, 0.311]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'OwnLook' -> 'SelfEst' (X -> M) with B=0.486 and p<0.001
#>    STEP 2 - 'SelfEst' -> 'MentWell' (M -> Y) with B=0.521 and p<0.001
#>    STEP 3 - 'OwnLook' -> 'MentWell' (X -> Y) with B=0.011 and p=0.809
#>             As STEP 1, STEP 2 and the Sobel's test above are significant
#>             and STEP 3 is not significant the mediation is complete.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'OwnLook' -> 'MentWell' (X -> Y) with B=0.011 and p=0.809
#>             As the Monte-Carlo test above is significant and STEP 1 is not
#>             significant there is indirect-only mediation (full mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.253/0.265) = 0.957
#>          Meaning that about 96% of the effect of 'OwnLook'
#>          on 'MentWell' is mediated by 'SelfEst'
#>    RID = (Indirect effect / Direct effect)
#>          RID is not reported: direct effect 0.011 is not significant (p = 0.809)
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.064, v(adj) = 0.063
#> 
#> 
#> Direct moderation effects
#>    OwnLook -> SelfEst             | smv: B = -0.136, se = 0.029, p = 0.000
#>    OwnLook -> MentWell            | smv: B = -0.008, se = 0.034, p = 0.813
#> 
#> Indirect moderation effect
#>    OwnLook -> SelfEst -> MentWell | smv: B = -0.071, se = 0.017, p = 0.000
#> 
#> Total moderation effect
#>    OwnLook -> MentWell            | smv: B = -0.079, se = 0.036, p = 0.026
#> 

## plssem (PLS-SEM)
if (requireNamespace("plssem", quietly = TRUE)) {
  model <- "
    OwnLook  =~ smv_attr_face + smv_attr_body + smv_sexy
    SelfEst  =~ ses_satis + ses_qualities + ses_able_todo
    MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
    SelfEst  ~ OwnLook
    MentWell ~ OwnLook + SelfEst
  "
  # small number of bootstrap samples to keep the example fast
  fit <- plssem::pls(model, rmedsem::mchoice, bootstrap = TRUE,
                     boot.R = 200, boot.iseed = 1)
  rmedsem(fit, indep = "OwnLook", med = "SelfEst", dep = "MentWell")
}
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'plssem'
#> Mediation effect: 'OwnLook' -> 'SelfEst' -> 'MentWell'
#> 
#>                          Sobel          Delta      Bootstrap
#> Indirect effect          0.316          0.316          0.316
#> Std. Err.                0.031          0.032          0.032
#> z-value                 10.152          9.915          9.828
#> p-value                 <2e-16         <2e-16         <2e-16
#> CI              [0.255, 0.377] [0.253, 0.378] [0.261, 0.388]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'OwnLook' -> 'SelfEst' (X -> M) with B=0.578 and p<0.001
#>    STEP 2 - 'SelfEst' -> 'MentWell' (M -> Y) with B=0.546 and p<0.001
#>    STEP 3 - 'OwnLook' -> 'MentWell' (X -> Y) with B=0.088 and p=0.031
#>             As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above
#>             are significant the mediation is partial.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Bootstrap
#>   STEP 1 - 'OwnLook' -> 'MentWell' (X -> Y) with B=0.088 and p=0.031
#>             As the Bootstrap test above is significant, STEP 1 is
#>             significant and their coefficients point in same direction,
#>             there is complementary mediation (partial mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.316/0.404) = 0.781
#>          Meaning that about 78% of the effect of 'OwnLook'
#>          on 'MentWell' is mediated by 'SelfEst'
#>    RID = (Indirect effect / Direct effect)
#>          (0.316/0.088) = 3.570
#>          That is, the mediated effect is about 3.6 times as
#>          large as the direct effect of 'OwnLook' on 'MentWell'
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.100, v(adj) = 0.099
#> 

## blavaan
if (requireNamespace("blavaan", quietly = TRUE)) {
  # blavaan's fitting functions need the package to be attached
  library(blavaan)
  # short single chain to keep the example fast; use more chains and
  # iterations in practice
  bmod <- bsem(mod.txt, data = rmedsem::hsbdemo, n.chains = 1,
               burnin = 500, sample = 500, seed = 1,
               bcontrol = list(refresh = 0))
  rmedsem(bmod, indep = "math", med = "read", dep = "science")

  # highest density intervals instead of equal-tailed intervals
  if (requireNamespace("HDInterval", quietly = TRUE))
    rmedsem(bmod, indep = "math", med = "read", dep = "science", hdi = TRUE)
}
#> Loading required package: Rcpp
#> This is blavaan 0.6-1
#> On multicore systems, we suggest use of future::plan("multicore") or
#>   future::plan("multisession") for faster post-MCMC computations.
#> Computing post-estimation metrics (including lvs if requested)...
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'blavaan'
#> Mediation effect: 'math' -> 'read' -> 'science'
#> 
#> Prior (regression coefs): normal(0,10)
#>                          Bayes
#> Indirect effect          0.246
#> Posterior SD             0.044
#> P(>0)                    1.000
#> P(<0)                    0.000
#> ER+                          ∞
#> ER-                          0
#> HDI             [0.172, 0.338]
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.246/0.629) = 0.391
#>          Meaning that about 39% of the effect of 'math'
#>          on 'science' is mediated by 'read'
#>    RID = (Indirect effect / Direct effect)
#>          (0.246/0.383) = 0.643
#>          That is, the mediated effect is about 0.6 times as
#>          large as the direct effect of 'math' on 'science'
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.061, v(adj) = 0.059
#>          Posterior mean(v) = 0.063, median(v) = 0.061
#>          95% HDI [0.024, 0.104]
#> 
# }
```
