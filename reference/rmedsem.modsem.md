# Mediation Analysis for Modsem Models

Mediation Analysis for Modsem Models

## Usage

``` r
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
  mcreps = NULL,
  ci.two.tailed = 0.95,
  ...
)
```

## Arguments

- mod:

  A fitted SEM model (modsem).

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

- moderator:

  A string indicating the name of the moderator variable in the model.

- standardized:

  A boolean indicating whether the coefficients should be standardized.
  The default value is FALSE.

- mcreps:

  An integer determining the number of monte-carlo samples.

- ci.two.tailed:

  A double giving the confidence level for two-tailed confidence
  intervals (default 0.95)

- ...:

  additional arguments (currently unused)

## Value

A `rmedsem` structure containing the results from the analysis

## Examples

``` r
# \donttest{
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

  est <- modsem::modsem(m, data = mchoice, method="lms")

  # mediated moderation
  rmedsem(indep="smv:OwnLook", dep="MentWell", med="SelfEst", mod=est)

  # moderated mediation
  rmedsem(indep="OwnLook", dep="MentWell", med="SelfEst", mod=est, moderator="smv")
}
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'modsem'
#> Mediation effect: 'OwnLook' -> 'SelfEst' -> 'MentWell'
#> 
#>                          Sobel          Delta    Monte-Carlo
#> Indirect effect         0.2532         0.2532         0.2532
#> Std. Err.               0.0287         0.0287         0.0283
#> z-value                 8.8217         8.8103         8.9114
#> p-value                      0              0              0
#> CI              [0.197, 0.309] [0.197, 0.309] [0.203, 0.311]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'OwnLook:SelfEst' (X -> M) with B=0.486 and p=0.000
#>    STEP 2 - 'SelfEst:MentWell' (M -> Y) with B=0.521 and p=0.000
#>    STEP 3 - 'OwnLook:MentWell' (X -> Y) with B=0.011 and p=0.809
#>             As STEP 1, STEP 2 and the Sobel's test above are significant
#>             and STEP 3 is not significant the mediation is complete.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'OwnLook:MentWell' (X -> Y) with B=0.011 and p=0.809
#>             As the Monte-Carlo test above is significant and STEP 1 is not
#>             significant there indirect-only mediation (full mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.253/0.265) = 0.957
#>          Meaning that about  96% of the effect of 'OwnLook'
#>          on 'MentWell' is mediated by 'SelfEst'
#>    RID = (Indirect effect / Direct effect)
#>          (0.253/0.011) = 22.232
#>          That is, the mediated effect is about 22.2 times as
#>          large as the direct effect of 'OwnLook' on 'MentWell'
#>    Upsilon (v) = Variance in 'MentWell' explained indirectly by 'OwnLook' through 'SelfEst'
#>          v(unadj) = 0.064, v(adj) = 0.063
#> 
#> 
#> Direct moderation effects
#>    SelfEst  -> OwnLook  | smv: B = -0.136, se = 0.029, p = 0.000
#>    MentWell -> OwnLook  | smv: B = -0.008, se = 0.034, p = 0.821
#> 
#> Indirect moderation effect
#>    SelfEst  -> OwnLook  | smv: B = -0.071, se = 0.017, p = 0.000
#> 
#> Total moderation effect
#>    SelfEst  -> OwnLook  | smv: B = -0.079, se = 0.036, p = 0.030
#> 
# }
```
