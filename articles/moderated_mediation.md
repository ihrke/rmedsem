# Moderated mediation and mediated moderation using rmedsem

It is often of interest to assess whether a mediation effect is
invariant across contexts, or whether its strength depends on the level
of another variable. In such cases, the indirect effect of an
independent variable on an outcome via a mediator is moderated by a
third variable (the moderator). These types of effects are often
referred to as moderated mediation or mediated moderation effects
(Preacher et al., 2007).

Currently, `rmedsem` implements moderated mediation and mediated
moderation. Mediated moderation is available for cases corresponding to
“Model 2” from Preacher et al., 2007 shown in the following graph:

![](moderated_mediation_files/figure-html/unnamed-chunk-2-1.png)

Mathematically, this model can be specified as

``` math
\begin{align*}
M &= a_0 + a_1 X + a_2 W + a_3 (X \times W) + \varepsilon_M\\
Y &= b_0 + b_1 M + b_2 X + b_3 (X \times W) + \varepsilon_Y
\end{align*}
```

where $`X`$ is the independent variable, $`Y`$ is the dependent
variable, $`M`$ is the mediator, and $`W`$ is the moderator. The
interaction term $`X \times W`$ is included in the equations for $`M`$
and $`Y`$.

To estimate moderated mediation and mediated moderation with `rmedsem`,
we need to specify the model using the [`modsem`](https://modsem.org)
package which allows to estimate structural equation models with
interaction terms using `lavaan`. We specify and estimate the following
model (corresponding to the conceptual model above) using the dataset
[`rmedsem::mchoice`](https://ihrke.github.io/rmedsem/reference/mchoice.md):

``` r

library(modsem)

model <- "
  OwnLook =~ smv_attr_face + smv_attr_body + smv_sexy
  SelfEst =~ ses_satis + ses_qualities + ses_able_todo
  MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
  OwnPers =~ smv_kind + smv_caring + smv_understanding +
    smv_make_laughh + smv_funny + smv_sociable

  MentWell ~ OwnLook + SelfEst + OwnPers + OwnPers:OwnLook
  SelfEst ~ OwnLook + OwnPers + OwnPers:OwnLook
"

est <- modsem(model, data = rmedsem::mchoice, method="lms")
```

## Mediated Moderation

*Mediated moderation* treats the interaction term `X:W` as the
independent variable and asks whether its effect on `Y` is carried
through `M`. In our example, we can test whether the interaction between
`OwnLook` and `OwnPers` on `MentWell` is mediated by `SelfEst`.

``` r

library(rmedsem)
rmedsem(indep="OwnPers:OwnLook", dep="MentWell", med="SelfEst", mod=est)
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'modsem'
#> Mediation effect: 'OwnPers:OwnLook' -> 'SelfEst' -> 'MentWell'
#> 
#>                            Sobel            Delta      Monte-Carlo
#> Indirect effect           -0.081           -0.081           -0.081
#> Std. Err.                  0.020            0.020            0.020
#> z-value                   -4.019           -4.020           -4.033
#> p-value                 5.84e-05         5.82e-05         5.50e-05
#> CI              [-0.121, -0.042] [-0.121, -0.042] [-0.121, -0.043]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'OwnPers:OwnLook' -> 'SelfEst' (X -> M) with B=-0.155 and p<0.001
#>    STEP 2 - 'SelfEst' -> 'MentWell' (M -> Y) with B=0.521 and p<0.001
#>    STEP 3 - 'OwnPers:OwnLook' -> 'MentWell' (X -> Y) with B=-0.007 and p=0.812
#>             As STEP 1, STEP 2 and the Sobel's test above are significant
#>             and STEP 3 is not significant the mediation is complete.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'OwnPers:OwnLook' -> 'MentWell' (X -> Y) with B=-0.007 and p=0.812
#>             As the Monte-Carlo test above is significant and STEP 1 is not
#>             significant there is indirect-only mediation (full mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          RIT is not reported: total effect 0.088 is too small (< 0.2)
#>    RID = (Indirect effect / Direct effect)
#>          RID is not reported: direct effect 0.007 is not significant (p = 0.812)
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.007, v(adj) = 0.006
```

## Moderated Mediation

*Moderated mediation* asks whether the indirect effect of an exposure
`X` on outcome `Y` via mediator `M` varies across levels of a third
variable `W`.  
Using the model from the previous example, we test how the indirect path
from `OwnLook` to `MentWell` through `SelfEst` depends on `OwnPers`.

``` r

rmedsem(indep="OwnLook", dep="MentWell", med="SelfEst", mod=est,
        moderator="OwnPers")
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'modsem'
#> Mediation effect: 'OwnLook' -> 'SelfEst' -> 'MentWell'
#> 
#>                          Sobel          Delta    Monte-Carlo
#> Indirect effect          0.253          0.253          0.253
#> Std. Err.                0.028          0.028          0.028
#> z-value                  8.897          8.886          8.916
#> p-value                 <2e-16         <2e-16         <2e-16
#> CI              [0.197, 0.309] [0.197, 0.309] [0.198, 0.311]
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
#>    OwnLook -> SelfEst             | OwnPers: B = -0.155, se = 0.037, p = 0.000
#>    OwnLook -> MentWell            | OwnPers: B = -0.007, se = 0.030, p = 0.812
#> 
#> Indirect moderation effect
#>    OwnLook -> SelfEst -> MentWell | OwnPers: B = -0.081, se = 0.020, p = 0.000
#> 
#> Total moderation effect
#>    OwnLook -> MentWell            | OwnPers: B = -0.088, se = 0.034, p = 0.009
```

In this case the difference between a *moderated mediation* and
*mediated moderation* is purely semantic. Indeed, the indirect and total
moderation effect when interpreted as a moderated mediation is the exact
same as the indirect and total effect in the previous example ($`.07`$
and $`.08`$).

That being said, moderated mediations can be more complex in nature than
mediated moderations, where the moderating variable `W` can affect the
paths of model differently. Here we can for example see a moderated
mediation where `OwnPers` not only affects the path from `OwnLook` to
`SelfEst` and `OwnLook` to `MentWell`, but also the path from `SelfEst`
to `MentWell`.

``` r

model2 <- "
  OwnLook =~ smv_attr_face + smv_attr_body + smv_sexy
  SelfEst =~ ses_satis + ses_qualities + ses_able_todo
  MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
  OwnPers =~ smv_kind + smv_caring + smv_understanding +
    smv_make_laughh + smv_funny + smv_sociable

  SelfEst ~ OwnLook + OwnPers + OwnPers:OwnLook
  MentWell ~ OwnLook + SelfEst + OwnPers + OwnPers:OwnLook + OwnPers:SelfEst
"

est2 <- modsem(model2, data = rmedsem::mchoice, method="lms")
```

[`rmedsem()`](https://ihrke.github.io/rmedsem/reference/rmedsem.md) will
automatically detect the paths wich are moderated by the `moderator` and
tailor the output accordingly.

``` r

rmedsem(indep="OwnLook", dep="MentWell", med="SelfEst", mod=est2,
        moderator="OwnPers")
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'modsem'
#> Mediation effect: 'OwnLook' -> 'SelfEst' -> 'MentWell'
#> 
#>                          Sobel          Delta    Monte-Carlo
#> Indirect effect          0.250          0.250          0.250
#> Std. Err.                0.028          0.028          0.028
#> z-value                  8.820          8.873          8.866
#> p-value                 <2e-16         <2e-16         <2e-16
#> CI              [0.195, 0.306] [0.195, 0.306] [0.197, 0.306]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'OwnLook' -> 'SelfEst' (X -> M) with B=0.488 and p<0.001
#>    STEP 2 - 'SelfEst' -> 'MentWell' (M -> Y) with B=0.513 and p<0.001
#>    STEP 3 - 'OwnLook' -> 'MentWell' (X -> Y) with B=0.031 and p=0.523
#>             As STEP 1, STEP 2 and the Sobel's test above are significant
#>             and STEP 3 is not significant the mediation is complete.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'OwnLook' -> 'MentWell' (X -> Y) with B=0.031 and p=0.523
#>             As the Monte-Carlo test above is significant and STEP 1 is not
#>             significant there is indirect-only mediation (full mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.250/0.281) = 0.891
#>          Meaning that about 89% of the effect of 'OwnLook'
#>          on 'MentWell' is mediated by 'SelfEst'
#>    RID = (Indirect effect / Direct effect)
#>          RID is not reported: direct effect 0.031 is not significant (p = 0.523)
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.063, v(adj) = 0.062
#> 
#> 
#> Direct moderation effects
#>    OwnLook -> SelfEst             | OwnPers: B = -0.137, se = 0.029, p = 0.000
#>    SelfEst -> MentWell            | OwnPers: B = 0.082, se = 0.045, p = 0.069
#>    OwnLook -> MentWell            | OwnPers: B = -0.085, se = 0.054, p = 0.116
#> 
#> Indirect moderation effect
#>    OwnLook -> SelfEst -> MentWell | OwnPers: B = -0.042, se = 0.024, p = 0.082
#> 
#> Total moderation effect
#>    OwnLook -> MentWell            | OwnPers: B = -0.126, se = 0.045, p = 0.005
```
