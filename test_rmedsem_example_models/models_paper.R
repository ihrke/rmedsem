library(lavaan)
library(modsem) # latest github version
devtools::load_all()


m1 <-  "
  OwnLook =~ smv_attr_face + smv_attr_body + smv_sexy
  SelfEst =~ ses_satis + ses_qualities + ses_able_todo
  MentWell =~ mwb_optimistic + mwb_useful + mwb_energy

  SelfEst ~ OwnLook
  MentWell ~ OwnLook + SelfEst
"

est1 <- sem(m1, data = mchoice)

rmedsem(indep="OwnLook", dep="MentWell", med="SelfEst", mod=est1)


m <- "
  OwnLook =~ smv_attr_face + smv_attr_body + smv_sexy
  SelfEst =~ ses_satis + ses_qualities + ses_able_todo
  MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
  smv =~ smv_kind + smv_caring + smv_understanding +
    smv_make_laughh + smv_funny + smv_sociable
  SelfEst ~ OwnLook + smv + smv:OwnLook
  MentWell ~ OwnLook + SelfEst + smv + smv:OwnLook
"


est <- modsem(m, data = mchoice, method="lms")
rmedsem(indep="smv:OwnLook", dep="MentWell", med="SelfEst", mod=est)
rmedsem(indep="OwnLook", dep="MentWell", med="SelfEst", mod=est,
        moderator="smv")


#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'modsem'
#> Mediation effect: 'OwnLook' -> 'SelfEst' -> 'MentWell'
#> 
#>                          Sobel          Delta   Monte-Carlo
#> Indirect effect         0.2542         0.2542        0.2542
#> Std. Err.               0.0394         0.0425        0.0397
#> z-value                 6.4596         5.9771        6.3959
#> p-value               1.05e-10       2.27e-09       1.6e-10
#> CI              [0.177, 0.331] [0.171, 0.338] [0.179, 0.33]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'OwnLook:SelfEst' (X -> M) with B=0.487 and p=0.000
#>    STEP 2 - 'SelfEst:MentWell' (M -> Y) with B=0.521 and p=0.000
#>    STEP 3 - 'OwnLook:MentWell' (X -> Y) with B=0.011 and p=0.844
#>             As STEP 1, STEP 2 and the Sobel's test above are significant
#>             and STEP 3 is not significant the mediation is complete.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'OwnLook:MentWell' (X -> Y) with B=0.011 and p=0.844
#>             As the Monte-Carlo test above is significant and STEP 1 is not
#>             significant there indirect-only mediation (full mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.254/0.265) = 0.958
#>          Meaning that about  96% of the effect of 'OwnLook'
#>          on 'MentWell' is mediated by 'SelfEst'
#>    RID = (Indirect effect / Direct effect)
#>          (0.254/0.011) = 22.610
#>          That is, the mediated effect is about 22.6 times as
#>          large as the direct effect of 'OwnLook' on 'MentWell'
#> 
#> Direct moderation effects
#>    SelfEst  -> OwnLook  | smv: B = -0.13, p = 0.000, ci = [-0.20,-0.06]
#>    MentWell -> OwnLook  | smv: B = -0.01, p = 0.841, ci = [-0.08, 0.07]
#> 
#> Indirect moderation effect
#>    SelfEst  -> OwnLook  | smv: B = -0.07, p = 0.000, ci = [-0.09,-0.05]
#> 
#> Total moderation effect
#>    SelfEst  -> OwnLook  | smv: B = -0.08, p = 0.000, ci = [-0.10,-0.06]
#> 
