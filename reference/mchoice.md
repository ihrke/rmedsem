# Mate-Choice Survey Data from Trondheim

Data from a survey on mate-choice in Trondheim, collected in 2021 using
a convenience sampling procedure. Participants rated how well physical
features (`smv_attr_face`, `smv_attr_body`, `smv_sexy`) and personality
features (`smv_kind` to `smv_sociable`) described them as romantic
partners, answered items on self-esteem (`ses_*`) and reported how often
they had experienced feelings related to mental well-being in the past
two weeks (`mwb_*`). All items were measured on a scale from 1 to 5.

## Usage

``` r
mchoice
```

## Format

### `mchoice`

A data frame with 1067 rows and 15 columns:

- smv_attr_face:

  How well does this describe you as a partner? (attractive face), \[1\]
  very bad - \[5\] very well

- smv_attr_body:

  How well does this describe you as a partner? (attractive body), \[1\]
  very bad - \[5\] very well

- smv_sexy:

  How well does this describe you as a partner? (sexy), \[1\] very bad -
  \[5\] very well

- ses_satis:

  On the whole, I am satisfied with myself, \[1\] totally disagree -
  \[5\] totally agree

- ses_qualities:

  I feel that I have a number of good qualities, \[1\] totally
  disagree - \[5\] totally agree

- ses_able_todo:

  I am able to do things as well as most other people, \[1\] totally
  disagree - \[5\] totally agree

- mwb_optimistic:

  I have been feeling optimistic about the future, \[1\] never - \[5\]
  always

- mwb_useful:

  I have been feeling useful, \[1\] never - \[5\] always

- mwb_energy:

  I have had energy to spare, \[1\] never - \[5\] always

- smv_kind:

  How well does this describe you as a partner? (kind), \[1\] very bad -
  \[5\] very well

- smv_caring:

  How well does this describe you as a partner? (caring), \[1\] very
  bad - \[5\] very well

- smv_understanding:

  How well does this describe you as a partner? (understanding), \[1\]
  very bad - \[5\] very well

- smv_make_laughh:

  How well does this describe you as a partner? (make people laugh),
  \[1\] very bad - \[5\] very well

- smv_funny:

  How well does this describe you as a partner? (funny), \[1\] very
  bad - \[5\] very well

- smv_sociable:

  How well does this describe you as a partner? (sociable), \[1\] very
  bad - \[5\] very well

## Source

Survey collected in Norway in 2021 using a convenience sampling
procedure.

## Examples

``` r
str(mchoice)
#> tibble [1,067 × 15] (S3: tbl_df/tbl/data.frame)
#>  $ smv_attr_face    : num [1:1067] 1 2 5 3 4 3 5 3 2 2 ...
#>   ..- attr(*, "format.spss")= chr "F8.2"
#>   ..- attr(*, "label")= chr "How well does this describe you as a partner?"
#>   ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
#>   .. ..- attr(*, "names")= chr [1:5] "very bad" "" "" "" ...
#>  $ smv_attr_body    : num [1:1067] 1 2 5 4 3 3 5 4 1 2 ...
#>   ..- attr(*, "format.spss")= chr "F8.2"
#>   ..- attr(*, "label")= chr "How well does this describe you as a partner?"
#>   ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
#>   .. ..- attr(*, "names")= chr [1:5] "very bad" "" "" "" ...
#>  $ smv_sexy         : num [1:1067] 2 2 4 4 3 3 5 3 1 2 ...
#>   ..- attr(*, "format.spss")= chr "F8.2"
#>   ..- attr(*, "label")= chr "How well does this describe you as a partner?"
#>   ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
#>   .. ..- attr(*, "names")= chr [1:5] "very bad" "" "" "" ...
#>  $ ses_satis        : num [1:1067] 3 2 5 4 5 4 5 4 1 1 ...
#>   ..- attr(*, "format.spss")= chr "F8.2"
#>   ..- attr(*, "label")= chr "On the whole, I am satisfied with myself"
#>   ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
#>   .. ..- attr(*, "names")= chr [1:5] "totally disgree" "" "" "" ...
#>  $ ses_qualities    : num [1:1067] 4 4 5 3 5 4 5 4 3 1 ...
#>   ..- attr(*, "format.spss")= chr "F8.2"
#>   ..- attr(*, "label")= chr "I feel that I have a number of good qualities"
#>   ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
#>   .. ..- attr(*, "names")= chr [1:5] "totally disgree" "" "" "" ...
#>  $ ses_able_todo    : num [1:1067] 2 5 5 4 4 4 4 4 4 1 ...
#>   ..- attr(*, "format.spss")= chr "F8.2"
#>   ..- attr(*, "label")= chr "I am able to do things as well as most other people"
#>   ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
#>   .. ..- attr(*, "names")= chr [1:5] "totally disgree" "" "" "" ...
#>  $ mwb_optimistic   : num [1:1067] 2 5 5 4 3 3 5 3 4 2 ...
#>   ..- attr(*, "format.spss")= chr "F8.2"
#>   ..- attr(*, "label")= chr "I have been feeling optimistic about the future"
#>   ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
#>   .. ..- attr(*, "names")= chr [1:5] "never" "" "" "" ...
#>  $ mwb_useful       : num [1:1067] 1 5 4 4 3 3 5 3 2 2 ...
#>   ..- attr(*, "format.spss")= chr "F8.2"
#>   ..- attr(*, "label")= chr "I have been feeling useful"
#>   ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
#>   .. ..- attr(*, "names")= chr [1:5] "never" "" "" "" ...
#>  $ mwb_energy       : num [1:1067] 2 3 3 4 4 2 5 3 2 1 ...
#>   ..- attr(*, "format.spss")= chr "F8.2"
#>   ..- attr(*, "label")= chr "I have had energy to spare"
#>   ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
#>   .. ..- attr(*, "names")= chr [1:5] "never" "" "" "" ...
#>  $ smv_kind         : num [1:1067] 5 4 5 5 5 4 5 5 3 5 ...
#>   ..- attr(*, "format.spss")= chr "F8.2"
#>   ..- attr(*, "label")= chr "How well does this describe you as a partner?"
#>   ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
#>   .. ..- attr(*, "names")= chr [1:5] "very bad" "" "" "" ...
#>  $ smv_caring       : num [1:1067] 5 5 4 4 5 5 5 5 4 5 ...
#>   ..- attr(*, "format.spss")= chr "F8.2"
#>   ..- attr(*, "label")= chr "How well does this describe you as a partner?"
#>   ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
#>   .. ..- attr(*, "names")= chr [1:5] "very bad" "" "" "" ...
#>  $ smv_understanding: num [1:1067] 4 5 5 4 4 4 5 5 4 5 ...
#>   ..- attr(*, "format.spss")= chr "F8.2"
#>   ..- attr(*, "label")= chr "How well does this describe you as a partner?"
#>   ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
#>   .. ..- attr(*, "names")= chr [1:5] "very bad" "" "" "" ...
#>  $ smv_make_laughh  : num [1:1067] 5 4 3 3 3 3 5 3 3 4 ...
#>   ..- attr(*, "format.spss")= chr "F8.2"
#>   ..- attr(*, "label")= chr "How well does this describe you as a partner?"
#>   ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
#>   .. ..- attr(*, "names")= chr [1:5] "very bad" "" "" "" ...
#>  $ smv_funny        : num [1:1067] 5 4 5 3 3 3 5 3 4 4 ...
#>   ..- attr(*, "format.spss")= chr "F8.2"
#>   ..- attr(*, "label")= chr "How well does this describe you as a partner?"
#>   ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
#>   .. ..- attr(*, "names")= chr [1:5] "very bad" "" "" "" ...
#>  $ smv_sociable     : num [1:1067] 3 3 4 3 2 4 5 2 4 3 ...
#>   ..- attr(*, "format.spss")= chr "F8.2"
#>   ..- attr(*, "label")= chr "How well does this describe you as a partner?"
#>   ..- attr(*, "labels")= Named num [1:5] 1 2 3 4 5
#>   .. ..- attr(*, "names")= chr [1:5] "very bad" "" "" "" ...

mod.txt <- "
  OwnLook  =~ smv_attr_face + smv_attr_body + smv_sexy
  SelfEst  =~ ses_satis + ses_qualities + ses_able_todo
  MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
  SelfEst  ~ OwnLook
  MentWell ~ OwnLook + SelfEst
"
mod <- lavaan::sem(mod.txt, data = mchoice)
rmedsem(mod, indep = "OwnLook", med = "SelfEst", dep = "MentWell")
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'OwnLook' -> 'SelfEst' -> 'MentWell'
#> 
#>                          Sobel          Delta    Monte-Carlo
#> Indirect effect          0.327          0.327          0.327
#> Std. Err.                0.027          0.029          0.029
#> z-value                 12.132         11.419         11.323
#> p-value                 <2e-16         <2e-16         <2e-16
#> CI              [0.274, 0.380] [0.271, 0.384] [0.273, 0.386]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'OwnLook' -> 'SelfEst' (X -> M) with B=0.583 and p<0.001
#>    STEP 2 - 'SelfEst' -> 'MentWell' (M -> Y) with B=0.561 and p<0.001
#>    STEP 3 - 'OwnLook' -> 'MentWell' (X -> Y) with B=0.076 and p=0.065
#>             As STEP 1, STEP 2 and the Sobel's test above are significant
#>             and STEP 3 is not significant the mediation is complete.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'OwnLook' -> 'MentWell' (X -> Y) with B=0.076 and p=0.065
#>             As the Monte-Carlo test above is significant and STEP 1 is not
#>             significant there is indirect-only mediation (full mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.327/0.404) = 0.811
#>          Meaning that about 81% of the effect of 'OwnLook'
#>          on 'MentWell' is mediated by 'SelfEst'
#>    RID = (Indirect effect / Direct effect)
#>          RID is not reported: direct effect 0.076 is not significant (p = 0.065)
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.107, v(adj) = 0.106
#> 
```
