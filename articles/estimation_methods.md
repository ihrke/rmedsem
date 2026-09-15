# Examples using the supported estimation methods

``` r

library(rmedsem)
```

The currently supported estimation methods are:

- covariance-based SEM estimated with the
  [lavaan](https://lavaan.ugent.be/) package
- PLS-SEM estimated using [cSEM](https://m-e-rademaker.github.io/cSEM/)
- Bayesian SEM estimated using
  [blavaan](https://ecmerkle.github.io/blavaan/index.html)

## Lavaan

### Example 1 (lavaan)

``` r

library(lavaan)
library(rmedsem)

mod.txt <- "
read ~ math
science ~ read + math
"
mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
out <- rmedsem(mod, indep="math", med="read", dep="science", 
               standardized=T, mcreps=5000,
               approach = c("bk","zlc"))
print(out)
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'math' -> 'read' -> 'science'
#> 
#>                          Sobel          Delta    Monte-Carlo
#> Indirect effect          0.251          0.251          0.251
#> Std. Err.                0.046          0.046          0.045
#> z-value                  5.501          5.446          5.516
#> p-value               3.79e-08       5.15e-08       3.46e-08
#> CI              [0.161, 0.340] [0.160, 0.341] [0.163, 0.341]
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
```

### Example 2 (lavaan)

``` r

model02 <- "
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
"
mod <- sem(model02, data=lavaan::PoliticalDemocracy)
out <- rmedsem(mod, indep="ind60", med="dem60", dep="dem65",
               standardized=T, mcreps=5000,
               approach = c("bk","zlc"))
print(out)
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'ind60' -> 'dem60' -> 'dem65'
#> 
#>                          Sobel          Delta    Monte-Carlo
#> Indirect effect          0.409          0.409          0.409
#> Std. Err.                0.096          0.095          0.095
#> z-value                  4.282          4.325          4.292
#> p-value               1.85e-05       1.53e-05       1.77e-05
#> CI              [0.222, 0.596] [0.224, 0.595] [0.226, 0.596]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'ind60' -> 'dem60' (X -> M) with B=0.448 and p<0.001
#>    STEP 2 - 'dem60' -> 'dem65' (M -> Y) with B=0.913 and p<0.001
#>    STEP 3 - 'ind60' -> 'dem65' (X -> Y) with B=0.146 and p=0.038
#>             As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above
#>             are significant the mediation is partial.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'ind60' -> 'dem65' (X -> Y) with B=0.146 and p=0.038
#>             As the Monte-Carlo test above is significant, STEP 1 is
#>             significant and their coefficients point in same direction,
#>             there is complementary mediation (partial mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.409/0.555) = 0.738
#>          Meaning that about 74% of the effect of 'ind60'
#>          on 'dem65' is mediated by 'dem60'
#>    RID = (Indirect effect / Direct effect)
#>          (0.409/0.146) = 2.811
#>          That is, the mediated effect is about 2.8 times as
#>          large as the direct effect of 'ind60' on 'dem65'
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.167, v(adj) = 0.158
```

### Example 3 (lavaan)

``` r

model03 <- "
   Attractive =~ face + sexy
   Appearance =~ body + appear + attract
   Muscle =~ muscle + strength + endur
   Weight =~ lweight + calories + cweight
   Appearance ~ Attractive + age
   Muscle ~ Appearance + Attractive + age
   Weight ~ Appearance + Attractive + age
"
mod <- sem(model03, data=rmedsem::workout)
rmedsem(mod, indep="Attractive", med="Appearance", dep="Muscle",
       standardized=T, mcreps=5000,
       approach = c("bk","zlc"))
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'Attractive' -> 'Appearance' -> 'Muscle'
#> 
#>                          Sobel           Delta    Monte-Carlo
#> Indirect effect          0.065           0.065          0.065
#> Std. Err.                0.033           0.033          0.034
#> z-value                  1.975           1.954          1.949
#> p-value                 0.0483          0.0507         0.0513
#> CI              [0.000, 0.130] [-0.000, 0.131] [0.004, 0.137]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'Attractive' -> 'Appearance' (X -> M) with B=0.158 and p=0.033
#>    STEP 2 - 'Appearance' -> 'Muscle' (M -> Y) with B=0.414 and p<0.001
#>    STEP 3 - 'Attractive' -> 'Muscle' (X -> Y) with B=-0.014 and p=0.850
#>             As STEP 1, STEP 2 and the Sobel's test above are significant
#>             and STEP 3 is not significant the mediation is complete.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'Attractive' -> 'Muscle' (X -> Y) with B=-0.014 and p=0.850
#>             As the Monte-Carlo test above is not significant and STEP 1 is
#>             not significant there is no effect nonmediation (no mediation).
#> 
#> Effect sizes
#>    WARNING: Total effect is smaller than indirect effect!
#>             Effect sizes should not be interpreted.
#>    RIT = (Indirect effect / Total effect)
#>          RIT is not reported: total effect 0.052 is too small (< 0.2)
#>    RID = (Indirect effect / Direct effect)
#>          RID is not reported: direct effect 0.014 is not significant (p = 0.850)
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.004, v(adj) = 0.003
```

``` r

rmedsem(mod, indep="Attractive", med="Appearance", dep="Weight",
        standardized=T, mcreps=5000,
        approach = c("bk","zlc"))
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'Attractive' -> 'Appearance' -> 'Weight'
#> 
#>                          Sobel          Delta    Monte-Carlo
#> Indirect effect          0.098          0.098          0.098
#> Std. Err.                0.047          0.048          0.048
#> z-value                  2.081          2.027          2.032
#> p-value                 0.0374         0.0427         0.0422
#> CI              [0.006, 0.190] [0.003, 0.193] [0.006, 0.196]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'Attractive' -> 'Appearance' (X -> M) with B=0.158 and p=0.033
#>    STEP 2 - 'Appearance' -> 'Weight' (M -> Y) with B=0.619 and p<0.001
#>    STEP 3 - 'Attractive' -> 'Weight' (X -> Y) with B=-0.125 and p=0.073
#>             As STEP 1, STEP 2 and the Sobel's test above are significant
#>             and STEP 3 is not significant the mediation is complete.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'Attractive' -> 'Weight' (X -> Y) with B=-0.125 and p=0.073
#>             As the Monte-Carlo test above is significant and STEP 1 is not
#>             significant there is indirect-only mediation (full mediation).
#> 
#> Effect sizes
#>    WARNING: Total effect is smaller than indirect effect!
#>             Effect sizes should not be interpreted.
#>    RIT = (Indirect effect / Total effect)
#>          RIT is not reported: total effect 0.027 is too small (< 0.2)
#>    RID = (Indirect effect / Direct effect)
#>          RID is not reported: direct effect 0.125 is not significant (p = 0.073)
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.010, v(adj) = 0.007
```

``` r

rmedsem(mod, indep="age", med="Appearance", dep="Muscle",
        standardized=T, mcreps=5000,
        approach = c("bk","zlc"))
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'age' -> 'Appearance' -> 'Muscle'
#> 
#>                            Sobel            Delta      Monte-Carlo
#> Indirect effect           -0.160           -0.160           -0.160
#> Std. Err.                  0.040            0.040            0.040
#> z-value                   -4.039           -3.956           -3.972
#> p-value                 5.37e-05         7.61e-05         7.13e-05
#> CI              [-0.238, -0.082] [-0.240, -0.081] [-0.245, -0.087]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'age' -> 'Appearance' (X -> M) with B=-0.387 and p<0.001
#>    STEP 2 - 'Appearance' -> 'Muscle' (M -> Y) with B=0.414 and p<0.001
#>    STEP 3 - 'age' -> 'Muscle' (X -> Y) with B=-0.147 and p=0.065
#>             As STEP 1, STEP 2 and the Sobel's test above are significant
#>             and STEP 3 is not significant the mediation is complete.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'age' -> 'Muscle' (X -> Y) with B=-0.147 and p=0.065
#>             As the Monte-Carlo test above is significant and STEP 1 is not
#>             significant there is indirect-only mediation (full mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.160/0.307) = 0.521
#>          Meaning that about 52% of the effect of 'age'
#>          on 'Muscle' is mediated by 'Appearance'
#>    RID = (Indirect effect / Direct effect)
#>          RID is not reported: direct effect 0.147 is not significant (p = 0.065)
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.026, v(adj) = 0.024
```

``` r

rmedsem(mod, indep="age", med="Appearance", dep="Weight",
        standardized=T, mcreps=5000,
        approach = c("bk","zlc"))
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'age' -> 'Appearance' -> 'Weight'
#> 
#>                            Sobel            Delta      Monte-Carlo
#> Indirect effect           -0.240           -0.240           -0.240
#> Std. Err.                  0.045            0.050            0.049
#> z-value                   -5.287           -4.836           -4.873
#> p-value                 1.25e-07         1.33e-06         1.10e-06
#> CI              [-0.329, -0.151] [-0.337, -0.143] [-0.346, -0.150]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'age' -> 'Appearance' (X -> M) with B=-0.387 and p<0.001
#>    STEP 2 - 'Appearance' -> 'Weight' (M -> Y) with B=0.619 and p<0.001
#>    STEP 3 - 'age' -> 'Weight' (X -> Y) with B=0.341 and p<0.001
#>             As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above
#>             are significant the mediation is partial.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'age' -> 'Weight' (X -> Y) with B=0.341 and p<0.001
#>             As the Monte-Carlo test above is significant, STEP 1 is
#>             significant and their coefficients point in opposite
#>             direction, there is competitive mediation (partial mediation).
#> 
#> Effect sizes
#>    WARNING: Total effect is smaller than indirect effect!
#>             Effect sizes should not be interpreted.
#>    RIT = (Indirect effect / Total effect)
#>          RIT is not reported: total effect 0.101 is too small (< 0.2)
#>    RID = (Indirect effect / Direct effect)
#>          (0.240/0.341) = 0.704
#>          That is, the mediated effect is about 0.7 times as
#>          large as the direct effect of 'age' on 'Weight'
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.057, v(adj) = 0.055
```

## cSEM

### Example 1 (cSEM)

``` r

library(cSEM)
library(rmedsem)

mod.txt <- "
# need to use single-item measurement models for PLS-SEM
Read =~ read
Math =~ math
Science =~ science

# the actual path model
Read ~ Math
Science ~ Read + Math
"
mod <- cSEM::csem(.model=mod.txt, .data=rmedsem::hsbdemo,
                 .resample_method = "bootstrap", .R = 200)
rmedsem(mod, indep="Math", med="Read", dep="Science", 
        approach = c("bk", "zlc"))
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'cSEM'
#> Mediation effect: 'Math' -> 'Read' -> 'Science'
#> 
#>                          Sobel          Delta      Bootstrap
#> Indirect effect          0.251          0.251          0.251
#> Std. Err.                0.051          0.049          0.050
#> z-value                  4.873          5.069          5.048
#> p-value               1.10e-06       4.01e-07       4.47e-07
#> CI              [0.150, 0.351] [0.154, 0.348] [0.152, 0.349]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'Math' -> 'Read' (X -> M) with B=0.662 and p<0.001
#>    STEP 2 - 'Read' -> 'Science' (M -> Y) with B=0.378 and p<0.001
#>    STEP 3 - 'Math' -> 'Science' (X -> Y) with B=0.380 and p<0.001
#>             As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above
#>             are significant the mediation is partial.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Bootstrap
#>   STEP 1 - 'Math' -> 'Science' (X -> Y) with B=0.380 and p<0.001
#>             As the Bootstrap test above is significant, STEP 1 is
#>             significant and their coefficients point in same direction,
#>             there is complementary mediation (partial mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.251/0.631) = 0.397
#>          Meaning that about 40% of the effect of 'Math'
#>          on 'Science' is mediated by 'Read'
#>    RID = (Indirect effect / Direct effect)
#>          (0.251/0.380) = 0.659
#>          That is, the mediated effect is about 0.7 times as
#>          large as the direct effect of 'Math' on 'Science'
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.063, v(adj) = 0.060
```

### Example 2 (cSEM)

``` r

model02 <- "
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
"
mod <- cSEM::csem(.model=model02, .data=lavaan::PoliticalDemocracy,
                  .resample_method = "bootstrap", .R = 200)
rmedsem(mod, indep="ind60", med="dem60", dep="dem65",
        approach = c("bk","zlc"))
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'cSEM'
#> Mediation effect: 'ind60' -> 'dem60' -> 'dem65'
#> 
#>                          Sobel          Delta      Bootstrap
#> Indirect effect          0.399          0.399          0.399
#> Std. Err.                0.098          0.091          0.089
#> z-value                  4.062          4.389          4.497
#> p-value               4.86e-05       1.14e-05       6.88e-06
#> CI              [0.206, 0.591] [0.221, 0.577] [0.247, 0.584]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'ind60' -> 'dem60' (X -> M) with B=0.439 and p<0.001
#>    STEP 2 - 'dem60' -> 'dem65' (M -> Y) with B=0.909 and p<0.001
#>    STEP 3 - 'ind60' -> 'dem65' (X -> Y) with B=0.159 and p=0.009
#>             As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above
#>             are significant the mediation is partial.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Bootstrap
#>   STEP 1 - 'ind60' -> 'dem65' (X -> Y) with B=0.159 and p=0.009
#>             As the Bootstrap test above is significant, STEP 1 is
#>             significant and their coefficients point in same direction,
#>             there is complementary mediation (partial mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.399/0.557) = 0.715
#>          Meaning that about 72% of the effect of 'ind60'
#>          on 'dem65' is mediated by 'dem60'
#>    RID = (Indirect effect / Direct effect)
#>          (0.399/0.159) = 2.514
#>          That is, the mediated effect is about 2.5 times as
#>          large as the direct effect of 'ind60' on 'dem65'
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.159, v(adj) = 0.149
```

### Example 3 (cSEM)

``` r

model03 <- "
   Attractive =~ face + sexy
   Appearance =~ body + appear + attract
   Muscle =~ muscle + strength + endur
   Weight =~ lweight + calories + cweight
   Age =~ age ## need single-indicator LV for cSEM
   Appearance ~ Attractive + Age
   Muscle ~ Appearance + Attractive + Age
   Weight ~ Appearance + Attractive + Age
"
mod <- cSEM::csem(.model=model03, .data=na.omit(rmedsem::workout), 
                  .resample_method = "bootstrap", .R = 200)
rmedsem(mod, indep="Attractive", med="Appearance", dep="Muscle",
        approach = c("bk","zlc"))
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'cSEM'
#> Mediation effect: 'Attractive' -> 'Appearance' -> 'Muscle'
#> 
#>                          Sobel          Delta      Bootstrap
#> Indirect effect          0.112          0.112          0.112
#> Std. Err.                0.041          0.041          0.043
#> z-value                  2.713          2.766          2.640
#> p-value                0.00667        0.00568        0.00829
#> CI              [0.031, 0.194] [0.033, 0.192] [0.044, 0.209]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'Attractive' -> 'Appearance' (X -> M) with B=0.236 and p=0.002
#>    STEP 2 - 'Appearance' -> 'Muscle' (M -> Y) with B=0.475 and p<0.001
#>    STEP 3 - 'Attractive' -> 'Muscle' (X -> Y) with B=-0.010 and p=0.898
#>             As STEP 1, STEP 2 and the Sobel's test above are significant
#>             and STEP 3 is not significant the mediation is complete.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Bootstrap
#>   STEP 1 - 'Attractive' -> 'Muscle' (X -> Y) with B=-0.010 and p=0.898
#>             As the Bootstrap test above is significant and STEP 1 is not
#>             significant there is indirect-only mediation (full mediation).
#> 
#> Effect sizes
#>    WARNING: Total effect is smaller than indirect effect!
#>             Effect sizes should not be interpreted.
#>    RIT = (Indirect effect / Total effect)
#>          RIT is not reported: total effect 0.102 is too small (< 0.2)
#>    RID = (Indirect effect / Direct effect)
#>          RID is not reported: direct effect 0.010 is not significant (p = 0.898)
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.013, v(adj) = 0.011
```

## blavaan

### Example 1 (blavaan)

``` r

library(blavaan)
library(rmedsem)

mod.txt <- "
read ~ math
science ~ read + math
"
mod <- bsem(mod.txt, data=rmedsem::hsbdemo, 
            n.chains=3, burnin=500, sample=500, 
            bcontrol = list(cores = 3))
out <- rmedsem(mod, indep="math", med="read", dep="science",
               approach = c("bk","zlc"))
print(out)
```

By default, the credible intervals are equal-tailed intervals. Highest
density intervals (HDI) can be requested with `hdi = TRUE` (requires the
[HDInterval](https://cran.r-project.org/package=HDInterval) package):

``` r

out.hdi <- rmedsem(mod, indep="math", med="read", dep="science", hdi=TRUE)
summary(out.hdi)
#> Mediation analysis: 'math' -> 'read' -> 'science'
#> Estimated with 'blavaan' (standardized), N = 200
#> 
#> Effects (95% HDI):
#>                  Estimate Std. Err. z-value p-value Lower Upper
#> Indirect (Bayes)    0.248     0.045   5.447   0.000 0.166 0.337
#> Direct              0.381     0.063           0.000 0.257 0.502
#> Total               0.628     0.038                 0.552 0.696
#> For Bayesian estimates, 'p-value' is the posterior probability of
#> the opposite sign.
#> 
#> 
#> Effect sizes:
#>   RIT = 0.394
#>   RID = 0.651
#>   Upsilon = 0.059
#>   Upsilon (unadj.) = 0.061
```

### Example 2 (blavaan)

``` r

model02 <- "
 # measurement model
   ind60 =~ x1 + x2 + x3
   dem60 =~ y1 + y2 + y3 + y4
   dem65 =~ y5 + y6 + y7 + y8
 # regressions
   dem60 ~ ind60
   dem65 ~ ind60 + dem60
"
mod <- bsem(model02, data=lavaan::PoliticalDemocracy, std.lv=T,
           meanstructure=T, n.chains=3,
           save.lvs=T, burnin=1000, sample=1000, bcontrol = list(cores = 3))
rmedsem(mod,  indep="ind60", med="dem60", dep="dem65")
```
