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
#>                         Sobel         Delta    Monte-Carlo
#> Indirect effect        0.2506         0.251         0.2506
#> Std. Err.              0.0456         0.046         0.0454
#> z-value                5.5006         5.446         5.5164
#> p-value              3.79e-08      5.15e-08       3.46e-08
#> CI              [0.161, 0.34] [0.16, 0.341] [0.163, 0.341]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'math:read' (X -> M) with B=0.662 and p=0.000
#>    STEP 2 - 'read:science' (M -> Y) with B=0.378 and p=0.000
#>    STEP 3 - 'math:science' (X -> Y) with B=0.380 and p=0.000
#>             As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above
#>             are significant the mediation is partial.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'math:science' (X -> Y) with B=0.380 and p=0.000
#>             As the Monte-Carlo test above is significant, STEP 1 is
#>             significant and their coefficients point in same direction,
#>             there is complementary mediation (partial mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.251/0.631) = 0.397
#>          Meaning that about  40% of the effect of 'math'
#>          on 'science' is mediated by 'read'
#>    RID = (Indirect effect / Direct effect)
#>          (0.251/0.380) = 0.659
#>          That is, the mediated effect is about 0.7 times as
#>          large as the direct effect of 'math' on 'science'
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
#> Indirect effect         0.4091         0.4091         0.4091
#> Std. Err.               0.0956         0.0946         0.0948
#> z-value                 4.2817         4.3248         4.2916
#> p-value               1.85e-05       1.53e-05       1.77e-05
#> CI              [0.222, 0.596] [0.224, 0.595] [0.226, 0.596]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'ind60:dem60' (X -> M) with B=0.448 and p=0.000
#>    STEP 2 - 'dem60:dem65' (M -> Y) with B=0.913 and p=0.000
#>    STEP 3 - 'ind60:dem65' (X -> Y) with B=0.146 and p=0.038
#>             As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above
#>             are significant the mediation is partial.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'ind60:dem65' (X -> Y) with B=0.146 and p=0.038
#>             As the Monte-Carlo test above is significant, STEP 1 is
#>             significant and their coefficients point in same direction,
#>             there is complementary mediation (partial mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.409/0.555) = 0.738
#>          Meaning that about  74% of the effect of 'ind60'
#>          on 'dem65' is mediated by 'dem60'
#>    RID = (Indirect effect / Direct effect)
#>          (0.409/0.146) = 2.811
#>          That is, the mediated effect is about 2.8 times as
#>          large as the direct effect of 'ind60' on 'dem65'
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
#>                            Sobel              Delta     Monte-Carlo
#> Indirect effect           0.0654             0.0654          0.0654
#> Std. Err.                 0.0331             0.0335          0.0339
#> z-value                   1.9748             1.9544          1.9490
#> p-value                   0.0483             0.0507          0.0513
#> CI              [0.000491, 0.13] [-0.000187, 0.131] [0.0043, 0.137]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'Attractive:Appearance' (X -> M) with B=0.158 and p=0.033
#>    STEP 2 - 'Appearance:Muscle' (M -> Y) with B=0.414 and p=0.000
#>    STEP 3 - 'Attractive:Muscle' (X -> Y) with B=-0.014 and p=0.850
#>             As STEP 1, STEP 2 and the Sobel's test above are significant
#>             and STEP 3 is not significant the mediation is complete.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'Attractive:Muscle' (X -> Y) with B=-0.014 and p=0.850
#>             As the Monte-Carlo test above is not significant and STEP 1 is
#>             not significant there is no effect nonmediation (no mediation).
#> 
#> Effect sizes
#>    WARNING: Total effect is smaller than indirect effect!
#>             Effect sizes should not be interpreted.
#>    RIT = (Indirect effect / Total effect)
#>          Total effect 0.052 is too small to calculate RIT
#>    RID = (Indirect effect / Direct effect)
#>          (0.065/0.014) = 4.714
#>          That is, the mediated effect is about 4.7 times as
#>          large as the direct effect of 'Attractive' on 'Muscle'
```

``` r
rmedsem(mod, indep="Attractive", med="Appearance", dep="Weight",
        standardized=T, mcreps=5000,
        approach = c("bk","zlc"))
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'Attractive' -> 'Appearance' -> 'Weight'
#> 
#>                           Sobel            Delta     Monte-Carlo
#> Indirect effect          0.0979           0.0979          0.0979
#> Std. Err.                0.0470           0.0483          0.0484
#> z-value                  2.0810           2.0267          2.0318
#> p-value                  0.0374           0.0427          0.0422
#> CI              [0.00569, 0.19] [0.00322, 0.193] [0.0062, 0.196]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'Attractive:Appearance' (X -> M) with B=0.158 and p=0.033
#>    STEP 2 - 'Appearance:Weight' (M -> Y) with B=0.619 and p=0.000
#>    STEP 3 - 'Attractive:Weight' (X -> Y) with B=-0.125 and p=0.073
#>             As STEP 1, STEP 2 and the Sobel's test above are significant
#>             and STEP 3 is not significant the mediation is complete.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'Attractive:Weight' (X -> Y) with B=-0.125 and p=0.073
#>             As the Monte-Carlo test above is significant and STEP 1 is not
#>             significant there indirect-only mediation (full mediation).
#> 
#> Effect sizes
#>    WARNING: Total effect is smaller than indirect effect!
#>             Effect sizes should not be interpreted.
#>    RIT = (Indirect effect / Total effect)
#>          Total effect 0.027 is too small to calculate RIT
#>    RID = (Indirect effect / Direct effect)
#>          (0.098/0.125) = 0.784
#>          That is, the mediated effect is about 0.8 times as
#>          large as the direct effect of 'Attractive' on 'Weight'
```

``` r
rmedsem(mod, indep="age", med="Appearance", dep="Muscle",
        standardized=T, mcreps=5000,
        approach = c("bk","zlc"))
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'age' -> 'Appearance' -> 'Muscle'
#> 
#>                             Sobel            Delta       Monte-Carlo
#> Indirect effect           -0.1602          -0.1602           -0.1602
#> Std. Err.                  0.0397           0.0405            0.0403
#> z-value                   -4.0391          -3.9565           -3.9719
#> p-value                  5.37e-05         7.61e-05          7.13e-05
#> CI              [-0.238, -0.0825] [-0.24, -0.0808] [-0.245, -0.0873]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'age:Appearance' (X -> M) with B=-0.387 and p=0.000
#>    STEP 2 - 'Appearance:Muscle' (M -> Y) with B=0.414 and p=0.000
#>    STEP 3 - 'age:Muscle' (X -> Y) with B=-0.147 and p=0.065
#>             As STEP 1, STEP 2 and the Sobel's test above are significant
#>             and STEP 3 is not significant the mediation is complete.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'age:Muscle' (X -> Y) with B=-0.147 and p=0.065
#>             As the Monte-Carlo test above is significant and STEP 1 is not
#>             significant there indirect-only mediation (full mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.160/0.307) = 0.521
#>          Meaning that about  52% of the effect of 'age'
#>          on 'Muscle' is mediated by 'Appearance'
#>    RID = (Indirect effect / Direct effect)
#>          (0.160/0.147) = 1.089
#>          That is, the mediated effect is about 1.1 times as
#>          large as the direct effect of 'age' on 'Muscle'
```

``` r
rmedsem(mod, indep="age", med="Appearance", dep="Weight",
        standardized=T, mcreps=5000,
        approach = c("bk","zlc"))
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'age' -> 'Appearance' -> 'Weight'
#> 
#>                            Sobel            Delta     Monte-Carlo
#> Indirect effect          -0.2397          -0.2397         -0.2397
#> Std. Err.                 0.0453           0.0496          0.0495
#> z-value                  -5.2867          -4.8356         -4.8734
#> p-value                 1.25e-07         1.33e-06         1.1e-06
#> CI              [-0.329, -0.151] [-0.337, -0.143] [-0.346, -0.15]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'age:Appearance' (X -> M) with B=-0.387 and p=0.000
#>    STEP 2 - 'Appearance:Weight' (M -> Y) with B=0.619 and p=0.000
#>    STEP 3 - 'age:Weight' (X -> Y) with B=0.341 and p=0.000
#>             As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above
#>             are significant the mediation is partial.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'age:Weight' (X -> Y) with B=0.341 and p=0.000
#>             As the Monte-Carlo test above is significant, STEP 1 is
#>             significant and their coefficients point in opposite
#>             direction, there is competitive mediation (partial mediation).
#> 
#> Effect sizes
#>    WARNING: Total effect is smaller than indirect effect!
#>             Effect sizes should not be interpreted.
#>    RIT = (Indirect effect / Total effect)
#>          Total effect 0.101 is too small to calculate RIT
#>    RID = (Indirect effect / Direct effect)
#>          (0.240/0.341) = 0.704
#>          That is, the mediated effect is about 0.7 times as
#>          large as the direct effect of 'age' on 'Weight'
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
#> Indirect effect         0.2506         0.2506          0.251
#> Std. Err.               0.0533         0.0547          0.052
#> z-value                 4.7015         4.5786          4.820
#> p-value               2.58e-06       4.68e-06       1.43e-06
#> CI              [0.146, 0.355] [0.143, 0.358] [0.148, 0.351]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'Math:Read' (X -> M) with B=0.662 and p=0.000
#>    STEP 2 - 'Read:Science' (M -> Y) with B=0.378 and p=0.000
#>    STEP 3 - 'Math:Science' (X -> Y) with B=0.380 and p=0.000
#>             As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above
#>             are significant the mediation is partial.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Bootstrap
#>   STEP 1 - 'Math:Science' (X -> Y) with B=0.380 and p=0.000
#>             As the Bootstrap test above is significant, STEP 1 is
#>             significant and their coefficients point in same direction,
#>             there is complementary mediation (partial mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.251/0.631) = 0.397
#>          Meaning that about  40% of the effect of 'Math'
#>          on 'Science' is mediated by 'Read'
#>    RID = (Indirect effect / Direct effect)
#>          (0.251/0.380) = 0.659
#>          That is, the mediated effect is about 0.7 times as
#>          large as the direct effect of 'Math' on 'Science'
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
#>                          Sobel          Delta     Bootstrap
#> Indirect effect         0.3988         0.3988        0.3988
#> Std. Err.               0.0943         0.0999        0.0864
#> z-value                 4.2303         3.9899        4.6179
#> p-value               2.33e-05       6.61e-05      3.88e-06
#> CI              [0.214, 0.584] [0.203, 0.595] [0.26, 0.574]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'ind60:dem60' (X -> M) with B=0.439 and p=0.000
#>    STEP 2 - 'dem60:dem65' (M -> Y) with B=0.909 and p=0.000
#>    STEP 3 - 'ind60:dem65' (X -> Y) with B=0.159 and p=0.016
#>             As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above
#>             are significant the mediation is partial.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Bootstrap
#>   STEP 1 - 'ind60:dem65' (X -> Y) with B=0.159 and p=0.016
#>             As the Bootstrap test above is significant, STEP 1 is
#>             significant and their coefficients point in same direction,
#>             there is complementary mediation (partial mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.399/0.557) = 0.715
#>          Meaning that about  72% of the effect of 'ind60'
#>          on 'dem65' is mediated by 'dem60'
#>    RID = (Indirect effect / Direct effect)
#>          (0.399/0.159) = 2.514
#>          That is, the mediated effect is about 2.5 times as
#>          large as the direct effect of 'ind60' on 'dem65'
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
#>                           Sobel           Delta        Bootstrap
#> Indirect effect          0.1124          0.1124           0.1124
#> Std. Err.                0.0404          0.0416           0.0408
#> z-value                  2.7844          2.7024           2.7529
#> p-value                 0.00536         0.00688          0.00591
#> CI              [0.0333, 0.191] [0.0309, 0.194] [0.0396, -0.273]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'Attractive:Appearance' (X -> M) with B=0.236 and p=0.002
#>    STEP 2 - 'Appearance:Muscle' (M -> Y) with B=0.475 and p=0.000
#>    STEP 3 - 'Attractive:Muscle' (X -> Y) with B=-0.010 and p=0.901
#>             As STEP 1, STEP 2 and the Sobel's test above are significant
#>             and STEP 3 is not significant the mediation is complete.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Bootstrap
#>   STEP 1 - 'Attractive:Muscle' (X -> Y) with B=-0.010 and p=0.901
#>             As the Bootstrap test above is significant and STEP 1 is not
#>             significant there indirect-only mediation (full mediation).
#> 
#> Effect sizes
#>    WARNING: Total effect is smaller than indirect effect!
#>             Effect sizes should not be interpreted.
#>    RIT = (Indirect effect / Total effect)
#>          Total effect 0.102 is too small to calculate RIT
#>    RID = (Indirect effect / Direct effect)
#>          (0.112/0.010) = 10.768
#>          That is, the mediated effect is about 10.8 times as
#>          large as the direct effect of 'Attractive' on 'Muscle'
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
