
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmedsem <a href="https://ihrke.github.io/rmedsem/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->
<!-- badges: end -->

The goal of rmedsem is to conduct a mediation analysis based on a model
(including observed or latent variables as well as combination of
observed and latent variables) estimated using lavaan’s -sem- function.
There are two methods medsem uses as the basis for its procedures. The
first method is the commonly known Baron and Kenny approach which is
adjusted by Iacobucci et al. (2007) for use with structural equation
modelling. The second approach is that of Zhao et al. (2010).

### Adjusted Baron and Kenny’s approach (default)

1)  If both or one of the X-\>M and M-\>Y coefficients is not
    significant, there is no mediation
2)  When both of the X-\>M and M-\>Y coefficients are significant, there
    is “some” mediation 2a) If the Sobel’s z-test is significant and the
    X-\>Y coefficient is not significant, then there is complete
    mediation 2b) If both the Sobel’s z-test and the X-\>Y coefficients
    are significant, then there is partial mediation 2c) If the Sobel’s
    z-test is not significant but the X-\>Y coefficient is significant,
    then there is partial mediation 2d) If neither Sobel’s z-test nor
    the X-\>Y coefficient are significant, then there is partial
    mediation

### Zhao, Lynch & Chen’s approach

1)  If neither Monte Carlo z-test nor the X-\>Y coefficient are
    significant, then there is no-effect nonmediation (i.e. no
    mediation)
2)  If Monte Carlo z-test is not significant and X-\>Y coefficient is
    significant, then there is direct-only nonmediation (i.e. no
    mediation)
3)  If Monte Carlo z-test is significant and X-\>Y coefficient is not
    significant, then there is indirect-only mediation (i.e. full
    mediation)
4)  If both of the Monte Carlo z-test and X-Y coefficient are
    significant and their coefficients point in same direction, then
    there is complementary mediation (i.e. partial mediation)
5)  If both of the Monte Carlo z-test and X-Y coefficient are
    significant and their coefficients point in opposite direction, then
    there is competitive mediation (i.e. partial mediation)

Note: Zhao et al. suggest bootstrap test of the indirect effect, medsem
uses the Monte Carlo test instead as it is less time-consuming and still
acceptable (see Jose(2013), page 122).

### References

- Iacobucci, D., Saldanha, N., & Deng, X. (2007). A Mediation on
  Mediation: Evidence That Structural Equation Models Perform Better
  Than Regressions. Journal of Consumer Psychology, 17(2), 140-154.
- Jose, P. E. (2013). Doing Statistical Mediation & Moderation. London:
  Guilford.
- MacKinnon, D. P. (2008). Introduction to statistical mediation
  analysis. New York: Lawrence Erlbaum.
- Zhao, X., Lynch, J. G. Jr., & Chen, Q. (2010). Reconsidering Baron and
  Kenny: Myths and Truths about Mediation Analysis. Journal of Consumer
  Research, 37(August), 197-206.

## Installation

You can install the development version of rmedsem from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ihrke/rmedsem")
```

## Examples

### Example 1

``` r
library(lavaan)
#> This is lavaan 0.6-15
#> lavaan is FREE software! Please report any bugs.
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
#> Mediation effect: 'math' -> 'read' -> 'science'
#> 
#>                         Sobel         Delta    Monte.Carlo
#> Indirect effect        0.2506        0.2506         0.2506
#> Std. Err.              0.0456        0.0456         0.0464
#> z-value                5.5006        5.4935         5.4050
#> p-value              3.79e-08      3.94e-08       6.48e-08
#> CI              [0.161, 0.34] [0.161, 0.34] [0.161, 0.345]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'math:read' (X -> M) with B=0.662 and p=0.000
#>    STEP 2 - 'read:science' (M -> Y) with B=0.378 and p=0.000
#>    STEP 3 - 'math:science' (X -> Y) with B=0.380 and p=0.000
#>             As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above
#>             are significant the mediation is partial.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#>   STEP 1 - 'math:science' (X -> Y) with B=0.380 and p=0.000
#>             As the Monte Carlo test above is significant, STEP 1 is
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

### Example 2

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
#> Mediation effect: 'ind60' -> 'dem60' -> 'dem65'
#> 
#>                          Sobel          Delta  Monte.Carlo
#> Indirect effect         0.4091         0.4091        0.409
#> Std. Err.               0.0956         0.0957        0.096
#> z-value                 4.2817         4.2761        4.243
#> p-value               1.85e-05        1.9e-05      2.2e-05
#> CI              [0.222, 0.596] [0.222, 0.597] [0.222, 0.6]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'ind60:dem60' (X -> M) with B=0.448 and p=0.000
#>    STEP 2 - 'dem60:dem65' (M -> Y) with B=0.913 and p=0.000
#>    STEP 3 - 'ind60:dem65' (X -> Y) with B=0.146 and p=0.038
#>             As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above
#>             are significant the mediation is partial.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#>   STEP 1 - 'ind60:dem65' (X -> Y) with B=0.146 and p=0.038
#>             As the Monte Carlo test above is significant, STEP 1 is
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

### Example 3

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
#> Warning in lav_object_post_check(object): lavaan WARNING: some estimated ov
#> variances are negative
out1 <- rmedsem(mod, indep="Attractive", med="Appearance", dep="Muscle",
               standardized=T, mcreps=5000,
               approach = c("bk","zlc"))
print(out1)
#> Significance testing of indirect effect (standardized)
#> Mediation effect: 'Attractive' -> 'Appearance' -> 'Muscle'
#> 
#>                            Sobel              Delta      Monte.Carlo
#> Indirect effect           0.0654             0.0654           0.0654
#> Std. Err.                 0.0331             0.0336           0.0346
#> z-value                   1.9748             1.9448           1.8828
#> p-value                   0.0483             0.0518           0.0597
#> CI              [0.000491, 0.13] [-0.000511, 0.131] [0.00422, 0.138]
#> 
#> Baron and Kenny approach to testing mediation
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#>   STEP 1 - 'Attractive:Muscle' (X -> Y) with B=-0.014 and p=0.850
#>             As the Monte Carlo test above is not significant and STEP 1 is
#>             not significant there is no effect nonmediation (no mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.065/0.052) = 1.269
#>          Meaning that about 127% of the effect of 'Attractive'
#>          on 'Muscle' is mediated by 'Appearance'
#>    RID = (Indirect effect / Direct effect)
#>          (0.065/0.014) = 4.714
#>          That is, the mediated effect is about 4.7 times as
#>          large as the direct effect of 'Attractive' on 'Muscle'
```

``` r
out2 <- rmedsem(mod, indep="Attractive", med="Appearance", dep="Weight",
               standardized=T, mcreps=5000,
               approach = c("bk","zlc"))
print(out2)
#> Significance testing of indirect effect (standardized)
#> Mediation effect: 'Attractive' -> 'Appearance' -> 'Weight'
#> 
#>                           Sobel            Delta     Monte.Carlo
#> Indirect effect          0.0979           0.0979          0.0979
#> Std. Err.                0.0470           0.0473          0.0473
#> z-value                  2.0810           2.0705          2.0970
#> p-value                  0.0374           0.0384           0.036
#> CI              [0.00569, 0.19] [0.00523, 0.191] [0.0113, 0.195]
#> 
#> Baron and Kenny approach to testing mediation
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#>   STEP 1 - 'Attractive:Weight' (X -> Y) with B=-0.125 and p=0.073
#>             As the Monte Carlo test above is significant and STEP 1 is not
#>             significant there indirect-only mediation (full mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.098/0.027) = 3.628
#>          Meaning that about 363% of the effect of 'Attractive'
#>          on 'Weight' is mediated by 'Appearance'
#>    RID = (Indirect effect / Direct effect)
#>          (0.098/0.125) = 0.784
#>          That is, the mediated effect is about 0.8 times as
#>          large as the direct effect of 'Attractive' on 'Weight'
```

``` r
out3 <- rmedsem(mod, indep="age", med="Appearance", dep="Muscle",
                standardized=T, mcreps=5000,
                approach = c("bk","zlc"))
print(out3)
#> Significance testing of indirect effect (standardized)
#> Mediation effect: 'age' -> 'Appearance' -> 'Muscle'
#> 
#>                             Sobel             Delta       Monte.Carlo
#> Indirect effect           -0.1602             -0.16           -0.1602
#> Std. Err.                  0.0397              0.04            0.0399
#> z-value                   -4.0391             -4.01           -3.9961
#> p-value                  5.37e-05          6.08e-05          6.44e-05
#> CI              [-0.238, -0.0825] [-0.239, -0.0819] [-0.244, -0.0877]
#> 
#> Baron and Kenny approach to testing mediation
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#>   STEP 1 - 'age:Muscle' (X -> Y) with B=-0.147 and p=0.065
#>             As the Monte Carlo test above is significant and STEP 1 is not
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
out4 <- rmedsem(mod, indep="age", med="Appearance", dep="Weight",
                standardized=T, mcreps=5000,
                approach = c("bk","zlc"))
print(out4)
#> Significance testing of indirect effect (standardized)
#> Mediation effect: 'age' -> 'Appearance' -> 'Weight'
#> 
#>                            Sobel           Delta      Monte.Carlo
#> Indirect effect          -0.2397         -0.2397          -0.2397
#> Std. Err.                 0.0453          0.0455           0.0458
#> z-value                  -5.2867         -5.2670          -5.2043
#> p-value                 1.25e-07        1.39e-07         1.95e-07
#> CI              [-0.329, -0.151] [-0.329, -0.15] [-0.333, -0.154]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'age:Appearance' (X -> M) with B=-0.387 and p=0.000
#>    STEP 2 - 'Appearance:Weight' (M -> Y) with B=0.619 and p=0.000
#>    STEP 3 - 'age:Weight' (X -> Y) with B=0.341 and p=0.000
#>             As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above
#>             are significant the mediation is partial.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#>   STEP 1 - 'age:Weight' (X -> Y) with B=0.341 and p=0.000
#>             As the Monte Carlo test above is significant, STEP 1 is
#>             significant and their coefficients point in opposite
#>             direction, there is competitive mediation (partial mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          (0.240/0.101) = 2.375
#>          Meaning that about 238% of the effect of 'age'
#>          on 'Weight' is mediated by 'Appearance'
#>    RID = (Indirect effect / Direct effect)
#>          (0.240/0.341) = 0.704
#>          That is, the mediated effect is about 0.7 times as
#>          large as the direct effect of 'age' on 'Weight'
```
