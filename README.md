
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmedsem

<!-- badges: start -->
<!-- badges: end -->

The goal of rmedsem is to conduct a mediation analysis based on a model (including observed or latent variables as well as combination of observed and latent variables) estimated using lavaan's -sem- function. There are two methods medsem uses as the basis for its procedures. The first method is the commonly known Baron and Kenny approach which is adjusted by Iacobucci et al. (2007) for use with structural equation modelling. The second approach is that of Zhao et al. (2010).
  
    Adjusted Baron and Kenny's approach (default)

    1)If both or one of the X->M and M->Y coefficients is not significant, there is no mediation

    2)When both of the X->M and M->Y coefficients are significant, there is "some" mediation

    2a)If the Sobel's z-test is significant and the X->Y coefficient is not significant, then there is complete mediation

    2b)If both the Sobel's z-test and the X->Y coefficients are significant, then there is partial mediation

    2c)If the Sobel's z-test is not significant but the X->Y coefficient is significant, then there is partial mediation

    2d)If neither Sobel's z-test nor the X->Y coefficient are significant, then there is partial mediation

    Zhao, Lynch & Chen's approach

    1)If neither Monte Carlo z-test nor the X->Y coefficient are significant, then there is no-effect nonmediation (i.e. no mediation)

    2)If Monte Carlo z-test is not significant and X->Y coefficient is significant, then there is direct-only nonmediation (i.e. no mediation)

    3)If Monte Carlo z-test is significant and X->Y coefficient is not significant, then there is indirect-only mediation (i.e. full mediation)

    4)If both of the Monte Carlo z-test and X-Y coefficient are significant and their coefficients point in same direction, then there is complementary
    mediation (i.e. partial mediation)

    5)If both of the Monte Carlo z-test and X-Y coefficient are significant and their coefficients point in opposite direction, then there is
    competitive mediation (i.e. partial mediation)

    Note: Zhao et al. suggest bootstrap test of the indirect effect, medsem uses the Monte Carlo test instead as it is less time-consuming and still
    acceptable (see Jose(2013), page 122).

References 

    Iacobucci, D., Saldanha, N., & Deng, X. (2007). A Mediation on Mediation: Evidence That Structural Equation Models Perform Better Than Regressions.
    Journal of Consumer Psychology, 17(2), 140-154.

    Jose, P. E. (2013). Doing Statistical Mediation & Moderation.  London: Guilford.

    MacKinnon, D. P. (2008). Introduction to statistical mediation analysis. New York: Lawrence Erlbaum.

    Zhao, X., Lynch, J. G. Jr., & Chen, Q. (2010). Reconsidering Baron and Kenny: Myths and Truths about Mediation Analysis. Journal of Consumer
    Research, 37(August), 197-206.

## Installation

You can install the development version of rmedsem from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ihrke/rmedsem")
```

## Example 1

``` r
library(rmedsem)

data <- haven::read_stata("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
mod.txt <- "
read ~ math
science ~ read + math
"
mod <- lavaan::sem(mod.txt, data=data)
out <- rmedsem(mod, indep="math", med="read", dep="science", 
               standardized=T, mcreps=5000,
               approach = c("bk","zlc"))
print(out)
#> Significance testing of indirect effect (standardized)
#> Mediation effect: 'math' -> 'read' -> 'science'
#> 
#>                         Sobel         Delta    Monte.Carlo
#> Indirect effect        0.2506        0.2506         0.2506
#> Std. Err.              0.0456        0.0456         0.0453
#> z-value                5.5006        5.4935         5.4939
#> p-value              3.79e-08      3.94e-08       3.93e-08
#> CI              [0.161, 0.34] [0.161, 0.34] [0.162, 0.339]
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

#Example 2
lavaan::PoliticalDemocracy
#
model02 <- "
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
"
mod <- sem(model02, data=PoliticalDemocracy)
out <- rmedsem(mod, indep="ind60", med="dem60", dep="dem65",
               standardized=T, mcreps=5000,
               approach = c("bk","zlc"))
print(out)

## Example 3 
load(file="workout.Rdata")
model03 <- "
   Attractive =~ face + sexy
   Appearance =~ body + appear + attract
   Muscle =~ muscle + strength + endur
   Weight =~ lweight + calories + cweight
   Appearance ~ Attractive + age
   Muscle ~ Appearance + Attractive + age
   Weight ~ Appearance + Attractive + age
"
mod <- sem(model03, data=workout)
out1 <- rmedsem(mod, indep="Attractive", med="Appearance", dep="Muscle",
               standardized=T, mcreps=5000,
               approach = c("bk","zlc"))
print(out1)
#
out2 <- rmedsem(mod, indep="Attractive", med="Appearance", dep="Weight",
               standardized=T, mcreps=5000,
               approach = c("bk","zlc"))
print(out2)
#
out3 <- rmedsem(mod, indep="age", med="Appearance", dep="Muscle",
                standardized=T, mcreps=5000,
                approach = c("bk","zlc"))
print(out3)
#
out4 <- rmedsem(mod, indep="age", med="Appearance", dep="Weight",
                standardized=T, mcreps=5000,
                approach = c("bk","zlc"))
print(out4)

```
