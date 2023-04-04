
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmedsem

<!-- badges: start -->
<!-- badges: end -->

The goal of rmedsem is to …

## Installation

You can install the development version of rmedsem from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ihrke/rmedsem")
```

## Example

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
```
