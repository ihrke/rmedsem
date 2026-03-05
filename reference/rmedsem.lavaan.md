# Mediation Analysis for Lavaan Models

Mediation Analysis for Lavaan Models

## Usage

``` r
# S3 method for class 'lavaan'
rmedsem(
  mod,
  indep,
  med,
  dep,
  approach = c("bk", "zlc"),
  p.threshold = 0.05,
  effect.size = c("RIT", "RID"),
  standardized = TRUE,
  mcreps = NULL,
  ci.two.tailed = 0.95,
  ...
)
```

## Arguments

- mod:

  A fitted SEM model (lavaan).

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

- standardized:

  A boolean indicating whether the coefficients should be standardized.
  The default value is F.

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
mod.txt <- "
read ~ math
science ~ read + math
"
mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
out <- rmedsem(mod, indep="math", med="read", dep="science",
               standardized=TRUE, mcreps=5000,
               approach = c("bk","zlc"))
print(out)
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'math' -> 'read' -> 'science'
#> 
#>                         Sobel         Delta    Monte-Carlo
#> Indirect effect        0.2506         0.251         0.2506
#> Std. Err.              0.0456         0.046         0.0456
#> z-value                5.5006         5.446         5.4679
#> p-value              3.79e-08      5.15e-08       4.55e-08
#> CI              [0.161, 0.34] [0.16, 0.341] [0.164, 0.342]
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
#> 
```
