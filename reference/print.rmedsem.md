# Print an rmedsem Object

Print an rmedsem Object

## Usage

``` r
# S3 method for class 'rmedsem'
print(x, digits = 3, indent = 3, ...)
```

## Arguments

- x:

  the `rmedsem` object to print

- digits:

  an integer, number of digits to print in table

- indent:

  an integer, number of spaces to indent

- ...:

  additional arguments (currently unused)

## Value

the `rmedsem` object `x` (invisibly)

## Examples

``` r
mod.txt <- "
read ~ math
science ~ read + math
"
mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
out <- rmedsem(mod, indep="math", med="read", dep="science")
print(out)
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'math' -> 'read' -> 'science'
#> 
#>                         Sobel         Delta    Monte-Carlo
#> Indirect effect        0.2506         0.251         0.2506
#> Std. Err.              0.0456         0.046         0.0411
#> z-value                5.5006         5.446         5.9763
#> p-value              3.79e-08      5.15e-08       2.28e-09
#> CI              [0.161, 0.34] [0.16, 0.341] [0.167, 0.325]
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
#>    Upsilon (v) = Variance in 'science' explained indirectly by 'math' through 'read'
#>          v(unadj) = 0.063, v(adj) = 0.061
#> 
```
