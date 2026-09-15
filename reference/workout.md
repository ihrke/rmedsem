# Fitness Center Survey Data from Trondheim

Data from a survey in a fitness center in Trondheim.

## Usage

``` r
workout
```

## Format

### `workout`

A data frame with 246 rows and 12 columns:

- age:

  Age in years

- lweight:

  How important is following to workout- to loose weight

- calories:

  How important is following to workout- to burn calories

- cweight:

  How important is following to workout- to control my weight

- body:

  How important is following to workout- to have a good body

- appear:

  How important is following to workout- to improve my appearance

- attract:

  How important is following to workout- to look more attractive

- muscle:

  How important is following to workout- to develop my muscles

- strength:

  How important is following to workout- to get stronger

- endur:

  How important is following to workout- to increase my endurance

- face:

  How well does the following describe you as a person - attractive face

- sexy:

  How well does the following describe you as a person - sexy

## Examples

``` r
str(workout)
#> tibble [246 × 12] (S3: tbl_df/tbl/data.frame)
#>  $ age     : num [1:246] 43 36 20 44 29 30 20 43 21 46 ...
#>   ..- attr(*, "label")= chr "Age"
#>   ..- attr(*, "format.stata")= chr "%8.0g"
#>  $ lweight : hvn_lbll [1:246]  3,  3,  1,  4,  5,  1,  5,  4,  6, NA,  4,  5,  2,  ...
#>    ..@ label       : chr "How important is following to workout- to loose weight"
#>    ..@ format.stata: chr "%31.0g"
#>    ..@ labels      : Named num [1:2] 1 6
#>    .. ..- attr(*, "names")= chr [1:2] "not important at all" "very important"
#>  $ calories: hvn_lbll [1:246]  4,  3,  1,  4,  5,  1,  5,  4,  6, NA,  5,  5,  4,  ...
#>    ..@ label       : chr "How important is following to workout- to burn calories"
#>    ..@ format.stata: chr "%31.0g"
#>    ..@ labels      : Named num [1:2] 1 6
#>    .. ..- attr(*, "names")= chr [1:2] "not important at all" "very important"
#>  $ cweight : hvn_lbll [1:246]  4,  5,  1,  4,  5,  1,  5,  5,  6, NA,  5,  6,  4,  ...
#>    ..@ label       : chr "How important is following to workout- to control my weight"
#>    ..@ format.stata: chr "%31.0g"
#>    ..@ labels      : Named num [1:2] 1 6
#>    .. ..- attr(*, "names")= chr [1:2] "not important at all" "very important"
#>  $ body    : hvn_lbll [1:246]  3,  4,  4,  4,  5,  5,  5,  2,  6, NA,  1,  5,  3,  ...
#>    ..@ label       : chr "How important is following to workout- to have a good body"
#>    ..@ format.stata: chr "%31.0g"
#>    ..@ labels      : Named num [1:2] 1 6
#>    .. ..- attr(*, "names")= chr [1:2] "not important at all" "very important"
#>  $ appear  : hvn_lbll [1:246]  2,  1,  4,  2,  5,  5,  5,  2,  6, NA,  1,  4,  3,  ...
#>    ..@ label       : chr "How important is following to workout- to improve my appearance"
#>    ..@ format.stata: chr "%31.0g"
#>    ..@ labels      : Named num [1:2] 1 6
#>    .. ..- attr(*, "names")= chr [1:2] "not important at all" "very important"
#>  $ attract : hvn_lbll [1:246]  2,  1,  4,  1,  5,  5,  5,  1,  6, NA,  1,  2,  1,  ...
#>    ..@ label       : chr "How important is following to workout- to look more attractive"
#>    ..@ format.stata: chr "%31.0g"
#>    ..@ labels      : Named num [1:2] 1 6
#>    .. ..- attr(*, "names")= chr [1:2] "not important at all" "very important"
#>  $ muscle  : hvn_lbll [1:246]  3,  1,  6,  1,  3,  5,  5,  1,  6, NA,  5,  6,  2,  ...
#>    ..@ label       : chr "How important is following to workout- to develop my muscles"
#>    ..@ format.stata: chr "%31.0g"
#>    ..@ labels      : Named num [1:2] 1 6
#>    .. ..- attr(*, "names")= chr [1:2] "not important at all" "very important"
#>  $ strength: hvn_lbll [1:246]  2,  5,  6,  4,  4,  5,  5,  4,  6, NA,  6,  6,  3,  ...
#>    ..@ label       : chr "How important is following to workout- to get stronger"
#>    ..@ format.stata: chr "%31.0g"
#>    ..@ labels      : Named num [1:2] 1 6
#>    .. ..- attr(*, "names")= chr [1:2] "not important at all" "very important"
#>  $ endur   : hvn_lbll [1:246]  4,  5,  1,  5,  4,  1,  5,  5,  6, NA,  6,  6,  4,  ...
#>    ..@ label       : chr "How important is following to workout- to increase my endurance"
#>    ..@ format.stata: chr "%31.0g"
#>    ..@ labels      : Named num [1:2] 1 6
#>    .. ..- attr(*, "names")= chr [1:2] "not important at all" "very important"
#>  $ face    : hvn_lbll [1:246]  3,  3, NA,  3,  3, NA,  4,  1,  3, NA,  3,  2,  3,  ...
#>    ..@ label       : chr "How well does the following describe you as a person -  attractive face"
#>    ..@ format.stata: chr "%17.0g"
#>    ..@ labels      : Named num [1:2] 1 6
#>    .. ..- attr(*, "names")= chr [1:2] "very badly" "very well"
#>  $ sexy    : hvn_lbll [1:246]  2,  3, NA,  3,  2, NA, NA,  1,  1, NA,  1,  2,  2,  ...
#>    ..@ label       : chr "How well does the following describe you as a person - sexy"
#>    ..@ format.stata: chr "%17.0g"
#>    ..@ labels      : Named num [1:2] 1 6
#>    .. ..- attr(*, "names")= chr [1:2] "very badly" "very well"

mod.txt <- "
  Attractive =~ face + sexy
  Appearance =~ body + appear + attract
  Muscle     =~ muscle + strength + endur
  Appearance ~ Attractive + age
  Muscle     ~ Appearance + Attractive + age
"
mod <- lavaan::sem(mod.txt, data = workout)
rmedsem(mod, indep = "Attractive", med = "Appearance", dep = "Muscle")
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'Attractive' -> 'Appearance' -> 'Muscle'
#> 
#>                          Sobel          Delta    Monte-Carlo
#> Indirect effect          0.077          0.077          0.077
#> Std. Err.                0.034          0.035          0.035
#> z-value                  2.245          2.222          2.207
#> p-value                 0.0247         0.0263         0.0273
#> CI              [0.010, 0.145] [0.009, 0.146] [0.015, 0.154]
#> 
#> Baron and Kenny approach to testing mediation
#>    STEP 1 - 'Attractive' -> 'Appearance' (X -> M) with B=0.190 and p=0.012
#>    STEP 2 - 'Appearance' -> 'Muscle' (M -> Y) with B=0.409 and p<0.001
#>    STEP 3 - 'Attractive' -> 'Muscle' (X -> Y) with B=0.002 and p=0.985
#>             As STEP 1, STEP 2 and the Sobel's test above are significant
#>             and STEP 3 is not significant the mediation is complete.
#> 
#> Zhao, Lynch & Chen's approach to testing mediation
#> Based on p-value estimated using Monte-Carlo
#>   STEP 1 - 'Attractive' -> 'Muscle' (X -> Y) with B=0.002 and p=0.985
#>             As the Monte-Carlo test above is significant and STEP 1 is not
#>             significant there is indirect-only mediation (full mediation).
#> 
#> Effect sizes
#>    RIT = (Indirect effect / Total effect)
#>          RIT is not reported: total effect 0.079 is too small (< 0.2)
#>    RID = (Indirect effect / Direct effect)
#>          RID is not reported: direct effect 0.002 is not significant (p = 0.985)
#>    Upsilon (v) = Variance in Y explained indirectly by X through M
#>          v(unadj) = 0.006, v(adj) = 0.005
#> 
```
