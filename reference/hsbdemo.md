# High School and Beyond Demo Dataset

Demographic information and standardized test scores of 200 students
from the High School and Beyond survey, as distributed by the UCLA
Statistical Methods and Data Analytics group.

## Usage

``` r
hsbdemo
```

## Format

### `hsbdemo`

A data frame with 200 rows and 13 columns:

- id:

  Student ID

- female:

  Gender, `"female"` or `"male"`

- ses:

  Socio-economic status, `"low"`, `"middle"` or `"high"`

- schtyp:

  School type, `"public"` or `"private"`

- prog:

  Type of program, `"general"`, `"academic"` or `"vocation"`

- read:

  Reading score

- write:

  Writing score

- math:

  Math score

- science:

  Science score

- socst:

  Social studies score

- honors:

  Enrollment in honors program, `"enrolled"` or `"not enrolled"`

- awards:

  Number of awards

- cid:

  Class ID

## Source

UCLA Statistical Methods and Data Analytics,
<https://stats.oarc.ucla.edu/stat/data/hsbdemo.dta>

## Examples

``` r
str(hsbdemo)
#> tibble [200 × 13] (S3: tbl_df/tbl/data.frame)
#>  $ id     : num [1:200] 45 108 15 67 153 51 164 133 2 53 ...
#>  $ female : chr [1:200] "female" "male" "male" "male" ...
#>  $ ses    : chr [1:200] "low" "middle" "high" "low" ...
#>  $ schtyp : chr [1:200] "public" "public" "public" "public" ...
#>  $ prog   : chr [1:200] "vocation" "general" "vocation" "vocation" ...
#>  $ read   : num [1:200] 34 34 39 37 39 42 31 50 39 34 ...
#>  $ write  : num [1:200] 35 33 39 37 31 36 36 31 41 37 ...
#>  $ math   : num [1:200] 41 41 44 42 40 42 46 40 33 46 ...
#>  $ science: num [1:200] 29 36 26 33 39 31 39 34 42 39 ...
#>  $ socst  : num [1:200] 26 36 42 32 51 39 46 31 41 31 ...
#>  $ honors : chr [1:200] "not enrolled" "not enrolled" "not enrolled" "not enrolled" ...
#>  $ awards : num [1:200] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ cid    : num [1:200] 1 1 1 1 1 1 1 1 1 1 ...

mod <- lavaan::sem("read ~ math\nscience ~ read + math", data = hsbdemo)
rmedsem(mod, indep = "math", med = "read", dep = "science")
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'math' -> 'read' -> 'science'
#> 
#>                          Sobel          Delta    Monte-Carlo
#> Indirect effect          0.251          0.251          0.251
#> Std. Err.                0.046          0.046          0.045
#> z-value                  5.501          5.446          5.506
#> p-value               3.79e-08       5.15e-08       3.67e-08
#> CI              [0.161, 0.340] [0.160, 0.341] [0.165, 0.341]
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
#> 
```
