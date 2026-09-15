# Methods for rmedsem Objects

Print, summarize and extract the results of
[`rmedsem()`](https://ihrke.github.io/rmedsem/reference/rmedsem.md).

## Usage

``` r
# S3 method for class 'rmedsem'
summary(object, ...)

# S3 method for class 'summary.rmedsem'
print(x, digits = 3, ...)

# S3 method for class 'rmedsem'
coef(object, method = NULL, ...)

# S3 method for class 'rmedsem'
confint(object, parm, level = NULL, method = NULL, ...)

# S3 method for class 'rmedsem'
nobs(object, ...)

# S3 method for class 'rmedsem'
as.data.frame(x, ...)

# S3 method for class 'rmedsem'
print(x, digits = 3, indent = 3, ...)

# S3 method for class 'rmedsem_blavaan'
print(x, digits = 3, indent = 3, ...)

# S3 method for class 'rmedsem_modsem'
print(x, digits = 3, indent = 3, ci_moderation = FALSE, ...)
```

## Arguments

- object:

  an `rmedsem` object

- ...:

  additional arguments (currently unused)

- x:

  an `rmedsem` object; for `print.summary.rmedsem()` a `summary.rmedsem`
  object

- digits:

  an integer, the number of decimal places to print

- method:

  estimation method for the indirect effect, one of `object$est.methods`
  (e.g., `"sobel"`, `"delta"`, `"montc"`, `"boot"` or `"bayes"`); see
  section 'Extracting results' for the default

- parm:

  character vector; a subset of `c("indirect", "direct", "total")`

- level:

  the confidence level. The intervals are computed by
  [`rmedsem()`](https://ihrke.github.io/rmedsem/reference/rmedsem.md)
  (argument `ci.two.tailed`), so `level` can only be used to check that
  the stored intervals have the requested level.

- indent:

  an integer, the number of spaces to indent

- ci_moderation:

  a logical, whether to print confidence intervals for the moderation
  effects (moderated mediation with `modsem` only)

## Value

[`print()`](https://rdrr.io/r/base/print.html) returns `x` invisibly.

[`summary()`](https://rdrr.io/r/base/summary.html) returns an object of
class `summary.rmedsem`, a list with elements

- `package`, `vars`, `standardized`, `nobs`, `ci.level`:

  copied from `object`.

- `ci.type`:

  type of the intervals: `"CI"` or, for `blavaan` models fitted with
  `hdi = TRUE`, `"HDI"`.

- `p.threshold`:

  the p-value threshold (`NULL` for `blavaan` models).

- `effects`:

  a data frame with columns `effect`, `method`, `estimate`, `se`,
  `zval`, `pval`, `lower` and `upper`; `NA` where a quantity is not
  available. For `blavaan` models, `pval` is the posterior probability
  of the opposite sign.

- `mediation`:

  a list with elements `bk` and `zlc` giving the type of mediation
  (`NULL` if the approach was not requested). `bk` is one of `"none"`,
  `"complete"` or `"partial"`; `zlc` is one of `"indirect-only"`,
  `"direct-only"`, `"no-effect"`, `"complementary"` or `"competitive"`.

- `zlc.method`:

  the estimation method used for the Zhao, Lynch & Chen approach.

- `effect.size`:

  a named numeric vector with the requested effect sizes (`RIT`, `RID`,
  `upsilon` (adjusted) and `upsilon.unadjusted`).

[`coef()`](https://rdrr.io/r/stats/coef.html) returns a named numeric
vector with elements `indirect`, `direct` and `total`.

[`confint()`](https://rdrr.io/r/stats/confint.html) returns a matrix
with one row per effect and columns giving the lower and upper limits,
labelled by their probabilities (e.g., `"2.5 %"` and `"97.5 %"`) or, for
highest density intervals, `"lower"` and `"upper"`.

[`nobs()`](https://rdrr.io/r/stats/nobs.html) returns an integer.

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns a
data frame with one row per estimation method of the indirect effect and
columns `package`, `method` and the estimates stored for that method
(`coef`, `se`, `zval`, `pval`, `lower` and `upper`; `blavaan` results
additionally contain the posterior probabilities `pvpos` and `pvneg` and
the evidence ratios `ERpos` and `ERneg`).

## Printing and summarizing

[`print()`](https://rdrr.io/r/base/print.html) gives a detailed,
step-by-step description of the results: a table of the tests of the
indirect effect (one column per estimation method), the steps and
conclusions of the Baron and Kenny and/or Zhao, Lynch & Chen approaches,
and the effect sizes. For `blavaan` models, the table reports posterior
summaries instead; for moderated mediation with `modsem`, the moderation
effects are printed in addition.

[`summary()`](https://rdrr.io/r/base/summary.html) collects the same
results in compact form: a table of the indirect (for each estimation
method), direct and total effects, the type of mediation, and the effect
sizes.

## Extracting results

[`coef()`](https://rdrr.io/r/stats/coef.html) returns the estimated
indirect, direct and total effects,
[`confint()`](https://rdrr.io/r/stats/confint.html) their confidence
(or, for `blavaan` models, credible) intervals, and
[`nobs()`](https://rdrr.io/r/stats/nobs.html) the number of
observations.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the estimates of the indirect effect for all estimation methods.

The indirect effect is estimated with several methods (see
`object$est.methods`), which all give the same point estimate but
different standard errors and intervals. By default,
[`coef()`](https://rdrr.io/r/stats/coef.html) and
[`confint()`](https://rdrr.io/r/stats/confint.html) use the method that
also underlies the Zhao, Lynch & Chen approach: `"montc"` (Monte-Carlo)
for `lavaan` and `modsem`, `"boot"` (bootstrap) for `cSEM` and `"bayes"`
for `blavaan`.

## Extending the printed output

`print.rmedsem()` handles all backends that provide the elements
described in section 'Adding a backend' of
[`rmedsem()`](https://ihrke.github.io/rmedsem/reference/rmedsem.md).
Backends that need a different output provide a method for their
subclass, either replacing the default output
(`print.rmedsem_blavaan()`) or extending it with
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html)
(`print.rmedsem_modsem()`).

## See also

[`rmedsem()`](https://ihrke.github.io/rmedsem/reference/rmedsem.md),
[effect-sizes](https://ihrke.github.io/rmedsem/reference/effect-sizes.md),
[`plot.rmedsem()`](https://ihrke.github.io/rmedsem/reference/plot.rmedsem.md)

## Examples

``` r
mod.txt <- "
read ~ math
science ~ read + math
"
mod <- lavaan::sem(mod.txt, data = rmedsem::hsbdemo)
out <- rmedsem(mod, indep = "math", med = "read", dep = "science")

# detailed output
print(out)
#> Significance testing of indirect effect (standardized)
#> Model estimated with package 'lavaan'
#> Mediation effect: 'math' -> 'read' -> 'science'
#> 
#>                          Sobel          Delta    Monte-Carlo
#> Indirect effect          0.251          0.251          0.251
#> Std. Err.                0.046          0.046          0.046
#> z-value                  5.501          5.446          5.446
#> p-value               3.79e-08       5.15e-08       5.16e-08
#> CI              [0.161, 0.340] [0.160, 0.341] [0.162, 0.343]
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

# compact summary and its elements
s <- summary(out)
s
#> Mediation analysis: 'math' -> 'read' -> 'science'
#> Estimated with 'lavaan' (standardized), N = 200
#> 
#> Effects (95% CI):
#>                        Estimate Std. Err. z-value  p-value Lower Upper
#> Indirect (Sobel)          0.251     0.046   5.501 3.79e-08 0.161 0.340
#> Indirect (Delta)          0.251     0.046   5.446 5.15e-08 0.160 0.341
#> Indirect (Monte-Carlo)    0.251     0.046   5.446 5.16e-08 0.162 0.343
#> Direct                    0.380     0.065         4.43e-09 0.253 0.507
#> Total                     0.630     0.039                  0.553 0.706
#> 
#> Type of mediation (significant: p < 0.05):
#>   Baron & Kenny:      partial mediation
#>   Zhao, Lynch & Chen: complementary mediation (partial mediation)
#>                       (based on Monte-Carlo test)
#> 
#> Effect sizes:
#>   RIT = 0.397
#>   RID = 0.659
#>   Upsilon = 0.061
#>   Upsilon (unadj.) = 0.063
#> 
s$mediation
#> $bk
#> [1] "partial"
#> 
#> $zlc
#> [1] "complementary"
#> 
s$effects
#>     effect method  estimate         se     zval         pval     lower
#> 1 indirect  sobel 0.2506159 0.04556196 5.500552 3.786031e-08 0.1613161
#> 2 indirect  delta 0.2506159 0.04601847 5.445986 5.151917e-08 0.1604214
#> 3 indirect  montc 0.2506159 0.04598692 5.445778 5.157931e-08 0.1615148
#> 4   direct   <NA> 0.3801172 0.06478771       NA 4.434311e-09 0.2531357
#> 5    total   <NA> 0.6303867 0.03883725       NA           NA 0.5531633
#>       upper
#> 1 0.3399157
#> 2 0.3408105
#> 3 0.3425981
#> 4 0.5070988
#> 5 0.7059270

# extract estimates
coef(out)
#>  indirect    direct     total 
#> 0.2506159 0.3801172 0.6303867 
confint(out)
#>              2.5 %    97.5 %
#> indirect 0.1615148 0.3425981
#> direct   0.2531357 0.5070988
#> total    0.5531633 0.7059270
confint(out, parm = "indirect", method = "sobel")
#>              2.5 %    97.5 %
#> indirect 0.1613161 0.3399157
nobs(out)
#> [1] 200
as.data.frame(out)
#>   package method      coef         se     zval         pval     lower     upper
#> 1  lavaan  sobel 0.2506159 0.04556196 5.500552 3.786031e-08 0.1613161 0.3399157
#> 2  lavaan  delta 0.2506159 0.04601847 5.445986 5.151917e-08 0.1604214 0.3408105
#> 3  lavaan  montc 0.2506159 0.04598692 5.445778 5.157931e-08 0.1615148 0.3425981
```
