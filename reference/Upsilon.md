# Upsilon Effect Size

Returns the Upsilon effect size (Lachowicz, Preacher & Kelley, 2018), an
R-squared-type measure representing the variance in Y explained
indirectly by X through M.

## Usage

``` r
Upsilon(res, ...)

# S3 method for class 'rmedsem'
Upsilon(res, adjusted = TRUE, ...)
```

## Arguments

- res:

  fitted `rmedsem` object

- ...:

  additional arguments (currently unused)

- adjusted:

  logical; if `TRUE` (default), return the bias-adjusted estimator; if
  `FALSE`, return the unadjusted estimator

## Examples

``` r
mod.txt <- "
read ~ math
science ~ read + math
"
mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
out <- rmedsem(mod, indep="math", med="read", dep="science",
               effect.size=c("RIT","RID","UPS"))
#> Error in validate_rmedsem_args(indep, med, dep, approach, p.threshold,     effect.size): 'effect.size' must be one or more of 'RIT', 'RID', 'upsilon'.
Upsilon(out)
#> Error: object 'out' not found
Upsilon(out, adjusted=FALSE)
#> Error: object 'out' not found
```
