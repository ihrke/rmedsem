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
Upsilon(out)
#> [1] 0.06073778
Upsilon(out, adjusted=FALSE)
#> [1] 0.06280834
```
