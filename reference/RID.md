# Ratio of Indirect to Direct Effect (RID)

Ratio of Indirect to Direct Effect (RID)

## Usage

``` r
RID(res, ...)

# S3 method for class 'rmedsem'
RID(res, ...)
```

## Arguments

- res:

  fitted `rmedsem` object

- ...:

  additional arguments (currently unused)

## Value

A numeric scalar giving the ratio of the indirect effect to the direct
effect (indirect / direct).

## Examples

``` r
mod.txt <- "
read ~ math
science ~ read + math
"
mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
out <- rmedsem(mod, indep="math", med="read", dep="science")
RID(out)
#> [1] 0.6593122
```
