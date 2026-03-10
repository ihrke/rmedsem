# Ratio of Indirect to Total Effect (RIT)

Ratio of Indirect to Total Effect (RIT)

## Usage

``` r
RIT(res, ...)

# S3 method for class 'rmedsem'
RIT(res, ...)
```

## Arguments

- res:

  fitted `rmedsem` object

- ...:

  additional arguments (currently unused)

## Value

A numeric scalar giving the ratio of the indirect effect to the total
effect (indirect / total).

## Examples

``` r
mod.txt <- "
read ~ math
science ~ read + math
"
mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
out <- rmedsem(mod, indep="math", med="read", dep="science")
RIT(out)
#> [1] 0.3973407
```
