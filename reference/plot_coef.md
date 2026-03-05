# Plot Coefficients for an rmedsem Object

Plot Coefficients for an rmedsem Object

## Usage

``` r
plot_coef(res)
```

## Arguments

- res:

  the `rmedsem` object

## Value

a `ggplot` object

## Examples

``` r
mod.txt <- "
read ~ math
science ~ read + math
"
mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
out <- rmedsem(mod, indep="math", med="read", dep="science")
plot_coef(out)
#> Warning: `position_dodge()` requires non-overlapping x intervals.

```
