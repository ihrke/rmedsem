# Plot Effect Sizes for an rmedsem Object

Plot Effect Sizes for an rmedsem Object

## Usage

``` r
plot_effect(res, description = TRUE)
```

## Arguments

- res:

  the `rmedsem` object

- description:

  logical, whether to add a description subtitle

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
plot_effect(out)

```
