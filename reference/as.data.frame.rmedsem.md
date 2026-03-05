# Convert an rmedsem Object to a Data Frame

Convert an rmedsem Object to a Data Frame

## Usage

``` r
# S3 method for class 'rmedsem'
as.data.frame(x, ...)
```

## Arguments

- x:

  the `rmedsem` object

- ...:

  additional arguments (currently unused)

## Value

a data.frame

## Examples

``` r
mod.txt <- "
read ~ math
science ~ read + math
"
mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
out <- rmedsem(mod, indep="math", med="read", dep="science")
as.data.frame(out)
#> # A tibble: 3 × 8
#>   package method  coef     se  zval          pval lower upper
#>   <chr>   <chr>  <dbl>  <dbl> <dbl>         <dbl> <dbl> <dbl>
#> 1 lavaan  sobel  0.251 0.0456  5.50 0.0000000379  0.161 0.340
#> 2 lavaan  delta  0.251 0.0456  5.50 0.0000000379  0.161 0.340
#> 3 lavaan  montc  0.251 0.0424  5.94 0.00000000285 0.180 0.337
```
