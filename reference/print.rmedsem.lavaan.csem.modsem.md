# Print rmedsem Results for Lavaan, cSEM, and Modsem Models

Print rmedsem Results for Lavaan, cSEM, and Modsem Models

## Usage

``` r
# S3 method for class 'rmedsem.lavaan.csem.modsem'
print(res, digits = 3, indent = 3, ci_moderation = FALSE)
```

## Arguments

- res:

  the `rmedsem` object to print

- digits:

  an integer, number of digits to print in table

- indent:

  an integer, number of spaces to indent

- ci_moderation:

  a logical, whether to print confidence intervals for direct, indirect
  and total moderation effects

## Value

`rmedsem` object `res` (invisibly)
