# Effect Sizes for Mediation Analysis

Extract the effect sizes computed by
[`rmedsem()`](https://ihrke.github.io/rmedsem/reference/rmedsem.md)
(argument `effect.size`).

## Usage

``` r
RIT(res, ...)

RID(res, ...)

# Default S3 method
RIT(res, ...)

# S3 method for class 'rmedsem'
RIT(res, ...)

# Default S3 method
RID(res, ...)

# S3 method for class 'rmedsem'
RID(res, ...)

Upsilon(res, ...)

# Default S3 method
Upsilon(res, ...)

# S3 method for class 'rmedsem'
Upsilon(res, adjusted = TRUE, ...)
```

## Arguments

- res:

  an `rmedsem` object

- ...:

  additional arguments (currently unused)

- adjusted:

  logical; if `TRUE` (default), return the bias-adjusted estimator of
  Upsilon; if `FALSE`, the unadjusted estimator

## Value

A numeric scalar.

## Details

- `RIT()`:

  Ratio of the indirect to the total effect, \|indirect\| / \|total\|,
  i.e., the proportion of the total effect that is mediated. Following
  Kenny (see <https://davidakenny.net/cm/mediate.htm>), it should only
  be interpreted if the total effect is not too small (\|total\| \>= 0.2
  for standardized coefficients).

- `RID()`:

  Ratio of the indirect to the direct effect, \|indirect\| / \|direct\|.

- `Upsilon()`:

  The Upsilon effect size (Lachowicz, Preacher & Kelley, 2018), an
  R-squared-type measure of the variance in Y explained indirectly by X
  through M, computed from standardized coefficients.

`RIT()` and `RID()` give a warning (and the printed output of
[`rmedsem()`](https://ihrke.github.io/rmedsem/reference/rmedsem.md) does
not report them) if they should not be interpreted: RIT if the total
effect is small (\|total\| \< 0.2), RID if the direct effect is not
significant (p-value not below `p.threshold`), as the ratio is then
unstable. Both also warn if the indirect effect is larger than the total
effect.

## References

Lachowicz, M. J., Preacher, K. J., & Kelley, K. (2018). A novel measure
of effect size for mediation analysis. *Psychological Methods*, 23(2),
244–261. [doi:10.1037/met0000165](https://doi.org/10.1037/met0000165)

## Examples

``` r
mod.txt <- "
read ~ math
science ~ read + math
"
mod <- lavaan::sem(mod.txt, data = rmedsem::hsbdemo)
out <- rmedsem(mod, indep = "math", med = "read", dep = "science")
RIT(out)
#> [1] 0.3973407
RID(out)
#> [1] 0.6593122
Upsilon(out)
#> [1] 0.06073778
Upsilon(out, adjusted = FALSE)
#> [1] 0.06280834
```
