# Plot rmedsem Results

Visualize the results of
[`rmedsem()`](https://ihrke.github.io/rmedsem/reference/rmedsem.md).

## Usage

``` r
# S3 method for class 'rmedsem'
plot(x, type = c("coef", "effect"), ...)

plot_effect(res, description = TRUE)

plot_coef(res)
```

## Arguments

- x, res:

  an `rmedsem` object

- type:

  character; `"coef"` (default) for a coefficient plot or `"effect"` for
  an effect size plot

- ...:

  additional arguments passed to `plot_coef()` or `plot_effect()`

- description:

  logical, whether to add a caption describing the proportion of the
  total effect that is mediated (default `TRUE`)

## Value

a `ggplot` object

## Details

- `plot_coef()`:

  A coefficient plot of the indirect effect (for each estimation
  method), the direct effect and the total effect, with their confidence
  (or credible) intervals.

- `plot_effect()`:

  A pie chart of the (absolute) indirect and direct effects, i.e., the
  proportion of the total effect that is mediated. Requires the effect
  sizes `"RIT"` and `"RID"`.

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) calls
`plot_coef()` (`type = "coef"`) or `plot_effect()` (`type = "effect"`).

## See also

[`rmedsem()`](https://ihrke.github.io/rmedsem/reference/rmedsem.md),
[rmedsem-methods](https://ihrke.github.io/rmedsem/reference/rmedsem-methods.md)

## Examples

``` r
mod.txt <- "
read ~ math
science ~ read + math
"
mod <- lavaan::sem(mod.txt, data = rmedsem::hsbdemo)
out <- rmedsem(mod, indep = "math", med = "read", dep = "science")
plot(out)

plot(out, type = "effect")

plot_effect(out, description = FALSE)

```
