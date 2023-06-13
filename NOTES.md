# Notes

## Standard errors

- Stata uses OIM (observed information matrix) for estimating standard errors by default
- lavaan uses EIM (expected information matrix)

To use EIM in Stata: 

```stata
qui sem (Ind60 -> x1-x3)(Dem60 -> y1-y4)(Dem65 -> y5-y8)(Dem60<-Ind60)(Dem65<-Dem60 Ind60),method(ml) vce(eim)
```

To use OIM in lavaan:

```r
mod <- sem(model02, information="observed", data=lavaan::PoliticalDemocracy)
```
