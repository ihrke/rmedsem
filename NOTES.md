# Notes

MATTHIAS:

- fix website

## Improvements

- catch misspecifications by the user, for example:

```r
full.lpa.mod2 <- "
                 #Measurement model (latent variables)
                   Attractive =~ face + sexy
                   Appearance =~ body + appear + attract
                   Muscle =~ muscle + strength + endur
                   Weight =~ lweight + calories + cweight
                
                   muscle ~~ endur
                   lweight ~~ body
                   Muscle ~~ 0*Weight #set covariance to 0
                
                #Structural model (regressions)
                   Appearance ~ Attractive
                   Muscle ~ Appearance #+ Attractive ## THIS IS THE PROBLEM
                   Weight ~ Appearance #+ Attractive
                 "
est.full.lpa.mod4 <- sem(full.lpa.mod2, data=workout2)
#
library(rmedsem)
med_results <- rmedsem(est.full.lpa.mod4, indep="Attractive", 
                                   med="Appearance", dep="Weight",
                       standardized=TRUE, mcreps=5000,
                       approach = c("bk", "zlc"))
print(med_results)
```

## Bootstrap vs. Monte-Carlo

- implement bootstrap also for CB-SEM
- implement monte carlo also for cSEM
- allow options for both methods in the same function
- also add bias-corrected bootstrap and accelerated bias-corrected bootstrap (see Preacher & Selig, 2012)


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
