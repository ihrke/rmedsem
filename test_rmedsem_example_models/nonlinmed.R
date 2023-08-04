library(tidyverse)
library(MplusAutomation)
library(lavaan)

model01 <- "
    readhigh ~ math + female + ses
    science ~ readhigh + math
"

rmedsem::hsbdemo |>
  mutate(readhigh=ifelse(read>median(read), 1, 0),
         math=as.numeric(scale(math)),
         science=as.numeric(scale(science)),
         female=as.integer(female=="female"),
         ses=as.integer(ordered(ses, levels=c("low","middle","high"))))  -> d

mod <- sem(model01, data=d)
indep="math";
med="readhigh";
dep="science";
standardized=F

# counterfactual values (reference and target, a* and a)
cf.values <- c(0, 1) # change from 0 to +1

## in func
moi <- sprintf("%s~%s", med, indep)
dom <- sprintf("%s~%s", dep, med)
doi <- sprintf("%s~%s", dep, indep)


if(standardized){
  coefs <- lavaan::standardizedsolution(mod)
  coefs$est <- coefs$est.std
} else {
  coefs <- lavaan::parameterEstimates(mod)
}

## determine type of mediator and dependent, we support binary and continuous
dat <- data.frame(mod@Data@X)
names(dat) <- mod@Data@ov.names[[1]]
## are the mediator or dependent variable(s)
lvs <- lavaan::lavaanNames(mod, "lv")
ovs <- lavaan::lavaanNames(mod, "ov")
med.type <- ifelse(length(unique(dat[,med]))==2, "binary", "continuous")
dep.type <- ifelse(length(unique(dat[,dep]))==2, "binary", "continuous")

# we don't implement moderated mediation yet
if(med.type=="binary" && dep.type=="continuous"){
  theta0 <- with(coefs, est[lhs==dep & rhs==indep])

}


mod1 <- mplusObject(MODEL="
    readhigh on math female ses;
    science on readhigh math;
                    ",
                    VARIABLE = "
                    CATEGORICAL ARE readhigh;
                    ",
                    MODELINDIRECT="
     science IND readhigh math(1 -1);
    ",
                    OUTPUT="sampstat stdyx mod cinterval(bcbootstrap);",
                    usevariables=c("readhigh","math","science"),
                    ANALYSIS = "BOOTSTRAP=100;",#ESTIMATOR=ML;
                    rdata=d
)
res1 <- mplusModeler(mod1, modelout = "test_rmedsem_example_models/mplus/model1.inp",
                     check=T, run = 1)
res1
