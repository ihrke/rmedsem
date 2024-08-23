library(lavaan)
library(rmedsem)
library(haven)
#Example1
hsbdemo <- read_stata("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
#
model01 <- "
    read ~ math
    science ~ read + math
"
mod <- sem(model01, data=hsbdemo)
out <- rmedsem(mod, indep="math", med="read", dep="science",
               standardized=T, mcreps=5000,
               approach = c("bk","zlc"))
print(out)

#Example2
lavaan::PoliticalDemocracy
#
model02 <- "
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
"

mod <- sem(model02, information="observed", data=lavaan::PoliticalDemocracy)
out <- rmedsem(mod, indep="ind60", med="dem60", dep="dem65",
               standardized=T, mcreps=5000,
               approach = c("bk","zlc"))
print(out)

#Example3
load(file="workout.Rdata")

model03 <- "
   Attractive =~ face + sexy
   Appearance =~ body + appear + attract
   Muscle =~ muscle + strength + endur
   Weight =~ lweight + calories + cweight
   Appearance ~ Attractive + age
   Muscle ~ Appearance + Attractive + age
   Weight ~ Appearance + Attractive + age
"
model03.csem <- "
   Attractive =~ face + sexy
   Appearance =~ body + appear + attract
   Muscle =~ muscle + strength + endur
   Weight =~ lweight + calories + cweight
   Age =~ age
   Appearance ~ Attractive + Age
   Muscle ~ Appearance + Attractive + Age
   Weight ~ Appearance + Attractive + Age
"

mod <- sem(model03, data=rmedsem::workout)
mod2 <- cSEM::csem(model03.csem, .data=na.omit(rmedsem::workout), .disattenuate = T)

out1 <- rmedsem(mod, indep="Attractive", med="Appearance", dep="Muscle",
               standardized=T, mcreps=5000,
               approach = c("bk","zlc"))
print(out1)
#
out2 <- rmedsem(mod, indep="Attractive", med="Appearance", dep="Weight",
               standardized=T, mcreps=5000,
               approach = c("bk","zlc"))
print(out2)
#
out3 <- rmedsem(mod, indep="age", med="Appearance", dep="Muscle",
                standardized=T, mcreps=5000,
                approach = c("bk","zlc"))
print(out3)
#
out4 <- rmedsem(mod, indep="age", med="Appearance", dep="Weight",
                standardized=T, mcreps=5000,
                approach = c("bk","zlc"))
print(out4)



## for paper
model <- "
   Attractive =~ face + sexy
   Appearance =~ body + appear + attract
   Muscle =~ muscle + strength + endur
   Appearance ~ Attractive
   Muscle ~ Appearance + Attractive
"
library(lavaan)
mod <- lavaan::sem(model, data=rmedsem::workout)
summary(mod)
library(rmedsem)
rmedsem(mod, indep="Attractive", med="Appearance", dep="Muscle",
        approach=c("zlc","bk"))

library(cSEM)
library(blavaan)
mod.lav  <- lavaan::sem(model, data=rmedsem::workout)
mod.csem <- cSEM::csem(model, .data=na.omit(rmedsem::workout), .disattenuate = T)
mod.blav <- blavaan::bsem(model, data=rmedsem::workout, save.lvs=T)

res.lav <- rmedsem(mod.lav, indep="Attractive", med="Appearance", dep="Muscle")
res.csem <- rmedsem(mod.csem, indep="Attractive", med="Appearance", dep="Muscle")
res.blav <- rmedsem(mod.blav, indep="Attractive", med="Appearance", dep="Muscle")
res



model <- "
EXPE ~ IMAG
QUAL ~ EXPE
VAL ~ EXPE + QUAL
SAT ~ IMAG + EXPE + QUAL + VAL
LOY ~ IMAG + SAT

IMAG <~ imag1 + imag2 + imag3
EXPE <~ expe1 + expe2 + expe3
QUAL <~ qual1 + qual2 + qual3 + qual4 + qual5
VAL <~ val1 + val2 + val3

SAT =~ sat1 + sat2 + sat3 + sat4
LOY =~ loy1 + loy2 + loy3 + loy4
"

res <- csem(.data = satisfaction, .model = model,.resample_method = 'bootstrap')
cov(res$Estimates$Estimates_resample$Estimates1$Path_estimates$Resampled)


## new model from Mehmet
med.model <- "
   Own_Look =~ smv_attr_face + smv_attr_body + smv_sexy
   Self_Est =~ ses_satis + ses_qualities + ses_able_todo
   Ment_Well =~ mwb_optimistic + mwb_useful + mwb_energy
   Self_Est ~ Own_Look
   Ment_Well ~ Own_Look + Self_Est
             "

library(cSEM) # PLS-SEM
mod.csem <- csem(med.model, .data=mchoice)
cSEM::summarize(mod.csem)
library(rmedsem)
rmedsem(mod.csem, indep="Own_Look", med="Self_Est", dep="Ment_Well", nbootstrap=1000)




