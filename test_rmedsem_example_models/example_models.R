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
mod <- sem(model03, data=rmedsem::workout)
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
