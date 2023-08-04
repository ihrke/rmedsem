library(tidyverse)
library(MplusAutomation)
library(lavaan)
library(blavaan)

model01 <- "
    read ~ math
    science ~ read + math
"

rmedsem::hsbdemo |>
  mutate(readhigh=ifelse(read>median(read), 1, 0),
         math=as.numeric(scale(math)),
         science=as.numeric(scale(science)),
         female=as.integer(female=="female"),
         ses=as.integer(ordered(ses, levels=c("low","middle","high"))))  -> d

lmod <- sem(model01, data=d)
mod <- bsem(model01, data=d, std.lv=T, meanstructure=T, n.chains=3,
            save.lvs=T, burnin=1000, sample=1000, bcontrol = list(cores = 3))
indep="math";
med="read";
dep="science";
out <- rmedsem.bayes(mod, indep, med, dep)
out


model02 <- "
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
"
lmod <- sem(model02, information="observed", data=lavaan::PoliticalDemocracy)
mod <- bsem(model02, data=lavaan::PoliticalDemocracy, std.lv=T, meanstructure=T, n.chains=3,
            save.lvs=T, burnin=1000, sample=1000, bcontrol = list(cores = 3))
out <- rmedsem(mod,  indep="ind60", med="dem60", dep="dem65")
lout <- rmedsem(lmod,  indep="ind60", med="dem60", dep="dem65", standardized = T)
out

indep="ind60"; med="dem60"; dep="dem65"






model01 <- "
    readhigh ~ math
    science ~ readhigh + math
"

rmedsem::hsbdemo |>
  mutate(readhigh=ordered(ifelse(read>median(read), 1, 0)),
         math=as.numeric(scale(math)),
         science=as.numeric(scale(science)),
         female=as.integer(female=="female"),
         ses=as.integer(ordered(ses, levels=c("low","middle","high"))))  -> d

mod <- bsem(model01, data=d, std.lv=T, meanstructure=T, n.chains=3,
            save.lvs=T, burnin=1000, sample=1000, bcontrol = list(cores = 3))
lmod <- sem(model01, data=d)


#hist(ptsamp)

#draws <- blavInspect(mod, "draws")
#blavInspect(mod, "postmean")


