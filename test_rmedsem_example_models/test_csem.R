library(cSEM)
#library(rmedsem)
library(haven)
#Example1
hsbdemo <- read_stata("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
#

## for reference
lmodel01 <- "
    read ~ math
    science ~ read + math
"
lmod <- lavaan::sem(lmodel01, data=hsbdemo)




model01 <- "
    # need to use single-item measurement models for PLS-SEM
    Read =~ read
    Math =~ math
    Science =~ science

    # the actual path model
    Read ~ Math
    Science ~ Read + Math

"

mod <- csem(.model=model01, .data=hsbdemo,
            .resample_method = "bootstrap", .R = 200)
indep="Math"; med="Read"; dep="Science";

out <- rmedsem(mod, indep, med, dep, approach="bk")
print(out)


mod$Estimates
mnobs(mod)


mod <- sem(model01, data=hsbdemo)
out <- rmedsem(mod, indep="math", med="read", dep="science",
               standardized=T, mcreps=5000,
               approach = c("bk","zlc"))
print(out)
