library(lavaan)


mod1.txt <- "
  appear ~ attract
  calories ~ appear + attract
"

mod1 <- lavaan::sem(mod1.txt, data=rmedsem::workout)


out1 <- rmedsem::rmedsem(mod1, indep="attract", med="appear", dep="calories",
                         approach = c("bk","zlc"))
print(out1)

mod2.txt <- "
  attract ~ appear
  calories ~ attract + appear
"

mod2 <- lavaan::sem(mod2.txt, data=rmedsem::workout)


out2 <- rmedsem::rmedsem(mod2, indep="appear", med="attract", dep="calories")
print(out2)
