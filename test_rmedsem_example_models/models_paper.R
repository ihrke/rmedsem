library(lavaan)
library(modsem) # latest github version
devtools::load_all()


m1 <-  "
  OwnLook =~ smv_attr_face + smv_attr_body + smv_sexy
  SelfEst =~ ses_satis + ses_qualities + ses_able_todo
  MentWell =~ mwb_optimistic + mwb_useful + mwb_energy

  SelfEst ~ OwnLook
  MentWell ~ OwnLook + SelfEst
"

est1 <- sem(m1, data = mchoice2)

rmedsem(indep="OwnLook", dep="MentWell", med="SelfEst", mod=est1)


m2 <- "
  OwnLook =~ smv_attr_face + smv_attr_body + smv_sexy
  SelfEst =~ ses_satis + ses_qualities + ses_able_todo
  MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
  smv =~ smv_kind + smv_caring + smv_understanding +
    smv_make_laughh + smv_funny + smv_sociable
  SelfEst ~ OwnLook + smv + smv:OwnLook
  MentWell ~ OwnLook + SelfEst + smv + smv:OwnLook
"


est2 <- modsem(m2, data = mchoice2, method="lms")
rmedsem(indep="smv:OwnLook", dep="MentWell", med="SelfEst", mod=est2)


# Create mchoice2
# load(file="data-raw/01_mchoice.Rdata")
# pt <- modsemify(m2)
# indicators <- unique(pt[pt$op == "=~", "rhs"])
# mchoice2 <- mchoice[indicators]
# save(mchoice2, file="data/mchoice2.rda")

