test_that("modsem: basic mediation works", {
  skip_on_cran()
  skip_if_not_installed("modsem")

  m <- "
    OwnLook =~ smv_attr_face + smv_attr_body + smv_sexy
    SelfEst =~ ses_satis + ses_qualities + ses_able_todo
    MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
    SelfEst ~ OwnLook
    MentWell ~ OwnLook + SelfEst
  "

  est <- modsem::modsem(m, data = rmedsem::mchoice, method = "lms")
  out <- rmedsem(est, indep = "OwnLook", med = "SelfEst", dep = "MentWell")

  expect_s3_class(out, "rmedsem")
  expect_s3_class(out, "rmedsem_modsem")
  expect_equal(out$package, "modsem")
})

test_that("modsem: effect sizes computed", {
  skip_on_cran()
  skip_if_not_installed("modsem")

  m <- "
    OwnLook =~ smv_attr_face + smv_attr_body + smv_sexy
    SelfEst =~ ses_satis + ses_qualities + ses_able_todo
    MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
    SelfEst ~ OwnLook
    MentWell ~ OwnLook + SelfEst
  "

  est <- modsem::modsem(m, data = rmedsem::mchoice, method = "lms")
  out <- rmedsem(est, indep = "OwnLook", med = "SelfEst", dep = "MentWell",
                 effect.size = c("RIT", "RID"))

  expect_true("RIT" %in% names(out$effect.size))
  expect_true("RID" %in% names(out$effect.size))
})

test_that("modsem: moderated mediation with moderator", {
  skip_on_cran()
  skip_if_not_installed("modsem")

  m <- "
    OwnLook =~ smv_attr_face + smv_attr_body + smv_sexy
    SelfEst =~ ses_satis + ses_qualities + ses_able_todo
    MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
    smv =~ smv_kind + smv_caring + smv_understanding +
      smv_make_laughh + smv_funny + smv_sociable
    SelfEst ~ OwnLook + smv + smv:OwnLook
    MentWell ~ OwnLook + SelfEst + smv + smv:OwnLook
  "

  est <- modsem::modsem(m, data = rmedsem::mchoice, method = "lms")
  out <- rmedsem(est, indep = "OwnLook", med = "SelfEst", dep = "MentWell",
                 moderator = "smv")

  expect_true(out$moderation$has.moderator)
  expect_equal(out$moderation$moderator, "smv")
})
