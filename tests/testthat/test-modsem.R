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

test_that("modsem: print extends default output with moderation block", {
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

  output <- capture.output(print(out, indent = 5))
  expect_true(any(grepl("Baron and Kenny", output)))
  expect_true(any(grepl("^     STEP 1", output)))   # args passed via NextMethod
  expect_true(any(grepl("Direct moderation effects", output)))
  expect_false(any(grepl("ci = ", output)))
  # arrows point from predictor to outcome
  expect_true(any(grepl("^   OwnLook -> SelfEst +\\| smv", output)))
  expect_false(any(grepl("SelfEst +-> OwnLook", output)))
  ind <- output[which(output == "Indirect moderation effect") + 1]
  expect_match(ind, "OwnLook -> SelfEst -> MentWell \\| smv")
  tot <- output[which(output == "Total moderation effect") + 1]
  expect_match(tot, "OwnLook -> MentWell +\\| smv")

  output_ci <- capture.output(print(out, ci_moderation = TRUE))
  expect_true(any(grepl("95% CI = \\[", output_ci)))
})

test_that("modsem: moderation output fits into 80 characters", {
  skip_on_cran()
  skip_if_not_installed("modsem")

  m <- "
    OwnLook =~ smv_attr_face + smv_attr_body + smv_sexy
    SelfEst =~ ses_satis + ses_qualities + ses_able_todo
    MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
    OwnPers =~ smv_kind + smv_caring + smv_understanding +
      smv_make_laughh + smv_funny + smv_sociable
    SelfEst ~ OwnLook + OwnPers + OwnPers:OwnLook
    MentWell ~ OwnLook + SelfEst + OwnPers + OwnPers:OwnLook
  "
  est <- modsem::modsem(m, data = rmedsem::mchoice, method = "lms")
  out1 <- rmedsem(est, indep = "OwnPers:OwnLook", med = "SelfEst", dep = "MentWell")
  out2 <- rmedsem(est, indep = "OwnLook", med = "SelfEst", dep = "MentWell",
                  moderator = "OwnPers")
  output <- c(capture.output(print(out1)), capture.output(print(summary(out1))),
              capture.output(print(out2, ci_moderation = TRUE)))
  expect_lte(max(nchar(output)), 80)
  expect_true(any(grepl("95% CI = \\[", output)))
})
