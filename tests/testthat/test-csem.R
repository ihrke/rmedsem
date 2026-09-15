test_that("cSEM: basic mediation works", {
  skip_on_cran()
  skip_if_not_installed("cSEM")

  model <- "
    Attractive =~ face + sexy
    Appearance =~ body + appear + attract
    Muscle =~ muscle + strength + endur
    Appearance ~ Attractive
    Muscle ~ Appearance + Attractive
  "

  mod <- cSEM::csem(model, .data = na.omit(rmedsem::workout), .disattenuate = TRUE)
  out <- rmedsem(mod, indep = "Attractive", med = "Appearance", dep = "Muscle")

  expect_s3_class(out, "rmedsem")
  expect_s3_class(out, "rmedsem_cSEM")
  expect_equal(out$package, "cSEM")
  expect_true(out$standardized)
})

test_that("cSEM: indirect effect structure is correct", {
  skip_on_cran()
  skip_if_not_installed("cSEM")

  model <- "
    Attractive =~ face + sexy
    Appearance =~ body + appear + attract
    Muscle =~ muscle + strength + endur
    Appearance ~ Attractive
    Muscle ~ Appearance + Attractive
  "

  mod <- cSEM::csem(model, .data = na.omit(rmedsem::workout), .disattenuate = TRUE)
  out <- rmedsem(mod, indep = "Attractive", med = "Appearance", dep = "Muscle")

  # Sobel and delta should have same coefficient
  expect_equal(out$sobel["coef"], out$delta["coef"])

  # CIs should bracket estimate
  expect_true(out$sobel["lower"] <= out$sobel["coef"])
  expect_true(out$sobel["coef"] <= out$sobel["upper"])
})

test_that("cSEM: effect sizes computed", {
  skip_on_cran()
  skip_if_not_installed("cSEM")

  model <- "
    Attractive =~ face + sexy
    Appearance =~ body + appear + attract
    Muscle =~ muscle + strength + endur
    Appearance ~ Attractive
    Muscle ~ Appearance + Attractive
  "

  mod <- cSEM::csem(model, .data = na.omit(rmedsem::workout), .disattenuate = TRUE)
  out <- rmedsem(mod, indep = "Attractive", med = "Appearance", dep = "Muscle",
                 effect.size = c("RIT", "RID"))

  expect_true("RIT" %in% names(out$effect.size))
  expect_true("RID" %in% names(out$effect.size))
  expect_true(out$effect.size$RIT$es > 0)
  expect_true(out$effect.size$RID$es > 0)
})

test_that("cSEM: uses boot estimation method", {
  skip_on_cran()
  skip_if_not_installed("cSEM")

  model <- "
    Attractive =~ face + sexy
    Appearance =~ body + appear + attract
    Muscle =~ muscle + strength + endur
    Appearance ~ Attractive
    Muscle ~ Appearance + Attractive
  "

  mod <- cSEM::csem(model, .data = na.omit(rmedsem::workout), .disattenuate = TRUE)
  out <- rmedsem(mod, indep = "Attractive", med = "Appearance", dep = "Muscle")

  expect_true("boot" %in% names(out))
  expect_equal(out$est.methods, c("sobel", "delta", "boot"))
})

test_that("cSEM: summary, coef, confint and nobs work", {
  skip_on_cran()
  skip_if_not_installed("cSEM")

  model <- "
    OwnLook =~ smv_attr_face + smv_attr_body + smv_sexy
    SelfEst =~ ses_satis + ses_qualities + ses_able_todo
    MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
    SelfEst ~ OwnLook
    MentWell ~ OwnLook + SelfEst
  "
  cs <- cSEM::csem(rmedsem::mchoice, model)
  out <- rmedsem(cs, indep = "OwnLook", med = "SelfEst", dep = "MentWell",
                 nbootstrap = 50)
  s <- summary(out)
  expect_equal(s$zlc.method, "boot")
  expect_equal(nobs(out), nrow(rmedsem::mchoice))
  expect_equal(confint(out)["indirect", ], unname(out$boot[c("lower", "upper")]),
               ignore_attr = TRUE)
})
