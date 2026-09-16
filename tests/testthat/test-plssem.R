# plssem models are fitted once and reused (bootstrapping takes a moment)
pls_base <- "
  OwnLook  =~ smv_attr_face + smv_attr_body + smv_sexy
  SelfEst  =~ ses_satis + ses_qualities + ses_able_todo
  MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
"
pls_fits <- new.env()

fit_pls <- function(name, paths, ...) {
  if (is.null(pls_fits[[name]]))
    # plssem warns about kept inadmissible bootstrap replicates
    pls_fits[[name]] <- suppressWarnings(
      plssem::pls(paste(pls_base, paths), rmedsem::mchoice, ...))
  pls_fits[[name]]
}

fit_pls_simple <- function()
  fit_pls("simple", "SelfEst ~ OwnLook\nMentWell ~ OwnLook + SelfEst",
          bootstrap = TRUE, boot.R = 200, boot.iseed = 1)


test_that("plssem: basic mediation returns correct structure", {
  skip_on_cran()
  skip_if_not_installed("plssem")

  out <- rmedsem(fit_pls_simple(), indep = "OwnLook", med = "SelfEst",
                 dep = "MentWell")
  expect_s3_class(out, "rmedsem_plssem")
  expect_s3_class(out, "rmedsem")
  expect_equal(out$package, "plssem")
  expect_equal(out$est.methods, c("sobel", "delta", "boot"))
  expect_equal(out$zlc.method, "boot")
  expect_equal(out$nobs, nrow(rmedsem::mchoice))
  expect_equal(out$nboot, 200)
})

test_that("plssem: estimates match the parameter table and bootstrap samples", {
  skip_on_cran()
  skip_if_not_installed("plssem")

  fit <- fit_pls_simple()
  out <- rmedsem(fit, indep = "OwnLook", med = "SelfEst", dep = "MentWell")
  pt <- as.data.frame(plssem::parameter_estimates(fit))
  b_moi <- pt$est[pt$lhs == "SelfEst" & pt$op == "~" & pt$rhs == "OwnLook"]
  b_dom <- pt$est[pt$lhs == "MentWell" & pt$op == "~" & pt$rhs == "SelfEst"]
  b_doi <- pt$est[pt$lhs == "MentWell" & pt$op == "~" & pt$rhs == "OwnLook"]

  expect_equal(unname(out$sobel["coef"]), b_moi * b_dom)
  expect_equal(unname(out$direct.effect["coef"]), b_doi)
  expect_equal(unname(out$total.effect["coef"]), b_moi * b_dom + b_doi)

  B <- unclass(plssem::boot(fit))
  ind <- B[, "SelfEst~OwnLook"] * B[, "MentWell~SelfEst"]
  expect_equal(unname(out$boot["se"]), sd(ind))
  expect_equal(unname(out$boot[c("lower", "upper")]),
               unname(quantile(ind, c(0.025, 0.975))))
})

test_that("plssem: ci.two.tailed determines the bootstrap intervals", {
  skip_on_cran()
  skip_if_not_installed("plssem")

  fit <- fit_pls_simple()
  out <- rmedsem(fit, indep = "OwnLook", med = "SelfEst", dep = "MentWell",
                 ci.two.tailed = 0.9)
  B <- unclass(plssem::boot(fit))
  ind <- B[, "SelfEst~OwnLook"] * B[, "MentWell~SelfEst"]
  expect_equal(unname(out$boot[c("lower", "upper")]),
               unname(quantile(ind, c(0.05, 0.95))))
  expect_equal(colnames(confint(out)), c("5 %", "95 %"))
})

test_that("plssem: models without bootstrap are rejected", {
  skip_on_cran()
  skip_if_not_installed("plssem")

  fit <- fit_pls("noboot", "SelfEst ~ OwnLook\nMentWell ~ OwnLook + SelfEst")
  expect_error(rmedsem(fit, indep = "OwnLook", med = "SelfEst", dep = "MentWell"),
               "bootstrap = TRUE")
})

test_that("plssem: variables and paths are checked", {
  skip_on_cran()
  skip_if_not_installed("plssem")

  expect_error(rmedsem(fit_pls_simple(), indep = "foo", med = "SelfEst",
                       dep = "MentWell"),
               "Variable 'foo' \\(argument 'indep'\\) not found")
  fit <- fit_pls("nodirect", "SelfEst ~ OwnLook\nMentWell ~ SelfEst",
                 bootstrap = TRUE, boot.R = 20, boot.iseed = 1)
  expect_error(rmedsem(fit, indep = "OwnLook", med = "SelfEst", dep = "MentWell"),
               "'MentWell ~ OwnLook' \\(X -> Y\\)")
})

test_that("plssem: labelled paths give the same results", {
  skip_on_cran()
  skip_if_not_installed("plssem")

  fitl <- fit_pls("labelled", "SelfEst ~ a*OwnLook\nMentWell ~ OwnLook + b*SelfEst",
                  bootstrap = TRUE, boot.R = 200, boot.iseed = 1)
  out <- rmedsem(fit_pls_simple(), indep = "OwnLook", med = "SelfEst", dep = "MentWell")
  outl <- rmedsem(fitl, indep = "OwnLook", med = "SelfEst", dep = "MentWell")
  expect_equal(outl$sobel, out$sobel)
  expect_equal(outl$delta, out$delta)
  expect_equal(outl$boot, out$boot)
})

test_that("plssem: interaction term as independent variable", {
  skip_on_cran()
  skip_if_not_installed("plssem")

  fit <- fit_pls("interaction", "
    OwnPers =~ smv_kind + smv_caring + smv_understanding +
      smv_make_laughh + smv_funny + smv_sociable
    SelfEst ~ OwnLook + OwnPers + OwnPers:OwnLook
    MentWell ~ OwnLook + SelfEst + OwnPers + OwnPers:OwnLook
  ", bootstrap = TRUE, boot.R = 50, boot.iseed = 1)
  out1 <- rmedsem(fit, indep = "OwnPers:OwnLook", med = "SelfEst", dep = "MentWell")
  out2 <- rmedsem(fit, indep = "OwnLook:OwnPers", med = "SelfEst", dep = "MentWell")
  expect_equal(out1$vars$indep, "OwnPers:OwnLook")
  expect_equal(out1$sobel, out2$sobel)
  expect_true(is.finite(out1$boot[["se"]]))
})

test_that("plssem: print, summary and accessors work", {
  skip_on_cran()
  skip_if_not_installed("plssem")

  out <- rmedsem(fit_pls_simple(), indep = "OwnLook", med = "SelfEst", dep = "MentWell")
  output <- capture.output(print(out))
  expect_true(any(grepl("Model estimated with package 'plssem'", output)))
  expect_true(any(grepl("Based on p-value estimated using Bootstrap", output)))
  s <- summary(out)
  expect_equal(s$zlc.method, "boot")
  expect_equal(nobs(out), nrow(rmedsem::mchoice))
  expect_equal(coef(out)[["indirect"]], unname(out$boot["coef"]))
  expect_s3_class(as.data.frame(out), "data.frame")
  expect_s3_class(plot(out), "ggplot")
  expect_lte(max(nchar(c(output, capture.output(print(s))))), 80)
})


# --- MC-PLS models (interaction model with ordinal indicators) ---

pls_int_paths <- "
  OwnPers =~ smv_kind + smv_caring + smv_understanding +
    smv_make_laughh + smv_funny + smv_sociable
  SelfEst ~ OwnLook + OwnPers + OwnPers:OwnLook
  MentWell ~ OwnLook + SelfEst + OwnPers + OwnPers:OwnLook
"

test_that("plssem: MC-PLS with delta SEs uses a Monte-Carlo test", {
  skip_on_cran()
  skip_if_not_installed("plssem")

  fit <- fit_pls("mcpls_delta", pls_int_paths,
                 ordered = names(rmedsem::mchoice),
                 bootstrap = TRUE, boot.R = 50, boot.iseed = 1)
  expect_true(plssem::is_mcpls(fit))

  set.seed(1)
  out <- rmedsem(fit, indep = "OwnPers:OwnLook", med = "SelfEst", dep = "MentWell")
  expect_equal(out$est.methods, c("sobel", "delta", "montc"))
  expect_equal(out$zlc.method, "montc")
  expect_null(out$boot)
  expect_null(out$nboot)
  expect_equal(out$mcreps, 5000)

  # the Monte-Carlo test refers to the corrected estimates and their vcov:
  # interval around the point estimate, SE close to the delta-method SE
  mid <- mean(out$montc[c("lower", "upper")])
  expect_lt(abs(mid - out$montc[["coef"]]), out$montc[["se"]])
  expect_equal(out$montc[["se"]], out$delta[["se"]], tolerance = 0.15)

  # the uncorrected bootstrap samples of plssem would give a clearly smaller SE
  B <- unclass(plssem::boot(fit))
  boot_se <- sd(B[, "SelfEst~OwnPers:OwnLook"] * B[, "MentWell~SelfEst"], na.rm = TRUE)
  expect_lt(boot_se, out$montc[["se"]])

  # reproducible with set.seed(), mcreps is used and checked
  set.seed(1)
  out2 <- rmedsem(fit, indep = "OwnPers:OwnLook", med = "SelfEst", dep = "MentWell")
  expect_equal(out2$montc, out$montc)
  out3 <- rmedsem(fit, indep = "OwnPers:OwnLook", med = "SelfEst", dep = "MentWell",
                  mcreps = 100)
  expect_equal(out3$mcreps, 100)
  expect_error(rmedsem(fit, indep = "OwnPers:OwnLook", med = "SelfEst",
                       dep = "MentWell", mcreps = 0), "'mcreps'")

  output <- capture.output(print(out))
  expect_true(any(grepl("Based on p-value estimated using Monte-Carlo", output)))
  expect_lte(max(nchar(output)), 80)
})

test_that("plssem: MC-PLS without delta SEs uses the bootstrap samples", {
  skip_on_cran()
  skip_if_not_installed("plssem")

  fit <- fit_pls("mcpls_boot", pls_int_paths,
                 ordered = names(rmedsem::mchoice),
                 bootstrap = TRUE, boot.R = 5, boot.iseed = 1,
                 mc.delta.se = FALSE)
  out <- rmedsem(fit, indep = "OwnPers:OwnLook", med = "SelfEst", dep = "MentWell")
  expect_equal(out$est.methods, c("sobel", "delta", "boot"))
  expect_equal(out$zlc.method, "boot")
})

test_that("plssem: ordinal indicators without interaction use the bootstrap", {
  skip_on_cran()
  skip_if_not_installed("plssem")

  fit <- fit_pls("ordinal_linear", "SelfEst ~ OwnLook\nMentWell ~ OwnLook + SelfEst",
                 ordered = names(rmedsem::mchoice),
                 bootstrap = TRUE, boot.R = 30, boot.iseed = 1)
  expect_false(plssem::is_mcpls(fit))
  out <- rmedsem(fit, indep = "OwnLook", med = "SelfEst", dep = "MentWell")
  expect_equal(out$zlc.method, "boot")
})
