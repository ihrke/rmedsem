# --- Helpers ---
fit_hsbdemo <- function(standardized = TRUE) {
  mod.txt <- "
    read ~ math
    science ~ read + math
  "
  mod <- lavaan::sem(mod.txt, data = rmedsem::hsbdemo)
  rmedsem(mod, indep = "math", med = "read", dep = "science",
          standardized = standardized,
          effect.size = c("RIT", "RID", "upsilon"))
}


# --- Hand-computed values ---

test_that("Upsilon matches hand-computed values (lavaan, standardized)", {
  mod.txt <- "
    read ~ math
    science ~ read + math
  "
  mod <- lavaan::sem(mod.txt, data = rmedsem::hsbdemo)
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                 standardized = TRUE, effect.size = c("upsilon"))

  std <- lavaan::standardizedsolution(mod)
  beta_MX  <- std$est.std[std$lhs == "read" & std$rhs == "math" & std$op == "~"]
  beta_YMX <- std$est.std[std$lhs == "science" & std$rhs == "read" & std$op == "~"]
  se_MX    <- std$se[std$lhs == "read" & std$rhs == "math" & std$op == "~"]
  se_YMX   <- std$se[std$lhs == "science" & std$rhs == "read" & std$op == "~"]

  expected_unadj <- beta_MX^2 * beta_YMX^2
  expected_adj   <- (beta_MX^2 - se_MX^2) * (beta_YMX^2 - se_YMX^2)

  expect_equal(out$effect.size$upsilon$unadjusted, expected_unadj)
  expect_equal(out$effect.size$upsilon$adjusted, expected_adj)
  expect_equal(out$effect.size$upsilon$beta_MX, beta_MX)
  expect_equal(out$effect.size$upsilon$beta_YMX, beta_YMX)
})


# --- Unstandardized model still computes Upsilon ---

test_that("Upsilon computed from standardized solution even when standardized=FALSE", {
  mod.txt <- "
    read ~ math
    science ~ read + math
  "
  mod <- lavaan::sem(mod.txt, data = rmedsem::hsbdemo)

  out_std   <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                       standardized = TRUE, effect.size = "upsilon")
  out_unstd <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                       standardized = FALSE, effect.size = "upsilon")

  expect_true(!is.null(out_unstd$effect.size$upsilon))
  expect_equal(out_unstd$effect.size$upsilon$unadjusted,
               out_std$effect.size$upsilon$unadjusted, tolerance = 1e-6)
  expect_equal(out_unstd$effect.size$upsilon$adjusted,
               out_std$effect.size$upsilon$adjusted, tolerance = 1e-6)
})


# --- Properties ---

test_that("Upsilon unadjusted is the squared standardized indirect effect", {
  out <- fit_hsbdemo()

  # Squared standardized indirect effect
  indirect <- unname(out$sobel["coef"])  # already standardized
  expected <- indirect^2

  expect_equal(out$effect.size$upsilon$unadjusted, expected, tolerance = 1e-10)
})

test_that("Upsilon unadjusted >= 0", {
  out <- fit_hsbdemo()
  expect_true(out$effect.size$upsilon$unadjusted >= 0)
})

test_that("Upsilon unadjusted <= 1 for well-behaved models", {
  out <- fit_hsbdemo()
  expect_true(out$effect.size$upsilon$unadjusted <= 1)
})

test_that("Upsilon adjusted < unadjusted (bias correction shrinks)", {
  out <- fit_hsbdemo()
  expect_true(out$effect.size$upsilon$adjusted < out$effect.size$upsilon$unadjusted)
})


# --- Accessor ---

test_that("Upsilon accessor returns adjusted by default", {
  out <- fit_hsbdemo()
  expect_equal(Upsilon(out), out$effect.size$upsilon$adjusted)
})

test_that("Upsilon accessor returns unadjusted when requested", {
  out <- fit_hsbdemo()
  expect_equal(Upsilon(out, adjusted = FALSE), out$effect.size$upsilon$unadjusted)
})

test_that("Upsilon accessor errors when not computed", {
  mod.txt <- "
    read ~ math
    science ~ read + math
  "
  mod <- lavaan::sem(mod.txt, data = rmedsem::hsbdemo)
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                 effect.size = "RIT")
  expect_error(Upsilon(out), "Upsilon was not computed")
})


# --- Printing ---

test_that("Upsilon appears in print output", {
  out <- fit_hsbdemo()
  output <- capture.output(print(out))
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Upsilon", combined))
  expect_true(grepl("v\\(unadj\\)", combined))
  expect_true(grepl("v\\(adj\\)", combined))
})


# --- Latent variable model ---

test_that("Upsilon works with latent variables", {
  mod.txt <- "
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  "
  mod <- lavaan::sem(mod.txt, data = lavaan::PoliticalDemocracy)
  out <- rmedsem(mod, indep = "ind60", med = "dem60", dep = "dem65",
                 effect.size = c("upsilon"))

  expect_true(!is.null(out$effect.size$upsilon))
  expect_true(is.finite(out$effect.size$upsilon$unadjusted))
  expect_true(is.finite(out$effect.size$upsilon$adjusted))
})


# --- Cross-validation with MBESS ---

# Taken from MBESS::upsilon() and patched to work with new lavaan.
# Original used lavaan::lavInspectSampleCov() which was removed.
# Replaced with lavaan::lavInspect(mod, "sampstat")$cov.
upsilon_mbess_patched <- function(x, mediator, dv, conf.level = 0.95,
                                  bootstrap = TRUE, bootstrap.package = "lavaan",
                                  bootstrap.type = "ordinary", B = 1000,
                                  boot.data.out = FALSE, ...) {
  data <- data.frame(x = x, mediator = mediator, dv = dv)
  med.model <- paste0(paste0(colnames(data)[2], "~", colnames(data)[1]),
                      " \n ", paste0(colnames(data)[3], "~", colnames(data)[1],
                                     "+", colnames(data)[2]))
  med.res <- lavaan::sem(med.model, data)
  # Patched: lavInspectSampleCov -> lavInspect(med.res, "sampstat")
  samp_cov <- lavaan::lavInspect(med.res, "sampstat")$cov
  upsilon <- lavaan::coef(med.res)[1]^2 * lavaan::coef(med.res)[3]^2 *
    samp_cov["x", "x"] / samp_cov["dv", "dv"]
  adj.upsilon <- (lavaan::coef(med.res)[1]^2 - lavaan::vcov(med.res)[1, 1]) *
    (lavaan::coef(med.res)[3]^2 - lavaan::vcov(med.res)[3, 3]) *
    samp_cov["x", "x"] / samp_cov["dv", "dv"]
  if (bootstrap == TRUE) {
    if (bootstrap.package == "lavaan") {
      lavaan.med.boot.fun <- function(out) {
        data.boot <- lavaan::lavInspect(out, what = "data")
        colnames(data.boot) <- lavaan::lavNames(out)
        # Patched: lavInspectSampleCov -> lavInspect(out, "sampstat")
        samp_cov_boot <- lavaan::lavInspect(out, "sampstat")$cov
        upsilon.boot <- lavaan::coef(out)[1]^2 * lavaan::coef(out)[3]^2 *
          samp_cov_boot["x", "x"] / samp_cov_boot["dv", "dv"]
        adj.upsilon.boot <- (lavaan::coef(out)[1]^2 - lavaan::vcov(out)[1, 1]) *
          (lavaan::coef(out)[3]^2 - lavaan::vcov(out)[3, 3]) *
          samp_cov_boot["x", "x"] / samp_cov_boot["dv", "dv"]
        as.vector(c(upsilon.boot, adj.upsilon.boot))
      }
      cat("Bootstrapping may take several minutes \n \n")
      boot.med.res <- lavaan::bootstrapLavaan(med.res, R = B,
                                               type = bootstrap.type,
                                               FUN = lavaan.med.boot.fun)
      upsES.out <- data.frame(
        est = c(upsilon, adj.upsilon),
        lcl = c(ups.LCL = quantile(boot.med.res[, 1], probs = (1 - conf.level)/2),
                adj.ups.UCL = quantile(boot.med.res[, 2], probs = (1 - conf.level)/2)),
        ucl = c(ups.LCL = quantile(boot.med.res[, 1], probs = 1 - (1 - conf.level)/2),
                adj.ups.UCL = quantile(boot.med.res[, 2], probs = 1 - (1 - conf.level)/2)),
        row.names = c("Upsilon", "Adj Upsilon"))
      colnames(upsES.out) <- c("Estimate",
                                paste0(conf.level * 100, "% ", bootstrap.type, " LCL"),
                                paste0(conf.level * 100, "% ", bootstrap.type, " UCL"))
      return(upsES.out)
    }
    if (bootstrap.package == "boot") {
      boot.med.boot.fun <- function(out, i) {
        data.boot <- out[i, ]
        colnames(data.boot) <- colnames(out)
        out.boot <- lavaan::sem(med.model, data.boot)
        # Patched: lavInspectSampleCov -> lavInspect(out.boot, "sampstat")
        samp_cov_boot <- lavaan::lavInspect(out.boot, "sampstat")$cov
        upsilon.boot <- lavaan::coef(out.boot)[1]^2 * lavaan::coef(out.boot)[3]^2 *
          samp_cov_boot["x", "x"] / samp_cov_boot["dv", "dv"]
        adj.upsilon.boot <- (lavaan::coef(out.boot)[1]^2 - lavaan::vcov(out.boot)[1, 1]) *
          (lavaan::coef(out.boot)[3]^2 - lavaan::vcov(out.boot)[3, 3]) *
          samp_cov_boot["x", "x"] / samp_cov_boot["dv", "dv"]
        as.vector(c(upsilon.boot, adj.upsilon.boot))
      }
      cat("Bootstrapping may take several minutes \n \n")
      boot.med.res <- boot::boot(data, boot.med.boot.fun, R = B)
      upsES.out <- data.frame(
        est = c(upsilon, adj.upsilon),
        lcl = c(ups.LCL = boot::boot.ci(boot.med.res, conf = conf.level,
                                          type = bootstrap.type, index = 1)[[4]][4],
                adj.ups.UCL = boot::boot.ci(boot.med.res, conf = conf.level,
                                             type = bootstrap.type, index = 2)[[4]][4]),
        ucl = c(ups.LCL = boot::boot.ci(boot.med.res, conf = conf.level,
                                          type = bootstrap.type, index = 1)[[4]][5],
                adj.ups.UCL = boot::boot.ci(boot.med.res, conf = conf.level,
                                             type = bootstrap.type, index = 2)[[4]][5]),
        row.names = c("Upsilon", "Adj Upsilon"))
      colnames(upsES.out) <- c("Estimate",
                                paste0(conf.level * 100, "% ", bootstrap.type, " LCL"),
                                paste0(conf.level * 100, "% ", bootstrap.type, " UCL"))
      if (boot.data.out == TRUE) {
        return(list(upsilon = upsES.out, bootstrap.data = boot.med.res))
      } else return(upsES.out)
    }
  } else {
    upsES.out <- data.frame(Est = c(upsilon, adj.upsilon),
                            row.names = c("Upsilon", "Adj Upsilon"))
    colnames(upsES.out) <- "Estimate"
    return(upsES.out)
  }
}

test_that("Upsilon matches MBESS upsilon (patched)", {
  d <- rmedsem::hsbdemo
  mbess_out <- upsilon_mbess_patched(x = d$math, mediator = d$read,
                                     dv = d$science, bootstrap = FALSE)

  mod <- lavaan::sem("read ~ math\nscience ~ read + math", data = d)
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                 standardized = TRUE, effect.size = "upsilon")

  # Unadjusted values should match exactly (algebraically equivalent).
  # Adjusted values differ slightly because MBESS uses unstandardized SEs
  # while we use standardized SEs for the bias correction.
  expect_equal(out$effect.size$upsilon$unadjusted, mbess_out["Upsilon", "Estimate"],
               tolerance = 1e-6)
  expect_equal(out$effect.size$upsilon$adjusted, mbess_out["Adj Upsilon", "Estimate"],
               tolerance = 1e-2)
})


# --- cSEM ---

test_that("Upsilon works with cSEM", {
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
                 effect.size = c("upsilon"))

  expect_true(!is.null(out$effect.size$upsilon))
  expect_true(out$effect.size$upsilon$unadjusted >= 0)
})


# --- modsem ---

test_that("Upsilon works with modsem", {
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
                 effect.size = c("upsilon"))

  expect_true(!is.null(out$effect.size$upsilon))
  expect_true(out$effect.size$upsilon$unadjusted >= 0)
})


# --- blavaan ---

test_that("Upsilon works with blavaan and has posterior samples", {
  skip_on_cran()
  skip_if_not_installed("blavaan")

  mod.txt <- "
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  "

  require(blavaan)
  suppressWarnings({
    mod <- blavaan::bsem(mod.txt, data = lavaan::PoliticalDemocracy,
                         std.lv = TRUE, meanstructure = TRUE,
                         n.chains = 1, save.lvs = TRUE,
                         burnin = 250, sample = 250)
  })
  out <- rmedsem(mod, indep = "ind60", med = "dem60", dep = "dem65",
                 effect.size = c("upsilon"))

  ups <- out$effect.size$upsilon
  expect_true(!is.null(ups))
  expect_true(is.finite(ups$unadjusted))
  expect_true(is.finite(ups$adjusted))
  expect_true(!is.null(ups$samples))
  expect_true(length(ups$samples) > 0)
  expect_true(is.finite(ups$posterior_mean))
  expect_true(is.finite(ups$posterior_median))
  expect_true(ups$lower <= ups$upper)
})
