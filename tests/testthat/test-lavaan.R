# --- Helpers ---
fit_hsbdemo <- function() {
  mod.txt <- "
    read ~ math
    science ~ read + math
  "
  lavaan::sem(mod.txt, data = rmedsem::hsbdemo)
}

fit_poldem <- function() {
  mod.txt <- "
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  "
  lavaan::sem(mod.txt, data = lavaan::PoliticalDemocracy)
}


# --- Simple mediation (hsbdemo, observed variables) ---

test_that("lavaan: basic mediation returns correct structure", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science")

  expect_s3_class(out, "rmedsem")
  expect_s3_class(out, "rmedsem_lavaan")
  expect_equal(out$package, "lavaan")
  expect_equal(out$vars, list(med = "read", indep = "math", dep = "science"))
  expect_true(out$standardized)
})

test_that("lavaan: indirect effect estimates are consistent across methods", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science")

  # All three methods should estimate the same indirect effect coefficient
  expect_equal(out$sobel["coef"], out$delta["coef"])
  expect_equal(out$sobel["coef"], out$montc["coef"], tolerance = 0.01)
})

test_that("lavaan: indirect effect matches product of path coefficients", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                 standardized = TRUE)

  std <- lavaan::standardizedsolution(mod)
  beta_MX  <- std$est.std[std$lhs == "read" & std$rhs == "math" & std$op == "~"]
  beta_YMX <- std$est.std[std$lhs == "science" & std$rhs == "read" & std$op == "~"]
  expected_indirect <- beta_MX * beta_YMX

  expect_equal(unname(out$sobel["coef"]), expected_indirect, tolerance = 1e-6)
})

test_that("lavaan: total effect = indirect + direct", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science")

  indirect <- unname(out$sobel["coef"])
  direct   <- unname(out$direct.effect["coef"])
  total    <- unname(out$total.effect["coef"])

  expect_equal(indirect + direct, total, tolerance = 0.02)
})

test_that("lavaan: RIT and RID are computed and sensible", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                 effect.size = c("RIT", "RID"))

  expect_true("RIT" %in% names(out$effect.size))
  expect_true("RID" %in% names(out$effect.size))

  rit <- out$effect.size$RIT
  rid <- out$effect.size$RID

  # RIT = ind / tot
  expect_equal(rit$es, rit$ind_eff / rit$tot_eff, tolerance = 1e-10)
  # RID = ind / dir
  expect_equal(rid$es, rid$ind_eff / rid$dir_eff, tolerance = 1e-10)
  # ind_eff should be same in both
  expect_equal(rit$ind_eff, rid$ind_eff)
  # tot = ind + dir
  expect_equal(rit$tot_eff, rit$ind_eff + rid$dir_eff, tolerance = 1e-10)
})

test_that("lavaan: effect.size parameter controls what is computed", {
  mod <- fit_hsbdemo()

  out_rit <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                     effect.size = "RIT")
  expect_true("RIT" %in% names(out_rit$effect.size))
  expect_false("RID" %in% names(out_rit$effect.size))

  out_rid <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                     effect.size = "RID")
  expect_false("RIT" %in% names(out_rid$effect.size))
  expect_true("RID" %in% names(out_rid$effect.size))
})

test_that("lavaan: unstandardized works", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                 standardized = FALSE)

  expect_false(out$standardized)
  expect_s3_class(out, "rmedsem")

  # Unstandardized indirect effect should match product of unstd path coefficients
  pars <- lavaan::parameterEstimates(mod)
  beta_MX  <- pars$est[pars$lhs == "read" & pars$rhs == "math" & pars$op == "~"]
  beta_YMX <- pars$est[pars$lhs == "science" & pars$rhs == "read" & pars$op == "~"]

  expect_equal(unname(out$sobel["coef"]), beta_MX * beta_YMX, tolerance = 1e-6)
})

test_that("lavaan: confidence intervals bracket the point estimate", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science")

  for (method in c("sobel", "delta", "montc")) {
    est <- out[[method]]
    expect_true(est["lower"] <= est["coef"])
    expect_true(est["coef"] <= est["upper"])
  }
})


# --- Latent variable model (PoliticalDemocracy) ---

test_that("lavaan: latent variable mediation works", {
  mod <- fit_poldem()
  out <- rmedsem(mod, indep = "ind60", med = "dem60", dep = "dem65")

  expect_s3_class(out, "rmedsem")
  expect_equal(out$vars$indep, "ind60")
  expect_equal(out$vars$med, "dem60")
  expect_equal(out$vars$dep, "dem65")
})

test_that("lavaan: latent model RIT/RID computed", {
  mod <- fit_poldem()
  out <- rmedsem(mod, indep = "ind60", med = "dem60", dep = "dem65",
                 effect.size = c("RIT", "RID"))

  expect_true(!is.null(out$effect.size$RIT))
  expect_true(!is.null(out$effect.size$RID))
  expect_true(is.finite(out$effect.size$RIT$es))
  expect_true(is.finite(out$effect.size$RID$es))
})


# --- Approach selection ---

test_that("lavaan: approach parameter controls output", {
  mod <- fit_hsbdemo()

  out_bk <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                    approach = "bk")
  expect_equal(out_bk$med.approach, "bk")

  out_zlc <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                     approach = "zlc")
  expect_equal(out_zlc$med.approach, "zlc")

  out_both <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                      approach = c("bk", "zlc"))
  expect_equal(out_both$med.approach, c("bk", "zlc"))
})


# --- Input validation ---

test_that("lavaan: invalid inputs produce errors", {
  mod <- fit_hsbdemo()

  expect_error(rmedsem(mod, indep = 1, med = "read", dep = "science"))
  expect_error(rmedsem(mod, indep = "math", med = "read", dep = "science",
                       approach = "invalid"))
  expect_error(rmedsem(mod, indep = "math", med = "read", dep = "science",
                       effect.size = "INVALID"))
  expect_error(rmedsem(mod, indep = "math", med = "read", dep = "science",
                       p.threshold = 2))
})
