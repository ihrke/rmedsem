# blavaan needs to be on the search path for its internal dispatch
# Fit the model once and reuse across tests (MCMC is slow)
blav_mod <- NULL

setup_blavaan <- function() {
  if (!is.null(blav_mod)) return(blav_mod)

  mod.txt <- "
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  "

  require(blavaan)
  suppressWarnings({
    blav_mod <<- blavaan::bsem(mod.txt, data = lavaan::PoliticalDemocracy,
                               std.lv = TRUE, meanstructure = TRUE,
                               n.chains = 1, save.lvs = TRUE,
                               burnin = 250, sample = 250)
  })
  blav_mod
}


test_that("blavaan: basic mediation works", {
  skip_on_cran()
  skip_if_not_installed("blavaan")

  mod <- setup_blavaan()
  out <- rmedsem(mod, indep = "ind60", med = "dem60", dep = "dem65")

  expect_s3_class(out, "rmedsem")
  expect_s3_class(out, "rmedsem_blavaan")
  expect_equal(out$package, "blavaan")
  expect_true(out$standardized)
})

test_that("blavaan: effect sizes are computed", {
  skip_on_cran()
  skip_if_not_installed("blavaan")

  mod <- setup_blavaan()
  out <- rmedsem(mod, indep = "ind60", med = "dem60", dep = "dem65",
                 effect.size = c("RIT", "RID"))

  expect_true("RIT" %in% names(out$effect.size))
  expect_true("RID" %in% names(out$effect.size))
  expect_true(is.finite(out$effect.size$RIT$es))
  expect_true(is.finite(out$effect.size$RID$es))
})

test_that("blavaan: Bayesian-specific output fields present", {
  skip_on_cran()
  skip_if_not_installed("blavaan")

  mod <- setup_blavaan()
  out <- rmedsem(mod, indep = "ind60", med = "dem60", dep = "dem65")

  # Check Bayesian-specific fields
  expect_true("bayes" %in% names(out))
  expect_true("pvpos" %in% names(out$bayes))
  expect_true("pvneg" %in% names(out$bayes))
  expect_true("ERpos" %in% names(out$bayes))
  expect_true("ERneg" %in% names(out$bayes))
  expect_true("prior" %in% names(out))
})
