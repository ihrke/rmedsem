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

test_that("blavaan: print table rows match the stored values", {
  skip_on_cran()
  skip_if_not_installed("blavaan")

  mod <- setup_blavaan()
  out <- rmedsem(mod, indep = "ind60", med = "dem60", dep = "dem65")
  output <- capture.output(print(out))

  row <- function(lab) trimws(sub(lab, "", output[startsWith(output, lab)], fixed = TRUE))
  expect_equal(as.numeric(row("P(z>0)")), unname(out$bayes["pvpos"]), tolerance = 1e-3)
  expect_equal(as.numeric(row("P(z<0)")), unname(out$bayes["pvneg"]), tolerance = 1e-3)
  ci <- as.numeric(strsplit(gsub("\\[|\\]", "", row("CI")), ",")[[1]])
  expect_equal(ci, unname(out$bayes[c("lower", "upper")]), tolerance = 1e-2)
  expect_false(any(grepl("Baron and Kenny", output)))
  expect_equal(output[length(output)], "")
})

test_that("blavaan: model is checked for variables and paths", {
  skip_on_cran()
  skip_if_not_installed("blavaan")

  mod <- setup_blavaan()
  expect_error(rmedsem(mod, indep = "ind60", med = "foo", dep = "dem65"),
               "Variable 'foo' \\(argument 'med'\\) not found")
  expect_error(rmedsem(mod, indep = "dem60", med = "ind60", dep = "dem65"),
               "'ind60 ~ dem60' \\(X -> M\\)")
})

test_that("blavaan: summary, coef, confint, nobs and as.data.frame work", {
  skip_on_cran()
  skip_if_not_installed("blavaan")

  mod <- setup_blavaan()
  out <- rmedsem(mod, indep = "ind60", med = "dem60", dep = "dem65",
                 ci.two.tailed = 0.9)
  s <- summary(out)
  expect_s3_class(s, "summary.rmedsem")
  expect_equal(s$effects$method[1], "bayes")
  expect_null(s$mediation$bk)
  expect_output(print(s), "posterior probability")

  expect_equal(coef(out)[["indirect"]], unname(out$bayes["coef"]))
  expect_equal(colnames(confint(out)), c("5 %", "95 %"))
  expect_equal(nobs(out), 75L)

  df <- as.data.frame(out)
  expect_s3_class(df, "data.frame")
  expect_equal(df$method, "bayes")
})
