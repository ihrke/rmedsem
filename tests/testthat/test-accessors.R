# --- Helpers ---
fit_hsbdemo <- function() {
  mod.txt <- "
    read ~ math
    science ~ read + math
  "
  lavaan::sem(mod.txt, data = rmedsem::hsbdemo)
}


# --- RIT/RID accessors ---

test_that("RIT accessor returns correct value", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                 effect.size = c("RIT", "RID"))

  expect_equal(RIT(out), out$effect.size$RIT$es)
})

test_that("RID accessor returns correct value", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                 effect.size = c("RIT", "RID"))

  expect_equal(RID(out), out$effect.size$RID$es)
})

test_that("RIT errors when not computed", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                 effect.size = "RID")

  expect_error(RIT(out), "RIT was not computed")
})

test_that("RID errors when not computed", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                 effect.size = "RIT")

  expect_error(RID(out), "RID was not computed")
})


# --- summary ---

test_that("summary.rmedsem returns a summary object", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science")
  s <- summary(out)

  expect_s3_class(s, "summary.rmedsem")
  expect_equal(s$nobs, 200)
  expect_equal(s$ci.level, 0.95)
  expect_equal(s$p.threshold, 0.05)
  expect_equal(s$effects$effect, c("indirect", "indirect", "indirect", "direct", "total"))
  expect_equal(s$effects$method[1:3], c("sobel", "delta", "montc"))
  expect_equal(s$effects$estimate[4], unname(out$direct.effect["coef"]))
  expect_equal(s$effects$pval[1], unname(out$sobel["pval"]))
  expect_true(is.na(s$effects$pval[5]))
  expect_equal(s$mediation, list(bk = "partial", zlc = "complementary"))
  expect_equal(s$zlc.method, "montc")
  expect_equal(unname(s$effect.size["RIT"]), RIT(out))
  expect_equal(unname(s$effect.size["upsilon"]), Upsilon(out))
})

test_that("summary respects requested approach and effect sizes", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                 approach = "bk", effect.size = "RID")
  s <- summary(out)
  expect_null(s$mediation$zlc)
  expect_equal(names(s$effect.size), "RID")
})

test_that("print.summary.rmedsem prints compact output", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science")
  s <- summary(out)

  output <- capture.output(ret <- print(s))
  expect_identical(ret, s)
  expect_true(any(grepl("Mediation analysis: 'math' -> 'read' -> 'science'", output)))
  expect_true(any(grepl("N = 200", output)))
  expect_true(any(grepl("^Indirect \\(Monte-Carlo\\)", output)))
  expect_true(any(grepl("Baron & Kenny: +partial mediation", output)))
  expect_true(any(grepl("complementary mediation", output)))
  expect_output(print(summary(out)), "Effect sizes")
  expect_error(print(s, digits = 0), "'digits'")
})


# --- coef / confint / nobs ---

test_that("coef returns indirect, direct and total effects", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science")
  cf <- coef(out)

  expect_named(cf, c("indirect", "direct", "total"))
  expect_equal(cf[["indirect"]], unname(out$montc["coef"]))
  expect_equal(cf[["direct"]], unname(out$direct.effect["coef"]))
  expect_equal(cf[["total"]], unname(out$total.effect["coef"]))
  expect_equal(coef(out, method = "sobel")[["indirect"]], unname(out$sobel["coef"]))
  expect_error(coef(out, method = "boot"), "'method' must be one of 'sobel', 'delta', 'montc'")
})

test_that("confint returns stored intervals", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science")
  ci <- confint(out)

  expect_equal(dim(ci), c(3L, 2L))
  expect_equal(rownames(ci), c("indirect", "direct", "total"))
  expect_equal(colnames(ci), c("2.5 %", "97.5 %"))
  expect_equal(ci["indirect", ], unname(out$montc[c("lower", "upper")]), ignore_attr = TRUE)
  expect_equal(confint(out, "indirect", method = "delta")[1, ],
               unname(out$delta[c("lower", "upper")]), ignore_attr = TRUE)
  expect_equal(rownames(confint(out, c("direct", "total"))), c("direct", "total"))
  expect_no_error(confint(out, level = 0.95))
  expect_error(confint(out, level = 0.9), "ci.two.tailed = 0.95")
  expect_error(confint(out, parm = "foo"), "'parm'")
})

test_that("ci.two.tailed determines all intervals, incl. Monte-Carlo", {
  mod <- fit_hsbdemo()
  set.seed(1)
  out95 <- rmedsem(mod, indep = "math", med = "read", dep = "science")
  set.seed(1)
  out50 <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                   ci.two.tailed = 0.5)
  w <- function(ci) ci[, 2] - ci[, 1]
  expect_true(all(w(confint(out50)) < w(confint(out95))))
  expect_equal(colnames(confint(out50)), c("25 %", "75 %"))
})

test_that("nobs returns the sample size", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science")
  expect_identical(nobs(out), 200L)
})


# --- print ---

test_that("print outputs expected sections", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science",
                 approach = c("bk", "zlc"))

  output <- capture.output(print(out))
  combined <- paste(output, collapse = "\n")

  expect_true(grepl("Significance testing", combined))
  expect_true(grepl("lavaan", combined))
  expect_true(grepl("math.*read.*science", combined))
  expect_true(grepl("Baron and Kenny", combined))
  expect_true(grepl("Zhao, Lynch & Chen", combined))
  expect_true(grepl("RIT", combined))
  expect_true(grepl("RID", combined))
})

test_that("print returns object invisibly", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science")

  result <- invisible(capture.output(ret <- print(out)))
  expect_identical(ret, out)
})


# --- as.data.frame ---

test_that("as.data.frame returns proper data frame", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science")

  df <- as.data.frame(out)

  expect_s3_class(df, "data.frame")
  expect_true("package" %in% names(df))
  expect_true("method" %in% names(df))
  expect_equal(nrow(df), length(out$est.methods))
  expect_false(inherits(df, "tbl_df"))
})


# --- plot ---

test_that("plot.rmedsem produces ggplot objects", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science")

  p1 <- plot(out, type = "coef")
  expect_s3_class(p1, "ggplot")

  p2 <- plot(out, type = "effect")
  expect_s3_class(p2, "ggplot")
})
