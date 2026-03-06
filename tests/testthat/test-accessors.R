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

test_that("summary.rmedsem works", {
  mod <- fit_hsbdemo()
  out <- rmedsem(mod, indep = "math", med = "read", dep = "science")

  expect_output(summary(out), "Significance testing")
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
