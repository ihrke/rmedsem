# --- Helpers ---
fit_hsbdemo_out <- function(...) {
  mod <- lavaan::sem("read ~ math\nscience ~ read + math", data = rmedsem::hsbdemo)
  rmedsem(mod, indep = "math", med = "read", dep = "science", ...)
}

# minimal object as a third-party backend would return it
fake_rmedsem <- function(p_moi = 0.01, p_dom = 0.01, p_doi = 0.01,
                         p_ind = 0.01, approach = c("bk", "zlc")) {
  est <- function(p) c(coef = 0.3, se = 0.1, zval = 3, pval = p, lower = 0.1, upper = 0.5)
  structure(list(
    package = "fakepkg", standardized = TRUE,
    vars = list(med = "M", indep = "X", dep = "Y"),
    est.methods = c("sobel", "perm"),
    sobel = est(p_ind), perm = est(p_ind),
    direct.effect = c(coef = 0.2, se = 0.1, pval = p_doi, lower = 0, upper = 0.4),
    total.effect = c(coef = 0.5, se = 0.1, lower = 0.3, upper = 0.7),
    med.approach = approach,
    effect.size = list(),
    med.data = list(sig_thresh = 0.05,
                    coefs = list(moi = 0.5, dom = 0.6, doi = 0.2),
                    pvals = list(moi = p_moi, dom = p_dom, doi = p_doi))
  ), class = c("rmedsem_fakepkg", "rmedsem"))
}


# --- S3 dispatch ---

test_that("print methods are registered S3 methods", {
  expect_false(is.null(getS3method("print", "rmedsem", optional = TRUE)))
  expect_false(is.null(getS3method("print", "rmedsem_blavaan", optional = TRUE)))
  expect_false(is.null(getS3method("print", "rmedsem_modsem", optional = TRUE)))
})

test_that("print works for a third-party backend without code changes", {
  out <- fake_rmedsem()
  output <- capture.output(print(out))
  expect_true(any(grepl("Model estimated with package 'fakepkg'", output)))
  # unknown method is labelled by its name and used for ZLC by default
  expect_true(any(grepl("perm", output[grepl("Sobel", output)])))
  expect_true(any(grepl("Based on p-value estimated using perm", output)))
})

test_that("zlc.method selects the method used for ZLC", {
  out <- fake_rmedsem()
  out$zlc.method <- "sobel"
  output <- capture.output(print(out))
  expect_true(any(grepl("Based on p-value estimated using Sobel", output)))
})

test_that("built-in frequentist backends store zlc.method", {
  out <- fit_hsbdemo_out()
  expect_equal(out$zlc.method, "montc")
})


# --- BK / ZLC conclusions ---

test_that("BK always prints a conclusion", {
  grid <- expand.grid(p_moi = c(0.01, 0.5), p_dom = c(0.01, 0.5),
                      p_doi = c(0.01, 0.5), p_ind = c(0.01, 0.5))
  for (i in seq_len(nrow(grid))) {
    out <- do.call(fake_rmedsem, c(as.list(grid[i, ]), approach = "bk"))
    output <- capture.output(print(out))
    expect_true(any(grepl("mediation is|no mediation", output)), info = i)
  }
})

test_that("ZLC always prints a conclusion, including competitive mediation", {
  grid <- expand.grid(p_doi = c(0.01, 0.5), p_ind = c(0.01, 0.5))
  for (i in seq_len(nrow(grid))) {
    out <- do.call(fake_rmedsem, c(as.list(grid[i, ]), approach = "zlc"))
    output <- capture.output(print(out))
    expect_true(any(grepl("there is .* mediation", output)), info = i)
  }
  out <- fake_rmedsem(approach = "zlc")
  out$med.data$coefs$doi <- -0.2
  expect_true(any(grepl("competitive", capture.output(print(out)))))
})

test_that("p-value equal to threshold counts as not significant", {
  out <- fake_rmedsem(p_moi = 0.05, approach = "bk")
  expect_true(any(grepl("there is no mediation", capture.output(print(out)))))

  out <- fake_rmedsem(p_doi = 0.05, approach = "zlc")
  expect_true(any(grepl("indirect-only mediation", capture.output(print(out)))))
})

test_that("NA p-values do not break printing", {
  out <- fake_rmedsem(p_doi = NA, p_ind = NA)
  expect_no_error(capture.output(print(out)))
})


# --- print arguments ---

test_that("digits and indent are passed on correctly", {
  out <- fit_hsbdemo_out()
  output <- capture.output(print(out, indent = 6))
  expect_true(any(grepl("^      STEP 1", output)))
  expect_true(any(grepl("^      RIT = ", output)))

  wide <- capture.output(print(out, digits = 6))
  narrow <- capture.output(print(out, digits = 2))
  expect_gt(nchar(wide[grepl("^Indirect effect", wide)]),
            nchar(narrow[grepl("^Indirect effect", narrow)]))
})

test_that("output ends with an empty line (prompt on new line)", {
  output <- capture.output(print(fit_hsbdemo_out(effect.size = "RID")))
  expect_equal(output[length(output)], "")
})

test_that("print and summary agree on the type of mediation", {
  grid <- expand.grid(p_moi = c(0.01, 0.5), p_doi = c(0.01, 0.5), p_ind = c(0.01, 0.5))
  bk_text <- c(none = "no mediation", complete = "mediation is complete",
               partial = "mediation is partial")
  zlc_text <- c("indirect-only" = "indirect-only", "direct-only" = "direct-only",
                "no-effect" = "no effect", complementary = "complementary",
                competitive = "competitive")
  for (i in seq_len(nrow(grid))) {
    out <- do.call(fake_rmedsem, as.list(grid[i, ]))
    s <- summary(out)
    output <- paste(capture.output(print(out)), collapse = " ")
    expect_true(grepl(bk_text[[s$mediation$bk]], output), info = i)
    expect_true(grepl(zlc_text[[s$mediation$zlc]], output), info = i)
  }
})

test_that("very small p-values are not printed as 0", {
  out <- fake_rmedsem(p_ind = 0)
  output <- capture.output(print(out))
  pline <- output[startsWith(output, "p-value")]
  expect_match(pline, "<")
  expect_false(grepl("\\b0\\b", sub("<.*", "", sub("^p-value", "", pline))))
})

test_that("table columns use the same number of decimals", {
  out <- fit_hsbdemo_out()
  output <- capture.output(print(out))
  se <- strsplit(trimws(sub("^Std. Err.", "", output[startsWith(output, "Std. Err.")])), " +")[[1]]
  expect_equal(unique(nchar(sub(".*\\.", "", se))), 3L)
})

test_that("RID is not reported if the direct effect is not significant", {
  out <- fake_rmedsem(p_doi = 0.5)
  out$effect.size <- list(RID = list(es = 1.5, ind_eff = 0.3, dir_eff = 0.2),
                          RIT = list(es = 0.6, ind_eff = 0.3, tot_eff = 0.5))
  output <- capture.output(print(out))
  expect_true(any(grepl("RID is not reported: direct effect 0.200 is not significant", output)))
  expect_false(any(grepl("times as", output)))
  expect_warning(RID(out), "RID should not be interpreted: direct effect")
  expect_no_warning(RIT(out))

  out$direct.effect[["pval"]] <- 0.01
  expect_true(any(grepl("times as", capture.output(print(out)))))
  expect_no_warning(RID(out))
})

test_that("RIT accessor warns if the total effect is too small", {
  out <- fake_rmedsem()
  out$effect.size <- list(RIT = list(es = 0.5, ind_eff = 0.05, tot_eff = 0.1))
  expect_warning(RIT(out), "total effect 0.100 is too small")
  expect_true(any(grepl("RIT is not reported", capture.output(print(out)))))
})

test_that("printed lines are at most 80 characters wide", {
  out <- fit_hsbdemo_out()
  widths <- nchar(c(capture.output(print(out)), capture.output(print(summary(out)))))
  expect_lte(max(widths), 80)
})

test_that("BK/ZLC steps use arrows and format small p-values", {
  out <- fake_rmedsem(p_moi = 0.0001, p_dom = 0.02)
  output <- capture.output(print(out))
  expect_true(any(grepl("STEP 1 - 'X' -> 'M' \\(X -> M\\) with B=0.500 and p<0.001", output)))
  expect_true(any(grepl("STEP 2 - 'M' -> 'Y' \\(M -> Y\\) with B=0.600 and p=0.020", output)))
  expect_false(any(grepl("'X:M'", output)))
})

test_that("RIT interpretation has no double space", {
  out <- fit_hsbdemo_out()
  expect_true(any(grepl("Meaning that about [0-9]+% of", capture.output(print(out)))))
})

test_that("summary and plot_effect flag effect sizes that should not be interpreted", {
  out <- fake_rmedsem(p_doi = 0.5)
  out$effect.size <- list(RIT = list(es = 0.5, ind_eff = 0.05, tot_eff = 0.1),
                          RID = list(es = 0.25, ind_eff = 0.05, dir_eff = 0.2))
  s <- summary(out)
  expect_named(s$effect.size.problems, c("RIT", "RID"))
  output <- capture.output(print(s))
  expect_true(any(grepl("not interpreted: total effect 0.100 is too small", output)))
  expect_true(any(grepl("not interpreted: direct effect 0.200 is not significant", output)))
  expect_lte(max(nchar(output)), 80)

  p <- plot_effect(out)
  expect_match(p$labels$caption, "should not be interpreted")
  expect_false(grepl("is\\smediated\\sby", p$labels$caption))

  ok <- fake_rmedsem()
  ok$effect.size <- list(RIT = list(es = 0.6, ind_eff = 0.3, tot_eff = 0.5),
                         RID = list(es = 1.5, ind_eff = 0.3, dir_eff = 0.2))
  expect_length(summary(ok)$effect.size.problems, 0)
  expect_match(plot_effect(ok)$labels$caption, "is\\smediated\\sby")
})

test_that("as.data.frame handles methods with different stored estimates", {
  out <- fake_rmedsem()
  out$perm <- c(out$perm, extra = 42)
  df <- as.data.frame(out)
  expect_s3_class(df, "data.frame")
  expect_equal(names(df), c("package", "method", "coef", "se", "zval", "pval",
                            "lower", "upper", "extra"))
  expect_equal(df$extra, c(NA, 42))
  expect_equal(df$method, c("sobel", "perm"))
})
