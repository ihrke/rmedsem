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
