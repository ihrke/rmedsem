# --- Helpers ---
hsb_model <- function(syntax = "read ~ math\nscience ~ read + math", ...) {
  lavaan::sem(syntax, data = rmedsem::hsbdemo, ...)
}

med_lavaan <- function(mod = hsb_model(), indep = "math", med = "read",
                       dep = "science", ...) {
  rmedsem(mod, indep = indep, med = med, dep = dep, ...)
}

mchoice_syntax <- "
  OwnLook =~ smv_attr_face + smv_attr_body + smv_sexy
  SelfEst =~ ses_satis + ses_qualities + ses_able_todo
  MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
"


# --- Common arguments ---

test_that("indep, med and dep must be single strings", {
  expect_error(med_lavaan(indep = 1), "'indep' must be a single")
  expect_error(med_lavaan(med = c("read", "math")), "'med' must be a single")
  expect_error(med_lavaan(dep = NA_character_), "'dep' must be a single")
  expect_error(med_lavaan(dep = ""), "'dep' must be a single")
})

test_that("indep, med and dep must be distinct", {
  expect_error(med_lavaan(med = "math"), "three different variables")
  expect_error(med_lavaan(dep = "read"), "three different variables")
})

test_that("p.threshold, approach and effect.size are checked", {
  expect_error(med_lavaan(p.threshold = 0), "'p.threshold'")
  expect_error(med_lavaan(p.threshold = 1), "'p.threshold'")
  expect_error(med_lavaan(p.threshold = NA_real_), "'p.threshold'")
  expect_error(med_lavaan(p.threshold = "0.05"), "'p.threshold'")
  expect_error(med_lavaan(approach = "sobel"), "'approach'")
  expect_error(med_lavaan(approach = character(0)), "'approach'")
  expect_error(med_lavaan(effect.size = "R2"), "'effect.size'")
})

test_that("backend-specific arguments are checked (lavaan)", {
  expect_error(med_lavaan(standardized = "yes"), "'standardized' must be a single logical")
  expect_error(med_lavaan(standardized = NA), "'standardized' must be a single logical")
  expect_error(med_lavaan(ci.two.tailed = 95), "'ci.two.tailed'")
  expect_error(med_lavaan(ci.two.tailed = 0), "'ci.two.tailed'")
  expect_error(med_lavaan(mcreps = -1), "'mcreps' must be a single integer")
  expect_error(med_lavaan(mcreps = 1000.5), "'mcreps' must be a single integer")
  expect_error(med_lavaan(mcreps = "1000"), "'mcreps' must be a single integer")
})

test_that("mcreps is used as given (default 5000)", {
  expect_no_message(out <- med_lavaan(mcreps = 10))
  expect_no_message(med_lavaan(mcreps = NULL))
  # with few Monte-Carlo samples the MC estimates differ from many samples
  set.seed(1)
  few <- med_lavaan(mcreps = 10)
  set.seed(1)
  many <- med_lavaan()
  expect_false(isTRUE(all.equal(few$montc[["se"]], many$montc[["se"]], tolerance = 1e-3)))
  set.seed(1)
  expect_equal(med_lavaan(mcreps = 5000)$montc, many$montc)
  set.seed(1)
  expect_equal(med_lavaan(mcreps = NULL)$montc, many$montc)
})


# --- Unsupported objects ---

test_that("rmedsem() on unsupported objects gives an informative error", {
  fit <- stats::lm(science ~ math, data = rmedsem::hsbdemo)
  expect_error(rmedsem(fit, indep = "math", med = "read", dep = "science"),
               "does not support objects of class 'lm'.*lavaan")
})

test_that("effect-size accessors require an rmedsem object", {
  expect_error(RIT(1), "requires an 'rmedsem' object")
  expect_error(RID(list()), "requires an 'rmedsem' object")
  expect_error(Upsilon("a"), "requires an 'rmedsem' object")
  expect_error(Upsilon(med_lavaan(), adjusted = "no"), "'adjusted'")
})


# --- lavaan model checks ---

test_that("lavaan: unknown variables give an informative error", {
  expect_error(med_lavaan(med = "foo"),
               "Variable 'foo' \\(argument 'med'\\) not found in the model")
})

test_that("lavaan: missing paths give an informative error", {
  expect_error(med_lavaan(hsb_model("read ~ math\nscience ~ read")),
               "'science ~ math' \\(X -> Y\\)")
  expect_error(med_lavaan(hsb_model("read ~ math\nscience ~ math")),
               "'science ~ read' \\(M -> Y\\)")
  expect_error(med_lavaan(hsb_model("science ~ read + math")),
               "'read ~ math' \\(X -> M\\)")
  # reversed path does not count
  expect_error(med_lavaan(hsb_model("math ~ read\nscience ~ read + math")),
               "'read ~ math' \\(X -> M\\)")
})

test_that("lavaan: multi-group models are rejected", {
  mod <- hsb_model(group = "female")
  expect_error(med_lavaan(mod), "Multi-group models are not supported")
})

test_that("lavaan: labelled paths give the same results as unlabelled ones", {
  set.seed(1)
  out <- med_lavaan(hsb_model())
  set.seed(1)
  out_lab <- med_lavaan(hsb_model("read ~ a*math\nscience ~ b*read + c*math\nind := a*b"))
  expect_equal(out_lab$sobel, out$sobel)
  expect_equal(out_lab$delta, out$delta)
  expect_equal(out_lab$montc, out$montc)
})


# --- print / plot arguments ---

test_that("print and plot arguments are checked", {
  out <- med_lavaan()
  expect_error(print(out, digits = 0), "'digits'")
  expect_error(print(out, digits = "3"), "'digits'")
  expect_error(print(out, indent = -1), "'indent'")
  expect_error(plot_effect(out, description = "yes"), "'description'")
  expect_error(plot(out, type = "pie"), "'arg' should be one of")
})


# --- cSEM model checks ---

test_that("cSEM: arguments and model are checked", {
  skip_on_cran()
  skip_if_not_installed("cSEM")

  cs <- cSEM::csem(rmedsem::mchoice,
                   paste(mchoice_syntax, "SelfEst ~ OwnLook\nMentWell ~ SelfEst"))
  expect_error(rmedsem(cs, indep = "OwnLook", med = "SelfEst", dep = "MentWell"),
               "'MentWell ~ OwnLook' \\(X -> Y\\)")
  expect_error(rmedsem(cs, indep = "foo", med = "SelfEst", dep = "MentWell"),
               "Variable 'foo' \\(argument 'indep'\\) not found")
  expect_error(rmedsem(cs, indep = "OwnLook", med = "SelfEst", dep = "MentWell",
                       nbootstrap = 0), "'nbootstrap'")
  expect_error(rmedsem(cs, indep = "OwnLook", med = "SelfEst", dep = "MentWell",
                       ci.two.tailed = 1.5), "'ci.two.tailed'")

  d <- rmedsem::mchoice
  d$grp <- rep(1:2, length.out = nrow(d))
  cs_multi <- cSEM::csem(d, paste(mchoice_syntax,
                                  "SelfEst ~ OwnLook\nMentWell ~ SelfEst + OwnLook"),
                         .id = "grp")
  expect_error(rmedsem(cs_multi, indep = "OwnLook", med = "SelfEst", dep = "MentWell"),
               "Only single-group")
})

test_that("cSEM: bootstrap CI follows ci.two.tailed", {
  skip_on_cran()
  skip_if_not_installed("cSEM")

  cs <- cSEM::csem(rmedsem::mchoice,
                   paste(mchoice_syntax, "SelfEst ~ OwnLook\nMentWell ~ SelfEst + OwnLook"))
  out <- rmedsem(cs, indep = "OwnLook", med = "SelfEst", dep = "MentWell",
                 nbootstrap = 200, ci.two.tailed = 0.5)
  expect_length(out$boot, 6)
  expect_length(out$total.effect, 4)
  # 50% CI must be narrower than the 95% normal-theory interval
  expect_lt(unname(out$boot["upper"] - out$boot["lower"]),
            2 * 1.96 * unname(out$boot["se"]))
})


# --- modsem model checks ---

test_that("modsem: arguments and model are checked", {
  skip_on_cran()
  skip_if_not_installed("modsem")

  m <- paste(mchoice_syntax, "
    smv =~ smv_kind + smv_caring + smv_understanding +
      smv_make_laughh + smv_funny + smv_sociable
    SelfEst ~ OwnLook + smv + smv:OwnLook
    MentWell ~ OwnLook + SelfEst + smv
  ")
  est <- modsem::modsem(m, data = rmedsem::mchoice, method = "lms")

  expect_error(rmedsem(est, indep = "OwnLook", med = "smv", dep = "MentWell"),
               "'smv ~ OwnLook' \\(X -> M\\)")
  expect_error(rmedsem(est, indep = "OwnLookk", med = "SelfEst", dep = "MentWell"),
               "Variable 'OwnLookk' \\(argument 'indep'\\) not found")
  expect_error(rmedsem(est, indep = "foo:OwnLook", med = "SelfEst", dep = "MentWell"),
               "Interaction term 'foo:OwnLook' not found")
  expect_error(rmedsem(est, indep = "OwnLook", med = "SelfEst", dep = "MentWell",
                       moderator = "foo"), "'moderator'|Moderator 'foo'")
  expect_error(rmedsem(est, indep = "OwnLook", med = "SelfEst", dep = "MentWell",
                       moderator = "SelfEst"), "'moderator' must be different")
  expect_error(rmedsem(est, indep = "OwnLook", med = "SelfEst", dep = "MentWell",
                       standardized = 1), "'standardized'")
})

test_that("modsem: moderator without interaction term is rejected", {
  skip_on_cran()
  skip_if_not_installed("modsem")

  m <- paste(mchoice_syntax, "
    smv =~ smv_kind + smv_caring + smv_understanding +
      smv_make_laughh + smv_funny + smv_sociable
    SelfEst ~ OwnLook + smv
    MentWell ~ OwnLook + SelfEst + smv
  ")
  est <- modsem::modsem(m, data = rmedsem::mchoice, method = "lms")
  expect_error(rmedsem(est, indep = "OwnLook", med = "SelfEst", dep = "MentWell",
                       moderator = "smv"), "no interaction term of moderator 'smv'")
})

test_that("modsem: labelled paths give the same results as unlabelled ones", {
  skip_on_cran()
  skip_if_not_installed("modsem")

  est <- modsem::modsem(paste(mchoice_syntax, "SelfEst ~ OwnLook\nMentWell ~ SelfEst + OwnLook"),
                        data = rmedsem::mchoice, method = "lms")
  est_lab <- modsem::modsem(paste(mchoice_syntax, "SelfEst ~ a*OwnLook\nMentWell ~ b*SelfEst + OwnLook"),
                            data = rmedsem::mchoice, method = "lms")
  out <- rmedsem(est, indep = "OwnLook", med = "SelfEst", dep = "MentWell")
  out_lab <- rmedsem(est_lab, indep = "OwnLook", med = "SelfEst", dep = "MentWell")
  expect_equal(out_lab$delta, out$delta, tolerance = 1e-3)
})
