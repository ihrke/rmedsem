#' Ratio of Indirect to Total Effect (RIT)
#'
#' @param res fitted `rmedsem` object
#' @param ... additional arguments (currently unused)
#'
#' @return A numeric scalar giving the ratio of the indirect effect to
#'   the total effect (indirect / total).
#'
#' @examples
#' mod.txt <- "
#' read ~ math
#' science ~ read + math
#' "
#' mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
#' out <- rmedsem(mod, indep="math", med="read", dep="science")
#' RIT(out)
#'
#' @export
RIT <- function (res, ...)
  UseMethod("RIT")

#' Ratio of Indirect to Direct Effect (RID)
#'
#' @param res fitted `rmedsem` object
#' @param ... additional arguments (currently unused)
#'
#' @return A numeric scalar giving the ratio of the indirect effect to
#'   the direct effect (indirect / direct).
#'
#' @examples
#' mod.txt <- "
#' read ~ math
#' science ~ read + math
#' "
#' mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
#' out <- rmedsem(mod, indep="math", med="read", dep="science")
#' RID(out)
#'
#' @export
RID <- function (res, ...)
  UseMethod("RID")


#' @rdname RIT
#' @export
RIT.default <- function(res, ...) {
  stop(sprintf("RIT() requires an 'rmedsem' object, as returned by rmedsem(), not an object of class '%s'.",
               paste(class(res), collapse="', '")), call.=FALSE)
}

#' @rdname RIT
#' @export
RIT.rmedsem <- function(res, ...) {
  if(is.null(res$effect.size$RIT))
    stop("RIT was not computed. Re-run rmedsem() with effect.size including 'RIT'.")
  if(with(res$effect.size$RIT, ind_eff>tot_eff)){
    warning("Indirect effect is larger than total effect! RIT should not be interpreted")
  }
   return(res$effect.size$RIT$es)
}

#' @rdname RID
#' @export
RID.default <- function(res, ...) {
  stop(sprintf("RID() requires an 'rmedsem' object, as returned by rmedsem(), not an object of class '%s'.",
               paste(class(res), collapse="', '")), call.=FALSE)
}

#' @rdname RID
#' @export
RID.rmedsem <- function(res, ...) {
  if(is.null(res$effect.size$RID))
    stop("RID was not computed. Re-run rmedsem() with effect.size including 'RID'.")
  if(is.null(res$effect.size$RIT)){
    warning("RIT was not computed, cannot check if indirect > total effect.")
  } else if(res$effect.size$RID$ind_eff>res$effect.size$RIT$tot_eff){
    warning("Indirect effect is larger than total effect! RID should not be interpreted")
  }
  return(res$effect.size$RID$es)
}

#' Upsilon Effect Size
#'
#' Returns the Upsilon effect size (Lachowicz, Preacher & Kelley, 2018),
#' an R-squared-type measure representing the variance in Y explained
#' indirectly by X through M.
#'
#' @param res fitted `rmedsem` object
#' @param adjusted logical; if `TRUE` (default), return the bias-adjusted
#'   estimator; if `FALSE`, return the unadjusted estimator
#' @param ... additional arguments (currently unused)
#'
#' @return A numeric scalar giving the Upsilon effect size, an R-squared-type
#'   measure of the variance in the dependent variable explained indirectly
#'   through the mediator.
#'
#' @examples
#' mod.txt <- "
#' read ~ math
#' science ~ read + math
#' "
#' mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
#' out <- rmedsem(mod, indep="math", med="read", dep="science",
#'                effect.size=c("RIT","RID","upsilon"))
#' Upsilon(out)
#' Upsilon(out, adjusted=FALSE)
#'
#' @export
Upsilon <- function (res, ...)
  UseMethod("Upsilon")

#' @rdname Upsilon
#' @export
Upsilon.default <- function(res, ...) {
  stop(sprintf("Upsilon() requires an 'rmedsem' object, as returned by rmedsem(), not an object of class '%s'.",
               paste(class(res), collapse="', '")), call.=FALSE)
}

#' @rdname Upsilon
#' @export
Upsilon.rmedsem <- function(res, adjusted=TRUE, ...) {
  check_flag(adjusted, "adjusted")
  if(is.null(res$effect.size$upsilon))
    stop("Upsilon was not computed. Re-run rmedsem() with effect.size including 'upsilon'.")
  if(adjusted) return(res$effect.size$upsilon$adjusted)
  return(res$effect.size$upsilon$unadjusted)
}

#' Summarize an rmedsem Object
#'
#' `summary()` collects the main results of a mediation analysis in a compact
#' form: a table of the indirect (for each estimation method), direct and
#' total effects, the type of mediation according to the Baron and Kenny
#' and/or Zhao, Lynch & Chen approaches, and the effect sizes. Printing the
#' `rmedsem` object itself ([print.rmedsem()]) gives a more verbose, step-by-step
#' description of the same results.
#'
#' @param object the `rmedsem` object
#' @param x a `summary.rmedsem` object
#' @param digits an integer, number of significant digits to print
#' @param ... additional arguments (currently unused)
#' @return `summary()` returns an object of class `summary.rmedsem`, a list
#'   with elements
#'   \describe{
#'     \item{`package`, `vars`, `standardized`, `nobs`, `ci.level`}{copied
#'       from `object`.}
#'     \item{`ci.type`}{type of the intervals: `"CI"` or, for Bayesian models
#'       fitted with `hdi = TRUE`, `"HDI"`.}
#'     \item{`p.threshold`}{the p-value threshold (`NULL` for Bayesian models).}
#'     \item{`effects`}{a data frame with columns `effect`, `method`,
#'       `estimate`, `se`, `zval`, `pval`, `lower` and `upper` (see
#'       [as.data.frame.rmedsem()]); `NA` where a quantity is not available.}
#'     \item{`mediation`}{a list with elements `bk` and `zlc` giving the type
#'       of mediation (`NULL` if the approach was not requested). `bk` is one
#'       of `"none"`, `"complete"` or `"partial"`; `zlc` is one of
#'       `"indirect-only"`, `"direct-only"`, `"no-effect"`, `"complementary"`
#'       or `"competitive"`.}
#'     \item{`zlc.method`}{the estimation method used for the Zhao, Lynch &
#'       Chen approach.}
#'     \item{`effect.size`}{a named numeric vector with the requested effect
#'       sizes (`RIT`, `RID`, `upsilon` (adjusted) and `upsilon.unadjusted`).}
#'   }
#'   `print.summary.rmedsem()` returns `x` invisibly.
#'
#' @examples
#' mod.txt <- "
#' read ~ math
#' science ~ read + math
#' "
#' mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
#' out <- rmedsem(mod, indep="math", med="read", dep="science")
#' s <- summary(out)
#' s
#' s$mediation
#' s$effects
#'
#' @export
summary.rmedsem <- function(object, ...) {
  bk <- zlc <- NULL
  if (!is.null(object$med.data)) {
    if ("bk" %in% object$med.approach)
      bk <- sub("^partial_.*", "partial", bk_type(object))
    if ("zlc" %in% object$med.approach)
      zlc <- zlc_type(object)
  }
  es <- object$effect.size
  effect.size <- c(RIT=es$RIT$es, RID=es$RID$es,
                   upsilon=es$upsilon$adjusted,
                   upsilon.unadjusted=es$upsilon$unadjusted)

  structure(list(
    package      = object$package,
    vars         = object$vars,
    standardized = object$standardized,
    nobs         = object$nobs,
    ci.level     = object$ci.level,
    ci.type      = ci_type(object),
    p.threshold  = object$med.data$sig_thresh,
    effects      = effects_table(object),
    mediation    = list(bk=bk, zlc=zlc),
    zlc.method   = if (!is.null(zlc)) zlc_method(object),
    effect.size  = effect.size
  ), class = "summary.rmedsem")
}

#' @rdname summary.rmedsem
#' @export
print.summary.rmedsem <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  check_count(digits, "digits")
  cat(with(x$vars, sprintf("Mediation analysis: '%s' -> '%s' -> '%s'\n", indep, med, dep)))
  cat(sprintf("Estimated with '%s' (%s)%s\n\n", x$package,
              ifelse(x$standardized, "standardized", "unstandardized"),
              if (is.null(x$nobs)) "" else sprintf(", N = %d", as.integer(x$nobs))))

  eff <- x$effects
  labs <- ifelse(is.na(eff$method), eff$effect,
                 sprintf("%s (%s)", eff$effect, method_label(eff$method)))
  substr(labs, 1, 1) <- toupper(substr(labs, 1, 1))
  fmt <- function(v) ifelse(is.na(v), "", format(v, digits=digits))
  bayes <- "bayes" %in% eff$method
  # Bayesian tail probabilities are estimated from samples: no "< eps" notation
  fmt_p <- function(v) ifelse(is.na(v), "",
                              if (bayes) format(v, digits=digits)
                              else format.pval(v, digits=digits))
  tab <- data.frame(Estimate=fmt(eff$estimate), `Std. Err.`=fmt(eff$se),
                    `z-value`=fmt(eff$zval), `p-value`=fmt_p(eff$pval),
                    Lower=fmt(eff$lower), Upper=fmt(eff$upper),
                    row.names=labs, check.names=FALSE)
  ci.level <- if (is.null(x$ci.level)) 0.95 else x$ci.level
  cat(sprintf("Effects (%s %s):\n", format_percent(ci.level), ci_type(x)))
  print(tab)
  if (bayes)
    cat("For Bayesian estimates, 'p-value' is the posterior probability of the",
        "opposite sign.\n")

  if (!is.null(x$mediation$bk) || !is.null(x$mediation$zlc)) {
    cat(sprintf("\nType of mediation (significant: p < %s):\n", format(x$p.threshold)))
    if (!is.null(x$mediation$bk))
      cat(sprintf("  Baron & Kenny:      %s\n",
                  switch(x$mediation$bk, none="no mediation",
                         complete="complete mediation", partial="partial mediation")))
    if (!is.null(x$mediation$zlc))
      cat(sprintf("  Zhao, Lynch & Chen: %s; based on %s\n",
                  switch(x$mediation$zlc,
                         "indirect-only"="indirect-only mediation (full mediation)",
                         "direct-only"="direct-only nonmediation (no mediation)",
                         "no-effect"="no-effect nonmediation (no mediation)",
                         complementary="complementary mediation (partial mediation)",
                         competitive="competitive mediation (partial mediation)"),
                  method_label(x$zlc.method)))
  }

  if (length(x$effect.size) > 0) {
    es <- x$effect.size
    names(es) <- sub("^upsilon$", "Upsilon", names(es))
    names(es) <- sub("^upsilon.unadjusted$", "Upsilon (unadj.)", names(es))
    cat("\nEffect sizes:\n")
    cat(paste0("  ", names(es), " = ", vapply(es, format, "", digits=digits), "\n"), sep="")
  }
  cat("\n")
  invisible(x)
}

#' Table of Indirect, Direct and Total Effects
#' @param res an `rmedsem` object
#' @return a data frame, see [summary.rmedsem()]
#' @noRd
effects_table <- function(res){
  get_num <- function(v, n) if (n %in% names(v)) unname(v[[n]]) else NA_real_
  row <- function(effect, method, v)
    data.frame(effect=effect, method=method,
               estimate=get_num(v, "coef"), se=get_num(v, "se"),
               zval=get_num(v, "zval"), pval=get_num(v, "pval"),
               lower=get_num(v, "lower"), upper=get_num(v, "upper"))
  do.call(rbind, c(
    lapply(res$est.methods, \(m) row("indirect", m, res[[m]])),
    list(row("direct", NA_character_, res$direct.effect),
         row("total", NA_character_, res$total.effect))))
}

#' Resolve the Estimation Method for Accessor Functions
#' @param res an `rmedsem` object
#' @param method `NULL` or one of `res$est.methods`
#' @return a single method name; defaults to [zlc_method()]
#' @noRd
resolve_method <- function(res, method){
  if (is.null(method))
    return(zlc_method(res))
  check_string(method, "method")
  if (!method %in% res$est.methods)
    stop(sprintf("'method' must be one of %s for this model.",
                 paste(sprintf("'%s'", res$est.methods), collapse=", ")), call.=FALSE)
  method
}

#' Extract Effects from an rmedsem Object
#'
#' `coef()` returns the estimated indirect, direct and total effects,
#' `confint()` their confidence (or, for Bayesian models, credible) intervals,
#' and `nobs()` the number of observations used to fit the model.
#'
#' The indirect effect is estimated with several methods (see
#' `object$est.methods`), which all give the same point estimate
#' (the product of coefficients; for Monte-Carlo and Bayesian estimation the
#' mean of the samples) but different standard errors and intervals. By
#' default, the method is used that also underlies the Zhao, Lynch & Chen
#' approach: `"montc"` (Monte-Carlo) for `lavaan` and `modsem`, `"boot"`
#' (bootstrap) for `cSEM` and `"bayes"` for `blavaan`.
#'
#' @param object the `rmedsem` object
#' @param method estimation method for the indirect effect, one of
#'   `object$est.methods` (e.g., `"sobel"`, `"delta"`, `"montc"`, `"boot"` or
#'   `"bayes"`); see Details for the default
#' @param parm character vector; a subset of `c("indirect", "direct", "total")`
#' @param level the confidence level. The intervals are computed when calling
#'   [rmedsem()] (argument `ci.two.tailed`), so `level` can only be used to
#'   check that the stored intervals have the requested level.
#' @param ... additional arguments (currently unused)
#'
#' @return `coef()`: a named numeric vector with elements `indirect`, `direct`
#'   and `total`. `confint()`: a matrix with one row per effect and columns
#'   giving the lower and upper limits, labelled by their probabilities (e.g.,
#'   `"2.5 %"` and `"97.5 %"`) or, for highest density intervals
#'   (`rmedsem(..., hdi = TRUE)` for `blavaan` models), `"lower"` and
#'   `"upper"`. `nobs()`: an integer.
#'
#' @examples
#' mod.txt <- "
#' read ~ math
#' science ~ read + math
#' "
#' mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
#' out <- rmedsem(mod, indep="math", med="read", dep="science")
#' coef(out)
#' confint(out)
#' confint(out, parm="indirect", method="sobel")
#' nobs(out)
#'
#' @importFrom stats coef confint nobs
#' @export
coef.rmedsem <- function(object, method=NULL, ...){
  method <- resolve_method(object, method)
  c(indirect=unname(object[[method]][["coef"]]),
    direct=unname(object$direct.effect[["coef"]]),
    total=unname(object$total.effect[["coef"]]))
}

#' @rdname coef.rmedsem
#' @export
confint.rmedsem <- function(object, parm, level=NULL, method=NULL, ...){
  method <- resolve_method(object, method)
  stored <- if (is.null(object$ci.level)) 0.95 else object$ci.level
  if (!is.null(level)) {
    check_ci_level(level, "level")
    if (abs(level - stored) > sqrt(.Machine$double.eps))
      stop(sprintf(paste0("The intervals were computed with ci.two.tailed = %s. ",
                          "Re-run rmedsem() with ci.two.tailed = %s."),
                   format(stored), format(level)), call.=FALSE)
  }
  ci <- rbind(indirect=unname(object[[method]][c("lower", "upper")]),
              direct=unname(object$direct.effect[c("lower", "upper")]),
              total=unname(object$total.effect[c("lower", "upper")]))
  if (ci_type(object) == "HDI") {
    colnames(ci) <- c("lower", "upper")
  } else {
    probs <- c((1-stored)/2, 1-(1-stored)/2)
    colnames(ci) <- paste(format(100*probs, trim=TRUE, scientific=FALSE, digits=3), "%")
  }
  if (!missing(parm)) {
    if (!is.character(parm) || !all(parm %in% rownames(ci)))
      stop("'parm' must be a subset of 'indirect', 'direct', 'total'.", call.=FALSE)
    ci <- ci[parm, , drop=FALSE]
  }
  ci
}

#' @rdname coef.rmedsem
#' @export
nobs.rmedsem <- function(object, ...){
  if (is.null(object$nobs))
    stop("The number of observations is not stored in this 'rmedsem' object.", call.=FALSE)
  as.integer(object$nobs)
}

#' Plot an rmedsem Object
#'
#' Creates a visualization of the mediation analysis results. By default,
#' produces a coefficient plot. Use `type = "effect"` for an effect size
#' pie chart.
#'
#' @param x the `rmedsem` object
#' @param type character; either `"coef"` (default) for a coefficient plot or
#'   `"effect"` for an effect size plot
#' @param ... additional arguments passed to [plot_coef()] or [plot_effect()]
#' @return a `ggplot` object
#'
#' @examples
#' mod.txt <- "
#' read ~ math
#' science ~ read + math
#' "
#' mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
#' out <- rmedsem(mod, indep="math", med="read", dep="science")
#' plot(out)
#' plot(out, type="effect")
#'
#' @export
plot.rmedsem <- function(x, type = c("coef", "effect"), ...) {
  type <- match.arg(type)
  if (type == "coef") {
    plot_coef(x, ...)
  } else {
    plot_effect(x, ...)
  }
}

#' Convert an rmedsem Object to a Data Frame
#'
#' @param x the `rmedsem` object
#' @param ... additional arguments (currently unused)
#' @return a data.frame with one row per estimation method of the indirect
#'   effect (see `x$est.methods`) and columns `package`, `method` and the
#'   estimates stored for that method (for frequentist methods `coef`, `se`,
#'   `zval`, `pval`, `lower` and `upper`; Bayesian estimates additionally
#'   contain posterior probabilities and evidence ratios). See
#'   [summary.rmedsem()] for a table that also includes the direct and total
#'   effects.
#'
#' @examples
#' mod.txt <- "
#' read ~ math
#' science ~ read + math
#' "
#' mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
#' out <- rmedsem(mod, indep="math", med="read", dep="science")
#' as.data.frame(out)
#'
#' @export
as.data.frame.rmedsem <- function(x, ...){
  res <- x
  df <- purrr::map_dfr(res$est.methods, ~ res[[.x]]) |>
    dplyr::bind_cols(method=res$est.methods, package=res$package) |>
    dplyr::relocate(package,method)
  as.data.frame(df)
}
