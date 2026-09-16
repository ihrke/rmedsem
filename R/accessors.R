#' Effect Sizes for Mediation Analysis
#'
#' Extract the effect sizes computed by [rmedsem()] (argument `effect.size`).
#'
#' \describe{
#'   \item{`RIT()`}{Ratio of the indirect to the total effect,
#'     |indirect| / |total|, i.e., the proportion of the total effect that is
#'     mediated. Following Kenny (see \url{https://davidakenny.net/cm/mediate.htm}),
#'     it should only be interpreted if the total effect is not too small
#'     (|total| >= 0.2 for standardized coefficients).}
#'   \item{`RID()`}{Ratio of the indirect to the direct effect,
#'     |indirect| / |direct|.}
#'   \item{`Upsilon()`}{The Upsilon effect size (Lachowicz, Preacher & Kelley,
#'     2018), an R-squared-type measure of the variance in Y explained
#'     indirectly by X through M, computed from standardized coefficients.}
#' }
#' `RIT()` and `RID()` give a warning (and the printed output of [rmedsem()]
#' does not report them) if they should not be interpreted: RIT if the total
#' effect is small (|total| < 0.2), RID if the direct effect is not
#' significant (p-value not below `p.threshold`), as the ratio is then
#' unstable. Both also warn if the indirect effect is larger than the total
#' effect.
#'
#' @param res an `rmedsem` object
#' @param adjusted logical; if `TRUE` (default), return the bias-adjusted
#'   estimator of Upsilon; if `FALSE`, the unadjusted estimator
#' @param ... additional arguments (currently unused)
#'
#' @return A numeric scalar.
#'
#' @references
#' Lachowicz, M. J., Preacher, K. J., & Kelley, K. (2018). A novel measure of
#' effect size for mediation analysis. *Psychological Methods*, 23(2),
#' 244--261. \doi{10.1037/met0000165}
#'
#' @examples
#' mod.txt <- "
#' read ~ math
#' science ~ read + math
#' "
#' mod <- lavaan::sem(mod.txt, data = rmedsem::hsbdemo)
#' out <- rmedsem(mod, indep = "math", med = "read", dep = "science")
#' RIT(out)
#' RID(out)
#' Upsilon(out)
#' Upsilon(out, adjusted = FALSE)
#'
#' @name effect-sizes
NULL

#' @rdname effect-sizes
#' @export
RIT <- function (res, ...)
  UseMethod("RIT")

#' @rdname effect-sizes
#' @export
RID <- function (res, ...)
  UseMethod("RID")


#' @rdname effect-sizes
#' @export
RIT.default <- function(res, ...) {
  stop(sprintf("RIT() requires an 'rmedsem' object, as returned by rmedsem(), not an object of class '%s'.",
               paste(class(res), collapse="', '")), call.=FALSE)
}

#' @rdname effect-sizes
#' @export
RIT.rmedsem <- function(res, ...) {
  if(is.null(res$effect.size$RIT))
    stop("RIT was not computed. Re-run rmedsem() with effect.size including 'RIT'.")
  if(with(res$effect.size$RIT, ind_eff>tot_eff)){
    warning("Indirect effect is larger than total effect! RIT should not be interpreted")
  }
  problem <- effect_size_problem(res, "RIT")
  if (!is.null(problem))
    warning(sprintf("RIT should not be interpreted: %s.", problem), call.=FALSE)
  return(res$effect.size$RIT$es)
}

#' @rdname effect-sizes
#' @export
RID.default <- function(res, ...) {
  stop(sprintf("RID() requires an 'rmedsem' object, as returned by rmedsem(), not an object of class '%s'.",
               paste(class(res), collapse="', '")), call.=FALSE)
}

#' @rdname effect-sizes
#' @export
RID.rmedsem <- function(res, ...) {
  if(is.null(res$effect.size$RID))
    stop("RID was not computed. Re-run rmedsem() with effect.size including 'RID'.")
  if(is.null(res$effect.size$RIT)){
    warning("RIT was not computed, cannot check if indirect > total effect.")
  } else if(res$effect.size$RID$ind_eff>res$effect.size$RIT$tot_eff){
    warning("Indirect effect is larger than total effect! RID should not be interpreted")
  }
  problem <- effect_size_problem(res, "RID")
  if (!is.null(problem))
    warning(sprintf("RID should not be interpreted: %s.", problem), call.=FALSE)
  return(res$effect.size$RID$es)
}

#' @rdname effect-sizes
#' @export
Upsilon <- function (res, ...)
  UseMethod("Upsilon")

#' @rdname effect-sizes
#' @export
Upsilon.default <- function(res, ...) {
  stop(sprintf("Upsilon() requires an 'rmedsem' object, as returned by rmedsem(), not an object of class '%s'.",
               paste(class(res), collapse="', '")), call.=FALSE)
}

#' @rdname effect-sizes
#' @export
Upsilon.rmedsem <- function(res, adjusted=TRUE, ...) {
  check_flag(adjusted, "adjusted")
  if(is.null(res$effect.size$upsilon))
    stop("Upsilon was not computed. Re-run rmedsem() with effect.size including 'upsilon'.")
  if(adjusted) return(res$effect.size$upsilon$adjusted)
  return(res$effect.size$upsilon$unadjusted)
}

#' Methods for rmedsem Objects
#'
#' Print, summarize and extract the results of [rmedsem()].
#'
#' @section Printing and summarizing:
#' `print()` gives a detailed, step-by-step description of the results: a
#' table of the tests of the indirect effect (one column per estimation
#' method), the steps and conclusions of the Baron and Kenny and/or Zhao,
#' Lynch & Chen approaches, and the effect sizes. For `blavaan` models, the
#' table reports posterior summaries instead; for moderated mediation with
#' `modsem`, the moderation effects are printed in addition.
#'
#' `summary()` collects the same results in compact form: a table of the
#' indirect (for each estimation method), direct and total effects, the type
#' of mediation, and the effect sizes.
#'
#' @section Extracting results:
#' `coef()` returns the estimated indirect, direct and total effects,
#' `confint()` their confidence (or, for `blavaan` models, credible)
#' intervals, and `nobs()` the number of observations. `as.data.frame()`
#' returns the estimates of the indirect effect for all estimation methods.
#'
#' The indirect effect is estimated with several methods (see
#' `object$est.methods`), which all give the same point estimate but
#' different standard errors and intervals. By default, `coef()` and
#' `confint()` use the method that also underlies the Zhao, Lynch & Chen
#' approach: `"montc"` (Monte-Carlo) for `lavaan` and `modsem`, `"boot"`
#' (bootstrap) for `cSEM` and `plssem`, and `"bayes"` for `blavaan`.
#'
#' @section Extending the printed output:
#' `print.rmedsem()` handles all backends that provide the elements described
#' in section 'Adding a backend' of [rmedsem()]. Backends that need a
#' different output provide a method for their subclass, either replacing
#' the default output (`print.rmedsem_blavaan()`) or extending it with
#' [NextMethod()] (`print.rmedsem_modsem()`).
#'
#' @param x an `rmedsem` object; for `print.summary.rmedsem()` a
#'   `summary.rmedsem` object
#' @param object an `rmedsem` object
#' @param digits an integer, the number of decimal places to print
#' @param indent an integer, the number of spaces to indent
#' @param ci_moderation a logical, whether to print confidence intervals for
#'   the moderation effects (moderated mediation with `modsem` only)
#' @param method estimation method for the indirect effect, one of
#'   `object$est.methods` (e.g., `"sobel"`, `"delta"`, `"montc"`, `"boot"` or
#'   `"bayes"`); see section 'Extracting results' for the default
#' @param parm character vector; a subset of `c("indirect", "direct", "total")`
#' @param level the confidence level. The intervals are computed by
#'   [rmedsem()] (argument `ci.two.tailed`), so `level` can only be used to
#'   check that the stored intervals have the requested level.
#' @param ... additional arguments (currently unused)
#'
#' @return
#' `print()` returns `x` invisibly.
#'
#' `summary()` returns an object of class `summary.rmedsem`, a list with
#' elements
#' \describe{
#'   \item{`package`, `vars`, `standardized`, `nobs`, `ci.level`}{copied
#'     from `object`.}
#'   \item{`ci.type`}{type of the intervals: `"CI"` or, for `blavaan` models
#'     fitted with `hdi = TRUE`, `"HDI"`.}
#'   \item{`p.threshold`}{the p-value threshold (`NULL` for `blavaan` models).}
#'   \item{`effects`}{a data frame with columns `effect`, `method`,
#'     `estimate`, `se`, `zval`, `pval`, `lower` and `upper`; `NA` where a
#'     quantity is not available. For `blavaan` models, `pval` is the
#'     posterior probability of the opposite sign.}
#'   \item{`mediation`}{a list with elements `bk` and `zlc` giving the type
#'     of mediation (`NULL` if the approach was not requested). `bk` is one
#'     of `"none"`, `"complete"` or `"partial"`; `zlc` is one of
#'     `"indirect-only"`, `"direct-only"`, `"no-effect"`, `"complementary"`
#'     or `"competitive"`.}
#'   \item{`zlc.method`}{the estimation method used for the Zhao, Lynch &
#'     Chen approach.}
#'   \item{`effect.size`}{a named numeric vector with the requested effect
#'     sizes (`RIT`, `RID`, `upsilon` (adjusted) and `upsilon.unadjusted`).}
#'   \item{`effect.size.problems`}{a named character vector with an entry for
#'     each of `RIT` and `RID` that should not be interpreted (see
#'     [effect-sizes]), describing the reason; empty if there is none.}
#' }
#'
#' `coef()` returns a named numeric vector with elements `indirect`,
#' `direct` and `total`.
#'
#' `confint()` returns a matrix with one row per effect and columns giving
#' the lower and upper limits, labelled by their probabilities (e.g.,
#' `"2.5 %"` and `"97.5 %"`) or, for highest density intervals, `"lower"`
#' and `"upper"`.
#'
#' `nobs()` returns an integer.
#'
#' `as.data.frame()` returns a data frame with one row per estimation method
#' of the indirect effect and columns `package`, `method` and the estimates
#' stored for that method (`coef`, `se`, `zval`, `pval`, `lower` and `upper`;
#' `blavaan` results additionally contain the posterior probabilities `pvpos`
#' and `pvneg` and the evidence ratios `ERpos` and `ERneg`).
#'
#' @seealso [rmedsem()], [effect-sizes], [plot.rmedsem()]
#'
#' @examples
#' mod.txt <- "
#' read ~ math
#' science ~ read + math
#' "
#' mod <- lavaan::sem(mod.txt, data = rmedsem::hsbdemo)
#' out <- rmedsem(mod, indep = "math", med = "read", dep = "science")
#'
#' # detailed output
#' print(out)
#'
#' # compact summary and its elements
#' s <- summary(out)
#' s
#' s$mediation
#' s$effects
#'
#' # extract estimates
#' coef(out)
#' confint(out)
#' confint(out, parm = "indirect", method = "sobel")
#' nobs(out)
#' as.data.frame(out)
#'
#' @name rmedsem-methods
NULL

#' @rdname rmedsem-methods
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
  problems <- unlist(lapply(intersect(c("RIT", "RID"), names(es)), \(w) {
    p <- effect_size_problem(object, w)
    if (!is.null(p)) stats::setNames(p, w)
  }))

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
    effect.size  = effect.size,
    effect.size.problems = if (is.null(problems)) character(0) else problems
  ), class = "summary.rmedsem")
}

#' @rdname rmedsem-methods
#' @export
print.summary.rmedsem <- function(x, digits = 3, ...) {
  check_count(digits, "digits")
  cat(with(x$vars, sprintf("Mediation analysis: '%s' -> '%s' -> '%s'\n", indep, med, dep)))
  cat(sprintf("Estimated with '%s' (%s)%s\n\n", x$package,
              ifelse(x$standardized, "standardized", "unstandardized"),
              if (is.null(x$nobs)) "" else sprintf(", N = %d", as.integer(x$nobs))))

  eff <- x$effects
  labs <- ifelse(is.na(eff$method), eff$effect,
                 sprintf("%s (%s)", eff$effect, method_label(eff$method)))
  substr(labs, 1, 1) <- toupper(substr(labs, 1, 1))
  fmt <- function(v) format_fixed(v, digits)
  bayes <- "bayes" %in% eff$method
  # Bayesian tail probabilities are estimated from samples: no "< eps" notation
  fmt_p <- function(v) ifelse(is.na(v), "",
                              if (bayes) format_fixed(v, digits)
                              else format.pval(v, digits=digits))
  tab <- data.frame(Estimate=fmt(eff$estimate), `Std. Err.`=fmt(eff$se),
                    `z-value`=fmt(eff$zval), `p-value`=fmt_p(eff$pval),
                    Lower=fmt(eff$lower), Upper=fmt(eff$upper),
                    row.names=labs, check.names=FALSE)
  ci.level <- if (is.null(x$ci.level)) 0.95 else x$ci.level
  cat(sprintf("Effects (%s %s):\n", format_percent(ci.level), ci_type(x)))
  print(tab)
  if (bayes)
    cat("For Bayesian estimates, 'p-value' is the posterior probability of",
        "the opposite sign.\n", sep="\n")

  if (!is.null(x$mediation$bk) || !is.null(x$mediation$zlc)) {
    cat(sprintf("\nType of mediation (significant: p < %s):\n", format(x$p.threshold)))
    if (!is.null(x$mediation$bk))
      cat(sprintf("  Baron & Kenny:      %s\n",
                  switch(x$mediation$bk, none="no mediation",
                         complete="complete mediation", partial="partial mediation")))
    if (!is.null(x$mediation$zlc))
      cat(sprintf("  Zhao, Lynch & Chen: %s\n                      (based on %s test)\n",
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
    problems <- x$effect.size.problems
    for (i in seq_along(es)) {
      cat(sprintf("  %s = %s\n", names(es)[i], format_fixed(es[[i]], digits)))
      if (names(x$effect.size)[i] %in% names(problems))
        cat(sprintf("      (not interpreted: %s)\n", problems[[names(x$effect.size)[i]]]))
    }
  }
  cat("\n")
  invisible(x)
}

#' Table of Indirect, Direct and Total Effects
#' @param res an `rmedsem` object
#' @return a data frame, see `summary.rmedsem()`
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
#' @return a single method name; defaults to `zlc_method()`
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

#' @rdname rmedsem-methods
#' @importFrom stats coef confint nobs
#' @export
coef.rmedsem <- function(object, method=NULL, ...){
  method <- resolve_method(object, method)
  c(indirect=unname(object[[method]][["coef"]]),
    direct=unname(object$direct.effect[["coef"]]),
    total=unname(object$total.effect[["coef"]]))
}

#' @rdname rmedsem-methods
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

#' @rdname rmedsem-methods
#' @export
nobs.rmedsem <- function(object, ...){
  if (is.null(object$nobs))
    stop("The number of observations is not stored in this 'rmedsem' object.", call.=FALSE)
  as.integer(object$nobs)
}

#' Plot rmedsem Results
#'
#' Visualize the results of [rmedsem()].
#'
#' \describe{
#'   \item{`plot_coef()`}{A coefficient plot of the indirect effect (for each
#'     estimation method), the direct effect and the total effect, with their
#'     confidence (or credible) intervals.}
#'   \item{`plot_effect()`}{A pie chart of the (absolute) indirect and direct
#'     effects, i.e., the proportion of the total effect that is mediated.
#'     Requires the effect sizes `"RIT"` and `"RID"`.}
#' }
#' `plot()` calls `plot_coef()` (`type = "coef"`) or `plot_effect()`
#' (`type = "effect"`).
#'
#' @param x,res an `rmedsem` object
#' @param type character; `"coef"` (default) for a coefficient plot or
#'   `"effect"` for an effect size plot
#' @param description logical, whether to add a caption describing the
#'   proportion of the total effect that is mediated (default `TRUE`)
#' @param ... additional arguments passed to `plot_coef()` or `plot_effect()`
#' @return a `ggplot` object
#'
#' @seealso [rmedsem()], [rmedsem-methods]
#'
#' @examples
#' mod.txt <- "
#' read ~ math
#' science ~ read + math
#' "
#' mod <- lavaan::sem(mod.txt, data = rmedsem::hsbdemo)
#' out <- rmedsem(mod, indep = "math", med = "read", dep = "science")
#' plot(out)
#' plot(out, type = "effect")
#' plot_effect(out, description = FALSE)
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

#' @rdname rmedsem-methods
#' @export
as.data.frame.rmedsem <- function(x, ...){
  res <- x
  # union of the estimates stored for the methods (NA where not available)
  cols <- unique(unlist(lapply(res$est.methods, \(m) names(res[[m]]))))
  est <- t(vapply(res$est.methods, \(m) unname(res[[m]][cols]), numeric(length(cols))))
  colnames(est) <- cols
  data.frame(package=res$package, method=res$est.methods, est,
             row.names=NULL, check.names=FALSE)
}
