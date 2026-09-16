# Suppress R CMD check NOTEs for NSE variables used in dplyr/ggplot2/with()
utils::globalVariables(c(
  "package", "method", "effect", "ypos", "coef", "lower", "upper",
  "eff", "value", "est", "lhs", "rhs", "std.error", "p.value",
  "est.std", "se", "pvalue"
))

#' Mediation Analysis for Structural Equation Models
#'
#' Tests the indirect effect of an independent variable X on a dependent
#' variable Y through a mediator M in a fitted structural equation model
#' (SEM), and determines the type of mediation using the Baron and Kenny
#' (1986) and/or Zhao, Lynch & Chen (2010) approaches. Models estimated with
#' \pkg{lavaan} (covariance-based SEM), \pkg{cSEM} and \pkg{plssem}
#' (PLS-SEM), \pkg{blavaan} (Bayesian SEM) and \pkg{modsem} (models with
#' latent interactions) are supported. The model must contain the regression
#' paths X -> M, M -> Y and X -> Y.
#'
#' @param mod a fitted SEM: an object of class `lavaan`, `cSEMResults`,
#'   `blavaan`, `modsem` or `PlsModel` (\pkg{plssem}). `blavaan` models
#'   containing latent variables must be fitted with `save.lvs = TRUE`;
#'   `PlsModel` objects must be fitted with `bootstrap = TRUE`.
#' @param indep a string, the name of the independent variable (X). For
#'   `modsem` and `PlsModel` models, this can be an interaction term such as
#'   `"W:X"`.
#' @param med a string, the name of the mediator (M)
#' @param dep a string, the name of the dependent variable (Y)
#' @param approach approach(es) to determine the type of mediation: `"bk"`
#'   (Baron and Kenny), `"zlc"` (Zhao, Lynch & Chen), or both (default).
#'   Ignored for `blavaan` models.
#' @param p.threshold a number between 0 and 1, the p-value threshold for
#'   significance (default 0.05). A p-value equal to the threshold counts as
#'   not significant.
#' @param effect.size character vector with the effect sizes to compute; one
#'   or more of `"RIT"` (ratio of indirect to total effect), `"RID"` (ratio
#'   of indirect to direct effect) and `"upsilon"` (Lachowicz et al., 2018);
#'   see [effect-sizes].
#' @param standardized (`lavaan`, `modsem`) a logical, whether to use
#'   standardized coefficients (default `TRUE`). `cSEM` and `blavaan` results
#'   are always standardized.
#' @param mcreps (`lavaan`, `modsem`, `PlsModel`) the number of Monte-Carlo
#'   samples, a positive integer (default 5000). For `PlsModel` objects, only
#'   used for MC-PLS models with delta-method standard errors (see section
#'   'Backends').
#' @param ci.two.tailed a number between 0 and 1, the level of all confidence
#'   (or, for `blavaan`, credible) intervals (default 0.95)
#' @param nbootstrap (`cSEM`) the number of bootstrap samples (default 1000)
#' @param seed (`cSEM`) `NULL` (default) or a non-negative integer, the seed
#'   for the bootstrap. If `NULL`, the seed is drawn from R's random number
#'   generator, so that results can be reproduced with [set.seed()].
#' @param moderator (`modsem`) `NULL` (default) or a string, the name of the
#'   moderator W for moderated mediation. The model must contain an
#'   interaction of the moderator with `indep` and/or `med`.
#' @param hdi (`blavaan`) a logical. If `FALSE` (default), equal-tailed
#'   credible intervals are computed; if `TRUE`, highest density intervals
#'   (requires the \pkg{HDInterval} package). Applies to the indirect, direct
#'   and total effects and to Upsilon.
#' @param ... additional arguments passed to methods (currently unused)
#'
#' @return an object of class `c("rmedsem_<pkg>", "rmedsem")`, where `<pkg>`
#'   identifies the backend (`lavaan`, `cSEM`, `blavaan`, `modsem` or
#'   `plssem`). See
#'   [rmedsem-methods] for functions to print, summarize and extract results,
#'   [effect-sizes] for effect sizes and [plot.rmedsem()] for plots. The
#'   structure of the object is described in section 'Adding a backend'.
#'
#' @section Backends:
#' \describe{
#'   \item{`lavaan`}{The indirect effect is tested with the Sobel, Delta and
#'     Monte-Carlo methods. The Zhao, Lynch & Chen approach is based on the
#'     Monte-Carlo test.}
#'   \item{`cSEMResults`}{The model is re-estimated with `nbootstrap`
#'     bootstrap samples. The indirect effect is tested with the Sobel, Delta
#'     and bootstrap methods, and the Zhao, Lynch & Chen approach is based on
#'     the bootstrap test. Only single-group, first-order models are supported.}
#'   \item{`blavaan`}{Estimates are based on the (standardized) posterior
#'     samples. The output reports posterior means, standard deviations,
#'     posterior probabilities of a positive and negative indirect effect,
#'     evidence ratios and credible intervals; the Baron and Kenny and Zhao,
#'     Lynch & Chen approaches are not applied.}
#'   \item{`modsem`}{As for `lavaan`. In addition, moderated mediation (via
#'     `moderator`) and mediated moderation (an interaction term as `indep`)
#'     are supported.}
#'   \item{`PlsModel`}{Models estimated with [plssem::pls()] (PLS-SEM and
#'     consistent PLSc-SEM, including models with interaction terms and ordinal
#'     indicators). The model must be estimated with `bootstrap = TRUE`; the
#'     number of bootstrap samples is set in [plssem::pls()] (`boot.R`), and
#'     results are reproducible with its `boot.iseed` argument. The indirect
#'     effect is tested with the Sobel, Delta and bootstrap methods, where the
#'     bootstrap test uses the bootstrap samples of \pkg{plssem}. The Zhao, Lynch
#'     & Chen approach is based on the bootstrap test. Mediated moderation (an
#'     interaction term as `indep`) is supported. For MC-PLS models (e.g.,
#'     interaction models with ordinal indicators) estimated with
#'     delta-method standard errors (the default `mc.delta.se = TRUE` in
#'     [plssem::pls()]), the bootstrap samples of \pkg{plssem} do not refer to
#'     the Monte-Carlo corrected estimates; a Monte-Carlo test based on the
#'     estimates and their variance-covariance matrix (`mcreps` samples) is
#'     used instead of the bootstrap test, also for the Zhao, Lynch & Chen
#'     approach. For ordinal indicators, all effects refer to the standardized
#'     latent variables.}
#' }
#' Multi-group and multilevel models are not supported.
#'
#' @section Adding a backend:
#' Support for further model classes is added by writing a method
#' `rmedsem.<class>()` that returns a list of class
#' `c("rmedsem_<pkg>", "rmedsem")`. For the default [print.rmedsem()],
#' [plot.rmedsem()] and [as.data.frame.rmedsem()] methods to work, the list
#' must contain the following elements:
#' \describe{
#'   \item{`package`}{name of the estimating package (character).}
#'   \item{`standardized`}{whether the coefficients are standardized (logical).}
#'   \item{`vars`}{list with elements `indep`, `med` and `dep`.}
#'   \item{`est.methods`}{character vector naming the estimation methods for
#'     the indirect effect, e.g. `c("sobel", "delta", "montc")`.}
#'   \item{one element per entry in `est.methods`}{a named numeric vector with
#'     elements `coef`, `se`, `zval`, `pval`, `lower` and `upper`. The Baron
#'     and Kenny approach requires the element `sobel`.}
#'   \item{`zlc.method`}{(optional) the entry of `est.methods` whose p-value
#'     is used for the Zhao, Lynch & Chen approach; defaults to the last
#'     entry of `est.methods`.}
#'   \item{`direct.effect`}{named numeric vector with elements `coef`, `se`,
#'     `pval`, `lower` and `upper`.}
#'   \item{`total.effect`}{named numeric vector with elements `coef`, `se`,
#'     `lower` and `upper`.}
#'   \item{`med.approach`}{character vector, a subset of `c("bk", "zlc")`.}
#'   \item{`med.data`}{list with elements `sig_thresh` (the p-value
#'     threshold), `coefs` and `pvals`; the latter two are lists with elements
#'     `moi` (X -> M), `dom` (M -> Y) and `doi` (X -> Y).}
#'   \item{`effect.size`}{list with (a subset of) elements `RIT`, `RID` and
#'     `upsilon`, as returned by the built-in backends.}
#'   \item{`nobs`}{(optional) number of observations, used by [stats::nobs()].}
#'   \item{`ci.level`}{(optional) level of the stored intervals, used by
#'     [stats::confint()] and [summary()]; defaults to 0.95.}
#'   \item{`ci.type`}{(optional) label of the stored intervals, `"CI"`
#'     (default) or `"HDI"`.}
#' }
#' A backend whose output does not fit this scheme can provide its own
#' `print.rmedsem_<pkg>()` method, either replacing the default output (as
#' for `blavaan`) or extending it via [NextMethod()] (as for `modsem`).
#'
#' @references
#' Baron, R. M., & Kenny, D. A. (1986). The moderator-mediator variable
#' distinction in social psychological research: Conceptual, strategic, and
#' statistical considerations. *Journal of Personality and Social
#' Psychology*, 51(6), 1173--1182. \doi{10.1037/0022-3514.51.6.1173}
#'
#' Lachowicz, M. J., Preacher, K. J., & Kelley, K. (2018). A novel measure of
#' effect size for mediation analysis. *Psychological Methods*, 23(2),
#' 244--261. \doi{10.1037/met0000165}
#'
#' Zhao, X., Lynch, J. G., & Chen, Q. (2010). Reconsidering Baron and Kenny:
#' Myths and truths about mediation analysis. *Journal of Consumer Research*,
#' 37(2), 197--206. \doi{10.1086/651257}
#'
#' @examples
#' ## lavaan: observed variables
#' mod.txt <- "
#' read ~ math
#' science ~ read + math
#' "
#' mod <- lavaan::sem(mod.txt, data = rmedsem::hsbdemo)
#' out <- rmedsem(mod, indep = "math", med = "read", dep = "science")
#' out
#'
#' # Zhao, Lynch & Chen approach only, unstandardized coefficients
#' rmedsem(mod, indep = "math", med = "read", dep = "science",
#'         approach = "zlc", standardized = FALSE, mcreps = 5000)
#'
#' \donttest{
#' ## cSEM
#' if (requireNamespace("cSEM", quietly = TRUE)) {
#'   model <- "
#'     OwnLook  =~ smv_attr_face + smv_attr_body + smv_sexy
#'     SelfEst  =~ ses_satis + ses_qualities + ses_able_todo
#'     MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
#'     SelfEst  ~ OwnLook
#'     MentWell ~ OwnLook + SelfEst
#'   "
#'   mod <- cSEM::csem(rmedsem::mchoice, model)
#'   # small number of bootstrap samples to keep the example fast
#'   rmedsem(mod, indep = "OwnLook", med = "SelfEst", dep = "MentWell",
#'           nbootstrap = 200)
#' }
#'
#' ## modsem: mediated moderation and moderated mediation
#' if (requireNamespace("modsem", quietly = TRUE)) {
#'   m <- "
#'     OwnLook =~ smv_attr_face + smv_attr_body + smv_sexy
#'     SelfEst =~ ses_satis + ses_qualities + ses_able_todo
#'     MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
#'     smv =~ smv_kind + smv_caring + smv_understanding +
#'       smv_make_laughh + smv_funny + smv_sociable
#'     SelfEst ~ OwnLook + smv + smv:OwnLook
#'     MentWell ~ OwnLook + SelfEst + smv + smv:OwnLook
#'   "
#'   est <- modsem::modsem(m, data = rmedsem::mchoice, method = "lms")
#'
#'   # mediated moderation
#'   rmedsem(est, indep = "smv:OwnLook", med = "SelfEst", dep = "MentWell")
#'
#'   # moderated mediation
#'   rmedsem(est, indep = "OwnLook", med = "SelfEst", dep = "MentWell",
#'           moderator = "smv")
#' }
#'
#' ## plssem (PLS-SEM)
#' if (requireNamespace("plssem", quietly = TRUE)) {
#'   model <- "
#'     OwnLook  =~ smv_attr_face + smv_attr_body + smv_sexy
#'     SelfEst  =~ ses_satis + ses_qualities + ses_able_todo
#'     MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
#'     SelfEst  ~ OwnLook
#'     MentWell ~ OwnLook + SelfEst
#'   "
#'   # small number of bootstrap samples to keep the example fast
#'   fit <- plssem::pls(model, rmedsem::mchoice, bootstrap = TRUE,
#'                      boot.R = 200, boot.iseed = 1)
#'   rmedsem(fit, indep = "OwnLook", med = "SelfEst", dep = "MentWell")
#' }
#'
#' ## blavaan
#' if (requireNamespace("blavaan", quietly = TRUE)) {
#'   # blavaan's fitting functions need the package to be attached
#'   library(blavaan)
#'   # short single chain to keep the example fast; use more chains and
#'   # iterations in practice
#'   bmod <- bsem(mod.txt, data = rmedsem::hsbdemo, n.chains = 1,
#'                burnin = 500, sample = 500, seed = 1,
#'                bcontrol = list(refresh = 0))
#'   rmedsem(bmod, indep = "math", med = "read", dep = "science")
#'
#'   # highest density intervals instead of equal-tailed intervals
#'   if (requireNamespace("HDInterval", quietly = TRUE))
#'     rmedsem(bmod, indep = "math", med = "read", dep = "science", hdi = TRUE)
#' }
#' }
#'
#' @export
rmedsem <- function (mod, indep, med, dep,
                     approach = c("bk", "zlc"),
                     p.threshold = 0.05,
                     effect.size = c("RIT", "RID", "upsilon"),
                     ...)
  UseMethod("rmedsem")

#' @rdname rmedsem
#' @export
rmedsem.default <- function(mod, indep, med, dep, ...){
  stop(sprintf(paste0("rmedsem() does not support objects of class '%s'.\n",
                      "Supported model classes are 'lavaan', 'blavaan', ",
                      "'cSEMResults', 'modsem' and 'PlsModel' (plssem)."),
               paste(class(mod), collapse="', '")), call.=FALSE)
}

#' Validate common rmedsem arguments
#' @noRd
validate_rmedsem_args <- function(indep, med, dep, approach, p.threshold, effect.size) {
  check_string(indep, "indep")
  check_string(med, "med")
  check_string(dep, "dep")
  if (anyDuplicated(c(indep, med, dep)))
    stop("'indep', 'med' and 'dep' must be three different variables.", call.=FALSE)
  if (!is.numeric(p.threshold) || length(p.threshold) != 1 || is.na(p.threshold) ||
      p.threshold <= 0 || p.threshold >= 1)
    stop("'p.threshold' must be a single number between 0 and 1.", call.=FALSE)
  if (!is.character(approach) || length(approach) == 0 ||
      !all(approach %in% c("bk", "zlc")))
    stop("'approach' must be one or more of 'bk', 'zlc'.", call.=FALSE)
  if (!is.null(effect.size) &&
      (!is.character(effect.size) || !all(effect.size %in% c("RIT", "RID", "upsilon"))))
    stop("'effect.size' must be NULL or one or more of 'RIT', 'RID', 'upsilon'.", call.=FALSE)
}

#' Argument checks
#'
#' Small helpers that stop with an informative message if an argument is not
#' of the expected form.
#' @param x the argument value
#' @param name the argument name used in the error message
#' @param min smallest allowed value
#' @return `x` (invisibly)
#' @noRd
check_string <- function(x, name){
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x))
    stop(sprintf("'%s' must be a single, non-empty character string.", name), call.=FALSE)
  invisible(x)
}

#' @rdname check_string
#' @noRd
check_flag <- function(x, name){
  if (!is.logical(x) || length(x) != 1 || is.na(x))
    stop(sprintf("'%s' must be a single logical value (TRUE or FALSE).", name), call.=FALSE)
  invisible(x)
}

#' @rdname check_string
#' @noRd
check_count <- function(x, name, min=1){
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || x != round(x) || x < min)
    stop(sprintf("'%s' must be a single integer >= %d.", name, min), call.=FALSE)
  invisible(x)
}

#' @rdname check_string
#' @noRd
check_ci_level <- function(x, name="ci.two.tailed"){
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x <= 0 || x >= 1)
    stop(sprintf("'%s' must be a single number between 0 and 1 (e.g., 0.95).", name),
         call.=FALSE)
  invisible(x)
}

#' Number of Monte-Carlo replications
#'
#' @param mcreps `NULL` (for backward compatibility; means 5000) or a positive
#'   integer
#' @return the number of Monte-Carlo samples
#' @noRd
resolve_mcreps <- function(mcreps){
  if (is.null(mcreps))
    return(5000L)
  check_count(mcreps, "mcreps")
  mcreps
}

#' Check that the mediation variables and paths exist in a model
#'
#' @param vars character vector with all variable names in the model
#' @param paths data frame with columns `lhs` and `rhs`, one row per
#'   regression path (`lhs ~ rhs`)
#' @param indep,med,dep names of the independent, mediator and dependent
#'   variable
#' @return `NULL` (invisibly); stops with an informative error if a variable
#'   or one of the paths X -> M, M -> Y and X -> Y is missing
#' @noRd
check_mediation_model <- function(vars, paths, indep, med, dep){
  roles <- c(indep=indep, med=med, dep=dep)
  missing.vars <- roles[!roles %in% vars]
  if (length(missing.vars) > 0)
    stop(sprintf("Variable %s not found in the model.\nVariables in the model: %s.",
                 paste(sprintf("'%s' (argument '%s')", missing.vars, names(missing.vars)),
                       collapse=", "),
                 paste(sprintf("'%s'", unique(vars)), collapse=", ")),
         call.=FALSE)

  required <- data.frame(lhs=c(med, dep, dep), rhs=c(indep, med, indep),
                         role=c("X -> M", "M -> Y", "X -> Y"))
  has.path <- mapply(\(l, r) any(paths$lhs == l & paths$rhs == r),
                     required$lhs, required$rhs)
  if (!all(has.path)) {
    miss <- required[!has.path, ]
    stop(sprintf(paste0("The model does not contain the regression path(s) %s.\n",
                        "rmedsem() requires the paths X -> M, M -> Y and X -> Y ",
                        "to be estimated."),
                 paste(sprintf("'%s ~ %s' (%s)", miss$lhs, miss$rhs, miss$role),
                       collapse=", ")),
         call.=FALSE)
  }
  invisible(NULL)
}

#' Name of a regression parameter in a variance-covariance matrix
#'
#' `lavaan` and `modsem` use the parameter label (if any) as row name in
#' `vcov()`, otherwise `lhs~rhs`.
#' @param partable a parameter table with columns `lhs`, `op`, `rhs` and
#'   (optionally) `label`
#' @param lhs,rhs left- and right-hand side of the regression path
#' @return a single string
#' @noRd
vcov_name <- function(partable, lhs, rhs){
  if (!is.null(partable$label)) {
    lab <- partable$label[partable$lhs == lhs & partable$op == "~" & partable$rhs == rhs]
    if (length(lab) == 1 && !is.na(lab) && nzchar(lab))
      return(lab)
  }
  sprintf("%s~%s", lhs, rhs)
}

