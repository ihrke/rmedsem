# Suppress R CMD check NOTEs for NSE variables used in dplyr/ggplot2/with()
utils::globalVariables(c(
  "package", "method", "effect", "ypos", "coef", "lower", "upper",
  "eff", "value", "est", "lhs", "rhs", "std.error", "p.value",
  "est.std", "se", "pvalue"
))

#' Mediation Analysis for Structural Equation Models
#'
#' Conducts mediation analysis on a fitted SEM model using the Baron and Kenny
#' (1986) and/or Zhao, Lynch & Chen (2010) approaches.
#'
#' @param mod a fitted SEM model object (from lavaan, blavaan, cSEM, or modsem)
#' @param indep a string indicating the name of the independent variable
#' @param med a string indicating the name of the mediator variable
#' @param dep a string indicating the name of the dependent variable
#' @param approach either `"bk"` or `"zlc"` or both `c("bk", "zlc")` (default)
#' @param p.threshold a numeric giving the p-value threshold for significance
#' @param effect.size character vector; one or more of `"RIT"`, `"RID"`, `"upsilon"`
#' @param ... additional arguments passed to methods
#'
#' @return an object of class `c("rmedsem_<pkg>", "rmedsem")`, where `<pkg>`
#'   identifies the backend (see section 'Adding a backend').
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
#' }
#' A backend whose output does not fit this scheme can provide its own
#' `print.rmedsem_<pkg>()` method, either replacing the default output (as
#' for `blavaan`) or extending it via [NextMethod()] (as for `modsem`).
#'
#' @examples
#' mod.txt <- "
#' read ~ math
#' science ~ read + math
#' "
#' mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
#' out <- rmedsem(mod, indep="math", med="read", dep="science")
#' out
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
                      "'cSEMResults' and 'modsem'."),
               paste(class(mod), collapse="', '")), call.=FALSE)
}

#' Validate common rmedsem arguments
#' @keywords internal
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
#' @keywords internal
check_string <- function(x, name){
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x))
    stop(sprintf("'%s' must be a single, non-empty character string.", name), call.=FALSE)
  invisible(x)
}

#' @rdname check_string
#' @keywords internal
check_flag <- function(x, name){
  if (!is.logical(x) || length(x) != 1 || is.na(x))
    stop(sprintf("'%s' must be a single logical value (TRUE or FALSE).", name), call.=FALSE)
  invisible(x)
}

#' @rdname check_string
#' @keywords internal
check_count <- function(x, name, min=1){
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x) || x != round(x) || x < min)
    stop(sprintf("'%s' must be a single integer >= %d.", name, min), call.=FALSE)
  invisible(x)
}

#' @rdname check_string
#' @keywords internal
check_ci_level <- function(x, name="ci.two.tailed"){
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x <= 0 || x >= 1)
    stop(sprintf("'%s' must be a single number between 0 and 1 (e.g., 0.95).", name),
         call.=FALSE)
  invisible(x)
}

#' Number of Monte-Carlo replications
#'
#' @param mcreps `NULL` or a positive integer
#' @param N sample size
#' @return `N` if `mcreps` is `NULL` or smaller than `N` (with a message in
#'   the latter case), otherwise `mcreps`
#' @keywords internal
resolve_mcreps <- function(mcreps, N){
  if (is.null(mcreps))
    return(N)
  check_count(mcreps, "mcreps")
  if (mcreps < N) {
    message(sprintf("'mcreps' (%d) is smaller than the sample size; using mcreps = %d.",
                    as.integer(mcreps), as.integer(N)))
    return(N)
  }
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
#' @keywords internal
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
#' @keywords internal
vcov_name <- function(partable, lhs, rhs){
  if (!is.null(partable$label)) {
    lab <- partable$label[partable$lhs == lhs & partable$op == "~" & partable$rhs == rhs]
    if (length(lab) == 1 && !is.na(lab) && nzchar(lab))
      return(lab)
  }
  sprintf("%s~%s", lhs, rhs)
}

