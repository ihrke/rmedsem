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
#' @param effect.size character vector; one or more of `"RIT"`, `"RID"`
#' @param ... additional arguments passed to methods
#'
#' @return an object of class `rmedsem`
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
                     effect.size = c("RIT", "RID"),
                     ...)
  UseMethod("rmedsem")

#' Validate common rmedsem arguments
#' @keywords internal
validate_rmedsem_args <- function(indep, med, dep, approach, p.threshold, effect.size) {
  if (!is.character(indep) || length(indep) != 1)
    stop("'indep' must be a single character string.")
  if (!is.character(med) || length(med) != 1)
    stop("'med' must be a single character string.")
  if (!is.character(dep) || length(dep) != 1)
    stop("'dep' must be a single character string.")
  if (!is.numeric(p.threshold) || length(p.threshold) != 1 ||
      p.threshold <= 0 || p.threshold >= 1)
    stop("'p.threshold' must be a single number between 0 and 1.")
  if (!all(approach %in% c("bk", "zlc")))
    stop("'approach' must be one or more of 'bk', 'zlc'.")
  if (!all(effect.size %in% c("RIT", "RID")))
    stop("'effect.size' must be one or more of 'RIT', 'RID'.")
}

