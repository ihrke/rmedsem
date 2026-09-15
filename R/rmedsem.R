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
  if (!all(effect.size %in% c("RIT", "RID", "upsilon")))
    stop("'effect.size' must be one or more of 'RIT', 'RID', 'upsilon'.")
}

