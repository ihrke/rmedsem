#' Mediation Analysis for Structural Equation Models
#'
#' @param mod a fitted SEM model object (from lavaan, blavaan, cSEM, or modsem)
#' @param ... additional arguments passed to methods
#'
#' @export
rmedsem <- function (mod, ...)
  UseMethod("rmedsem")

