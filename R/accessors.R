#' Ratio of Indirect to Total Effect (RIT)
#'
#' @param res fitted `rmedsem` object
#' @param ... additional arguments (currently unused)
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
#' @examples
#' mod.txt <- "
#' read ~ math
#' science ~ read + math
#' "
#' mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
#' out <- rmedsem(mod, indep="math", med="read", dep="science",
#'                effect.size=c("RIT","RID","UPS"))
#' Upsilon(out)
#' Upsilon(out, adjusted=FALSE)
#'
#' @export
Upsilon <- function (res, ...)
  UseMethod("Upsilon")

#' @rdname Upsilon
#' @export
Upsilon.rmedsem <- function(res, adjusted=TRUE, ...) {
  if(is.null(res$effect.size$UPS))
    stop("Upsilon was not computed. Re-run rmedsem() with effect.size including 'UPS'.")
  if(adjusted) return(res$effect.size$UPS$adjusted)
  return(res$effect.size$UPS$unadjusted)
}

#' Summarize an rmedsem Object
#'
#' Prints the mediation analysis results to the console.
#'
#' @param object the `rmedsem` object
#' @param ... additional arguments passed to [print.rmedsem()]
#' @return the `rmedsem` object (invisibly)
#'
#' @examples
#' mod.txt <- "
#' read ~ math
#' science ~ read + math
#' "
#' mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
#' out <- rmedsem(mod, indep="math", med="read", dep="science")
#' summary(out)
#'
#' @export
summary.rmedsem <- function(object, ...) {
  print(object, ...)
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
#' @return a data.frame
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
  return(df)
}
