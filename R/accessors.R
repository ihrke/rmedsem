#' Ratio of Indirect to Total Effect (RIT)
#'
#' @param res fitted `rmedsem` object
#' @param ... additional arguments (currently unused)
#'
#' @export
RIT <- function (res, ...)
  UseMethod("RIT")

#' Ratio of Indirect to Direct Effect (RID)
#'
#' @param res fitted `rmedsem` object
#' @param ... additional arguments (currently unused)
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

#' Convert an rmedsem Object to a Data Frame
#'
#' @param x the `rmedsem` object
#' @param ... additional arguments (currently unused)
#' @return a data.frame
#' @export
as.data.frame.rmedsem <- function(x, ...){
  res <- x
  df <- purrr::map_dfr(res$est.methods, ~ res[[.x]]) |>
    dplyr::bind_cols(method=res$est.methods, package=res$package) |>
    dplyr::relocate(package,method)
  #if(format=="long"){
  #  df <- df |> tidyr::gather(variable, value, -package, -method)
  #}
  return(df)
}
