#' Return ratio of indirect to total effect (RIT).
#'
#' @param res fitted `rmedsem` object
#'
#' @export
RIT <- function (res, ...)
  UseMethod("RIT")

#' Return ratio of indirect to direct effect (RID).
#'
#' @param res fitted `rmedsem` object
#'
#' @export
RID <- function (res, ...)
  UseMethod("RID")


#' Return ratio of indirect to total effect (RIT).
#'
#' @param res The `rmedsem` object.
#' @returns a number
#' @export
#'
RIT.rmedsem <- function(res) {
  if(with(res$effect.size$RIT, ind_eff>tot_eff)){
    warning("Indirect effect is larger than total effect! RIT should not be interpreted")
  }
   return(res$effect.size$RIT$es)
}

#' Return ratio of indirect to direct effect (RID).
#'
#' @param res The `rmedsem` object.
#' @returns a number
#' @export
#'
RID.rmedsem <- function(res) {
  if(res$effect.size$RID$ind_eff>res$effect.size$RIT$tot_eff){
    warning("Indirect effect is larger than total effect! RID should not be interpreted")
  }
  return(res$effect.size$RID$es)
}

#' Convert `rmedsem` object to data-frame.
#'
#' @param res the `rmedsem` object
#' @return a data.frame
#' @export
#'
as.data.frame.rmedsem <- function(res, ...){
  df <- purrr::map_dfr(res$est.methods, ~ res[[.x]]) |>
    dplyr::bind_cols(method=res$est.methods, package=res$package) |>
    dplyr::relocate(package,method)
  #if(format=="long"){
  #  df <- df |> tidyr::gather(variable, value, -package, -method)
  #}
  return(df)
}
