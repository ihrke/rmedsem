#' Calculate a mediation analysis for an SEM based on a modsem model.
#'
#' @param mod A fitted SEM model (modsem).
#' @param indep A string indicating the name of the independent variable in the model.
#' @param med A string indicating the name of the mediator variable in the model.
#' @param dep A string indicating the name of the dependent variable in the model.
#'
#' @param standardized A boolean indicating whether the coefficients should be
#' standardized. The default value is F.
#' @param approach either 'bk' or 'zlc' or both c("bk", "zlc") (default)
#' @param mcreps An integer determining the number of monte-carlo samples.
#' @param p.threshold A double giving the p-value for determining whether a path
#'  is significant or not
#' @param effect.size calculate different effect-sizes; one or more of "RIT", "RID"
#'
#' @return A `rmedsem` structure containing the results from the analysis
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' tpb <- "
#' # Outer Model (Based on Hagger et al., 2007)
#'   ATT =~ att1 + att2 + att3 + att4 + att5
#'   SN =~ sn1 + sn2
#'   PBC =~ pbc1 + pbc2 + pbc3
#'   INT =~ int1 + int2 + int3
#'   BEH =~ b1 + b2
#' 
#' # Inner Model (Based on Steinmetz et al., 2011)
#'   # Covariances
#'   ATT ~~ SN + PBC
#'   PBC ~~ SN
#'   # Causal Relationsships
#'   INT ~ ATT + SN + PBC
#'   BEH ~ INT + PBC
#'   INT ~ PBC:SN
#'   BEH ~ PBC:SN
#' "
#' 
#' est <- modsem(tpb, TPB, method = "qml")
#' rmedsem(est, indep = "PBC:SN", med = "INT", dep = "BEH")
#'
#' }
#'
rmedsem.modsem <- function(mod, indep, med, dep, standardized=TRUE, mcreps=NULL,
                           approach=c("bk", "zlc"), p.threshold=0.05,
                           effect.size=c("RIT","RID"), ci.two.tailed=0.95){
  ci.width <- qnorm(1-(1-ci.two.tailed)/2)

  # if estimated lavaan, we just extract the lavaan document
  if (inherits(mod, "modsem_pi")) {
    # if int-term has ":" we should remove them
    indep <- str_remove(indep, ":")
    med <- str_remove(med, ":")
    dep <- str_remove(dep, ":")
    return(rmedsem.lavaan(mod=modsem::extract_lavaan(mod), indep=indep,
                          dep=dep, med=med, standardized=standardized, mcreps=mcreps,
                          approach=approach, p.threshold=p.threshold,
                          effect.size=effect.size))
  } 
  N <- modsem::modsem_nobs(mod)

  if(is.null(mcreps) || mcreps < N){
    mcreps=N
  }
  
  if (standardized) {
    coefs <- modsem::standardized_estimates(mod)
  } else {
    coefs <- modsem::parameter_estimates(mod)
  }

  # check if int-term is ordered correctly
  indep <- get_correct_intterm(indep, coefs) 
  med <- get_correct_intterm(med, coefs)
  dep <- get_correct_intterm(dep, coefs)

  V <- modsem::modsem_vcov(mod)
  moi <- sprintf("%s~%s", med, indep)
  dom <- sprintf("%s~%s", dep, med)
  doi <- sprintf("%s~%s", dep, indep)

  corrmoidom = abs(V[moi,dom])
  corrmoidoi = abs(V[moi,doi])
  corrdomdoi = abs(V[dom,doi])

  # IV -> M
  coef_moi <- with(coefs, est[lhs==med & rhs==indep])
  se_moi   <- with(coefs, std.error[lhs==med & rhs==indep])
  var_moi  <- se_moi^2
  pval_moi <- with(coefs, p.value[lhs==med & rhs==indep])

  # M -> DV
  coef_dom <- with(coefs, est[lhs==dep & rhs==med])
  se_dom   <- with(coefs, std.error[lhs==dep & rhs==med])
  var_dom  <- se_dom^2
  pval_dom <- with(coefs, p.value[lhs==dep & rhs==med])

  # IV -> DV direct effect
  coef_doi <- with(coefs, est[lhs==dep & rhs==indep])
  se_doi   <- with(coefs, std.error[lhs==dep & rhs==indep])
  var_doi  <- se_doi^2
  pval_doi <- with(coefs, p.value[lhs==dep & rhs==indep])
  lci_doi <- coef_doi - ci.width*se_doi
  uci_doi <- coef_doi + ci.width*se_doi

  prodterm <- coef_moi * coef_dom

  sobel_se  <- sqrt((coef_dom^2)*var_moi + (coef_moi^2)*var_dom)
  sobel_z   <- prodterm/sobel_se
  sobel_pv  <- 2*(1-stats::pnorm(abs(sobel_z)))
  sobel_lci <- prodterm - ci.width*sobel_se
  sobel_uci <- prodterm + ci.width*sobel_se

  # here I use normal theory confidence limits, however according
  # to MacKinnon on page 97, these may not always be precise, however
  # in the DELTA METHOD below, it seems like normaly theory limits
  # are used there as well, that is ci.width is used

  #delta_se <- sqrt( (coef_dom^2)*var_moi + (coef_moi^2)*var_dom + (var_moi*var_dom) )
  delta_se <- sqrt( (coef_dom^2)*var_moi + (coef_moi^2)*var_dom + 2*coef_dom*coef_moi*corrmoidom )

  delta_z  <- prodterm/delta_se
  delta_pv  <- 2*(1-stats::pnorm(abs(delta_z)))
  delta_lci <- prodterm - ci.width*delta_se
  delta_uci <- prodterm + ci.width*delta_se

  sigma <- matrix(c(se_moi, corrmoidom, corrmoidom, se_dom), nrow=2, ncol=2, byrow = F)
  coefx <- mvtnorm::rmvnorm(n=mcreps, mean=c(coef_moi, coef_dom), sigma = sigma**2)
  prod_coef <- apply(coefx, 1, prod)
  montc_prod <- mean(prod_coef)
  montc_se   <- stats::sd(prod_coef)
  montc_z    <- montc_prod/montc_se
  montc_pv  <- 2*(1-stats::pnorm(abs(montc_z)))
  montc_qs <- stats::quantile(prod_coef, c(0.025, 0.975))
  montc_lci <- montc_qs[1]
  montc_uci <- montc_qs[2]
  names(montc_lci) <- NULL
  names(montc_uci) <- NULL

  # TE = IND + DE
  coef_tot <- coef_doi + prodterm
  sigma <- matrix(c(se_moi, corrmoidom, corrmoidoi,
                    corrmoidom, se_dom, corrdomdoi,
                    corrmoidoi, corrdomdoi, se_doi), nrow=3, ncol=3, byrow = F)
  coefx <- mvtnorm::rmvnorm(n=mcreps, mean=c(coef_moi, coef_dom, coef_doi), sigma = sigma**2)
  tot_eff_samp <- (coefx[,1]*coefx[,2])+coefx[,3]
  coef_tot <- mean(tot_eff_samp)
  se_tot <- stats::sd(tot_eff_samp)
  tot_qs <- stats::quantile(tot_eff_samp, c(0.025, 0.975))
  lci_tot <- tot_qs[1]
  uci_tot <- tot_qs[2]
  names(lci_tot) <- NULL
  names(uci_tot) <- NULL

  es <- list()
  ind_eff <- abs(prodterm)
  tot_eff <- abs(prodterm+coef_doi)
  dir_eff <- abs(coef_doi)
  if("RIT" %in% effect.size ){
    es$RIT=list(es=ind_eff/tot_eff, ind_eff=ind_eff, tot_eff=tot_eff)
  }
  if("RID" %in% effect.size ){
    es$RID=list(es=ind_eff/dir_eff, ind_eff=ind_eff, dir_eff=dir_eff)
  }


  res <- list(package="modsem", standardized=standardized,
              vars =list(med=med, indep=indep, dep=dep),
              est.methods = c("sobel","delta", "montc"),
              direct.effect = c(coef=coef_doi, se=se_doi, pval=pval_doi, lower=lci_doi, upper=uci_doi),
              total.effect = c(coef=coef_tot, se=se_tot, lower=lci_tot, upper=uci_tot),
              sobel=c(coef=prodterm, se=sobel_se, zval=sobel_z, pval=sobel_pv, lower=sobel_lci, upper=sobel_uci),
              delta=c(coef=prodterm, se=delta_se, zval=delta_z, pval=delta_pv, lower=delta_lci, upper=delta_uci),
              montc=c(coef=prodterm, se=montc_se, zval=montc_z, pval=montc_pv, lower=montc_lci, upper=montc_uci),
              med.approach=approach,
              effect.size=es,
              med.data=list(sig_thresh=p.threshold,
                            coefs=list(moi=coef_moi, dom=coef_dom, doi=coef_doi),
                            pvals=list(moi=pval_moi, dom=pval_dom, doi=pval_doi))
  )

  class(res) <- "rmedsem"
  return(res)
}


reverse_intterm <- function(xz) {
  if (length(xz) != 1) stop("xz must be a single string")
  paste0(rev(strsplit(xz, ":")[[1]]), collapse = ":")
}


str_remove <- function(x, pattern) {
  gsub(pattern=pattern, replacement="", x=x)
}


get_correct_intterm <- function(x, parTable) {
  if (length(x) != 1) stop("x must be a single string")
  rhs <- parTable$rhs

  if (x %in% rhs) {
    return(x)
  } else if ((xz <- reverse_intterm(x)) %in% rhs) {
    return(xz)
  } 
  stop("Unable to find variable ", x)
}
