#' Calculate a mediation analysis for an SEM based on a lavaan model.
#'
#' @param mod A fitted SEM model (lavaan).
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
#'
#' mod.txt <- "
#' read ~ math
#' science ~ read + math
#' "
#' mod <- lavaan::sem(mod.txt, data=rmedsem::hsbdemo)
#' out <- rmedsem(mod, indep="math", med="read", dep="science",
#'                standardized=TRUE, mcreps=5000,
#'                approach = c("bk","zlc"))
#' print(out)
#'
rmedsem.lavaan <- function(mod, indep, med, dep, standardized=TRUE, mcreps=NULL,
                           approach=c("bk", "zlc"), p.threshold=0.05, ci.two.tailed=0.95,
                           effect.size=c("RIT","RID")){
  ci.width <- qnorm(1-(1-ci.two.tailed)/2)

  # for testing
  # indep="math"; med="read"; dep="science";
  # indep="SES"; med="Alien67"; dep="Alien71";
  # indep="ind60"; med="dem60"; dep="dem65"
  N <- lavaan::nobs(mod)
  if (is.null(mcreps) || mcreps < N){
    mcreps=N
  }

  moi <- sprintf("%s~%s", med, indep)
  dom <- sprintf("%s~%s", dep, med)
  doi <- sprintf("%s~%s", dep, indep)

  if(standardized){
    V <- lavaan::lavInspect(mod, what="vcov.std.all")
    coefs <- lavaan::standardizedsolution(mod)
    coefs$est <- coefs$est.std
  } else {
    V <- lavaan::vcov(mod)
    coefs <- lavaan::parameterEstimates(mod)
  }
  
  covmoidom = V[moi,dom]
  covmoidoi = V[moi,doi]
  covdomdoi = V[dom,doi]

  # IV -> M
  coef_moi <- with(coefs, est[lhs==med & rhs==indep])
  se_moi   <- with(coefs, se[lhs==med & rhs==indep])
  var_moi  <- se_moi^2
  pval_moi <- with(coefs, pvalue[lhs==med & rhs==indep])

  # M -> DV
  coef_dom <- with(coefs, est[lhs==dep & rhs==med])
  se_dom   <- with(coefs, se[lhs==dep & rhs==med])
  var_dom  <- se_dom^2
  pval_dom <- with(coefs, pvalue[lhs==dep & rhs==med])

  # IV -> DV direct effect
  coef_doi <- with(coefs, est[lhs==dep & rhs==indep])
  se_doi   <- with(coefs, se[lhs==dep & rhs==indep])
  var_doi  <- se_doi^2
  pval_doi <- with(coefs, pvalue[lhs==dep & rhs==indep])
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
  delta_se <- sqrt( (coef_dom^2)*var_moi + (coef_moi^2)*var_dom + 2*coef_dom*coef_moi*covmoidom )

  delta_z  <- prodterm/delta_se
  delta_pv  <- 2*(1-stats::pnorm(abs(delta_z)))
  delta_lci <- prodterm - ci.width*delta_se
  delta_uci <- prodterm + ci.width*delta_se

  sigma <- matrix(c(var_moi, covmoidom, covmoidom, var_dom), nrow=2, ncol=2)
  coefx <- mvtnorm::rmvnorm(n=mcreps, mean=c(coef_moi, coef_dom), sigma = sigma)
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
  sigma <- matrix(c(var_moi, covmoidom, covmoidoi,
                    covmoidom, var_dom, covdomdoi,
                    covmoidoi, covdomdoi, var_doi), nrow=3, ncol=3)
  coefx <- mvtnorm::rmvnorm(n=mcreps, mean=c(coef_moi, coef_dom, coef_doi), sigma = sigma)
  tot_eff_samp <- (coefx[,1]*coefx[,2])+coefx[,3]
  coef_tot <- mean(tot_eff_samp)
  se_tot <- stats::sd(tot_eff_samp)
  tot_qs <- stats::quantile(tot_eff_samp, c(0.025, 0.975))
  lci_tot <- tot_qs[1]
  uci_tot <- tot_qs[2]
  names(lci_tot) <- NULL
  names(uci_tot) <- NULL

  #
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


  res <- list(package="lavaan", standardized=standardized,
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
