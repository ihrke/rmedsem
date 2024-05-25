#' Calculate a mediation analysis for an SEM based on a cSEM model.
#'
#' @param mod A fitted SEM model (cSEM).
#' @param indep A string indicating the name of the independent variable in the model.
#' @param med A string indicating the name of the mediator variable in the model.
#' @param dep A string indicating the name of the dependent variable in the model.
#'
#' @param approach either 'bk' or 'zlc' or both c("bk", "zlc") (default)
#' @param p.threshold A double giving the p-value for determining whether a path
#'  is significant or not
#' @param effect.size calculate different effect-sizes; one or more of "RIT", "RID"
#'
#' @return A `rmedsem` structure containing the results from the analysis
#' @export
#'
#'
#'
rmedsem.cSEMResults <- function(mod, indep, med, dep,
                                approach=c("bk", "zlc"), p.threshold=0.05,
                                effect.size=c("RIT","RID")){
  #indep="Math"; med="Read"; dep="Science";

  N <- nrow(mod$Information$Data)
  moi <- sprintf("%s ~ %s", med, indep)
  dom <- sprintf("%s ~ %s", dep, med)
  doi <- sprintf("%s ~ %s", dep, indep)

  mod <- resamplecSEMResults(mod)
  #imod <- cSEM::infer(mod)
  smod <- cSEM::summarize(mod)
  coefs <- smod$Estimates$Path_estimates

  # IV -> M
  coef_moi <- with(coefs, Estimate[Name==moi])
  se_moi   <- with(coefs, Std_err[Name==moi])
  var_moi  <- se_moi^2
  pval_moi <- with(coefs, p_value[Name==moi])

  # M -> DV
  coef_dom <- with(coefs, Estimate[Name==dom])
  se_dom   <- with(coefs, Std_err[Name==dom])
  var_dom  <- se_dom^2
  pval_dom <- with(coefs, p_value[Name==dom])

  # IV -> DV direct effect
  coef_doi <- with(coefs, Estimate[Name==doi])
  se_doi   <- with(coefs, Std_err[Name==doi])
  var_doi  <- se_doi^2
  pval_doi <- with(coefs, p_value[Name==doi])
  lci_doi <- coef_doi - 1.959964*se_doi
  uci_doi <- coef_doi + 1.959964*se_doi

  # Total effect
  totix <- which(smod$Estimates$Effect_estimates$Total_effect$Name==doi)
  coef_tot <- smod$Estimates$Effect_estimates$Total_effect$Estimate[totix]
  se_tot <- smod$Estimates$Effect_estimates$Total_effect$Std_err[totix]
  lci_tot <- smod$Estimates$Effect_estimates$Total_effect$`CI_percentile.95%L`[totix]
  uci_tot <- smod$Estimates$Effect_estimates$Total_effect$`CI_percentile.95%U`[totix]

  prodterm <- coef_moi * coef_dom

  sobel_se  <- sqrt((coef_dom^2)*var_moi + (coef_moi^2)*var_dom)
  sobel_z   <- prodterm/sobel_se
  sobel_pv  <- 2*(1-stats::pnorm(abs(sobel_z)))
  sobel_lci <- prodterm - 1.959964*sobel_se
  sobel_uci <- prodterm + 1.959964*sobel_se

  delta_se <- sqrt( (coef_dom^2)*var_moi + (coef_moi^2)*var_dom + (var_moi*var_dom) )
  delta_z  <- prodterm/delta_se
  delta_pv  <- 2*(1-stats::pnorm(abs(delta_z)))
  delta_lci <- prodterm - 1.959964*delta_se
  delta_uci <- prodterm + 1.959964*delta_se

  indtab <- smod$Estimates$Effect_estimates$Indirect_effect
  boot_se <- with(indtab, Std_err[Name==doi])
  boot_z <- prodterm/boot_se
  boot_pv <- with(indtab, p_value[Name==doi])
  boot_lci <- indtab[,7]
  boot_uci <- indtab[,8]

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

  res <- list(package="cSEM", standardized=TRUE,
              vars =list(med=med, indep=indep, dep=dep),
              est.methods=c("sobel","delta","boot"),
              direct.effect = c(coef=coef_doi, se=se_doi, pval=pval_doi, lower=lci_doi, upper=uci_doi),
              total.effect =  c(coef=coef_tot, se=se_tot, lower=lci_tot, upper=uci_tot),
              sobel=c(coef=prodterm, se=sobel_se, zval=sobel_z, pval=sobel_pv, lower=sobel_lci, upper=sobel_uci),
              delta=c(coef=prodterm, se=delta_se, zval=delta_z, pval=delta_pv, lower=delta_lci, upper=delta_uci),
              boot=c(coef=prodterm, se=boot_se, zval=boot_z, pval=boot_pv, lower=boot_lci, upper=boot_uci),
              med.approach=approach,
              effect.size=es,
              med.data=list(sig_thresh=p.threshold,
                            coefs=list(moi=coef_moi, dom=coef_dom, doi=coef_doi),
                            pvals=list(moi=pval_moi, dom=pval_dom, doi=pval_doi))
  )
  class(res) <- "rmedsem"
  return(res)
}

