#' Calculate a mediation analysis for an SEM based on a blavaan model.
#'
#' @param mod A fitted SEM model (blavaan). Note that the model has to be fit using
#'            `save.lvs=T` if the mediation model contains latent variables.
#' @param indep A string indicating the name of the independent variable in the model.
#' @param med A string indicating the name of the mediator variable in the model.
#' @param dep A string indicating the name of the dependent variable in the model.
#'
#' @param effect.size calculate different effect-sizes; one or more of "RIT", "RID"
#'
#' @return A `rmedsem` structure containing the results from the analysis
#' @export
#'
#' @examples
#'
#' model02 <- "
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#' "
#' library(blavaan)
#' mod <- bsem(model02, data=lavaan::PoliticalDemocracy, std.lv=TRUE,
#'             meanstructure=TRUE, n.chains=1,
#'             save.lvs=TRUE, burnin=500, sample=500)
#' out <- rmedsem(mod,  indep="ind60", med="dem60", dep="dem65")
#' print(out)
#'
rmedsem.blavaan <- function(mod, indep, med, dep,
                            approach=c("bk", "zlc"), p.threshold=0.05,
                            effect.size=c("RIT","RID")){
  ## convergence check
  if(max(blavaan::blavInspect(mod, "rhat"))>1.05)
    warning("Some Rhat>1.05, check convergence!")
  draws <- blavaan::standardizedposterior(mod)
  moi <- sprintf("%s~%s", med, indep)
  dom <- sprintf("%s~%s", dep, med)
  doi <- sprintf("%s~%s", dep, indep)

  # samples for product term
  ptsamp <- draws[,moi]*draws[,dom]
  nsamp <- length(ptsamp)
  bayes_coef <- mean(ptsamp)
  bayes_qs <- stats::quantile(ptsamp, c(0.025, 0.975))
  bayes_lci <- bayes_qs[1]
  bayes_uci <- bayes_qs[2]
  names(bayes_lci) <- NULL
  names(bayes_uci) <- NULL
  bayes_se <- stats::sd(ptsamp)
  bayes_z <- bayes_coef/bayes_se

  # direct effect samples
  desamp <- draws[,doi]
  RITsamp <- ptsamp/(ptsamp+desamp)
  RIT <- stats::median(RITsamp)
  RID <- bayes_coef/base::mean(desamp)

  # direct effect estimates
  coef_doi <- base::mean(desamp)
  se_doi <- stats::sd(desamp)
  pval_doi <- 1-base::mean(desamp>0)
  qs_doi <- stats::quantile(desamp, c(0.025, 0.975))
  lci_doi <- qs_doi[1]
  uci_doi <- qs_doi[2]
  names(lci_doi) <- NULL
  names(uci_doi) <- NULL

  # total effect
  totsamp <- ptsamp+desamp
  coef_tot <- mean(totsamp)
  se_tot <- stats::sd(totsamp)
  qs_tot <- stats::quantile(totsamp, c(0.025, 0.975))
  lci_tot <- qs_tot[1]
  uci_tot <- qs_tot[2]
  names(lci_tot) <- NULL
  names(uci_tot) <- NULL

  # Bayesian p-values and evidence ratios
  bayes_proppos <- sum(ptsamp>0)/nsamp
  bayes_propneg <- sum(ptsamp<0)/nsamp
  ERpos <- bayes_proppos/(1-bayes_proppos)
  ERneg <- bayes_propneg/(1-bayes_propneg)

  prior_beta <- blavaan::blavInspect(mod, "dp")["beta"]

  #
  es <- list()
  ind_eff <- abs(bayes_coef)
  tot_eff <- abs(bayes_coef+mean(desamp))
  dir_eff <- abs(mean(desamp))
  if("RIT" %in% effect.size ){
    es$RIT=list(es=ind_eff/tot_eff, ind_eff=ind_eff, tot_eff=tot_eff)
  }
  if("RID" %in% effect.size ){
    es$RID=list(es=ind_eff/dir_eff, ind_eff=ind_eff, dir_eff=dir_eff)
  }


  res <- list(package="blavaan", standardized=T,
              vars =list(med=med, indep=indep, dep=dep),
              direct.effect = c(coef=coef_doi, se=se_doi, pval=pval_doi, lower=lci_doi, upper=uci_doi),
              total.effect =  c(coef=coef_tot, se=se_tot, lower=lci_tot, upper=uci_tot),
              est.methods=c("bayes"),
              bayes=c(coef=bayes_coef, se=bayes_se, zval=bayes_z,
                      pval=min(bayes_proppos, bayes_propneg),
                      pvpos=bayes_proppos, pvneg=bayes_propneg,
                      ERpos=ERpos, ERneg=ERneg,
                      lower=bayes_lci, upper=bayes_uci),
              prior=list(beta=prior_beta),
              #med.approach=approach,
              effect.size=es
              #med.data=list(sig_thresh=p.threshold,
              #              coefs=list(moi=coef_moi, dom=coef_dom, doi=coef_doi),
              #              pvals=list(moi=pval_moi, dom=pval_dom, doi=pval_doi))
  )
  class(res) <- "rmedsem"
  res
}
