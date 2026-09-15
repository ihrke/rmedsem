#' Mediation Analysis for cSEM Models
#'
#' @param mod A fitted SEM model (cSEM).
#' @param indep A string indicating the name of the independent variable in the model.
#' @param med A string indicating the name of the mediator variable in the model.
#' @param dep A string indicating the name of the dependent variable in the model.
#'
#' @param approach either 'bk' or 'zlc' or both c("bk", "zlc") (default)
#' @param p.threshold A double giving the p-value for determining whether a path
#'  is significant or not
#' @param nbootstrap number of bootstrap samples (integer >= 2), default=1000
#' @param effect.size calculate different effect-sizes; one or more of "RIT", "RID"
#' @param ci.two.tailed A double giving the confidence level for two-tailed confidence intervals (default 0.95)
#' @param ... additional arguments (currently unused)
#'
#' @return A `rmedsem` structure containing the results from the analysis
#' @export
#'
#' @examples
#' \donttest{
#' if (requireNamespace("cSEM", quietly = TRUE)) {
#'   model <- "
#'     # measurement model
#'     OwnLook  =~ smv_attr_face + smv_attr_body + smv_sexy
#'     SelfEst  =~ ses_satis + ses_qualities + ses_able_todo
#'     MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
#'     # structural model
#'     SelfEst  ~ OwnLook
#'     MentWell ~ OwnLook + SelfEst
#'   "
#'   mod <- cSEM::csem(rmedsem::mchoice, model)
#'   # small number of bootstrap samples to keep the example fast
#'   out <- rmedsem(mod, indep="OwnLook", med="SelfEst", dep="MentWell",
#'                  nbootstrap=200)
#'   print(out)
#' }
#' }
rmedsem.cSEMResults <- function(mod, indep, med, dep,
                                approach=c("bk", "zlc"), p.threshold=0.05,
                                effect.size=c("RIT","RID","upsilon"),
                                nbootstrap=1000,
                                ci.two.tailed=0.95, ...){
  if (!requireNamespace("cSEM", quietly = TRUE))
    stop("Package 'cSEM' is required for this method. Please install it.")
  validate_rmedsem_args(indep, med, dep, approach, p.threshold, effect.size)
  check_count(nbootstrap, "nbootstrap", min=2)
  check_ci_level(ci.two.tailed)
  if (!inherits(mod, "cSEMResults_default"))
    stop("Only single-group, first-order cSEM models (class 'cSEMResults_default') ",
         "are supported by rmedsem().", call.=FALSE)
  structural <- mod$Information$Model$structural
  path.idx <- which(structural == 1, arr.ind = TRUE)
  check_mediation_model(vars=rownames(structural),
                        paths=data.frame(lhs=rownames(structural)[path.idx[, 1]],
                                         rhs=colnames(structural)[path.idx[, 2]]),
                        indep=indep, med=med, dep=dep)
  ci.width <- stats::qnorm(1-(1-ci.two.tailed)/2)

  N <- nrow(mod$Information$Data)
  moi <- sprintf("%s ~ %s", med, indep)
  dom <- sprintf("%s ~ %s", dep, med)
  doi <- sprintf("%s ~ %s", dep, indep)

  mod <- cSEM::resamplecSEMResults(mod, .force = TRUE, .R=nbootstrap, .resample_method="bootstrap")
  #imod <- cSEM::infer(mod)
  smod <- cSEM::summarize(mod, .alpha = 1-ci.two.tailed, .ci = "CI_percentile")
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
  lci_doi <- coef_doi - ci.width*se_doi
  uci_doi <- coef_doi + ci.width*se_doi

  # Total effect
  tottab <- smod$Estimates$Effect_estimates$Total_effect
  totix <- which(tottab$Name==doi)
  coef_tot <- tottab$Estimate[totix]
  se_tot <- tottab$Std_err[totix]
  lci_tot <- tottab[totix, grep("^CI_percentile.*L$", names(tottab))]
  uci_tot <- tottab[totix, grep("^CI_percentile.*U$", names(tottab))]

  prodterm <- coef_moi * coef_dom

  sobel_se  <- sqrt((coef_dom^2)*var_moi + (coef_moi^2)*var_dom)
  sobel_z   <- prodterm/sobel_se
  sobel_pv  <- 2*(1-stats::pnorm(abs(sobel_z)))
  sobel_lci <- prodterm - ci.width*sobel_se
  sobel_uci <- prodterm + ci.width*sobel_se

  # see https://github.com/FloSchuberth/cSEM/issues/542 for how to get the
  # covariance matrix of the path estimates for PLS-SEM based on
  # bootstrapping
  V <- stats::cov(smod$Estimates$Estimates_resample$Estimates1$Path_estimates$Resampled)
  covmoidom = V[moi,dom]

  #delta_se <- sqrt( (coef_dom^2)*var_moi + (coef_moi^2)*var_dom + (var_moi*var_dom) )
  delta_se <- sqrt( (coef_dom^2)*var_moi + (coef_moi^2)*var_dom + 2*coef_dom*coef_moi*covmoidom )
  delta_z  <- prodterm/delta_se
  delta_pv  <- 2*(1-stats::pnorm(abs(delta_z)))
  delta_lci <- prodterm - ci.width*delta_se
  delta_uci <- prodterm + ci.width*delta_se

  indtab <- smod$Estimates$Effect_estimates$Indirect_effect
  indix <- which(indtab$Name==doi)
  boot_se <- indtab$Std_err[indix]
  boot_z <- prodterm/boot_se
  boot_pv <- indtab$p_value[indix]
  boot_lci <- indtab[indix, grep("^CI_percentile.*L$", names(indtab))]
  boot_uci <- indtab[indix, grep("^CI_percentile.*U$", names(indtab))]

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
  if("upsilon" %in% effect.size){
    ups_unadj <- coef_moi^2 * coef_dom^2
    ups_adj   <- (coef_moi^2 - se_moi^2) * (coef_dom^2 - se_dom^2)
    es$upsilon <- list(unadjusted=ups_unadj, adjusted=ups_adj,
                       beta_MX=coef_moi, beta_YMX=coef_dom,
                       se_MX=se_moi, se_YMX=se_dom)
  }

  res <- list(package="cSEM", standardized=TRUE, nobs=N,
              ci.level=ci.two.tailed,
              vars =list(med=med, indep=indep, dep=dep),
              est.methods=c("sobel","delta","boot"),
              zlc.method="boot",
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
  class(res) <- c("rmedsem_cSEM", "rmedsem")
  return(res)
}

