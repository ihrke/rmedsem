#' @rdname rmedsem
#' @export
rmedsem.blavaan <- function(mod, indep, med, dep,
                            approach=c("bk", "zlc"), p.threshold=0.05,
                            effect.size=c("RIT","RID","upsilon"),
                            ci.two.tailed=0.95, hdi=FALSE, ...){
  if (!requireNamespace("blavaan", quietly = TRUE))
    stop("Package 'blavaan' is required for this method. Please install it.")
  validate_rmedsem_args(indep, med, dep, approach, p.threshold, effect.size)
  check_ci_level(ci.two.tailed)
  check_flag(hdi, "hdi")
  if (hdi && !requireNamespace("HDInterval", quietly = TRUE))
    stop("Package 'HDInterval' is required for 'hdi = TRUE'. Please install it.",
         call.=FALSE)
  check_lavaan_model(mod, indep, med, dep)
  # credible interval (lower, upper) of posterior samples
  interval <- function(x) {
    if (hdi)
      unname(HDInterval::hdi(x, credMass=ci.two.tailed)[c("lower", "upper")])
    else
      unname(stats::quantile(x, c((1-ci.two.tailed)/2, 1-(1-ci.two.tailed)/2)))
  }
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
  bayes_qs <- interval(ptsamp)
  bayes_lci <- bayes_qs[1]
  bayes_uci <- bayes_qs[2]
  bayes_se <- stats::sd(ptsamp)
  bayes_z <- bayes_coef/bayes_se

  # direct effect samples
  desamp <- draws[,doi]

  # direct effect estimates
  coef_doi <- base::mean(desamp)
  se_doi <- stats::sd(desamp)
  pval_doi <- min(base::mean(desamp>0), base::mean(desamp<0))
  qs_doi <- interval(desamp)
  lci_doi <- qs_doi[1]
  uci_doi <- qs_doi[2]

  # total effect
  totsamp <- ptsamp+desamp
  coef_tot <- mean(totsamp)
  se_tot <- stats::sd(totsamp)
  qs_tot <- interval(totsamp)
  lci_tot <- qs_tot[1]
  uci_tot <- qs_tot[2]

  # Bayesian p-values and evidence ratios
  bayes_proppos <- sum(ptsamp>0)/nsamp
  bayes_propneg <- sum(ptsamp<0)/nsamp
  ERpos <- bayes_proppos/(1-bayes_proppos)
  ERneg <- bayes_propneg/(1-bayes_propneg)

  prior_beta <- blavaan::blavInspect(mod, "dp")["beta"]

  # effect sizes
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
  if("upsilon" %in% effect.size){
    ups_samples <- draws[,moi]^2 * draws[,dom]^2
    ups_unadj <- mean(draws[,moi])^2 * mean(draws[,dom])^2
    ups_adj   <- (mean(draws[,moi])^2 - stats::var(draws[,moi])) *
                 (mean(draws[,dom])^2 - stats::var(draws[,dom]))
    ups_qs <- interval(ups_samples)
    es$upsilon <- list(unadjusted=ups_unadj, adjusted=ups_adj,
                   samples=ups_samples,
                   posterior_mean=mean(ups_samples),
                   posterior_median=stats::median(ups_samples),
                   lower=unname(ups_qs[1]), upper=unname(ups_qs[2]),
                   beta_MX=mean(draws[,moi]), beta_YMX=mean(draws[,dom]),
                   se_MX=stats::sd(draws[,moi]), se_YMX=stats::sd(draws[,dom]))
  }

  res <- list(package="blavaan", standardized=TRUE, nobs=lavaan::nobs(mod),
              ci.level=ci.two.tailed, ci.type=if (hdi) "HDI" else "CI",
              vars =list(med=med, indep=indep, dep=dep),
              direct.effect = c(coef=coef_doi, se=se_doi, pval=pval_doi, lower=lci_doi, upper=uci_doi),
              total.effect =  c(coef=coef_tot, se=se_tot, lower=lci_tot, upper=uci_tot),
              est.methods=c("bayes"),
              bayes=c(coef=bayes_coef, se=bayes_se, zval=bayes_z,
                      pval=min(bayes_proppos, bayes_propneg),
                      pvpos=bayes_proppos, pvneg=bayes_propneg,
                      ERpos=ERpos, ERneg=ERneg,
                      lower=bayes_lci, upper=bayes_uci),
              p.threshold=p.threshold,
              prior=list(beta=prior_beta),
              effect.size=es
  )
  class(res) <- c("rmedsem_blavaan", "rmedsem")
  res
}
