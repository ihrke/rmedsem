#' @rdname rmedsem
#' @export
rmedsem.PlsModel <- function(mod, indep, med, dep,
                             approach=c("bk", "zlc"), p.threshold=0.05,
                             effect.size=c("RIT","RID","upsilon"),
                             mcreps=5000, ci.two.tailed=0.95, ...){
  if (!requireNamespace("plssem", quietly = TRUE))
    stop("Package 'plssem' is required for this method. Please install it.")
  validate_rmedsem_args(indep, med, dep, approach, p.threshold, effect.size)
  check_ci_level(ci.two.tailed)
  mcreps <- resolve_mcreps(mcreps)
  if (isTRUE(mod@info$is.mlm))
    stop("Multilevel models are not supported by rmedsem().", call.=FALSE)
  ci.width <- stats::qnorm(1-(1-ci.two.tailed)/2)
  probs <- c((1-ci.two.tailed)/2, 1-(1-ci.two.tailed)/2)

  parTable <- as.data.frame(plssem::parameter_estimates(mod))
  model.rows <- parTable$op %in% c("=~", "~", "~~")
  model.vars <- unique(c(parTable$lhs[model.rows], parTable$rhs[model.rows]))
  coefs <- parTable[parTable$op == "~", , drop = FALSE]

  # interaction terms (e.g., "W:X") may be given in either order
  indep <- get_correct_intterm(indep, coefs)
  med   <- get_correct_intterm(med, coefs)
  dep   <- get_correct_intterm(dep, coefs)
  check_mediation_model(vars=model.vars, paths=coefs[, c("lhs", "rhs")],
                        indep=indep, med=med, dep=dep)

  # standard errors, vcov and bootstrap samples are only available if the
  # model was estimated with bootstrapping
  boot.samples <- unclass(plssem::boot(mod))
  if (!NROW(boot.samples) || all(is.na(coefs$se)))
    stop("The plssem model must be estimated with bootstrapping, ",
         "e.g., pls(..., bootstrap = TRUE).", call.=FALSE)
  V <- unclass(plssem::vcov(mod, use.labels = FALSE))

  moi <- sprintf("%s~%s", med, indep)
  dom <- sprintf("%s~%s", dep, med)
  doi <- sprintf("%s~%s", dep, indep)

  path <- function(lhs, rhs, col) coefs[[col]][coefs$lhs == lhs & coefs$rhs == rhs]

  # IV -> M
  coef_moi <- path(med, indep, "est")
  se_moi   <- path(med, indep, "se")
  var_moi  <- se_moi^2
  pval_moi <- path(med, indep, "pvalue")

  # M -> DV
  coef_dom <- path(dep, med, "est")
  se_dom   <- path(dep, med, "se")
  var_dom  <- se_dom^2
  pval_dom <- path(dep, med, "pvalue")

  # IV -> DV direct effect
  coef_doi <- path(dep, indep, "est")
  se_doi   <- path(dep, indep, "se")
  pval_doi <- path(dep, indep, "pvalue")
  lci_doi  <- coef_doi - ci.width*se_doi
  uci_doi  <- coef_doi + ci.width*se_doi

  prodterm <- coef_moi * coef_dom

  sobel_se  <- sqrt((coef_dom^2)*var_moi + (coef_moi^2)*var_dom)
  sobel_z   <- prodterm/sobel_se
  sobel_pv  <- 2*stats::pnorm(-abs(sobel_z))
  sobel_lci <- prodterm - ci.width*sobel_se
  sobel_uci <- prodterm + ci.width*sobel_se

  covmoidom <- V[moi, dom]
  delta_se  <- sqrt((coef_dom^2)*var_moi + (coef_moi^2)*var_dom + 2*coef_dom*coef_moi*covmoidom)
  delta_z   <- prodterm/delta_se
  delta_pv  <- 2*stats::pnorm(-abs(delta_z))
  delta_lci <- prodterm - ci.width*delta_se
  delta_uci <- prodterm + ci.width*delta_se

  # Third test of the indirect effect. The bootstrap samples of plssem are
  # used if they are samples of the reported estimator. This is not the case
  # for MC-PLS models (e.g., interaction models with ordinal indicators) with
  # delta-method standard errors (plssem's default, `mc.delta.se = TRUE`): the
  # bootstrap replicates are then estimated without the Monte-Carlo correction
  # and only the delta-method vcov refers to the reported estimates. In that
  # case, a Monte-Carlo test based on the estimates and their vcov is used.
  use.boot <- !(isTRUE(plssem::is_mcpls(mod)) && isTRUE(mod@info$mc.args$delta.se))
  if (use.boot) {
    samples <- boot.samples[, c(moi, dom, doi), drop = FALSE]
    # failed or dropped bootstrap replicates are NA
    samples <- samples[stats::complete.cases(samples), , drop = FALSE]
    third.method <- "boot"
  } else {
    sigma <- V[c(moi, dom, doi), c(moi, dom, doi)]
    samples <- mvtnorm::rmvnorm(n=mcreps, mean=c(coef_moi, coef_dom, coef_doi),
                                sigma=sigma)
    colnames(samples) <- c(moi, dom, doi)
    third.method <- "montc"
  }
  ind_samp <- samples[, moi] * samples[, dom]
  tot_samp <- ind_samp + samples[, doi]

  third_se <- stats::sd(ind_samp)
  third_z  <- prodterm/third_se
  third_pv <- 2*stats::pnorm(-abs(third_z))
  third_qs <- unname(stats::quantile(ind_samp, probs))

  coef_tot <- prodterm + coef_doi
  se_tot   <- stats::sd(tot_samp)
  tot_qs   <- unname(stats::quantile(tot_samp, probs))

  # effect sizes
  es <- list()
  ind_eff <- abs(prodterm)
  tot_eff <- abs(prodterm+coef_doi)
  dir_eff <- abs(coef_doi)
  if("RIT" %in% effect.size){
    es$RIT=list(es=ind_eff/tot_eff, ind_eff=ind_eff, tot_eff=tot_eff)
  }
  if("RID" %in% effect.size){
    es$RID=list(es=ind_eff/dir_eff, ind_eff=ind_eff, dir_eff=dir_eff)
  }
  if("upsilon" %in% effect.size){
    ups_unadj <- coef_moi^2 * coef_dom^2
    ups_adj   <- (coef_moi^2 - se_moi^2) * (coef_dom^2 - se_dom^2)
    es$upsilon <- list(unadjusted=ups_unadj, adjusted=ups_adj,
                       beta_MX=coef_moi, beta_YMX=coef_dom,
                       se_MX=se_moi, se_YMX=se_dom)
  }

  N <- mod@info$n
  if (is.null(N)) N <- nrow(mod@data)

  res <- list(package="plssem", standardized=TRUE, nobs=N,
              ci.level=ci.two.tailed,
              vars=list(med=med, indep=indep, dep=dep),
              est.methods=c("sobel", "delta", third.method),
              zlc.method=third.method,
              nboot=if (use.boot) length(ind_samp) else NULL,
              mcreps=if (use.boot) NULL else mcreps,
              direct.effect=c(coef=coef_doi, se=se_doi, pval=pval_doi, lower=lci_doi, upper=uci_doi),
              total.effect=c(coef=coef_tot, se=se_tot, lower=tot_qs[1], upper=tot_qs[2]),
              sobel=c(coef=prodterm, se=sobel_se, zval=sobel_z, pval=sobel_pv, lower=sobel_lci, upper=sobel_uci),
              delta=c(coef=prodterm, se=delta_se, zval=delta_z, pval=delta_pv, lower=delta_lci, upper=delta_uci),
              med.approach=approach,
              effect.size=es,
              med.data=list(sig_thresh=p.threshold,
                            coefs=list(moi=coef_moi, dom=coef_dom, doi=coef_doi),
                            pvals=list(moi=pval_moi, dom=pval_dom, doi=pval_doi))
  )
  res[[third.method]] <- c(coef=prodterm, se=third_se, zval=third_z, pval=third_pv,
                           lower=third_qs[1], upper=third_qs[2])
  class(res) <- c("rmedsem_plssem", "rmedsem")
  res
}
