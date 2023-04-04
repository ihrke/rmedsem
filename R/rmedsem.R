
#' Calculate a mediation analysis for an SEM.
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
#' data <- haven::read_stata("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
#' mod.txt <- "
#' read ~ math
#' science ~ read + math
#' "
#' summary(mod <- lavaan::sem(mod.txt, data=data))
#' out <- rmedsem(mod, indep="math", med="read", dep="science",
#'                standardized=T, mcreps=5000,
#'                approach = c("bk","zlc"))
#' print(out)
#' summary(out)
#'
rmedsem <- function(mod, indep, med, dep, standardized=F, mcreps=NULL,
                    approach="bk", p.threshold=0.05,
                    effect.size=c("RIT","RID")){
  # for testing
  #indep="math"; med="read"; dep="science";
  #indep="SES"; med="Alien67"; dep="Alien71";

  N <- lavaan::nobs(mod)
  if(is.null(mcreps) || mcreps < N){
    mcreps=N
  }

  V <- lavaan::vcov(mod)
  moi <- sprintf("%s~%s", med, indep)
  dom <- sprintf("%s~%s", dep, med)
  corrmoidom = abs(V[moi,dom])

  if(standardized){
    coefs <- lavaan::standardizedsolution(mod)
    coefs$est <- coefs$est.std
  } else {
    coefs <- lavaan::parameterEstimates(mod)
  }

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

  prodterm <- coef_moi * coef_dom

  sobel_se  <- sqrt((coef_dom^2)*var_moi + (coef_moi^2)*var_dom)
  sobel_z   <- prodterm/sobel_se
  sobel_pv  <- 2*(1-stats::pnorm(abs(sobel_z)))
  sobel_lci <- prodterm - 1.959964*sobel_se
  sobel_uci <- prodterm + 1.959964*sobel_se

  # here I use normal theory confidence limits, however according
  # to MacKinnon on page 97, these may not always be precise, however
  # in the DELTA METHOD below, it seems like normaly theory limits
  # are used there as well, that is 1.959964 is used

  delta_se <- sqrt( (coef_dom^2)*var_moi + (coef_moi^2)*var_dom + (var_moi*var_dom) )
  delta_z  <- prodterm/delta_se
  delta_pv  <- 2*(1-stats::pnorm(abs(delta_z)))
  delta_lci <- prodterm - 1.959964*delta_se
  delta_uci <- prodterm + 1.959964*delta_se

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


  res <- list(standardized=standardized,
              vars =list(med=med, indep=indep, dep=dep),
              sobel=c(prodterm, sobel_se,sobel_z, sobel_pv, sobel_lci, sobel_uci),
              delta=c(prodterm, delta_se,delta_z, delta_pv, delta_lci, delta_uci),
              montc=c(prodterm, montc_se,montc_z, montc_pv, montc_lci, montc_uci),
              med.approach=approach,
              effect.size=es,
              med.data=list(sig_thresh=p.threshold,
                            coefs=list(moi=coef_moi, dom=coef_dom, doi=coef_doi),
                            pvals=list(moi=pval_moi, dom=pval_dom, doi=pval_doi))
  )
  class(res) <- "rmedsem"
  return(res)
}

#' @export
summary.rmedsem <- function(res, digits=3){

}

#' @export
print.rmedsem <- function(res, digits=3){
  cat(sprintf("Significance testing of indirect effect (%s)\n",
              ifelse(res$standardized, "standardized", "unstandardized")))
  ## print the tests
  rowlab <- c("Indirect effect", "Std. Err.", "z-value", "p-value", "CI")
  mat <- tibble(Estimates=rowlab,
                Sobel=c( format(res$sobel[1:3], digit=digits),
                         format(res$sobel[4], digit=digits),
                         sprintf("[%s, %s]", format(res$sobel[5],digits=digits),
                                 format(res$sobel[6],digits=digits))),
                Delta=c( format(res$delta[1:3], digit=digits),
                         format(res$delta[4], digit=digits),
                         sprintf("[%s, %s]", format(res$delta[5],digits=digits),
                                 format(res$delta[6],digits=digits))),
                `Monte Carlo`=c( format(res$montc[1:3], digit=digits),
                                 format(res$montc[4], digit=digits),
                                 sprintf("[%s, %s]", format(res$montc[5],digits=digits),
                                         format(res$montc[6],digits=digits)))
  )
  print( mat )

  ## print the steps from BK or ZLC
  d <- res$med.data
  pth <- d$sig_thresh

  if("bk" %in% res$med.approach){ # BK
    cat("Baron and Kenny approach to testing mediation\n")

    if(d$pvals$moi>pth | d$pvals$dom>pth){
      cat(sprintf("  STEP 1 - '%s:%s' (X -> M) with B=%5.3f and p=%5.3f\n", res$vars$indep, res$vars$med, d$coefs$moi, d$pvals$moi))
      cat(sprintf("  STEP 2 - '%s:%s' (M -> Y) with B=%5.3f and p=%5.3f\n", res$vars$med,   res$vars$dep, d$coefs$dom, d$pvals$dom))
      cat("           As either STEP 1 or STEP 2 (or both) are not significant,\n")
      cat("           there is no mediation!")
    } else if(d$pvals$moi<pth & d$pvals$dom<pth & res$sobel[4] > pth){
      cat(sprintf("  STEP 1 - '%s:%s' (X -> M) with B=%5.3f and p=%5.3f\n", res$vars$indep, res$vars$med, d$coefs$moi, d$pvals$moi))
      cat(sprintf("  STEP 2 - '%s:%s' (M -> Y) with B=%5.3f and p=%5.3f\n", res$vars$med,   res$vars$dep, d$coefs$dom, d$pvals$dom))
      cat(sprintf("  STEP 3 - '%s:%s' (X -> Y) with B=%5.3f and p=%5.3f\n", res$vars$indep, res$vars$dep, d$coefs$doi, d$pvals$doi))
      cat("           As STEP 1, STEP 2 and the Sobel's test above are significant\n")
      cat("           and STEP 3 is not significant the mediation is complete!")
    } else if(d$pvals$moi<pth & d$pvals$dom<pth & res$sobel[4] < pth & d$pvals$doi<pth){
      cat(sprintf("  STEP 1 - '%s:%s' (X -> M) with B=%5.3f and p=%5.3f\n", res$vars$indep, res$vars$med, d$coefs$moi, d$pvals$moi))
      cat(sprintf("  STEP 2 - '%s:%s' (M -> Y) with B=%5.3f and p=%5.3f\n", res$vars$med,   res$vars$dep, d$coefs$dom, d$pvals$dom))
      cat(sprintf("  STEP 3 - '%s:%s' (X -> Y) with B=%5.3f and p=%5.3f\n", res$vars$indep, res$vars$dep, d$coefs$doi, d$pvals$doi))
      cat("           As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above\n")
      cat("           are significant the mediation is partial!")
    } else if(d$pvals$moi<pth & d$pvals$dom<pth & res$sobel[4] > pth & d$pvals$doi<pth){
      cat(sprintf("  STEP 1 - '%s:%s' (X -> M) with B=%5.3f and p=%5.3f\n", res$vars$indep, res$vars$med, d$coefs$moi, d$pvals$moi))
      cat(sprintf("  STEP 2 - '%s:%s' (M -> Y) with B=%5.3f and p=%5.3f\n", res$vars$med,   res$vars$dep, d$coefs$dom, d$pvals$dom))
      cat(sprintf("  STEP 3 - '%s:%s' (X -> Y) with B=%5.3f and p=%5.3f\n", res$vars$indep, res$vars$dep, d$coefs$doi, d$pvals$doi))
      cat("           As STEP 1, STEP 2 and STEP 3 are all significant and the\n")
      cat("           Sobel's test above is not significant the mediation is partial!")
    } else if(d$pvals$moi<pth & d$pvals$dom<pth & res$sobel[4] > pth & d$pvals$doi>pth){
      cat(sprintf("  STEP 1 - '%s:%s' (X -> M) with B=%5.3f and p=%5.3f\n", res$vars$indep, res$vars$med, d$coefs$moi, d$pvals$moi))
      cat(sprintf("  STEP 2 - '%s:%s' (M -> Y) with B=%5.3f and p=%5.3f\n", res$vars$med,   res$vars$dep, d$coefs$dom, d$pvals$dom))
      cat(sprintf("  STEP 3 - '%s:%s' (X -> Y) with B=%5.3f and p=%5.3f\n", res$vars$indep, res$vars$dep, d$coefs$doi, d$pvals$doi))
      cat("           As STEP 1 and STEP 2 are significant and neither STEP 3 nor\n")
      cat("           the Sobel's test above is significant the mediation is partial!")
    }
    cat("\n")
  }

  if("zlc" %in% res$med.approach){ # ZLC
    cat("Zhao, Lynch & Chen's approach to testing mediation\n")
    axbxc <- with(d$coefs, moi*dom*doi)
    if(res$montc[4] < pth & d$pvals$doi > pth){
      cat(sprintf("  STEP 1 - '%s:%s' (X -> Y) with B=%5.3f and p=%5.3f\n", res$vars$indep, res$vars$dep, d$coefs$doi, d$pvals$doi))
      cat("           As the Monte Carlo test above is significant and STEP 1 is not\n")
      cat("           significant you have indirect-only mediation (full mediation)!")
    } else if(res$montc[4] > pth & d$pvals$doi < pth){
      cat(sprintf("  STEP 1 - '%s:%s' (X -> Y) with B=%5.3f and p=%5.3f\n", res$vars$indep, res$vars$dep, d$coefs$doi, d$pvals$doi))
      cat("           As the Monte Carlo test above is not significant and STEP 1 is\n")
      cat("           significant you have direct-only nonmediation (no mediation)!")
    } else if(res$montc[4] > pth & d$pvals$doi > pth){
      cat(sprintf("  STEP 1 - '%s:%s' (X -> Y) with B=%5.3f and p=%5.3f\n", res$vars$indep, res$vars$dep, d$coefs$doi, d$pvals$doi))
      cat("           As the Monte Carlo test above is not significant and STEP 1 is\n")
      cat("           not significant you have no effect nonmediation (no mediation)!")
    } else if(res$montc[4] < pth & d$pvals$doi < pth & axbxc > 0){
      cat(sprintf("  STEP 1 - '%s:%s' (X -> Y) with B=%5.3f and p=%5.3f\n", res$vars$indep, res$vars$dep, d$coefs$doi, d$pvals$doi))
      cat("           As the Monte Carlo test above is significant, STEP 1 is\n")
      cat("           significant and their coefficients point in same direction,\n")
      cat("           you have complementary mediation (partial mediation)!")
    } else if(res$montc[4] < pth & d$pvals$doi < pth & axbxc < 0){
      cat(sprintf("  STEP 1 - '%s:%s' (X -> Y) with B=%5.3f and p=%5.3f\n", res$vars$indep, res$vars$dep, d$coefs$doi, d$pvals$doi))
      cat("           As the Monte Carlo test above is significant, STEP 1 is\n")
      cat("           significant and their coefficients point in opposite\n")
      cat("           direction, you have competitive mediation (partial mediation)!")
    }
    cat("\n")
  }

  es=res$effect.size
  if("RIT" %in% names(es)){
    cat(sprintf("  RIT  =   (Indirect effect / Total effect)\n"))
    with(es$RIT, cat(sprintf("           (%5.3f/%5.3f) = %5.3f\n", ind_eff, tot_eff, es)))
    with(es$RIT, cat(sprintf("           Meaning that about %3.0f%% of the effect of '%s'\n", es*100, res$vars$indep)))
    with(es$RIT, cat(sprintf("           on '%s' is mediated by '%s'!\n", res$vars$dep, res$vars$med)))
  }
  if("RID" %in% names(es)){
    cat(sprintf("  RID  =   (Indirect effect / Direct effect)\n"))
    with(es$RID, cat(sprintf("           (%5.3f/%5.3f) = %5.3f\n", ind_eff, dir_eff, es)))
    with(es$RID, cat(sprintf("           That is, the mediated effect is about %3.1f times as\n", es)))
    with(es$RID, cat(sprintf("           large as the direct effect of '%s' on '%s'!", res$vars$indep, res$vars$dep)))
  }
}



