#'
#'
#' @export
rmedsem <- function (mod, ...)
  UseMethod("rmedsem")


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
rmedsem.lavaan <- function(mod, indep, med, dep, standardized=FALSE, mcreps=NULL,
                    approach="bk", p.threshold=0.05,
                    effect.size=c("RIT","RID")){
  # for testing
  #indep="math"; med="read"; dep="science";
  #indep="SES"; med="Alien67"; dep="Alien71";
  #indep="ind60"; med="dem60"; dep="dem65"
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


  res <- list(package="lavaan", standardized=standardized,
              vars =list(med=med, indep=indep, dep=dep),
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
                         approach="bk", p.threshold=0.05,
                         effect.size=c("RIT","RID")){
  #indep="Math"; med="Read"; dep="Science";

  N <- nrow(mod$Information$Data)
  moi <- sprintf("%s ~ %s", med, indep)
  dom <- sprintf("%s ~ %s", dep, med)
  doi <- sprintf("%s ~ %s", dep, indep)

  imod <- cSEM::infer(mod)
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




#' indent s by indent many spaces; merge into a single string
#' @param s a string
#' @param indent an integer how much to indent s
#' @return indented string
#'
pre_indent_merge <- function(s, indent){
  indstr <- strrep(" ", indent)
  sapply(s, \(.x) paste0(indstr,.x,collapse="")) |> paste0(collapse="")
}

#' Function for printing `rmedsem` objects
#'
#' @param res the `rmedsem` object to print
#' @param digits an integer, number of digits to print in table
#' @return `rmedsem` obect `res`
#'
#' @export
print.rmedsem <- function(res, digits=3){
  # indentation
  indent <- 3
  indstr <- strrep(" ", indent)
  indent.conclusion <- indent + 9
  indcstr <- strrep(" ", indent.conclusion)
  formatstr <- "%5.3f" # format for real numbers

  cat(sprintf("Significance testing of indirect effect (%s)\n",
              ifelse(res$standardized, "standardized", "unstandardized")))
  cat(sprintf("Model estimated with package '%s'\n", res$package))
  cat(with(res$vars, sprintf("Mediation effect: '%s' -> '%s' -> '%s'\n\n",
                             indep,med,dep)))
  ## print the tests
  rowlab <- c("Indirect effect", "Std. Err.", "z-value", "p-value", "CI")

  mat <- data.frame(Sobel=c( format(res$sobel[1:3], digit=digits),
                         format(res$sobel[4], digit=digits),
                         sprintf("[%s, %s]", format(res$sobel[5],digits=digits),
                                 format(res$sobel[6],digits=digits))),
                Delta=c( format(res$delta[1:3], digit=digits),
                         format(res$delta[4], digit=digits),
                         sprintf("[%s, %s]", format(res$delta[5],digits=digits),
                                 format(res$delta[6],digits=digits))))
  if(res$package=="lavaan"){
    mat <- cbind(mat, `Monte-Carlo`=c( format(res$montc[1:3], digit=digits),
                     format(res$montc[4], digit=digits),
                     sprintf("[%s, %s]", format(res$montc[5],digits=digits),
                             format(res$montc[6],digits=digits))))
  } else if(res$package=="cSEM"){
    mat <- cbind(mat, `Bootstrap`=c( format(res$boot[1:3], digit=digits),
                                       format(res$boot[4], digit=digits),
                                       sprintf("[%s, %s]", format(res$boot[5],digits=digits),
                                               format(res$boot[6],digits=digits))))
  }
  rownames(mat) <- rowlab
  print( mat )
  cat("\n")

  ## print the steps from BK or ZLC
  d <- res$med.data
  pth <- d$sig_thresh

  if("bk" %in% res$med.approach){ # BK
    cat("Baron and Kenny approach to testing mediation\n")
    step1 <- sprintf("%sSTEP 1 - '%s:%s' (X -> M) with B=%5.3f and p=%5.3f\n",
                     indstr, res$vars$indep, res$vars$med, d$coefs$moi, d$pvals$moi)
    step2 <- sprintf("%sSTEP 2 - '%s:%s' (M -> Y) with B=%5.3f and p=%5.3f\n",
                     indstr, res$vars$med,   res$vars$dep, d$coefs$dom, d$pvals$dom)
    step3 <- sprintf("%sSTEP 3 - '%s:%s' (X -> Y) with B=%5.3f and p=%5.3f\n",
                     indstr, res$vars$indep, res$vars$dep, d$coefs$doi, d$pvals$doi)
    if(d$pvals$moi>pth | d$pvals$dom>pth){
      conclusion <- c(
        "As either STEP 1 or STEP 2 (or both) are not significant,\n",
        "there is no mediation.\n"
      ) |> pre_indent_merge(indent.conclusion)
      cat(step1,step2,conclusion, sep="")
    } else if(d$pvals$moi<pth & d$pvals$dom<pth & res$sobel[4] > pth){
      conclusion <- c(
        "As STEP 1, STEP 2 and the Sobel's test above are significant\n",
        "and STEP 3 is not significant the mediation is complete.\n"
      ) |> pre_indent_merge(indent.conclusion)
      cat(step1,step2,step3,conclusion, sep="")
    } else if(d$pvals$moi<pth & d$pvals$dom<pth & res$sobel[4] < pth & d$pvals$doi<pth){
      conclusion <- c(
        "As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above\n",
        "are significant the mediation is partial.\n"
      ) |> pre_indent_merge(indent.conclusion)
      cat(step1,step2,step3,conclusion, sep="")
    } else if(d$pvals$moi<pth & d$pvals$dom<pth & res$sobel[4] > pth & d$pvals$doi<pth){
      conclusion <- c(
        "As STEP 1, STEP 2 and STEP 3 are all significant and the\n",
        "Sobel's test above is not significant the mediation is partial.\n"
      ) |> pre_indent_merge(indent.conclusion)
      cat(step1,step2,step3,conclusion, sep="")
    } else if(d$pvals$moi<pth & d$pvals$dom<pth & res$sobel[4] > pth & d$pvals$doi>pth){
      conclusion <- c(
        "As STEP 1 and STEP 2 are significant and neither STEP 3 nor\n",
        "the Sobel's test above is significant the mediation is partial.\n"
      ) |> pre_indent_merge(indent.conclusion)
      cat(step1,step2,step3,conclusion, sep="")
    }
    cat("\n")
  }

  if("zlc" %in% res$med.approach){ # ZLC
    cat("Zhao, Lynch & Chen's approach to testing mediation\n")
    if(res$package=="lavaan"){
      cat("Based on p-value estimated using Monte-Carlo\n")
      zlc.pv <- res$montc[4]
      zlc.met <- "Monte-Carlo"
    } else if(res$package=="cSEM"){
      cat("Based on p-value estimated using Bootstrap\n")
      zlc.pv <- res$boot[4]
      zlc.met <- "Bootstrap"
    }

    axbxc <- with(d$coefs, moi*dom*doi)
    step1 <- sprintf("  STEP 1 - '%s:%s' (X -> Y) with B=%5.3f and p=%5.3f\n",
                     res$vars$indep, res$vars$dep, d$coefs$doi, d$pvals$doi)
    if(zlc.pv < pth & d$pvals$doi > pth){
      conclusion <- c(
        sprintf("As the %s test above is significant and STEP 1 is not\n", zlc.met),
        "significant there indirect-only mediation (full mediation).\n"
      ) |> pre_indent_merge(indent.conclusion)
    } else if(zlc.pv > pth & d$pvals$doi < pth){
      conclusion <- c(
        sprintf("As the %s test above is not significant and STEP 1 is\n", zlc.met),
        "significant there is direct-only nonmediation (no mediation).\n"
      ) |> pre_indent_merge(indent.conclusion)
    } else if(zlc.pv > pth & d$pvals$doi > pth){
      conclusion <- c(
        sprintf("As the %s test above is not significant and STEP 1 is\n", zlc.met),
        "not significant there is no effect nonmediation (no mediation).\n"
      ) |> pre_indent_merge(indent.conclusion)
    } else if(zlc.pv < pth & d$pvals$doi < pth & axbxc > 0){
      conclusion <- c(
        sprintf("As the %s test above is significant, STEP 1 is\n", zlc.met),
        "significant and their coefficients point in same direction,\n",
        "there is complementary mediation (partial mediation).\n"
      ) |> pre_indent_merge(indent.conclusion)
    } else if(zlc.pv < pth & d$pvals$doi < pth & axbxc < 0){
      conclusion <- c(
        sprintf("As the %s test above is significant, STEP 1 is\n", zlc.met),
        "significant and their coefficients point in opposite\n",
        "direction, there is competitive mediation (partial mediation).\n"
      ) |> pre_indent_merge(indent.conclusion)
    }
    cat(step1, conclusion, sep="")
    cat("\n")
  }

  es=res$effect.size
  if(length(es)>0){
    cat("Effect sizes\n")
  }
  indesstr = strrep(" ", indent+6)
  if("RIT" %in% names(es)){
    cat(sprintf("%sRIT = (Indirect effect / Total effect)\n", indstr))

    with(es$RIT, cat(sprintf("%s(%5.3f/%5.3f) = %5.3f\n", indesstr, ind_eff, tot_eff, es)))
    with(es$RIT, cat(sprintf("%sMeaning that about %3.0f%% of the effect of '%s'\n", indesstr, es*100, res$vars$indep)))
    with(es$RIT, cat(sprintf("%son '%s' is mediated by '%s'\n", indesstr, res$vars$dep, res$vars$med)))
  }
  if("RID" %in% names(es)){
    cat(sprintf("%sRID = (Indirect effect / Direct effect)\n", indstr))
    with(es$RID, cat(sprintf("%s(%5.3f/%5.3f) = %5.3f\n", indesstr, ind_eff, dir_eff, es)))
    with(es$RID, cat(sprintf("%sThat is, the mediated effect is about %3.1f times as\n", indesstr, es)))
    with(es$RID, cat(sprintf("%slarge as the direct effect of '%s' on '%s'", indesstr, res$vars$indep, res$vars$dep)))
  }
}



