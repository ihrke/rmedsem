#' indent s by indent many spaces; merge into a single string
#' @param s a string
#' @param indent an integer how much to indent s
#' @return indented string
#'
pre_indent_merge <- function(s, indent){
  indstr <- strrep(" ", indent)
  sapply(s, \(.x) paste0(indstr,.x,collapse="")) |> paste0(collapse="")
}


#' Function for printing `rmedsem` objects based on `blavaan` models.
#'
#' @param res the `rmedsem` object to print
#' @param digits an integer, number of digits to print in table
#' @return `rmedsem` obect `res`
#'
print.rmedsem.blavaan <- function(res, digits=3, indent=3){
  # indentation
  indstr <- strrep(" ", indent)
  indent.conclusion <- indent + 9
  indcstr <- strrep(" ", indent.conclusion)
  formatstr <- "%5.3f" # format for real numbers

  cat(sprintf("Prior (regression coefs): %s\n", res$prior$beta))

  # indentation
  indstr <- strrep(" ", indent)
  indent.conclusion <- indent + 9
  indcstr <- strrep(" ", indent.conclusion)
  formatstr <- "%5.3f" # format for real numbers

  ## print the tests
  rowlab <- c("Indirect effect", "Std. Err.", "z-value", "P(z>0)", "P(z<0)", "ER+", "ER-", "HDI")

  mat <- data.frame(Bayes=c( format(res$bayes[1:5], digit=digits),
                             ifelse(is.infinite(res$bayes[6:7]), "∞", format(res$bayes[6:7], digits=digits)),
                             sprintf("[%s, %s]", format(res$bayes[8],digits=digits),
                                     format(res$bayes[9],digits=digits))))
  rownames(mat) <- rowlab
  print( mat )
  cat("\n")

  es=res$effect.size
  print_effectsize(es, indent, digits)

}


#' Function for printing the effect size from the estimation methods
#'
#' @param res the `rmedsem` object to print
#' @param digits an integer, number of digits to print in table
#' @return `rmedsem` obect `res`
#'
print_effectsize <- function(res, digits=3, indent=3){
  es=res$effect.size
  # indentation
  indstr <- strrep(" ", indent)
  indent.conclusion <- indent + 9
  indcstr <- strrep(" ", indent.conclusion)
  formatstr <- "%5.3f" # format for real numbers

  if(length(es)>0){
    cat("Effect sizes\n")
  }
  indesstr = strrep(" ", indent+6)
  if(es$RIT$tot_eff<es$RIT$ind_eff){
    cat(sprintf("%sWARNING: Total effect is smaller than indirect effect!\n", indstr))
    cat(sprintf("%s         Effect sizes should not be interpreted.\n", indstr))
  }

  if("RIT" %in% names(es)){
    cat(sprintf("%sRIT = (Indirect effect / Total effect)\n", indstr))
    if(abs(es$RIT$tot_eff)<0.2){
      ## According to https://davidakenny.net/cm/mediate.htm, we should only
      ## calculate RIT if the total effect is > +-.2
      with(es$RIT, cat(sprintf("%sTotal effect %5.3f is too small to calculate RIT\n", indesstr, tot_eff)))
    } else {
      with(es$RIT, cat(sprintf("%s(%5.3f/%5.3f) = %5.3f\n", indesstr, ind_eff, tot_eff, es)))
      with(es$RIT, cat(sprintf("%sMeaning that about %3.0f%% of the effect of '%s'\n", indesstr, es*100, res$vars$indep)))
      with(es$RIT, cat(sprintf("%son '%s' is mediated by '%s'\n", indesstr, res$vars$dep, res$vars$med)))
    }
  }
  if("RID" %in% names(es)){
    cat(sprintf("%sRID = (Indirect effect / Direct effect)\n", indstr))
    with(es$RID, cat(sprintf("%s(%5.3f/%5.3f) = %5.3f\n", indesstr, ind_eff, dir_eff, es)))
    with(es$RID, cat(sprintf("%sThat is, the mediated effect is about %3.1f times as\n", indesstr, es)))
    with(es$RID, cat(sprintf("%slarge as the direct effect of '%s' on '%s'", indesstr, res$vars$indep, res$vars$dep)))
  }

}


#' Function for printing `rmedsem` objects
#'
#' @param res the `rmedsem` object to print
#' @param digits an integer, number of digits to print in table
#' @return `rmedsem` obect `res`
print.rmedsem.lavaan.csem <- function(res, digits=3, indent=3){
  # indentation
  indstr <- strrep(" ", indent)
  indent.conclusion <- indent + 9
  indcstr <- strrep(" ", indent.conclusion)
  formatstr <- "%5.3f" # format for real numbers

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
    } else if(d$pvals$moi<pth & d$pvals$dom<pth & res$sobel[4] < pth){
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

  print_effectsize(res, indent, digits)
}


#' Function for printing `rmedsem` objects
#'
#' @param res the `rmedsem` object to print
#' @param digits an integer, number of digits to print in table
#' @return `rmedsem` obect `res`
#'
#' @export
print.rmedsem <- function(res, digits=3, indent=3){

  cat(sprintf("Significance testing of indirect effect (%s)\n",
              ifelse(res$standardized, "standardized", "unstandardized")))
  cat(sprintf("Model estimated with package '%s'\n", res$package))
  cat(with(res$vars, sprintf("Mediation effect: '%s' -> '%s' -> '%s'\n\n",
                             indep,med,dep)))

  if(res$package=="blavaan"){
    print.rmedsem.blavaan(res,digits=digits, indent=indent)
  } else {
    print.rmedsem.lavaan.csem(res, digits=digits, indent=indent)
  }
}



