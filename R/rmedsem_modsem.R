#' Calculate a mediation analysis for an SEM based on a modsem model.
#'
#' @param mod A fitted SEM model (modsem).
#' @param indep A string indicating the name of the independent variable in the model.
#' @param med A string indicating the name of the mediator variable in the model.
#' @param dep A string indicating the name of the dependent variable in the model.
#' @param moderator A string indicating the name of the moderator variable in the model.
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
#' m <- "
#'   OwnLook =~ smv_attr_face + smv_attr_body + smv_sexy
#'   SelfEst =~ ses_satis + ses_qualities + ses_able_todo
#'   MentWell =~ mwb_optimistic + mwb_useful + mwb_energy
#'   smv =~ smv_kind + smv_caring + smv_understanding +
#'     smv_make_laughh + smv_funny + smv_sociable
#'   SelfEst ~ OwnLook + smv + smv:OwnLook
#'   MentWell ~ OwnLook + SelfEst + smv + smv:OwnLook
#' "
#' 
#' 
#' est <- modsem(m, data = mchoice2, method="lms")
#'
#' # mediated moderation
#' rmedsem(indep="smv:OwnLook", dep="MentWell", med="SelfEst", mod=est)
#'
#' # moderated mediation
#' rmedsem(indep="OwnLook", dep="MentWell", med="SelfEst", mod=est, moderator="smv")
#' 
#' }
#'
rmedsem.modsem <- function(mod, indep, med, dep, moderator=NULL, 
                           standardized=TRUE, mcreps=NULL,
                           approach=c("bk", "zlc"), p.threshold=0.05,
                           effect.size=c("RIT","RID"), ci.two.tailed=0.95){
  ci.width <- qnorm(1-(1-ci.two.tailed)/2)

  # if estimated lavaan, we just extract the lavaan document
  N <- modsem::modsem_nobs(mod)

  if (is.null(mcreps) || mcreps < N)
    mcreps=N
  
  if (standardized)
    coefs <- modsem::standardized_estimates(mod)
  else {
    coefs <- modsem::parameter_estimates(mod)
    coefs[["se"]] <- NULL # bug in modsem
  }
 
  lookup <- c(std.error="se", p.value="pvalue", est="est.std") # in case of lavaan
  coefs <- dplyr::rename(coefs, dplyr::any_of(lookup)) # rename columns

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
  delta_se <- sqrt((coef_dom^2)*var_moi + (coef_moi^2)*var_dom + 2*coef_dom*coef_moi*corrmoidom)

  delta_z  <- prodterm/delta_se
  delta_pv  <- 2*(1-stats::pnorm(abs(delta_z)))
  delta_lci <- prodterm - ci.width*delta_se
  delta_uci <- prodterm + ci.width*delta_se

  sigma <- symmetric(se_moi, corrmoidom, corrmoidom, se_dom)
  coefx <- mvtnorm::rmvnorm(n=mcreps, mean=c(coef_moi, coef_dom), sigma = sigma**2)

  prod_coef  <- apply(coefx, 1, prod)
  montc_prod <- mean(prod_coef)
  montc_se   <- stats::sd(prod_coef)
  montc_z    <- montc_prod/montc_se
  montc_pv   <- 2*(1-stats::pnorm(abs(montc_z)))
  montc_qs   <- stats::quantile(prod_coef, c(0.025, 0.975))
  montc_lci  <- montc_qs[1]
  montc_uci  <- montc_qs[2]
  names(montc_lci) <- NULL
  names(montc_uci) <- NULL

  # TE = IND + DE
  coef_tot <- coef_doi + prodterm
  sigma <- symmetric(se_moi, corrmoidom, corrmoidoi,
                     corrmoidom, se_dom, corrdomdoi,
                     corrmoidoi, corrdomdoi, se_doi)
  coefx <- mvtnorm::rmvnorm(n=mcreps, mean=c(coef_moi, coef_dom, coef_doi), sigma = sigma**2)
  tot_eff_samp <- (coefx[,1]*coefx[,2])+coefx[,3]
  coef_tot <- mean(tot_eff_samp)
  se_tot   <- stats::sd(tot_eff_samp)
  tot_qs   <- stats::quantile(tot_eff_samp, c(0.025, 0.975))
  lci_tot  <- tot_qs[1]
  uci_tot  <- tot_qs[2]
  names(lci_tot) <- NULL
  names(uci_tot) <- NULL

  es <- list()
  ind_eff <- abs(prodterm)
  tot_eff <- abs(prodterm+coef_doi)
  dir_eff <- abs(coef_doi)

  if ("RIT" %in% effect.size)
    es$RIT=list(es=ind_eff/tot_eff, ind_eff=ind_eff, tot_eff=tot_eff)
  if ("RID" %in% effect.size)
    es$RID=list(es=ind_eff/dir_eff, ind_eff=ind_eff, dir_eff=dir_eff)

  if (!is.null(moderator)) {
    modind <- get_correct_intterm(mod_c(indep, moderator), coefs, error=FALSE,
                                  return_err = "__modmed__")
    modmed <- get_correct_intterm(mod_c(med, moderator), coefs, error=FALSE,
                                  return_err = "__modmmed__")

    moi_mod <- sprintf("%s~%s", med, modind)
    dom_mod <- sprintf("%s~%s", dep, modmed)
    doi_mod <- sprintf("%s~%s", dep, modind)

    # IV -> M | mod
    coef_moi_mod  <- with0(coefs, est[lhs==med & rhs==modind])
    se_moi_mod    <- with0(coefs, std.error[lhs==med & rhs==modind])
    var_moi_mod   <- se_moi_mod^2
    pval_moi_mod  <- with0(coefs, p.value[lhs==med & rhs==modind])
    upper_moi_mod <- coef_moi_mod + ci.width*se_moi_mod
    lower_moi_mod <- coef_moi_mod - ci.width*se_moi_mod

    # M -> DV | mod
    coef_dom_mod  <- with0(coefs, est[lhs==dep & rhs==modmed])
    se_dom_mod    <- with0(coefs, std.error[lhs==dep & rhs==modmed])
    var_dom_mod   <- se_dom_mod^2
    pval_dom_mod  <- with0(coefs, p.value[lhs==dep & rhs==modmed])
    upper_dom_mod <- coef_dom_mod + ci.width*se_dom_mod
    lower_dom_mod <- coef_dom_mod - ci.width*se_dom_mod

    # IV -> DV direct effect | mod 
    coef_doi_mod  <- with0(coefs, est[lhs==dep & rhs==modind])
    se_doi_mod    <- with0(coefs, std.error[lhs==dep & rhs==modind])
    var_doi_mod   <- se_doi_mod^2
    pval_doi_mod  <- with0(coefs, p.value[lhs==dep & rhs==modind])
    upper_doi_mod <- coef_doi_mod + ci.width*se_doi_mod
    lower_doi_mod <- coef_doi_mod - ci.width*se_doi_mod

    # TE = IND + DE | mod
    # B = (coef_moi + coef_moi_mod * z) * (coef_dom + coef_dom_mod * z)
    # z = 1 ==> B = B_z1 = coef_moi * coef_dom + coef_moi * coef_dom_mod + coef_moi_mod * coef_dom + coef_moi_mod * coef_dom_mod
    # z = 0 ==> B = B_z0 = coef_moi * coef_dom
    # (B_z1 - Bz0) / z1-z0 = B_z1 - B_z0 = 
    # coef_moi * coef_dom_mod + coef_moi_mod * coef_dom + coef_moi_mod * coef_dom_mod
    formula_indir <- substitute(coef_moi * coef_dom_mod + 
                                coef_dom * coef_moi_mod + 
                                coef_moi_mod * coef_dom_mod, env=list())

    coef_vars <- c(moi_mod, dom_mod, doi_mod, dom, moi)
    sigma <- get_sub_squaremat(M=V, names=coef_vars)

    if (standardized) {
      new_variances <- c(var_moi_mod, var_dom_mod, var_doi_mod, var_dom, var_moi)
      sigma <- rescale_vcov(sigma, rescaled_variances=new_variances)
    }

    # CHECK computation of std.errors!
    coefx <- rmvnorm_df(n=mcreps, sigma = sigma, 
                        mean=c(coef_moi_mod, coef_dom_mod, coef_doi_mod, coef_dom, coef_moi),
                        names=c("coef_moi_mod", "coef_dom_mod", "coef_doi_mod", 
                                "coef_dom", "coef_moi"))

    ind_eff_mod <- eval(formula_indir)
    tot_eff_mod   <- ind_eff_mod + coef_doi_mod

    ind_eff_samp_mod <- with(coefx, eval(formula_indir))
    tot_eff_samp_mod <- ind_eff_samp_mod + with(coefx, coef_doi_mod)

    # total
    se_tot_mod    <- stats::sd(tot_eff_samp_mod)
    zval_tot_mod  <- tot_eff_mod / se_tot_mod
    pv_tot_mod    <- 2*(1-stats::pnorm(abs(zval_tot_mod)))
    upper_tot_mod <- tot_eff_mod + ci.width*se_tot_mod
    lower_tot_mod <- tot_eff_mod - ci.width*se_tot_mod

    # indirect
    se_ind_mod    <- stats::sd(ind_eff_samp_mod)
    zval_ind_mod  <- ind_eff_mod / se_ind_mod
    pv_ind_mod    <- 2*(1-stats::pnorm(abs(zval_ind_mod)))
    upper_ind_mod <- ind_eff_mod + ci.width*se_ind_mod
    lower_ind_mod <- ind_eff_mod - ci.width*se_ind_mod

    f_vars <- format(c(indep, med, dep))
    f_indep <- f_vars[1]
    f_med <- f_vars[2]
    f_dep <- f_vars[3]

    moderation.info <- list( 
      has.moderator = TRUE,
      moderator = moderator, 

      coefs = list(
         moi=list(lhs=f_med, rhs=f_indep, coef=coef_moi_mod, 
                  se=se_moi_mod, pval=pval_moi_mod, 
                  lower=lower_moi_mod, upper=upper_moi_mod),
         dom=list(lhs=f_dep, rhs=f_med, coef=coef_dom_mod, 
                  se=se_dom_mod, pval=pval_dom_mod, 
                  lower=lower_dom_mod, upper=upper_dom_mod),
         doi=list(lhs=f_dep, rhs=f_indep, coef=coef_doi_mod, 
                  se=se_doi_mod, pval=pval_doi_mod, 
                  lower=lower_doi_mod, upper=upper_doi_mod)
      ),

      total.effect=list(lhs=f_dep, rhs=f_indep, coef=tot_eff_mod, 
                        se=se_tot_mod, pval=pv_tot_mod, 
                        lower=lower_tot_mod, upper=upper_tot_mod),
      indirect.effect=list(lhs=f_med, rhs=f_indep,
                           coef=ind_eff_mod, se=se_ind_mod, pval=pv_ind_mod, 
                           lower=lower_ind_mod, upper=upper_ind_mod)
    )

  } else moderation.info <- list(has.moderator = FALSE)


  res <- list(package="modsem", standardized=standardized,
              vars =list(med=med, indep=indep, dep=dep),
              est.methods = c("sobel","delta", "montc"),
              direct.effect = c(coef=coef_doi, se=se_doi, pval=pval_doi, lower=lci_doi, upper=uci_doi),
              total.effect = c(coef=coef_tot, se=se_tot, lower=lci_tot, upper=uci_tot),
              sobel=c(coef=prodterm, se=sobel_se, zval=sobel_z, pval=sobel_pv, lower=sobel_lci, upper=sobel_uci),
              delta=c(coef=prodterm, se=delta_se, zval=delta_z, pval=delta_pv, lower=delta_lci, upper=delta_uci),
              montc=c(coef=prodterm, se=montc_se, zval=montc_z, pval=montc_pv, lower=montc_lci, upper=montc_uci),
              moderation = moderation.info,
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


mod_c <- function(...) {
  paste(..., sep = ":")
}


with0 <- function(data, expr) {
  out <- eval(substitute(expr), envir=data, enclos=parent.frame())
  if (!length(out)) 0 else out
}


symmetric <- function(...) {
  x <- c(...)

  k <- sqrt(length(x))
  if (k != round(k)) stop("x must be a square matrix")

  matrix(x, nrow=k, ncol=k, byrow=FALSE)
}


get_correct_intterm <- function(x, parTable, error=TRUE, return_err=NULL) {
  if (length(x) != 1) stop("x must be a single string")
  else if (is.null(x) || !grepl(":", x)) return(x)

  rhs <- parTable$rhs

  x_z <- x
  z_x <- reverse_intterm(x_z)
  xz <- str_remove(x_z, ":")
  zx <- str_remove(z_x, ":")

  if       (xz %in% rhs) xz
  else if  (zx %in% rhs) zx
  else if (x_z %in% rhs) x_z
  else if (z_x %in% rhs) z_x
  else if (error) stop("Unable to find variable ", xz)
  else return_err
}


get_sub_squaremat <- function(M, names) {
    missing_names <-  setdiff(names, colnames(M))
    matching_names <- intersect(names, colnames(M))

    M <- M[matching_names, matching_names]
    k <- length(missing_names)
    m <- ncol(M)

    KK <- matrix(0, nrow=k, ncol=k, dimnames=list(missing_names, missing_names))
    KM <- matrix(0, nrow=k, ncol=m, dimnames=list(missing_names, colnames(M)))
    MK <- t(KM)

    Z <- cbind(rbind(M, KM),
               rbind(MK, KK))

    Z[names, names] # sort values by names
}


rmvnorm_df <- function(n, mean, sigma, names=NULL, ...) {
  coefx <- as.data.frame(mvtnorm::rmvnorm(n=n, mean=mean, sigma = sigma, ...))

  if (!is.null(names)) 
    colnames(coefx) <- names

  coefx
}


rescale_vcov <- function(vcov, rescaled_variances) {
  is_zero <- rescaled_variances == 0
  S <- vcov[!is_zero, !is_zero]

  D_old_inv <- diag(1 / sqrt(diag(S)))
  D_new <- diag(sqrt(rescaled_variances[!is_zero]))

  R <- D_old_inv %*% S %*% D_old_inv
  Z <- D_new %*% R %*% D_new

  V <- vcov
  V[!is_zero, !is_zero] <- Z

  V
}
