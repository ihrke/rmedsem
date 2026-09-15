#' Indent and merge strings
#' @param s a string
#' @param indent an integer how much to indent s
#' @return indented string
#' @noRd
pre_indent_merge <- function(s, indent){
  indstr <- strrep(" ", indent)
  sapply(s, \(.x) paste0(indstr,.x,collapse="")) |> paste0(collapse="")
}


#' Human-readable labels for estimation methods
#'
#' Unknown methods (e.g., from third-party backends) are labelled by their
#' name in `res$est.methods`.
#' @param methods character vector of method names
#' @return character vector of labels
#' @noRd
method_label <- function(methods){
  labels <- c(sobel="Sobel", delta="Delta", montc="Monte-Carlo",
              boot="Bootstrap", bayes="Bayes")
  ifelse(methods %in% names(labels), labels[methods], methods)
}


#' Estimation method used for the Zhao, Lynch & Chen approach
#'
#' Uses `res$zlc.method` if present, otherwise the last element of
#' `res$est.methods`.
#' @param res an `rmedsem` object
#' @return a single method name
#' @noRd
zlc_method <- function(res){
  if (!is.null(res$zlc.method)) res$zlc.method else res$est.methods[length(res$est.methods)]
}


#' Print the Header of an rmedsem Object
#' @param res an `rmedsem` object
#' @return `NULL` (invisibly)
#' @noRd
print_header <- function(res){
  cat(sprintf("Significance testing of indirect effect (%s)\n",
              ifelse(res$standardized, "standardized", "unstandardized")))
  cat(sprintf("Model estimated with package '%s'\n", res$package))
  cat(with(res$vars, sprintf("Mediation effect: '%s' -> '%s' -> '%s'\n\n",
                             indep,med,dep)))
  invisible(NULL)
}


#' Print the Table of Frequentist Indirect-Effect Tests
#'
#' One column per element of `res$est.methods`; each `res[[method]]` must be
#' a numeric vector with elements `coef, se, zval, pval, lower, upper`.
#' @param res an `rmedsem` object
#' @param digits an integer, number of digits to print in table
#' @return `NULL` (invisibly)
#' @noRd
print_freq_table <- function(res, digits=3){
  # one row per quantity; fixed decimal places so that all columns use the
  # same precision
  get <- function(n) vapply(res$est.methods, \(m) unname(res[[m]][[n]]), numeric(1))
  fmt <- function(v) format_fixed(v, digits)
  mat <- rbind(fmt(get("coef")), fmt(get("se")), fmt(get("zval")),
               format.pval(get("pval"), digits=digits),
               sprintf("[%s, %s]", fmt(get("lower")), fmt(get("upper"))))
  dimnames(mat) <- list(c("Indirect effect", "Std. Err.", "z-value", "p-value", "CI"),
                        method_label(res$est.methods))
  print(mat, quote=FALSE, right=TRUE)
  cat("\n")
  invisible(NULL)
}


#' Print the Table of Bayesian Indirect-Effect Tests
#' @param res an `rmedsem` object with a `bayes` element
#' @param digits an integer, number of digits to print in table
#' @return `NULL` (invisibly)
#' @noRd
print_bayes_table <- function(res, digits=3){
  b <- res$bayes
  fmt_er <- function(x) ifelse(is.infinite(x), "\u221E", format_fixed(x, digits))
  mat <- data.frame(Bayes=c(format_fixed(b[c("coef","se","zval","pvpos","pvneg")], digits),
                            fmt_er(b[c("ERpos","ERneg")]),
                            sprintf("[%s, %s]", format_fixed(b[["lower"]], digits),
                                    format_fixed(b[["upper"]], digits))))
  rownames(mat) <- c("Indirect effect", "Std. Err.", "z-value", "P(z>0)", "P(z<0)",
                     "ER+", "ER-", ci_type(res))
  print(mat)
  cat("\n")
  invisible(NULL)
}


#' Type of the stored intervals
#' @param res an `rmedsem` object (or `summary.rmedsem` object)
#' @return `res$ci.type` if present (e.g. `"HDI"`), otherwise `"CI"`
#' @noRd
ci_type <- function(res){
  if (is.null(res$ci.type)) "CI" else res$ci.type
}


#' Format numbers with a fixed number of decimal places
#' @param x numeric vector
#' @param digits number of decimal places
#' @return character vector; `NA` becomes `""`
#' @noRd
format_fixed <- function(x, digits){
  ifelse(is.na(x), "", formatC(x, format="f", digits=digits))
}


#' Format a proportion as a percentage label
#' @param x a proportion, e.g. 0.95
#' @return a string, e.g. `"95%"`
#' @noRd
format_percent <- function(x){
  paste0(format(100*x, trim=TRUE, digits=3), "%")
}


#' Type of Mediation According to Baron and Kenny
#'
#' A p-value is considered significant if it is strictly smaller than the
#' threshold; a p-value equal to the threshold (or `NA`) is not significant.
#' @param res an `rmedsem` object with elements `med.data` and `sobel`
#' @return one of `"none"` (STEP 1 or STEP 2 not significant), `"complete"`
#'   (Sobel significant, STEP 3 not), `"partial"` (all significant),
#'   `"partial_sobel_ns"` (STEP 3 significant, Sobel not) or
#'   `"partial_all_ns"` (neither STEP 3 nor Sobel significant)
#' @noRd
bk_type <- function(res){
  d <- res$med.data
  sig <- function(p) isTRUE(p < d$sig_thresh)
  if (!sig(d$pvals$moi) || !sig(d$pvals$dom))
    return("none")
  sobel.sig <- sig(res$sobel[["pval"]])
  step3.sig <- sig(d$pvals$doi)
  if (sobel.sig && !step3.sig) "complete"
  else if (sobel.sig && step3.sig) "partial"
  else if (step3.sig) "partial_sobel_ns"
  else "partial_all_ns"
}


#' Type of Mediation According to Zhao, Lynch & Chen
#'
#' The test of the indirect effect is based on the method returned by
#' `zlc_method()`. A p-value is considered significant if it is strictly
#' smaller than the threshold; a p-value equal to the threshold (or `NA`) is
#' not significant.
#' @param res an `rmedsem` object with element `med.data`
#' @return one of `"indirect-only"`, `"direct-only"`, `"no-effect"`,
#'   `"complementary"` or `"competitive"`
#' @noRd
zlc_type <- function(res){
  d <- res$med.data
  sig <- function(p) isTRUE(p < d$sig_thresh)
  ind.sig <- sig(res[[zlc_method(res)]][["pval"]])
  dir.sig <- sig(d$pvals$doi)
  if (ind.sig && !dir.sig) "indirect-only"
  else if (!ind.sig && dir.sig) "direct-only"
  else if (!ind.sig && !dir.sig) "no-effect"
  else if (with(d$coefs, moi*dom*doi) > 0) "complementary"
  else "competitive"
}


#' Print the Baron and Kenny Mediation Steps
#'
#' @param res an `rmedsem` object
#' @param indent an integer, number of spaces to indent
#' @return `NULL` (invisibly)
#' @noRd
print_bk <- function(res, indent=3){
  d <- res$med.data
  indstr <- strrep(" ", indent)
  indent.conclusion <- indent + 9

  cat("Baron and Kenny approach to testing mediation\n")
  step1 <- sprintf("%sSTEP 1 - '%s:%s' (X -> M) with B=%5.3f and p=%5.3f\n",
                   indstr, res$vars$indep, res$vars$med, d$coefs$moi, d$pvals$moi)
  step2 <- sprintf("%sSTEP 2 - '%s:%s' (M -> Y) with B=%5.3f and p=%5.3f\n",
                   indstr, res$vars$med,   res$vars$dep, d$coefs$dom, d$pvals$dom)
  step3 <- sprintf("%sSTEP 3 - '%s:%s' (X -> Y) with B=%5.3f and p=%5.3f\n",
                   indstr, res$vars$indep, res$vars$dep, d$coefs$doi, d$pvals$doi)

  type <- bk_type(res)
  steps <- if (type == "none") c(step1, step2) else c(step1, step2, step3)
  conclusion <- switch(type,
    none = c(
      "As either STEP 1 or STEP 2 (or both) are not significant,\n",
      "there is no mediation.\n"),
    complete = c(
      "As STEP 1, STEP 2 and the Sobel's test above are significant\n",
      "and STEP 3 is not significant the mediation is complete.\n"),
    partial = c(
      "As STEP 1, STEP 2 and STEP 3 as well as the Sobel's test above\n",
      "are significant the mediation is partial.\n"),
    partial_sobel_ns = c(
      "As STEP 1, STEP 2 and STEP 3 are all significant and the\n",
      "Sobel's test above is not significant the mediation is partial.\n"),
    partial_all_ns = c(
      "As STEP 1 and STEP 2 are significant and neither STEP 3 nor\n",
      "the Sobel's test above is significant the mediation is partial.\n"))
  cat(steps, pre_indent_merge(conclusion, indent.conclusion), "\n", sep="")
  invisible(NULL)
}


#' Print the Zhao, Lynch & Chen Mediation Steps
#'
#' @param res an `rmedsem` object
#' @param indent an integer, number of spaces to indent
#' @return `NULL` (invisibly)
#' @noRd
print_zlc <- function(res, indent=3){
  d <- res$med.data
  indent.conclusion <- indent + 9

  zlc.lab <- method_label(zlc_method(res))
  cat("Zhao, Lynch & Chen's approach to testing mediation\n")
  cat(sprintf("Based on p-value estimated using %s\n", zlc.lab))

  step1 <- sprintf("  STEP 1 - '%s:%s' (X -> Y) with B=%5.3f and p=%5.3f\n",
                   res$vars$indep, res$vars$dep, d$coefs$doi, d$pvals$doi)

  conclusion <- switch(zlc_type(res),
    "indirect-only" = c(
      sprintf("As the %s test above is significant and STEP 1 is not\n", zlc.lab),
      "significant there is indirect-only mediation (full mediation).\n"),
    "direct-only" = c(
      sprintf("As the %s test above is not significant and STEP 1 is\n", zlc.lab),
      "significant there is direct-only nonmediation (no mediation).\n"),
    "no-effect" = c(
      sprintf("As the %s test above is not significant and STEP 1 is\n", zlc.lab),
      "not significant there is no effect nonmediation (no mediation).\n"),
    complementary = c(
      sprintf("As the %s test above is significant, STEP 1 is\n", zlc.lab),
      "significant and their coefficients point in same direction,\n",
      "there is complementary mediation (partial mediation).\n"),
    competitive = c(
      sprintf("As the %s test above is significant, STEP 1 is\n", zlc.lab),
      "significant and their coefficients point in opposite\n",
      "direction, there is competitive mediation (partial mediation).\n"))
  cat(step1, pre_indent_merge(conclusion, indent.conclusion), "\n", sep="")
  invisible(NULL)
}


#' p-value threshold of an rmedsem object
#' @param res an `rmedsem` object
#' @return the threshold used by [rmedsem()] (default 0.05)
#' @noRd
sig_threshold <- function(res){
  if (!is.null(res$med.data$sig_thresh)) res$med.data$sig_thresh
  else if (!is.null(res$p.threshold)) res$p.threshold
  else 0.05
}


#' Check whether an effect size can be interpreted
#'
#' RIT is not interpreted if the total effect is small (|total| < 0.2,
#' following Kenny, https://davidakenny.net/cm/mediate.htm), RID is not
#' interpreted if the direct effect is not significant, as the ratio is then
#' unstable.
#' @param res an `rmedsem` object
#' @param which `"RIT"` or `"RID"`
#' @return `NULL` if the effect size can be interpreted, otherwise a short
#'   description of the problem
#' @noRd
effect_size_problem <- function(res, which){
  es <- res$effect.size
  if (which == "RIT") {
    if (abs(es$RIT$tot_eff) < 0.2)
      return(sprintf("total effect %5.3f is too small (< 0.2)", es$RIT$tot_eff))
  } else if (which == "RID") {
    pval <- res$direct.effect[["pval"]]
    if (!isTRUE(pval < sig_threshold(res)))
      return(sprintf("direct effect %5.3f is not significant (p = %5.3f)",
                     es$RID$dir_eff, pval))
  }
  NULL
}


#' Print Effect Sizes from Mediation Analysis
#'
#' @param res the `rmedsem` object to print
#' @param digits an integer, number of digits to print in table
#' @param indent an integer, number of spaces to indent
#' @return `NULL` (invisibly)
#' @noRd
print_effectsize <- function(res, digits=3, indent=3){
  es <- res$effect.size
  indstr <- strrep(" ", indent)
  indesstr <- strrep(" ", indent+6)

  if(length(es)>0){
    cat("Effect sizes\n")
  }
  if("RIT" %in% names(es) && es$RIT$tot_eff < es$RIT$ind_eff){
    cat(sprintf("%sWARNING: Total effect is smaller than indirect effect!\n", indstr))
    cat(sprintf("%s         Effect sizes should not be interpreted.\n", indstr))
  }

  if("RIT" %in% names(es)){
    cat(sprintf("%sRIT = (Indirect effect / Total effect)\n", indstr))
    problem <- effect_size_problem(res, "RIT")
    if(!is.null(problem)){
      cat(sprintf("%sRIT is not reported: %s\n", indesstr, problem))
    } else {
      with(es$RIT, cat(sprintf("%s(%5.3f/%5.3f) = %5.3f\n", indesstr, ind_eff, tot_eff, es)))
      with(es$RIT, cat(sprintf("%sMeaning that about %3.0f%% of the effect of '%s'\n", indesstr, es*100, res$vars$indep)))
      with(es$RIT, cat(sprintf("%son '%s' is mediated by '%s'\n", indesstr, res$vars$dep, res$vars$med)))
    }
  }

  if("RID" %in% names(es)){
    cat(sprintf("%sRID = (Indirect effect / Direct effect)\n", indstr))
    problem <- effect_size_problem(res, "RID")
    if(!is.null(problem)){
      cat(sprintf("%sRID is not reported: %s\n", indesstr, problem))
    } else {
      with(es$RID, cat(sprintf("%s(%5.3f/%5.3f) = %5.3f\n", indesstr, ind_eff, dir_eff, es)))
      with(es$RID, cat(sprintf("%sThat is, the mediated effect is about %3.1f times as\n", indesstr, es)))
      with(es$RID, cat(sprintf("%slarge as the direct effect of '%s' on '%s'\n", indesstr, res$vars$indep, res$vars$dep)))
    }
  }

  if("upsilon" %in% names(es)){
    cat(sprintf("%sUpsilon (v) = Variance in Y explained indirectly by X through M\n", indstr))
    cat(sprintf("%sv(unadj) = %5.3f, v(adj) = %5.3f\n",
                indesstr, es$upsilon$unadjusted, es$upsilon$adjusted))
    if(!is.null(es$upsilon$posterior_mean)){
      ci.level <- if (is.null(res$ci.level)) 0.95 else res$ci.level
      cat(sprintf("%sPosterior mean(v) = %5.3f, median(v) = %5.3f\n",
                  indesstr, es$upsilon$posterior_mean, es$upsilon$posterior_median))
      cat(sprintf("%s%s %s [%5.3f, %5.3f]\n", indesstr,
                  format_percent(ci.level), ci_type(res),
                  es$upsilon$lower, es$upsilon$upper))
    }
  }
  cat("\n")
  invisible(NULL)
}


#' Print Moderation Effects of a Moderated Mediation Model
#' @param res an `rmedsem` object with a `moderation` element
#' @param ci_moderation a logical, whether to print confidence intervals
#' @return `NULL` (invisibly)
#' @noRd
print_moderation <- function(res, ci_moderation=FALSE){
  moderation <- res$moderation
  moderator <- moderation$moderator

  # lhs/rhs follow the lavaan convention (lhs ~ rhs), so paths run rhs -> lhs
  direct <- Filter(\(e) !(e$coef == 0 && e$se == 0), moderation$coefs)
  indirect <- moderation$indirect.effect
  total <- moderation$total.effect
  labels <- format(c(vapply(direct, \(e) sprintf("%s -> %s", e$rhs, e$lhs), ""),
                     sprintf("%s -> %s -> %s", indirect$rhs, indirect$med, indirect$lhs),
                     sprintf("%s -> %s", total$rhs, total$lhs)))
  n.direct <- length(direct)

  ci.level <- if (is.null(res$ci.level)) 0.95 else res$ci.level
  fmt_mod <- function(label, e) {
    out <- sprintf("   %s | %s: B = %5.3f, se = %5.3f, p = %5.3f\n",
                   label, moderator, e$coef, e$se, e$pval)
    if (ci_moderation)
      out <- paste0(out, sprintf("   %s   %s CI = [%5.3f, %5.3f]\n",
                                 strrep(" ", nchar(label)), format_percent(ci.level),
                                 e$lower, e$upper))
    out
  }

  cat("\nDirect moderation effects\n")
  for (i in seq_len(n.direct))
    cat(fmt_mod(labels[i], direct[[i]]))

  cat("\nIndirect moderation effect\n")
  cat(fmt_mod(labels[n.direct + 1], indirect))

  cat("\nTotal moderation effect\n")
  cat(fmt_mod(labels[n.direct + 2], total))
  cat("\n")
  invisible(NULL)
}


#' @rdname rmedsem-methods
#' @export
print.rmedsem <- function(x, digits=3, indent=3, ...){
  check_count(digits, "digits")
  check_count(indent, "indent", min=0)
  print_header(x)
  print_freq_table(x, digits=digits)
  if ("bk" %in% x$med.approach)
    print_bk(x, indent=indent)
  if ("zlc" %in% x$med.approach)
    print_zlc(x, indent=indent)
  print_effectsize(x, digits=digits, indent=indent)
  invisible(x)
}


#' @rdname rmedsem-methods
#' @export
print.rmedsem_blavaan <- function(x, digits=3, indent=3, ...){
  check_count(digits, "digits")
  check_count(indent, "indent", min=0)
  print_header(x)
  cat(sprintf("Prior (regression coefs): %s\n", x$prior$beta))
  print_bayes_table(x, digits=digits)
  print_effectsize(x, digits=digits, indent=indent)
  invisible(x)
}


#' @rdname rmedsem-methods
#' @export
print.rmedsem_modsem <- function(x, digits=3, indent=3, ci_moderation=FALSE, ...){
  check_flag(ci_moderation, "ci_moderation")
  NextMethod()
  if (isTRUE(x$moderation$has.moderator))
    print_moderation(x, ci_moderation=ci_moderation)
  invisible(x)
}
