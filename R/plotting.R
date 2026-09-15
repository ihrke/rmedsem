#' @rdname plot.rmedsem
#' @export
plot_effect <- function(res, description=TRUE){
  if (!inherits(res, "rmedsem"))
    stop("'res' must be an 'rmedsem' object.")
  check_flag(description, "description")
  if (is.null(res$effect.size$RIT) || is.null(res$effect.size$RID))
    stop("Both RIT and RID effect sizes are required. Re-run rmedsem() with effect.size = c('RIT', 'RID').")
  es <- res$effect.size
  if(es$RIT$tot_eff<es$RIT$ind_eff){
    warning("Total effect is smaller than indirect effect!\nEffect sizes should not be interpreted.")
  }
  effs <- data.frame(
    eff=c("indirect", "direct"), #"total"),
    value=c(es$RIT$ind_eff, es$RID$dir_eff) #, es$RIT$tot_eff)
  )

  effs <- effs[order(effs$eff, decreasing=TRUE), ]
  effs$ypos <- cumsum(effs$value) - 0.5*effs$value

  # wrap long texts so that they fit into small figures
  wrap <- function(txt, width) paste(strwrap(txt, width=width), collapse="\n")
  descr.label <- ""
  if(description){
    problem <- effect_size_problem(res, "RIT")
    descr.label <- wrap(
      if (is.null(problem))
        sprintf("Total effect = %.3f. That means %.1f%% of the total effect of '%s' on '%s' is mediated by '%s'.",
                es$RIT$tot_eff, 100*es$RIT$ind_eff/es$RIT$tot_eff,
                res$vars$indep, res$vars$dep, res$vars$med)
      else
        sprintf("Total effect = %.3f. The proportion mediated should not be interpreted: %s.",
                es$RIT$tot_eff, problem),
      width=45)
  }

  ggplot2::ggplot(effs, ggplot2::aes(x="", y=value, fill=eff)) +
    ggplot2::geom_bar(stat="identity", width=1, color="white") +
    ggplot2::coord_polar("y", start=0)+
    ggplot2::theme_void()+
    ggplot2::guides(fill="none")+
    ggplot2::geom_text(ggplot2::aes(y = ypos, label = sprintf("%.3f\n%s",value,eff)),
                       color = "white", size=4.5)+
    ggplot2::scale_fill_brewer(palette="Set1")+
    ggplot2::labs(title=wrap(sprintf("Effect sizes for '%s' on '%s' via '%s'",
                                     res$vars$indep, res$vars$dep, res$vars$med),
                             width=35),
                  subtitle=sprintf("Estimation: %s", res$package),
                    caption=descr.label)+
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
}

#' @rdname plot.rmedsem
#' @export
plot_coef <- function(res){
  if (!inherits(res, "rmedsem"))
    stop("'res' must be an 'rmedsem' object.")
  row <- function(method, effect, v)
    data.frame(method=method, effect=effect, coef=unname(v[["coef"]]),
               lower=unname(v[["lower"]]), upper=unname(v[["upper"]]))
  d <- do.call(rbind, c(
    lapply(res$est.methods, \(m) row(m, "indirect", res[[m]])),
    list(row(NA, "direct", res$direct.effect), row(NA, "total", res$total.effect))))
  ymet <- as.numeric(factor(d$method))
  ymet[is.na(ymet)] <- 0
  d$var <- ifelse(is.na(d$method), d$effect,
                  sprintf("%s (%s)", d$effect, method_label(d$method)))
  d$effect <- ordered(d$effect, levels=c("total", "direct", "indirect"))
  # indirect effects of the different methods are spread around their position
  d$ypos <- as.numeric(d$effect) +
    (ymet>0)*0.25*(ymet-ceiling(length(res$est.methods)/2))

  ggplot2::ggplot(d, ggplot2::aes(x=ypos, y=coef, ymin=lower, ymax=upper, color=method))+
    ggplot2::geom_pointrange()+
    ggplot2::geom_hline(yintercept=0, linetype="dashed")+
    ggplot2::coord_flip()+
    ggplot2::theme_bw()+
    ggplot2::guides(color="none")+
    ggplot2::scale_x_continuous(breaks=d$ypos, labels=d$var)+
    ggplot2::labs(title="Coefficient plot",
                  subtitle=sprintf("Estimation: %s", res$package),
                  x="Method", y="Coefficient")+
    ggplot2::scale_fill_brewer(palette="Set1")+
    # turn off grid
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = ggplot2::rel(1.1)))

}
