#' Function for plotting the effect size for fitted `rmedsem` objects.
#'
#' @param res the `rmedsem` object to print
#' @return `ggplot` object of the plot
#' @export
#'
plot_effect <- function(res, description=TRUE){
  es <- res$effect.size
  effs <- data.frame(
    eff=c("indirect", "direct"), #"total"),
    value=c(es$RIT$ind_eff, es$RID$dir_eff) #, es$RIT$tot_eff)
  )

  effs |>
    dplyr::arrange(dplyr::desc(eff)) |>
    dplyr::mutate(ypos = cumsum(value) - 0.5*value ) -> effs

  descr.label <- ""
  if(description){
    descr.label <- sprintf("Total effect=%.3f\nThat means %.1f%% of the total effect of '%s' on '%s' is mediated by '%s'.",
                           es$RIT$tot_eff, 100*es$RIT$ind_eff/es$RIT$tot_eff,
                           res$vars$indep, res$vars$dep, res$vars$med)
  }

  ggplot2::ggplot(effs, ggplot2::aes(x="", y=value, fill=eff)) +
    ggplot2::geom_bar(stat="identity", width=1, color="white") +
    ggplot2::coord_polar("y", start=0)+
    ggplot2::theme_void()+
    ggplot2::guides(fill="none")+
    ggplot2::geom_text(ggplot2::aes(y = ypos, label = sprintf("%.3f\n%s",value,eff)),
                       color = "white", size=6)+
    ggplot2::scale_fill_brewer(palette="Set1")+
    ggplot2::labs(title=sprintf("Effect sizes for '%s' on '%s' via '%s'",
                                res$vars$indep, res$vars$dep, res$vars$med),
                  subtitle=sprintf("Estimation: %s", res$package),
                    caption=descr.label)+
    ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))
}

#' Function for plotting the coefficients for fitted `rmedsem` objects.
#'
#' @param res the `rmedsem` object to print
#' @return `ggplot` object of the plot
#' @export
#'
plot_coef <- function(res){
  purrr::map_dfr(res$est.methods, \(method){
    data.frame(method=method, effect="indirect", res[[method]][c("coef","lower","upper")] |> t())
  }) -> d
  d <- rbind(d, data.frame(method=NA, effect="direct", res$direct.effect[c("coef","lower","upper")] |> t()))
  d <- rbind(d, data.frame(method=NA, effect="total", res$total.effect[c("coef","lower","upper")] |> t()))
  ymet <- as.numeric(factor(d$method))
  ymet[is.na(ymet)] <- 0
  d |> dplyr::mutate(var = ifelse(is.na(method), effect, sprintf("%s - %s", effect, method))) |>
    dplyr::mutate(effect=ordered(effect, levels=c("total", "direct", "indirect")),
                  ypos=as.numeric(effect)+(ymet>0)*0.1*(ymet-ceiling(length(res$est.methods)/2))) ->d

  ggplot2::ggplot(d, ggplot2::aes(x=ypos, y=coef, ymin=lower, ymax=upper, color=method))+
    ggplot2::geom_pointrange(position=ggplot2::position_dodge(width=0.5))+
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
                   panel.background = ggplot2::element_blank())

}
