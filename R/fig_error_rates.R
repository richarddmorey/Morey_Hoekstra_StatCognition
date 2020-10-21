

#' Error/correct response rates as a function of effect size
#'
#' @param filter_func function to filter the data before plotting
#' @param print_plot (logical) set to FALSE to prevent printing the ggplot object
#' @return Invisibly returns the ggplot object
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr complete
#' @export
#'
#' @examples
#' suppressWarnings(
#'   fig_error_rates()
#' )
fig_error_rates <- function(
  filter_func = function(data, ...) return(data),
  print_plot = TRUE
) {

  dat <- MoreyHoekstra2019::christmas_stats_participants %>%
    filter_func()

  summaries <-
    dat %>%
    mutate(effect_size = abs(effect_size)) %>%
    group_by(effect_size, true_winner, response) %>%
    tally() %>%
    complete(effect_size, true_winner, response, fill = list(n = 0)) %>%
    ungroup() %>%
    filter(response != "bored") %>%
    mutate(
      dec_type = case_when(
        true_winner != "null" &
          true_winner != response &
          response %in% c("jinglies", "sparklies") ~ "sign_error",
        response %in% c("jinglies", "sparklies") ~ "report_difference",
        response == "same" ~ "same",
        response == "no_detect" ~ "no_detect"
      )
    ) %>%
    group_by(dec_type, effect_size) %>%
    tally( wt = n ) %>%
    complete(dec_type, effect_size, fill = list(n = 0)) %>%
    group_by(effect_size) %>%
    mutate(total = sum(n))

  summaries_all_null <-
    summaries %>%
    mutate(dec_type = ifelse(dec_type %in% c("same", "no_detect"), "null", dec_type)) %>%
    filter(dec_type == "null") %>%
    group_by(dec_type, effect_size) %>%
    summarize(n = sum(n), total = first(total))

  rbind(summaries, summaries_all_null) %>%
    mutate(
      p = n / total,
      stderr = sqrt(p * (1 - p) / total),
      lo = qlogis(p),
      lo.stderr = stderr / dlogis(qlogis(p))
    ) -> summaries

  summaries %>%
    filter(dec_type != "same") %>%
    mutate(
      stderr = ifelse(dec_type == "report_difference", stderr, NA),
      dec_type = case_when(
        dec_type == "no_detect" ~ "No detection",
        dec_type == "null" ~ "No detection / same",
        dec_type == "report_difference" ~ "Difference",
        dec_type == "sign_error" ~ "Sign error"
      ),
      Decision = dec_type,
    ) %>%
    ggplot(aes(x = effect_size, y = p, group = Decision)) +
    geom_rect(
      xmin = 0,
      xmax = 1,
      ymin = 0,
      ymax = 1,
      color = NA,
      fill = "#EEEEEE",
      alpha = .1
    ) +
    geom_rect(
      xmin = 0,
      xmax = 1,
      ymin = .8,
      ymax = 1,
      color = NA,
      fill = "#DDDDDD",
      alpha = .1
    ) +
    geom_rect(
      xmin = 0,
      xmax = 1,
      ymin = 0,
      ymax = .2,
      color = NA,
      fill = "#DDDDDD",
      alpha = .1
    ) +
    geom_rect(
      xmin = 0,
      xmax = 1,
      ymin = .95,
      ymax = 1,
      color = NA,
      fill = "#CCCCCC",
      alpha = .1
    ) +
    geom_rect(
      xmin = 0,
      xmax = 1,
      ymin = 0,
      ymax = .05,
      color = NA,
      fill = "#CCCCCC",
      alpha = .1
    ) +
    geom_segment(aes(
      x = 0,
      y = 0,
      xend = 0,
      yend = 1
    )) +
    geom_hline(yintercept = c(0, 1)) +
    geom_line(aes(linetype = Decision, col = Decision), size = .7) +
    geom_point(aes(col = Decision)) +
    geom_point(aes(x = effect_size, y = 1)) +
    annotate(
      "text",
      x = summaries_all_null$effect_size,
      y = 1.06,
      label = paste0("(", summaries_all_null$total, ")"),
      size = 3,
      family = pkg_options("ggplot_family")
    ) +
    annotate(
      "text",
      x = .5,
      y = 1.15,
      label = "(Number of participants)",
      size = 3,
      family = pkg_options("ggplot_family")
    ) +
    geom_ribbon(aes(
      ymin = p - stderr,
      ymax = p + stderr,
      fill = Decision
    ),
    alpha = 0.2) +
    scale_linetype_manual(values = c(1, 2, 2, 3)) +
    scale_color_manual(values = c("#FF0000", "#888888", "#000000", "#FF33AA")) +
    scale_fill_manual(values = c("#FF0000", "#FFFFFF00", "#FFFFFF00", "#FFFFFF00")) +
    xlab("Effect size") + ylab("Proportion of responses") +
    coord_cartesian(
      ylim = c(0, 1.2),
      xlim = c(0, 1),
      expand = FALSE,
      clip = "off"
    ) +
    scale_y_continuous(breaks = seq(0, 1, .1)) +
    scale_x_continuous(breaks = seq(0, 1, .2)) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      text = element_text(family = pkg_options("ggplot_family"))
    ) -> g


  if(print_plot) print(g)

  invisible(g)

}

#' Error/correct response rates as a function of effect size, with model fits
#'
#' @param filter_func function to filter the data before plotting
#' @param print_plot (logical) set to FALSE to prevent printing the ggplot object
#' @return Invisibly returns the ggplot object
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr complete pivot_longer
#' @export
#'
#' @examples
#' suppressWarnings(
#'   fig_error_rates_dprime()
#' )
fig_error_rates_dprime <- function(
  filter_func = function(data, ...) return(data),
  print_plot = TRUE
) {
  
  MoreyHoekstra2019::christmas_stats_participants %>%
    filter_func() %>%
    mutate(
      response = as.character(response),
      response = case_when(
        response %in% c("same","no_detect") ~ "no detect/same",
        TRUE ~ response
      )
    ) %>%
    group_by(effect_size, response) %>%
    summarise(n = n()) -> freqs0
  
  freqs0 %>%
    pivot_wider(names_from = "response", values_from = "n") %>%
    replace_na(list("no detect/same"=0, "sparklies"=0, "jinglies"=0)) -> freqs
  
  
  sd_results = sigdet_model_fit(freqs)
  
  mu0 = sd_results$mu0
  dp  = sd_results$dp
  probs  = sd_results$probs
  
  probs %>%
    `[`(,c(2,1,3)) %>%
    data.frame() %>%
    `colnames<-`(colnames(freqs)[-1]) %>%
    mutate(effect_size = !!freqs$effect_size) %>%
    pivot_longer(cols=1:3,names_to = "response", values_to = "predicted") %>%
    left_join(freqs0, by = c("effect_size", "response")) %>%
    complete(n, fill = list(n=0)) %>%
    arrange(effect_size) %>%
    group_by(effect_size) %>%
    mutate(n_total = sum(n),
           prob_obs = n / n_total) %>%
    mutate(
      pred_lo = qbinom(pnorm(-1), n_total, predicted)/n_total,
      pred_up = qbinom(pnorm(1), n_total, predicted)/n_total
    ) -> error_rates
  
  error_rates %>%
    mutate(response = factor(tools::toTitleCase(response),
                             levels = c("Sparklies","No Detect/Same", "Jinglies"),
                             ordered = TRUE)) %>%
    ggplot(aes(x=effect_size,y=predicted,group=response,color=response,linetype=response,shape=response)) +
    geom_segment(aes(x=effect_size,xend=effect_size), y=0, yend=1, color = "gray", alpha = .5, size = .2) +
    geom_line() +
    geom_ribbon(aes(ymin=pred_lo,ymax=pred_up,fill=response), alpha=.3, linetype=0) +
    scale_x_continuous(limits=c(-1,1),expand=c(0,0), name = "Effect size", breaks = round(unique(error_rates$effect_size),2)) +
    scale_y_continuous(limits=c(0,1.25),expand=c(0,0), name = "Probability", breaks = seq(0,1,.2)) +
    scale_color_manual(name="Response", values = c("#0000FF", "#999999", "#FF0000")) +
    scale_fill_manual(name="Response",values = c("#0000FF", "#999999", "#FF0000")) +
    scale_linetype_manual(name="Response", values=c("dashed","solid","dotted")) +
    scale_shape_manual(name="Response", values=c(15,19,17)) +
    geom_point(aes(y = prob_obs)) +
    coord_cartesian(clip="off") +
    annotate(
      "text",
      x = freqs$effect_size,
      y = 1.06,
      label = paste0("(", rowSums(freqs[,2:4]), ")"),
      size = 3,
      angle = 90,
      family = MoreyHoekstra2019::pkg_options("ggplot_family")
    ) +
    annotate(
      "text",
      x = 0,
      y = 1.20,
      label = "(Number of participants)",
      size = 3,
      family = MoreyHoekstra2019::pkg_options("ggplot_family")
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      text = element_text(family =MoreyHoekstra2019::pkg_options("ggplot_family")),
      legend.position = "top",
      legend.key.width = unit(1, "cm")
    ) -> g
  
  
  if(print_plot) print(g)
  
  invisible(g)
  
}

#' d prime as a function of effect size (model fits)
#'
#' @param filter_func function to filter the data before plotting
#' @param print_plot (logical) set to FALSE to prevent printing the ggplot object
#' @return Invisibly returns the ggplot object
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr complete pivot_longer
#' @export
#'
#' @examples
#' suppressWarnings(
#'   fig_dprime_effect_size()
#' )
fig_dprime_effect_size <- function(
  filter_func = function(data, ...) return(data),
  print_plot = TRUE
) {
  
  MoreyHoekstra2019::christmas_stats_participants %>%
    filter_func() %>%
    mutate(
      response = as.character(response),
      response = case_when(
        response %in% c("same","no_detect") ~ "no detect/same",
        TRUE ~ response
      )
    ) %>%
    group_by(effect_size, response) %>%
    summarise(n = n()) -> freqs0
  
  freqs0 %>%
    pivot_wider(names_from = "response", values_from = "n") %>%
    replace_na(list("no detect/same"=0, "sparklies"=0, "jinglies"=0)) -> freqs
  
  sd_results = sigdet_model_fit(freqs)
  
  mu0 = sd_results$mu0
  dp  = sd_results$dp
  
  tibble(dprime = dp, effect_size = freqs$effect_size) %>%
    ggplot(aes(x = effect_size, y = dp)) +
    annotate("rect",xmin=-1,xmax=0,ymin=-Inf,ymax=Inf,
             fill="blue", alpha = .2, linetype=0) +
    annotate("rect",xmin=1,xmax=0,ymin=-Inf,ymax=Inf,
             fill="red", alpha = .2, linetype=0) +
    annotate("text", x = -1, y = 0, angle = 90, color = "blue", label="Sparklies", vjust=1.1, size = 5,
             family = MoreyHoekstra2019::pkg_options("ggplot_family")
    ) +
    annotate("text", x = 1, y = 0, angle = -90, color = "red", label="Jinglies", vjust=1.1, size = 5,
             family = MoreyHoekstra2019::pkg_options("ggplot_family")
    ) +
    geom_line() +
    geom_point(size = 2) +
    scale_y_continuous(name = "Estimated d'", expand = c(0,0)) +
    scale_x_continuous(name = "Effect size", expand = c(0,0)) +
    coord_cartesian(clip="off") +
    geom_point(x=0,y=0,size=2, shape = 21, color = "black", fill = "white") +
    theme_minimal() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_blank(),
      text = element_text(family = MoreyHoekstra2019::pkg_options("ggplot_family"))
    ) -> g
  
  
  if(print_plot) print(g)
  
  invisible(g)
  
}

#' Fit signal detection model to data
#'
#' @param freqs summary data
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr pivot_wider replace_na
#' 
#' @return
sigdet_model_fit <- function(freqs){
  
  loglike = function(pars, d, return_probs = FALSE){
    
    mu0 = cumsum(exp(pars[1:7]))
    mu     = c(-rev(mu0), 0, mu0)
    crit1  = pars[8]
    crit_d = exp(pars[9])
    crits  = c(-Inf, crit1, crit1 + crit_d, Inf)
    
    Y = as.matrix(freqs[,-1])[,c(2,1,3)]
    
    pr = t(apply(outer(mu, crits, function(m, c) pnorm(c,m)),
                 1, diff))
    if(return_probs) return(pr)
    
    -sum(Y * log(pr))
  }
  
  pars_init = c(rep(-.5,7),-1,1)
  
  g = optim(pars_init, loglike, d=freqs)
  mu0 = cumsum(exp(g$par[1:7]))
  dp     = c(-rev(mu0), 0, mu0)
  
  probs = loglike(g$par, freqs, return_probs = TRUE)
  
  return(list(
    g = g,
    mu0 = mu0,
    dp = dp,
    probs = probs
  ))
  
}



