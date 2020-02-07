


#' Number of samples per participant for each activity, by effect size
#'
#' @param filter_func function to filter the data before plotting
#' @param print_plot (logical) set to FALSE to prevent printing the ggplot object
#' @return Invisibly returns a list containing the ggplot objects
#' and the result of the robust regression analysis
#' @import dplyr
#' @import ggplot2
#' @importFrom ggpubr ggarrange
#' @importFrom MASS rlm
#'
#' @export
#' @examples
#' suppressWarnings(
#'   fig_sampling_behavior()
#' )
fig_sampling_behavior <- function(
  filter_func = function(data, ...) return(data),
  print_plot = TRUE
  ) {
  dat <- MoreyHoekstra2019::christmas_stats_participants %>%
    filter_func()

  dat %>%
    MASS::rlm(log(n_null) ~ abs(effect_size), data = .) -> rlm_obj_null

  rlm_obj_null %>% coef() %>% `*`(., log10(exp(1))) -> coefs_null_lm

  dat %>%
    mutate(effect_size = abs(effect_size)) %>%
    ggplot(aes(y = n_null, x = effect_size, group = effect_size)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(position = position_jitter(width = .01, height = 0), alpha = .4) +
    geom_abline(
      intercept = coefs_null_lm[1],
      slope = coefs_null_lm[2],
      linetype = 2,
      col = "red"
    ) +
    scale_y_continuous(trans = "log10") +
    ylab("Null samples") +
    xlab("Effect size") +
    theme(text = element_text(family = pkg_options("ggplot_family"))) -> g1

  dat %>%
    MASS::rlm(log(n_expt) ~ abs(effect_size), data = .) -> rlm_obj_expt

  rlm_obj_expt %>% coef() %>% `*`(., log10(exp(1))) -> coefs_expt_lm

  dat %>%
    mutate(effect_size = abs(effect_size)) %>%
    ggplot(aes(y = n_expt, x = effect_size, group = effect_size)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(position = position_jitter(width = .01, height = 0), alpha = .4) +
    geom_abline(
      intercept = coefs_expt_lm[1],
      slope = coefs_expt_lm[2],
      linetype = 2,
      col = "red"
    ) +
    scale_y_continuous(trans = "log10") +
    ylab("Experimental samples") +
    xlab("Effect size") +
    theme(text = element_text(family = pkg_options("ggplot_family"))) -> g2

  g0 <- ggpubr::ggarrange(
    g1,
    g2,
    labels = c("A", "B"),
    label.x = 1,
    label.y = 1,
    hjust = 1,
    ncol = 2,
    nrow = 1
  )

  if(print_plot)
    print(g0)

  invisible(
    list(
      plot_null = g1,
      plot_expt = g2,
      rlm_null = rlm_obj_null,
      rlm_expt = rlm_obj_expt
      )
    )
}
