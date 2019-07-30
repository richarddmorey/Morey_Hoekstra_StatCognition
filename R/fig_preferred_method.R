

#' Distribution of self-reported preferences for statistical methods
#'
#' @param filter_func function to filter the data before plotting
#' @param print_plot (logical) set to FALSE to prevent printing the ggplot object
#' @return Invisibly returns the ggplot object
#' @import dplyr
#' @import ggplot2
#'
#' @export
#' @examples
#' fig_preferred_method()
fig_preferred_method <- function(
  filter_func = function(data, ...) return(data),
  print_plot = TRUE
  ) {

  dat <- MoreyHoekstra2019::christmas_stats_participants %>%
    filter_func()

  dat %>%
    group_by(preferred) %>%
    summarise(n = n()) %>%
    mutate(total = sum(n),
           p = n / total) %>%
    ggplot(aes(x = preferred)) +
    geom_bar(stat = "identity",
             mapping = aes(
               x = reorder(preferred, p),
               y = p,
               group = 1
             )) +
    geom_text(stat = "identity",
              aes(y = p, label = n, group = 1),
              hjust = -.2) +
    ylim(0, 1) +
    #coord_cartesian(ylim = c(0,1), expand = FALSE, clip = "off") +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      #panel.background = element_blank(),
      text = element_text()
    ) +
    geom_hline(yintercept = c(0, 1)) +
    xlab("Preferred statistical method") +
    ylab("Proportion of participants") +
    coord_flip() -> g

  if(print_plot)
    print(g)

  invisible(g)

}
