
#' Participants' reports of whether others would call their field scientific
#'
#' @param filter_func function to filter the data before plotting
#' @param print_plot (logical) set to FALSE to prevent printing the ggplot object
#' @return Invisibly returns the ggplot object
#' @import ggplot2
#' @import dplyr
#' @export
#'
#' @examples
#' suppressWarnings(
#'   fig_field_scientific()
#' )
fig_field_scientific <- function(
  filter_func = function(data, ...) return(data),
  print_plot = TRUE
  ) {

  dat <- MoreyHoekstra2019::christmas_stats_participants %>%
    filter_func()


  dat %>%
    ggplot(aes(x = is_science)) +
    geom_bar(mapping = aes(x = is_science, y = ..prop.., group = 1)) +
    geom_text(stat = "count",
              aes(y = ..prop.., label = ..count.., group = 1),
              hjust = -.2,
              family = pkg_options("ggplot_family")) +
    ylim(0, 1) +
    #coord_cartesian(ylim = c(0,1), expand = FALSE, clip = "off") +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      #panel.background = element_blank(),
      text = element_text(family = pkg_options("ggplot_family"))
    ) +
    geom_hline(yintercept = c(0, 1)) +
    xlab("Field is considered scientific") +
    ylab("Proportion of participants") +
    coord_flip() -> g

  if(print_plot) print(g)

  invisible(g)

}
