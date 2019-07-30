

#' Distribution of attained educational level in the sample
#'
#' @param filter_func function to filter the data before plotting
#' @param print_plot (logical) set to FALSE to prevent printing the ggplot object
#' @return Invisibly returns the ggplot object
#' @import ggplot2
#' @import dplyr
#' @export
#'
#' @examples
#' fig_edu_training_edu()
fig_edu_training_edu <- function(filter_func = function(data, ...) return(data),
                                 print_plot = TRUE) {

  dat <- MoreyHoekstra2019::christmas_stats_participants %>%
    filter_func()

  dat %>%
    ggplot(aes(x = education)) +
    geom_bar(mapping = aes(x = education, y = ..prop.., group = 1)) +
    geom_text(stat = "count",
              aes(y = ..prop.., label = ..count.., group = 1),
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
    xlab("Highest education") +
    ylab("Proportion of participants") +
    coord_flip() -> g

  if(print_plot){
    print(g)
  }

  invisible(g)

}

#' Distribution of reported number of years of statistical training in the sample
#'
#' @param filter_func function to filter the data before plotting
#' @param print_plot (logical) set to FALSE to prevent printing the ggplot object
#' @return Invisibly returns the ggplot object
#' @import ggplot2
#' @import dplyr
#' @export
#'
#' @examples
#' fig_edu_training_stats()
fig_edu_training_stats <- function(
  filter_func = function(data, ...) return(data),
  print_plot = TRUE
) {

  dat <- MoreyHoekstra2019::christmas_stats_participants %>%
    filter_func()

  dat %>%
    summarise(missing = round(100 * mean(is.na(formal_training)), 1)) %>%
    unlist() %>% paste0("No response: ", . , "%") -> missing

  dat %>% ggplot(aes(x = formal_training)) +
    geom_histogram(binwidth = 1) +
    annotate(
      "text",
      Inf,
      Inf,
      label = missing,
      hjust = 1.1,
      vjust = 1.5,
      size = 5
    ) +
    xlab("Self-reported years of statistical training") +
    ylab("Frequency") +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      #panel.background = element_blank(),
      text = element_text()
    ) +
    geom_hline(yintercept = 0) -> g

  if(print_plot) print(g)

  invisible(g)

}
