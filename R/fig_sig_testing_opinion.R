

#' Distribution of opinions about significance testing in sample
#'
#' @param filter_func function to filter the data before plotting
#' @param print_plot (logical) set to FALSE to prevent printing the ggplot object
#' @return Invisibly returns the ggplot object
#' @import dplyr
#' @import ggplot2
#'
#' @export
#' @examples
#' suppressWarnings(
#'   fig_sig_testing_opinion()
#' )
fig_sig_testing_opinion <- function(
  filter_func = function(data, ...) return(data),
  print_plot = TRUE
  ){

  dat <- MoreyHoekstra2019::christmas_stats_participants %>%
    filter_func()

  dat %>%
    select(starts_with("sig_testing."), -matches("wordcount")) %>%
    mutate(sig_testing.other = !is.na(sig_testing.other)) -> sig_cols

  colnames(sig_cols) = substring(colnames(sig_cols), 13)

  sig_cols %>%
    mutate("no response" = rowSums(.) == 0) %>%
    summarise_all(mean) %>%
    unlist() %>% sort() -> summaries

  tibble(
    Opinion = names(summaries),
    count = summaries * nrow(dat),
    prop = summaries,
    "No response" = factor(names(summaries) == "no response")
  ) %>%
    mutate(order = case_when(`No response` == TRUE ~ -Inf,
                             TRUE ~ prop)) %>% ggplot(aes(
                               x = reorder(Opinion, order),
                               y = prop,
                               fill = `No response`
                             )) +
    geom_bar(stat = "identity") +
    xlab("Opinion about significance testing") + ylab("Proportion of participants") +
    ylim(0, 1) +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(family = pkg_options("ggplot_family"))) +
    geom_hline(yintercept = c(0, 1)) +
    geom_text(
      stat = "identity",
      aes(
        y = summaries,
        label = summaries * nrow(dat),
        group = 1
      ),
      hjust = -.2,
      family = pkg_options("ggplot_family")
    ) +
    coord_flip() -> g

  if(print_plot)
    print(g)

  invisible(g)

}
