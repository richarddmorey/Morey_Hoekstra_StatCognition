

#' Distribution of self-reported ways participants use statistics
#'
#' @param filter_func function to filter the data before plotting
#' @param print_plot (logical) set to FALSE to prevent printing the ggplot object
#' @return Invisibly returns the ggplot object
#' @import dplyr
#' @import ggplot2
#'
#' @export
#' @examples
#' fig_how_use_stat()
fig_how_use_stat <- function(
  filter_func = function(data, ...) return(data),
  print_plot = TRUE
  ) {

  dat <- MoreyHoekstra2019::christmas_stats_participants %>%
    filter_func()


  dat %>%
    select(starts_with("how_use."), -matches("wordcount") ) %>%
    mutate(how_use.other = !is.na(how_use.other)) -> use_cols

  colnames(use_cols) = substring(colnames(use_cols), 9)

  use_cols %>%
    mutate("no response" = rowSums(.) == 0) %>%
    summarise_all(mean) %>%
    unlist() %>% sort() -> summaries

  tibble(
    How = names(summaries),
    count = summaries * nrow(dat),
    prop = summaries,
    "No response" = factor(names(summaries) == "no response")
  ) %>%
    mutate(order = case_when(`No response` == TRUE ~ -Inf,
                             TRUE ~ prop)) %>% ggplot(aes(
                               x = reorder(How, order),
                               y = prop,
                               fill = `No response`
                             )) +
    geom_bar(stat = "identity") +
    xlab("Reported use of statistics in position") + ylab("Proportion of participants") +
    ylim(0, 1) +
    theme_bw() +
    theme(legend.position = "none") +
    geom_hline(yintercept = c(0, 1)) +
    geom_text(
      stat = "identity",
      aes(
        y = summaries,
        label = summaries * nrow(dat),
        group = 1
      ),
      hjust = -.2
    ) +
    coord_flip() -> g

  if(print_plot) print(g)

  invisible(g)

}
