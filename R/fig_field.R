

#' Distribution of self-reported scientific field in the sample
#'
#' @param filter_func function to filter the data before plotting
#' @param print_plot (logical) set to FALSE to prevent printing the ggplot object
#' @return Invisibly returns the ggplot object
#' @import dplyr
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' fig_field()
fig_field <- function(
  filter_func = function(data, ...) return(data),
  print_plot = TRUE
  ) {

  dat <- MoreyHoekstra2019::christmas_stats_participants %>%
    filter_func()

  dat %>%
    select(starts_with("field."), -matches("wordcount") ) %>%
    mutate(field.other = !is.na(field.other)) %>%
    mutate("field.no response" = rowSums(.) == 0) %>%
    summarise_all(mean) %>%
    unlist() %>% sort() -> summaries

  tibble(
    Field = substring(names(summaries), 7),
    count = summaries * nrow(dat),
    prop = summaries,
    "No response" = factor(names(summaries) == "field.no response")
  ) %>%
    mutate(order = case_when(`No response` == TRUE ~ -Inf,
                             TRUE ~ prop)) %>% ggplot(aes(
                               x = reorder(Field, order),
                               y = prop,
                               fill = `No response`
                             )) +
    geom_bar(stat = "identity") +
    xlab("Field") + ylab("Proportion of participants") +
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
