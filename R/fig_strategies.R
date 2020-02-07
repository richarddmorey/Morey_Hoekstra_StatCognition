

#' Frequencies of self-reported strategies
#'
#' Coded responses to the three open-ended text questions
#' about participants' strategies.
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
#' suppressWarnings(
#'   fig_strategies()
#' )
fig_strategies <- function(
  filter_func = function(data, ...) return(data),
  print_plot = TRUE
  ){

  dat <- MoreyHoekstra2019::christmas_stats_participants %>%
    filter_func()

  dat %>%
    mutate(text_strong = text_comparison | text_sampling_var,
                  text_only_weak = (text_asymmetry | text_inc_asymmetry ) & !text_strong,
                  text_none = !(text_strong | text_only_weak),
                  text_missing = text_missing | text_irrelevant) %>%
    mutate(text_response_type = case_when(
      text_strong ~ "Strong",
      text_only_weak ~ "Only weak",
      text_none ~ "None",
      TRUE ~ "."
    ),
    text_response_subtype = case_when(
      text_no_shuffle ~ "...denies using shuffles at all",
      text_missing ~ "...is missing/irrelevant",
      TRUE ~ "other"
    ),
    text_response_type = factor(text_response_type, ordered = TRUE),
    text_response_subtype = factor(text_response_subtype,
                                   levels = c("...is missing/irrelevant","...denies using shuffles at all","other"),
                                   ordered = TRUE)
    ) %>%
    count(text_response_type,
          text_response_subtype,
          .drop = FALSE) -> for_barplot


  for_barplot %>%
    rename(`Response...` = text_response_subtype) %>%
    ggplot(aes(fill=`Response...`, y=n, x=text_response_type)) +
    geom_bar( stat="identity", color="black") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 400)) +
    coord_flip() +
    theme_classic() +
    theme(legend.position = "top",
          text = element_text(size=12, family = pkg_options("ggplot_family")),
          plot.margin = unit(c(0,1,0,0), "cm")) +
    xlab("Sig. test content in text") -> g

    if(print_plot)
      print(g)

  invisible(g)

}
