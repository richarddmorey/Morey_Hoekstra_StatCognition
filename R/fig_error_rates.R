

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
#' fig_error_rates()
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
      size = 3
    ) +
    annotate(
      "text",
      x = .5,
      y = 1.15,
      label = "(Number of participants)",
      size = 3
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
      text = element_text()
    ) -> g


  if(print_plot) print(g)

  invisible(g)

}
