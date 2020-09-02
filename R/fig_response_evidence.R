

#' Distribution of "evidence" in the display by response
#'
#' @param filter_func function to filter the data before plotting
#' @return Invisibly returns NULL

#' @import dplyr
#' @importFrom purrr map map_dbl
#'
#' @export
#' @examples
#' suppressWarnings(
#'   fig_response_evidence()
#' )
fig_response_evidence <- function(filter_func = function(data, ...) return(data)) {

  dat <- MoreyHoekstra2019::christmas_stats_participants %>%
    filter_func()
  samples <- MoreyHoekstra2019::christmas_stats_samples %>%
    filter(id %in% dat$id)
  
  
samples %>%
    mutate(ev0 = ev * sgn) %>%
    split(.$id) -> data_split
  
  names(data_split) %>%
    map_df(function(id){
      decision = dat$response[dat$id==id]
      alt = switch(as.character(decision),
                   "jinglies" = "greater",
                   "sparklies" = "less",
                   "two.sided"
      )
      d = data_split[[id]]
      # This is annoying seems is necessary due to numerical precision
      # issues with high p values
      w2_g = wilcox.test(ev0 ~ type, alternative = "greater", data = d)
      w2_l = wilcox.test(ev0 ~ type, alternative = "less", data = d)
      w1_g = wilcox.test(ev0 ~ 1, alternative = "greater", data = d %>% filter(type=="expt"))
      w1_l = wilcox.test(ev0 ~ 1, alternative = "less", data = d %>% filter(type=="expt"))
      
      w1_p = min(w1_l$p.value, w1_g$p.value)
      w2_p = min(w2_l$p.value, w2_g$p.value)
      
      w1_winner = ifelse(w1_l$p.value<w1_g$p.value, "sparklies","jinglies")
      w2_winner = ifelse(w2_l$p.value<w2_g$p.value, "sparklies","jinglies")
      
      bind_cols(id = id, two=w2_p, one=w1_p,
                two_winner = w2_winner, 
                one_winner = w1_winner)
    }) %>%
    mutate(lowest = pmin(one,two),
           lowest_winner = case_when(
             one<two ~ one_winner,
             TRUE ~ two_winner
           ),
           qplot = case_when(
             lowest_winner == "jinglies" ~ -qlogis(2*lowest),
             TRUE ~ qlogis(2*lowest)
           )
    ) %>% 
    left_join(dat, by="id") -> w_stats
  
  w_stats %>%
    filter(response %in% c("same", "no_detect")) %>%
    pull(qplot) %>% 
    density() -> dns
  
  j = w_stats$response == "jinglies"
  s = w_stats$response == "sparklies"
  n = w_stats$response %in% c("same", "no_detect")
  
  par(cex.axis = .8, cex.lab = 1)
  
  plot(
    dns,
    ty = 'n',
    xlim = c(-40, 40),
    axes = FALSE,
    ylim = c(-.06, max(dns$y) * 1.0),
    xlab = "Wilcoxon two-sided p value",
    ylab = "",
    main = ""
  )
  polygon(c(dns$x, rev(dns$x)),
          c(dns$y, dns$y * 0),
          col = rgb(0, 0, 0, .4),
          border = NA)
  
  expt = seq(-15, -3, 3)
  x.val = qlogis((10 ^ expt / 2))
  x.val = c(x.val, 0,-rev(x.val))
  expt = c(expt, 0, rev(expt))
  strng = sapply(expt, function(e)
    substitute(10 ^ q, list(q = e)))
  strng[[ceiling(length(strng) / 2)]] = 1
  
  for (i in 1:length(expt))
    axis(1, at = x.val[i], labels = strng[[i]])
  
  points(
    w_stats$qplot[n],
    jitter(-(abs(w_stats$effect_size[n])>.01)*.01 - .01,amount = .001),
    cex = .5,
    pch = c(1, 19, 1)[sign(w_stats$effect_size[n]) + 2],
    col = rgb(0, 0, 0, .5)
  )
  points(
    w_stats$qplot[j],
    jitter(-(w_stats$effect_size[j]>.01)*.01 - .03,amount = .001),
    cex = .5,
    pch = c(8, 1, 19)[sign(w_stats$effect_size[j]) + 2],
    col = rgb(1, 0, 0, .5)
  )
  points(
    w_stats$qplot[s],
    jitter((w_stats$effect_size[s]> -.01)*.01 - .06,amount = .001),
    cex = .5,
    pch = c(19, 1, 8)[sign(w_stats$effect_size[s]) + 2],
    col = rgb(0, 0, 1, .5)
  )
  
  abline(h=-.025, col =rgb(0,0,0,.2), lty = 2)
  abline(h=-.045, col =rgb(0,0,0,.2), lty = 2)
  
  text(par()$usr[1], -.0125, "N", adj=c(-.1,.5), cex = .8, col = "darkgray")
  text(par()$usr[1], -.035, "J", adj=c(-.1,.5), cex = .8, col = "red")
  text(par()$usr[1], -.055, "S", adj=c(-.1,.5), cex = .8, col = "blue")
  
  dns = density(w_stats$qplot[j], na.rm = TRUE)
  polygon(c(dns$x, rev(dns$x)),
          c(dns$y, dns$y * 0),
          col = rgb(1, 0, 0, .4),
          border = NA)
  dns = density(w_stats$qplot[s], na.rm = TRUE)
  polygon(c(dns$x, rev(dns$x)),
          c(dns$y, dns$y * 0),
          col = rgb(0, 0, 1, .4),
          border = NA)
  
  abline(v = qlogis(c(.025, .975)), col = rgb(0,0,0,.8), lty = 2)
  text(
    qlogis(.025),
    par()$usr[4],
    expression(p == 0.05),
    srt = 90,
    col = rgb(0,0,0,.8),
    adj = c(1.1, -.1),
    cex = .7
  )
  text(
    qlogis(.975),
    par()$usr[4],
    expression(p == 0.05),
    srt = 90,
    col = rgb(0,0,0,.8),
    adj = c(1.1, 1.1),
    cex = .7
  )
  
  abline(h = 0, col = rgb(0, 0, 0, .3))
  text(
    par()$usr[1],
    par()$usr[4],
    "Evidence for Sparklies",
    srt = 90,
    adj = c(1.1, 1.1),
    cex = .8
  )
  text(
    par()$usr[2],
    par()$usr[4],
    "Evidence for Jinglies",
    srt = -90,
    adj = c(-0.1, 1.1),
    cex = .8
  )
  mtext(
    "Concluded no detection/same",
    3,
    .1,
    adj = .5,
    cex = .8,
    col = rgb(0,0,0,.8)
  )
  text(
    par()$usr[1],
    par()$usr[4] * .1,
    "Concluded Sparklies",
    adj = 0,
    cex = .8,
    col = "blue"
  )
  text(
    par()$usr[2],
    par()$usr[4] * .1,
    "Concluded Jinglies",
    adj = 1,
    cex = .8,
    col = "red"
  )
  
  
  invisible(NULL)

}
