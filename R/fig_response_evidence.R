

#' Distribution of "evidence" in the display by response
#'
#' @param filter_func function to filter the data before plotting
#' @return Invisibly returns NULL

#' @import dplyr
#' @importFrom purrr map map_dbl
#'
#' @export
#' @examples
#' fig_response_evidence()
fig_response_evidence <- function(filter_func = function(data, ...) return(data)) {

  dat <- MoreyHoekstra2019::christmas_stats_participants %>%
    filter_func()
  samples <- MoreyHoekstra2019::christmas_stats_samples %>%
    filter(id %in% dat$id)


  w_stats = samples %>%
    mutate(ev0 = ev * sgn) %>%
    split(.$id) %>%
    map( ~ wilcox.test(ev0 ~ type, alternative = "less", data = .)) %>%
    map_dbl("p.value")

  w_stats = w_stats[match(dat$id, names(w_stats))]

  j = dat$response == "jinglies"
  s = dat$response == "sparklies"
  n = dat$response %in% c("same", "no_detect")

  dns = density(qlogis(w_stats[n]), na.rm = TRUE)

  par(cex.axis = .8, cex.lab = 1)

  plot(
    dns,
    ty = 'n',
    xlim = c(-40, 40),
    axes = FALSE,
    ylim = c(-.03, max(dns$y) * 1.0),
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
    qlogis(w_stats[n]),
    jitter(w_stats[n] * 0 - .01, amount = .002),
    cex = .5,
    pch = c(1, 19, 1)[sign(dat$effect_size[n]) + 2],
    col = rgb(0, 0, 0, .5)
  )
  points(
    qlogis(w_stats[j]),
    jitter(w_stats[j] * 0 - .02, amount = .002),
    cex = .5,
    pch = c(22, 1, 19)[sign(dat$effect_size[j]) + 2],
    col = rgb(1, 0, 0, .5)
  )
  points(
    qlogis(w_stats[s]),
    jitter(w_stats[s] * 0 - .03, amount = .002),
    cex = .5,
    pch = c(19, 1, 22)[sign(dat$effect_size[s]) + 2],
    col = rgb(0, 0, 1, .5)
  )

  dns = density(qlogis(w_stats[j]), na.rm = TRUE)
  polygon(c(dns$x, rev(dns$x)),
          c(dns$y, dns$y * 0),
          col = rgb(1, 0, 0, .4),
          border = NA)
  dns = density(qlogis(w_stats[s]), na.rm = TRUE)
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
