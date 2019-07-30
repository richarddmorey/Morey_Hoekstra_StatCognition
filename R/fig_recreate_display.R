



#' Generate image to show a participant's sampling behaviour
#'
#' @param id ID of participant whose samples to show
#' @param type String indicating type of samples ("expt" for experiments; "null" for random shuffle reports)
#' @param ps p values to overlay on the display
#'
#' @return Returns a file path to the generated PNG file
#' @import dplyr
#' @importFrom magrittr %<>%
#' @importFrom png readPNG
#' @importFrom showtext showtext_begin showtext_end
#' @importFrom grDevices dev.off png rgb
#'
#' @export
#' @examples
#' fig_recreate_display(id = "R_7V3ufsG6NYL27Ul")
fig_recreate_display <- function(id,
                                 type = "expt",
                                 ps =
                                   c(.005, .025, .05, .125, .25, .5, .75, .875, .95, .975, .995),
                                 letter = "") {
  tf = tempfile(fileext = ".png")

  dat <- MoreyHoekstra2019::christmas_stats_participants
  samples <- MoreyHoekstra2019::christmas_stats_samples

  q <- dat %>% filter(id == !!id) %>% select( evidence_power )
  q <- q[1,1]

  max_n_idx <- max(samples$n_idx)
  samples %<>%
    filter(id == !!id, type == !!type) %>%
    mutate(ev = sgn * ev)

  # ima is a global variable
  img_dim = c(dim(ima)[2], dim(ima)[1])
  res = 144

  lower_left = c(239, img_dim[2] - 929)
  lower_right = c(2028, lower_left[2])
  upper_left = c(lower_left[1], img_dim[2] - 36)
  plot_width = lower_right[1] - lower_left[1]
  plot_height = upper_left[2] - lower_left[2]


  if (length(ps)) {
    evs <- q.ev(ps, d = 0, n = 10, q = q)
    locs <- plot_width / 2 * evs + lower_left[1] + plot_width / 2
    locs0 <- c(lower_left[1], locs, lower_right[1])

    cols = rev(viridis::plasma(ceiling(length(locs) / 2), alpha = .05))
  }

  png(
    filename = tf,
    width = img_dim[1],
    height = img_dim[2],
    units = "px",
    res = res
  )

  showtext::showtext_begin()

  #Set up the plot area
  par(mar = c(0, 0, 0, 0),
      pin = img_dim / res)
  plot(
    0,
    0,
    type = 'n',
    main = "",
    xlab = "",
    ylab = "",
    axes = FALSE,
    xaxs = "i",
    yaxs = "i",
    xlim = c(0, img_dim[1]),
    ylim = c(0, img_dim[2])
  )

  rasterImage(ima, par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4])

  # plot sampled points
  points(
    samples$ev * plot_width/2 + lower_left[1] + plot_width / 2,
    (samples$n_idx + 1) / (max_n_idx + 2) * plot_height + lower_left[2],
    pch = 21,
    col = rgb(0, 0, 0, .5),
    bg = rgb(1, 0, 0, abs(samples$ev)),
    cex = 2.25
  )

  if (length(ps)) {
    segments(locs, lower_left[2], locs, upper_left[2],
             col = rgb(0,0,0,.4))
    text(
      locs,
      locs * 0 + upper_left[2],
      2 * ifelse(ps > .5, 1 - ps, ps),
      srt = 90,
      adj = c(1.1, 1.1),
      cex = 2,
      col = rgb(0, 0, 0, .2)
    )
    segments(lower_left[1] + plot_width / 2,
             lower_left[2],
             lower_left[1] + plot_width / 2,
             upper_left[2])
  } #end p value content
  mtext(letter, 1,-2, adj = .01, cex = 4)

  showtext::showtext_end()

  dev.off()

  return(tf)
}
