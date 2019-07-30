
# load it so everything can use it
ima <- png::readPNG(system.file("pkg_html/img/clean_interface.png",
                                package = "MoreyHoekstra2019"))

#' Generate image to show the interface with p values overlaid
#'
#' @param q Evidence scale/power
#' @param ps vector of p values to plot
#' @param letter label to add to plot
#'
#' @return Returns a file path to the generated PNG file
#' @importFrom viridis magma
#' @importFrom png readPNG
#' @importFrom showtext showtext_begin showtext_end
#'
#' @export
#'
#' @examples
#' fig_evidence_p_vals(q = 7)
fig_evidence_p_vals <- function(q,
                                ps = c(.005, .025, .05, .125, .25, .5, .75, .875, .95, .975, .995),
                                letter = ""){

  tf = tempfile(fileext = ".png")

  # ima is a global variable
  img_dim = c(dim(ima)[2], dim(ima)[1])
  res = 144

  lower_left = c(239, img_dim[2] - 929)
  lower_right = c(2028, lower_left[2])
  upper_left = c(lower_left[1], img_dim[2] - 36)
  plot_width = lower_right[1] - lower_left[1]
  plot_height = upper_left[2] - lower_left[2]


  evs <- q.ev(ps, d = 0, n = 10, q = q)
  locs <- plot_width / 2 * evs + lower_left[1] + plot_width / 2
  locs0 <- c(lower_left[1], locs, lower_right[1])

  cols = rev(viridis::plasma(ceiling(length(locs) / 2), alpha = .05))

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
  for (i in 1:ceiling(length(locs) / 2)) {
    rect(
      locs0[i],
      locs * 0 + lower_left[2],
      locs0[i + 1],
      locs * 0 + upper_left[2],
      col = cols[i],
      border = NA
    )
    rect(
      locs0[length(locs) - i + 3],
      locs * 0 + lower_left[2],
      locs0[length(locs) - i + 2],
      locs * 0 + upper_left[2],
      col = cols[i],
      border = NA
    )
  }
  #segments(locs, locs*0 + lower_left[2], locs, locs*0 + upper_left[2], col = rgb(0,0,0,.7))
  text(
    locs,
    locs * 0 + upper_left[2],
    2 * ifelse(ps > .5, 1 - ps, ps),
    srt = 90,
    adj = c(1.1, 1.1),
    cex = 2,
    col = rgb(0, 0, 0, .7)
  )
  segments(lower_left[1] + plot_width / 2,
           lower_left[2],
           lower_left[1] + plot_width / 2,
           upper_left[2])
  mtext(letter, 1, -2, adj = .01, cex = 4)

  showtext::showtext_end()

  dev.off()

  return(tf)
}
