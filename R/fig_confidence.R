
#' Response confidence ratings
#'
#' @param which_resp Which responses to plot (responses indicating null or alt)
#' @param shade_limit N for column below which will be shaded to indicate interpretability
#' problems from small N
#' @param filter_func function to filter the data before plotting
#'
#' @return Invisibly returns NULL
#' @import dplyr
#' @importFrom viridis viridis
#' @importFrom stringr str_pad
#'
#' @export
#'
#' @examples
#' fig_confidence( which_resp = "alt" )
fig_confidence <- function(which_resp = c("null", "alt"),
                           shade_limit = 20,
                           filter_func = function(data, ...) return(data)) {
  which_resp = which_resp[1]
  stopifnot(which_resp %in% c("null", "alt"))

  dat <- MoreyHoekstra2019::christmas_stats_participants %>%
    filter_func()

  x = table(
    dat$confidence,
    abs(dat$effect_size),
    dat$response %in% c("jinglies", "sparklies")
  )
  x0 = lapply(1:2, function(i)
    t(t(x[, , i]) / colSums(x[, , i])))
  names(x0) = c("Reported no detection/no difference", "Detected difference")

  cx = .7
  par(
    mar = c(5.1, 11.1, 4.1, 2.1),
    cex = cx,
    cex.axis = .8,
    cex.lab = 1
  )

  scaling = .4
  cols = viridis::viridis(4)

  d = as.numeric(colnames(x))
  J = length(d)
  mids = (d[-J] + d[-1]) / 2

  if (which_resp == "null") {
    i = 1
  } else{
    i = 2
  }

  plot(
    d,
    d,
    ty = 'n',
    ylim = c(.5, 4.5),
    axes = FALSE,
    xlab = "Effect size",
    ylab = "",
    xlim = c(d[1] - (mids[1] - d[1]), d[J] + (d[J] - mids[J - 1])),
    xaxs = "i"
  )
  abline(v = mids, col = rgb(0, 0, 0, 1))
  pin = par()$pin
  usr = par()$usr
  x.s = diff(usr[1:2]) / pin[1]
  y.s = diff(usr[3:4]) / pin[2]
  asp.s = x.s / y.s

  mids0 = c(par()$usr[1], mids, par()$usr[2])

  mtext("Confidence", 2, 9, cex = .8)
  mtext(names(x0)[i], 3, 2.5, adj = .9, cex = 1.3)
  mtext("(Number of participants)",
        3,
        .8,
        adj = .5,
        cex = .8)
  abline(h = 1:4, col = rgb(0, 0, 0, .2))
  axis(1, at = d, labels = round(d, 2))
  axis(
    2,
    at = 1:4,
    labels = rownames(x0[[i]]),
    las = 1,
    tick = FALSE
  )

  for (j in 1:J) {
    for (k in 1:4) {
      w = sqrt(x0[[i]][k, j]) * scaling
      rect(d[j] - w / 2 * asp.s,
           k - w / 2,
           d[j] + w / 2 * asp.s,
           k + w / 2, col = cols[k])
      text(
        d[j],
        k + w / 2,
        col = rgb(0, 0, 0, .5),
        stringr::str_pad(
          paste0(round(x0[[i]][k, j] * 100, 0), "%"),
          side = "left",
          width = 4,
          pad = " "
        ),
        adj = c(.5,-.2),
        cex = .8
      )
    }
    n = colSums(x[, , i])[j]
    if (n < shade_limit)
      rect(
        mids0[j],
        par()$usr[3],
        mids0[j + 1],
        par()$usr[4],
        border = NA,
        col = rgb(1, 1, 1, .6)
      )
    text(d[j],
         par()$usr[4],
         paste0("(", n, ")"),
         xpd = TRUE,
         cex = .8)
  }


  invisible(NULL)

}
