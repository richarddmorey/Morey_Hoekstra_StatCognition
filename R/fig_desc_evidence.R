#' Show the transformation between the z/p value and the visual location
#'
#' @return Invisibly returns NULL
#' @importFrom viridis viridis
#'
#' @export
#'
#' @examples
#' fig_desc_evidence_transform()
fig_desc_evidence_transform <- function() {
  cols = viridis::viridis(3)

  par(las = 1)
  curve(
    evp(x, 3),
    0,
    1,
    500,
    type = "n",
    xaxs = "i",
    yaxs = "i",
    ylab = "Evidence / x location",
    xlab = "One-sided p value [Pr(Z<z)]"
  )
  rect(0, -1, .025, 1, border = NA, col = rgb(0, 0, 0, .2))
  rect(.975, -1, 1, 1, border = NA, col = rgb(0, 0, 0, .2))
  curve(evp(x, 3),
        0,
        1,
        500,
        lwd = 2,
        col = cols[1],
        add = TRUE)
  curve(
    evp(x, 7),
    0,
    1,
    500,
    add = TRUE,
    lwd = 2,
    col = cols[2],
    lty = 2
  )
  text(.2, 0, "q = 7", col = cols[2], cex = 1.3)
  text(.2, -.4, "q = 3", col = cols[1], cex = 1.3)

  axis(3, at = pnorm(seq(-2,2,1)), lab = seq(-2,2,1))
  mtext("Z test statistic", 3, 2.5, adj = .5)

  invisible(NULL)

}



#' Show distributions of the "evidence" as a function of sample size, evidence scale/power, and true effect size
#'
#' @param n sample sizes (single value or vector)
#' @param q evidence scale/power (single value or vector)
#' @param d true effect size (single value or vector)
#' @param letter Label to add to the graph
#' @param scale_y_on How to choose a scale for the y axis; see details
#' @param d_scale Scale multiplier to be applied to effect sizes
#'
#' @details \code{scale_y_on} can be one of several values:
#' \itemize{
#' \item NULL: scale on the the median maximum density value across distributions
#' \item 0: scale on the maximum density value across distributions
#' \item negative real number: the absolute value of this value will be used as the maximum y value
#' \item positive integer: the index of the distribution whose mode will be used as the maximum
#' }
#'
#' @return Returns the maximum y value (useful for further plotting)
#' @importFrom viridis viridis

#' @export
#'
#' @examples
#' fig_desc_evidence()
fig_desc_evidence <- function(n = 10,
                              q = 7,
                              d = NULL,
                              letter = "",
                              scale_y_on = 1,
                              d_scale = 1) {

  samples <- MoreyHoekstra2019::christmas_stats_samples

  letter = ifelse(nchar(letter)>0,
                  paste(letter, ": ", collapse = ""), "")

  xx = seq(-.999, .999, length.out = 500)

  lns = c(n=length(n), q=length(q), d=length(d))

  if(sum(lns==0)!=1)
    stop("Exactly one of n, q, and d must be null.")

  if(any(lns)>1)
    stop("If n, q, or d are not null, they must have length 1.")

  if(is.null(d)){
    ds = sort(unique(abs(samples$d)))
    txt = paste(letter, "q =",q,", n =",n, collapse = "")
    txt2 = "Each density: one effect size"
  }else{
    ds = d
  }

  if(is.null(n)){
    ns = sort(unique(samples$n))
    txt = paste(letter,"d =",round(d,2),", q =",q, collapse = "")
    txt2 = "Each density: one sample size"
  }else{
    ns = n
  }

  if(is.null(q)){
    qs = c(3, 7)
    txt = paste(letter,"d =",round(d,2),", n =",n, collapse = "")
    txt2 = "Each density: one evidence power"
    }else{
    qs = q
  }

  ds = ds * d_scale
  all_design = expand.grid(n = ns, d = ds, q = qs)
  n_curves = nrow(all_design)

  cols = viridis::viridis(n_curves, alpha = .2)

  Z = matrix(nrow = n_curves, ncol = length(xx))

  for (i in 1:n_curves) {
    n0 = all_design[i, "n"]
    q0 = all_design[i, "q"]
    d0 = all_design[i, "d"]
    Z[i, ] = dens.ev(xx, d0, n0, q0)
  }

  if( is.null(scale_y_on) ){
    ymax = median( apply(Z,1,max) ) * 1.2
  }else if(scale_y_on == 0){
    ymax = max(Z) * 1.2
  }else if(scale_y_on < 0){
    ymax = abs(scale_y_on)
  }else{
    ymax = max(Z[floor(scale_y_on),]) * 1.2
  }

  par(mar = c(5.1, 4.1, 1.2, 2.1), cex.lab = 1.5)

  plot(
    0,
    0,
    ty = 'n',
    xlim = c(-1, 1),
    ylim = c(0,ymax),
    axes = FALSE,
    ylab = "Density",
    xlab = "Evidence / x location",
    yaxs = "i",
    xaxs = "i"
  )
  axis(1)

  mtext(txt, 3, .1, adj = .05)
  mtext(txt2, 3, .1, adj = .95)

  for (i in 1:n_curves) {
    n0 = all_design[i, "n"]
    q0 = all_design[i, "q"]
    d0 = all_design[i, "d"]
    polygon(c(xx, rev(xx)),
            c(Z[i,], xx * 0),
            border = rgb(0, 0, 0, .3),
            col = cols[i])
  }

  invisible(ymax)

}

# Helper functions --------------------------------------------------------


evz = function(z, q)
  (1 - (1 - pchisq(z ^ 2, 1)) ^ (1 / q)) * sign(z)

evp = function(p, q)
  evz(qnorm(p), q)

zev = function(ev, q)
  - sign(ev) * qnorm(((1 - abs(ev)) ^ q) / 2)

d.zev.ev <- function(ev, q)
  1 / dnorm(qnorm(((1 - abs(
    ev
  )) ^ q) / 2)) * q / 2 * ((1 - abs(ev))) ^ (q - 1)

dens.ev = function(ev, d, n, q)
  dnorm(zev(ev, q), d * sqrt(n / 2), 1) * abs(d.zev.ev(ev, q))

p.ev <- function(ev, q, d = 0, n = 10)
  pnorm(zev(ev, q), d*sqrt(n))

q.ev <- function(p, q, d = 0, n = 10)
  evz(qnorm(p, d*sqrt(n), 1), q)


