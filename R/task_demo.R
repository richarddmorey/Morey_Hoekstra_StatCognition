#' Start a web server with a demo of the experimental task and open browser
#'
#' @param toy_name Name of the toy; determines effect size
#' @param evidence_power Power for the p value transform
#'
#' @return Invisibly returns a list of the job number of the HTTP server and url
#' @export
#' @importFrom servr httd
#'
task_demo <- function(toy_name = NULL, evidence_power = NULL){

  toy_names = c("whizbang balls",
                "constructo bricks",
                "rainbow clickers",
                "doodle noodles",
                "singing bling rings",
                "brahma buddies",
                "magic colorclay",
                "moon-candy makers")

  if(is.null(toy_name)){
    probs = c(1/4, 3/4*rep(1/7, 7))
    idx = which(rmultinom(1, 1, probs)>0)
    toy_name = toy_names[idx]
  }

  if(is.null(evidence_power)){
    evidence_power = sample(c(3,7), size = 1)
  }

  if( !(evidence_power %in% c(3,7) ) ){
    stop("evidence_power must be either 3 or 7.")
  }
  if( !( toy_name %in% toy_names ) ){
    stop("toy_name must be one of", toy_names,".")
  }

  sf = tempfile()
  zz <- file(sf, open = "wt")
  dn <- system.file("pkg_html/",
                    package = "MoreyHoekstra2019")

  initpath = paste0("/task.html?toy_name=", URLencode(toy_name), "&q=",evidence_power)

  sink(file = zz, type = "message")
  servr::httd(dir = dn, initpath = initpath, browse = FALSE)
  sink(type = "message")
  close(zz)
  mesg <- readLines(sf, -1)
  job <- regmatches(mesg[1],
                    regexpr("[0-9]+", mesg[1], perl = TRUE)
  )
  url_actual <- regmatches(mesg[2],
                           regexpr("http://.+$", mesg[2], perl = TRUE)
  )
  utils::browseURL(url_actual)
  message(mesg)

  invisible(c(job = job, url = url_actual))

}
