% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fig_confidence.R
\name{fig_confidence}
\alias{fig_confidence}
\title{Response confidence ratings}
\usage{
fig_confidence(
  which_resp = c("null", "alt"),
  shade_limit = 20,
  filter_func = function(data, ...) return(data)
)
}
\arguments{
\item{which_resp}{Which responses to plot (responses indicating null or alt)}

\item{shade_limit}{N for column below which will be shaded to indicate interpretability
problems from small N}

\item{filter_func}{function to filter the data before plotting}
}
\value{
Invisibly returns NULL
}
\description{
Response confidence ratings
}
\examples{
fig_confidence( which_resp = "alt" )
}
