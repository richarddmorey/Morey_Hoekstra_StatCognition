% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fig_evidence_scale.R
\name{fig_evidence_scale_boxes}
\alias{fig_evidence_scale_boxes}
\title{Number of samples as a function of sample type, evidence scale
and true effect size}
\usage{
fig_evidence_scale_boxes(
  filter_func = function(data, ...) return(data),
  print_plot = TRUE
)
}
\arguments{
\item{filter_func}{function to filter the data before plotting}

\item{print_plot}{(logical) set to FALSE to prevent printing the ggplot object}
}
\value{
Invisibly returns the ggplot object
}
\description{
Number of samples as a function of sample type, evidence scale
and true effect size
}
\examples{
suppressWarnings(
  fig_evidence_scale_boxes()
)
}
