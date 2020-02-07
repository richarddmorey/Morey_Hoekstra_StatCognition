# Variable, global to package's namespace. 
# This function is not exported to user space and does not need to be documented.
MYPKGOPTIONS <- settings::options_manager(
  ggplot_family = "Lato Light"
)

# User function that gets exported:

#' Set or get options for my package
#' 
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{ggplot_family}}{(\code{character};"Lato Light") Font family to use for ggplot2 graphs }
#' }
#'
#' @export
pkg_options <- function(...){
  # protect against the use of reserved words.
  settings::stop_if_reserved(...)
  MYPKGOPTIONS(...)
}