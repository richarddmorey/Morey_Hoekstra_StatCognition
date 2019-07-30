

#' Run shiny app to explore the (cleaned) data set
#'
#' @return Returns the result of shiny::runApp()
#' @export
#' @importFrom shiny runApp
#'
explore_data <- function(){
  shiny::runApp(system.file("shiny/explore",
                          package = "MoreyHoekstra2019"))
}
