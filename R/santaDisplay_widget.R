#' HTML widget for displaying an animation of participant behavior
#'
#' @param value Participant ID to display
#' @param label label for the widget
#' @param tooltip tooltip for the widget
#' @param width width of div
#' @param height height of div
#' @param elementId widget div element id
#'
#' @return An object of class \code{htmlwidget} for printing into HTML
#' @export
#' @import dplyr
#' @import htmlwidgets
#' @importFrom rjson toJSON
#'
santaDisplay <- function(value, label = "", tooltip = "", width = NULL, height = NULL, elementId = NULL)
{

  snippet_fn <- system.file("htmlwidgets/html/interface_snippet.html",
                            package = "MoreyHoekstra2019")
  snippet <- paste(readLines(snippet_fn, n = -1), collapse = "\n")

  stopifnot( value %in% MoreyHoekstra2019::christmas_stats_participants$id )

  df <- MoreyHoekstra2019::christmas_stats_samples %>%
    filter(id == value) %>%
    arrange(time)



  # create widget
  htmlwidgets::createWidget(
    name = 'santaDisplay',
    rjson::toJSON(list(df = df, snippet = snippet)),
    width = width,
    height = height,
    package = 'MoreyHoekstra2019',
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = 200,
      defaultHeight = 200
    )
  )
}


#' Shiny bindings for santaDisplay
#'
#' Output and render functions for using santaDisplay within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a ct_test
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name santaDisplay-shiny
#'
#' @import htmlwidgets
#' @export
santaDisplayOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'santaDisplay', width, height, package = 'MoreyHoekstra2019')
}

#' @rdname santaDisplay-shiny
#' @import htmlwidgets
#' @export
renderSantaDisplay <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, santaDisplayOutput, env, quoted = TRUE)
}
