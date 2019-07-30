#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Morey & Hoekstra 2019 Data"),
    withMathJax(),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("type", "Sample type in 'Display' tab",
                        c(Experiments="expt",
                          "Null samples" = "null") ),
            checkboxInput("pvals", "p values in 'Display' tab",
                          value = FALSE),
            textInput("strategyTextFilter", "Filter by strategy text", width = 200)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Data", DT::dataTableOutput("dataTable")),
                tabPanel("Display", imageOutput("displayPlot")),
                tabPanel("Animate", MoreyHoekstra2019::santaDisplayOutput(outputId = "santaDisplay")),
                tabPanel("Strategy text", uiOutput("strategyText")),
                tabPanel("Distributions", plotOutput("distributionsPlot"))#,
                #tabPanel("Explanation", uiOutput("explText"))
            )
        )
    )
))
