
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)
library(googleCharts)

shinyUI(fluidPage(
  # This line loads the Google Charts JS library
  googleChartsInit(),
  #This is the title of the page
  titlePanel("US Tax Distribution"),
  sidebarLayout(
    sidebarPanel(
      p(strong("NOTE: "), "Since this loads many maps, it can take a few seconds to startup. Even after that, there may be some delay with more complex maps due to how ggvis renders them."),
      hr(),
      p("This is an interactive maps ( ggvis ) display of the some portions of the US tax distribution"),
      p(strong("Author:")," Omar Farooq"),
      p(strong(tags$a(href="https://github.com/umarfarooq360/USTaxAnalysis" ,"Source code") ) ),
       hr(), selectInput("selector", label = h3("Choose data to plot"),
                        choices = list("Average Home Mortgage Deduction" = 1, "Average Capital Gains" = 2,
                                       "Average Unemployment Compensation" = 3), selected = 1),uiOutput("txtOut"),
      width=3
    ),
    #This is the main (right) panel
    mainPanel(
      tabsetPanel(
        tabPanel("Tax Map",  htmlOutput("map_plot")), tabPanel("Income Bracket Time Series", googleBubbleChart("income_time_chart", width="100%", height = "475px",
           # Set the default options for this chart; they can be
           # overridden in server.R on a per-update basis. See
           # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
           # for option documentation.
           options = list(
             fontName = "Source Sans Pro",
             fontSize = 13,
             # Set axis labels and ranges
             hAxis = list(
               title = "Income data Time Series"

             ),
             vAxis = list(
               title = "Life expectancy (years)"

             ),
             # The default padding is a little too spaced out
             chartArea = list(
               top = 50, left = 75,
               height = "75%", width = "75%"
             ),
             # Allow pan/zoom
             explorer = list(),
             # Set bubble visual props
             bubble = list(
               opacity = 0.4, stroke = "none",
               # Hide bubble label
               textStyle = list(
                 color = "none"
               )
             ),
             # Set fonts
             titleTextStyle = list(
               fontSize = 16
             ),
             tooltip = list(
               textStyle = list(
                 fontSize = 12
               )
             )

           )#list ends here
        ),#googlechart ends here

             sliderInput("year", "Year",   min = 2001, max = 2012,  value = 2001, animate = TRUE)
        )

      ),
      width=9
    )
  )
))