
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)

me_crime <- read.csv("data/me_crime.csv", stringsAsFactors=FALSE)

shinyUI(fluidPage(
  titlePanel("US Tax Distribution"),
  sidebarLayout(
    sidebarPanel(
      p(strong("NOTE: "), "Since this loads many maps, it can take a few seconds to startup. Even after that, there may be some delay with more complex maps due to how ggvis renders them."),
      hr(),
      p("This is an example of making static & interactive maps with ggvis and wiring them up interactively in a Shiny app."),
      p("The first two render static Maine state maps (with & without county labels). The second two let you interactively explore Maine county crime data for 2013 (by 1K popualtion)."),
      p("The U.S. Drought Map interactively shows drought levels as of 2014-12-23 and is also an example of using a projection (Albers) & also custom colors outside of any ggvis scale). It also shows that many polygons can take a while to render (initially)."),
      p("The final example is a world map with a Winkel-Tripel projection and also shows how to add projected points to the map (no interactivity)."),
      hr(), selectInput("selector", label = h3("Choose data to plot"),
                        choices = list("Number of returns" = 1, "Adjusted Gross Income" = 2,
                                       "Dividends" = 3), selected = 1),uiOutput("txtOut"),
      width=3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tax Map", wellPanel(p("This shows how to use a projection and also a custom color scale without using anh of the built-in ", code("scale_") , " functions. It also shows the use of a tooltip where there is no data for some polygons.")), htmlOutput("map_plot"))

      ),
      width=9
    )
  )
))