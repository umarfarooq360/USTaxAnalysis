
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)
library(rgdal)
library(rgeos)
library(magrittr)
library(dplyr)
library(RColorBrewer)
library(data.table)
library(maptools)

shinyServer(function(input, output, session) {

# Show progress bar while loading everything ------------------------------

  progress <- shiny::Progress$new()
  progress$set(message="Loading data", value=0)


# Code to load required files ---------------------------------------------
  load("tmpData.Rdata")
  if(!exists("droughts") ){
    print("Reading data");
    us <- readOGR("data/us_geojson", "OGRGeoJSON")
    #Restrict to mainland US not alaska and stuff
    us <- us[!us$STATEFP %in% c("02", "15", "72"),]

    us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

    map <- ggplot2::fortify(us_aea, region="GEOID")

    #Read the data
    droughts <- read.csv("data/12cyallagi.csv")

    #Remove the totals for state data
    droughts <- droughts[!(droughts$COUNTYFIPS == 0),];

    #Aggregating the data to remove agi brackets for county
    droughts =  aggregate( . ~ COUNTYFIPS+COUNTYNAME+STATE+STATEFIPS, droughts, FUN=sum)

    #Create standard five digit IDs for plotting states
    droughts$id <- paste(sprintf("%02d%03d", as.numeric(as.character(droughts$STATEFIPS)),as.numeric(as.character(droughts$COUNTYFIPS)) )  );

    #Create a color pallete
    ramp <- colorRampPalette(c("white", brewer.pal(n=9, name="YlOrRd")), space="Lab")

    #This is the variable under consideration
    droughts$total <- with(droughts, (as.integer((N1))))

    #Save the data
    print("Saving variables");
    save("droughts","ramp","map","doesDataExist", file="tmpData.Rdata");

  }
  #This is the function that generates unique plots -------------------------------
  #-------------------------------------------------------------------------------

  output$txtOut <-renderUI({


    if(file.exists(paste("plot", input$selector,".Rdata" ,sep='')))
    {
      print("Loading old plot ");
      load(paste("plot", input$selector,".Rdata" ,sep=''));
    }

    if(exists("whichPlotsSaved")  ){
      if( (input$selector %in% whichPlotsSaved)){
        print("found plot ")

        #Figure out how to pre-load plots

        }
    }

    #Set new progress
    progress$set(message="Creating map", value=0.5)

    if(input$selector == 1 ){ #Average mortgage interest deduction
      #Create a color pallete
      ramp <- colorRampPalette(c("white", brewer.pal(n=9, name="YlOrRd")), space="Lab")

      #This is the variable under consideration
      droughts$total <- with(droughts, (as.integer((A19300/N19300 ))))
    }    else if(input$selector == 2 ){  #capital gains
      #Use a different color pallete
      ramp <- colorRampPalette(c("white", brewer.pal(n=9, name="BuPu")), space="Lab")


      droughts$total <- with(droughts, (as.integer(( A01000/N01000 ))))
    }    else if(input$selector == 3 ){ #Unemployment compensation
      #Use a different color pallete
      ramp <- colorRampPalette(c("white", brewer.pal(n=9, name="BuGn")), space="Lab")


      droughts$total <- with(droughts, (as.integer(( A02300/N02300 ))))
    }



    #Merge the map and the data
    map_d <<- merge(map, droughts, all.x=TRUE)



    #Break data into 10 percentiles and see what value falls into which category. Then fill colors accordingly
    data_chunks <- quantile(map_d$total,seq(0,1,0.1),na.rm=T )
    #Remove duplicates
    data_chunks <- data_chunks[!duplicated(data_chunks )]

    #Create color assignments to states
    map_d$fill_col <- as.character(cut(map_d$total, data_chunks , include.lowest=TRUE, labels=ramp(length(data_chunks)-1)))
    map_d$fill_col <- ifelse(is.na(map_d$fill_col), "#FFFFFF", map_d$fill_col)

    #This displays values when you hover
    drought_values <- function(x) {
      if(is.null(x) | !(x$id %in% droughts$id)) return(NULL)
      y <- subset(droughts, select=(c("COUNTYNAME","STATE","N1","A00100","id") )) %>% filter(id==x$id)
      sprintf("<table width='100%%'>%s</table>",
              paste0("<tr><td style='text-align:left'>", c( "County Name","State","# Returns", "AGI"),
                     ":</td><td style='text-align:right'>", format(subset(y, select=1:4)), collapse="</td></tr>"))
    }


    map_d %>%
      group_by(group, id  ) %>%
      ggvis(~long, ~lat) %>%
      layer_paths(fill:=~fill_col, strokeOpacity := 0.5, strokeWidth := 0.25) %>%
      add_tooltip(drought_values, "hover") %>%
      hide_legend("fill") %>%
      hide_axis("x") %>% hide_axis("y") %>%
      set_options(width=900, height=600, keep_aspect=TRUE) %>%
      bind_shiny("drought")



    # Turn off progress bar ---------------------------------------------------
    #Set new progress
    progress$set(message="Done!", value=0)

    #Which plots were saved and will be preloaded
    if(!exists("whichPlotsSaved" ) ){whichPlotsSaved = c(0 ,input$selector ) }
    else{whichPlotsSaved= c(whichPlotsSaved, input$selector )}
    save(map_d, whichPlotsSaved ,file=paste("plot",input$selector,".Rdata" ,sep=''))

    return(NULL);
  })

  output$map_plot <- renderUI({
    print("Rendered new plot")
    ggvisOutput("drought");
  })

#--------------------------------
#Stuff for the time series chart
#--------------------------------

# Provide explicit colors for regions, so they don't get recoded when the
# different series happen to be ordered differently from year to year.
timeData = read.csv("data/timeTaxData.csv")

#Create a color pallete
pallete <- colorRampPalette(c( brewer.pal(n=9, name="Set1")), space="Lab")

defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")

series <- structure(
  lapply( pallete(3) , function(color) { list(color=color) }),
  names = levels(timeData$Eclass)
)

yearData <- reactive({
  # Filter to the desired year, and put the columns
  # in the order that Google's Bubble Chart expects
  # them (name, x, y, color, size). Also sort by region
  # so that Google Charts orders and colors the regions
  # consistently.
  df <- timeData %>%
    filter( Year == input$year ) %>%
    select(Ybracket, AvTax, NumRet, Eclass, PerCapitaAGI)

})

output$income_time_chart <- reactive({
  # Return the data and options
  list(
    data = googleDataTable(yearData()),
    options = list(
      title = sprintf(
        "Income data with brackets, %s",
        input$year),
      series = series
    )
  )
})




})