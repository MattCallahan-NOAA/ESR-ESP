library(shiny)
library(ggplot2)
library(dplyr)
library(mapdata)
library(slider)
library(rerddap)
library(ncdf4)
library(tidync)
library(lubridate)
library(scales)
library(cmocean)
library(shinycssloaders)

# load data

ui <- fluidPage(
  
  tabsetPanel(type="tabs",
              tabPanel("Bering Sea",
                       downloadButton("bs_map_dl", "Download maps"),
                       downloadButton("bs_series_dl","Download time series"),
                       fluidRow(
                         column(12, align = "center",
                                # Image 1
                                img(src = "chla_maps.png", height = 800#, width = 300
                                    )
                         )
                       ),
                       fluidRow(
                         column(12, align = "center",
                                # Image 2
                                img(src = "Chla_annual_lines.png", height = 800#, width = 300
                                    )
                         )
                       )

                       )
  )
)

server <- function(input, output) {

  
  output$bs_map_dl<-downloadHandler(
    filename = function() {
      paste("BS-chla-maps-", Sys.Date(), ".png", sep="")
    },
    contentType = "image/png",
    content= function(file){
      file.copy("www/chla_maps.png", file)
    }
    
  )
  
  output$bs_series_dl<-downloadHandler(
    filename = function() {
      paste("BS-chla-time-series-", Sys.Date(), ".png", sep="")
    },
    contentType = "image/png",
    content= function(file){
      file.copy("www/Chla_annual_lines.png", file)
    }
    
  )
  
}

shinyApp(ui = ui, server = server)
