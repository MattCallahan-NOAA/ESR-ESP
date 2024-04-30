library(shiny)
# library(ggplot2)
# library(dplyr)
# library(mapdata)
# library(slider)
# library(rerddap)
# library(ncdf4)
# library(tidync)
# library(lubridate)
# library(scales)
# library(cmocean)
# library(shinycssloaders)

# load data

ui <- fluidPage(
  
  tabsetPanel(type="tabs",
              tabPanel("Map",
                       column(12, align = "center",
                              tags$blockquote("Weekly averaged satellite Chl-a estimates from near-real time VIIRS for each of the ecosystem regions managed by the Alaska Fisheries Science Center. Near real-time chla data are preliminary estimates that have yet to be fully quality controlled."),
                              tags$blockquote("Chl-a are updated automatically using satellite data curated by NOAA's Coast Watch Program, while ice fraction data are updated automatically using satellite data curated by NOAA's Coral Reef Watch Program."),
                              tags$blockquote("Images: Spatial chla and ice data (Bering Sea only) from the most recent week (bottom right) and preceeding weeks. Images were chosen based on the fewest missing pixels for each period. Near real-time chla data (nesdisVHNchlaWeekly) are preliminary estimates that have yet to be fully quality controlled. For better visualization the Chl-a color scale is log-transformed and maximum Chl-a scale is set to a maximum of 15 ug/l."),
                              tags$blockquote("Time series: The current year's daily temperatures (black lines) are compared to the previous year (blue line), the daily average (2013-2022, purple), and each of the individual years since 2013 (grey lines). Current year uses near real time while previous years use science quality chlorophyll (nesdisVHNSQchlaWeekly_Lon0360). Depths are filtered to deeper than 30m for all regions. The Aleutians have no minimum depth filter but the time series is limited to shallower than 500m for the Gulf of Alaska and 200m for the Bering Sea."),
                              tags$blockquote("Coverage: Proportion of the strata with available chlorophyll data"),
                              # Image 1
                              img(src = "reference_map.png", height = 800#, width = 300
                              )
                       )
              ),
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
                       ),
                       fluidRow(
                         column(12, align = "center",
                                # Image 2
                                img(src = "chla_coverage.png", height = 800#, width = 300
                                )
                         )
                       )

                       ),
 tabPanel("Gulf of Alaska",
          fluidRow(
            column(12, align = "center",
                   # Image 1
                   img(src = "chla_maps_goa.png", height = 800#, width = 300
                   )
            )
          ),
          fluidRow(
            column(12, align = "center",
                   # Image 2
                   img(src = "Chla_annual_lines_goa.png", height = 800#, width = 300
                   )
            )
          ),
          fluidRow(
            column(12, align = "center",
                   # Image 2
                   img(src = "chla_coverage_goa.png", height = 800#, width = 300
                   )
            )
          )
 ),
 tabPanel("Aleutian Islands",
          fluidRow(
            column(12, align = "center",
                   # Image 1
                   img(src = "chla_maps_ai.png", height = 800#, width = 300
                   )
            )
          ),
          fluidRow(
            column(12, align = "center",
                   # Image 2
                   img(src = "Chla_annual_lines_ai.png", height = 800#, width = 300
                   )
            )
          ),
          fluidRow(
            column(12, align = "center",
                   # Image 2
                   img(src = "chla_coverage_ai.png", height = 800#, width = 300
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
