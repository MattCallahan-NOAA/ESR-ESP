library(httr)
#devtools::install_github("atyrell3/AKesp")
library(AKesp)
library(tidyverse)

#from Abigail Tyrell's AKesp package
get_esp_data <- function(stock) {
  base_url <- "https://apex.psmfc.org/akfin/data_marts/akmp/esp_indicators?intended_esp="
  stock_url <- stock %>%
    stringr::str_replace_all(" ", "%20")
  
  url <- paste0(base_url, stock_url)
  
  data <- httr::content(httr::GET(url),
                        type = "application/json"
  ) %>%
    dplyr::bind_rows()
  
  return(data)
}

#heatwave
data<-get_esp_data("Alaska Sablefish")%>%
  filter(INDICATOR_NAME=="Annual_Heatwave_GOA_Model" )

unique(data$PRODUCT_DESCRIPTION)

#chlorophyll
data <- httr::content(httr::GET("https://apex.psmfc.org/akfin/data_marts/akmp/esp_indicators?"),
                      type = "application/json"
) %>%
  dplyr::bind_rows()

unique(data$INDICATOR_NAME)
"Spring_Chlorophylla_Biomass_SMBKC_Satellite"
"Spring_Chlorophylla_Biomass_WCGOA_Satellite"
"Spring_Chlorophylla_Peak_WCGOA_Satellite" 
"Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite" 
"Spring_Chlorophylla_Biomass_EGOA_Satellite"                            
"Spring_Chlorophylla_Biomass_SEBS_Satellite"                            
"Spring_Chlorophylla_Peak_EGOA_Satellite"                               
"Spring_Chlorophylla_Peak_SEBS_Satellite"

data2<-data%>%filter(grepl('Chlorophylla', INDICATOR_NAME))
unique(data2$CONTACT)
unique(data2$PRODUCT_DESCRIPTION)
table( data2$INDICATOR_NAME, data2$INTENDED_ESP_NAME)
data2%>%group_by(INDICATOR_NAME)%>%
  summarise(max_year=max(YEAR))

#sst
jw<-data%>%filter(CONTACT=="Jordan Watson")
table( jw$INTENDED_ESP_NAME,jw$INDICATOR_NAME)
unique(jw$INDICATOR_NAME)
jw<-jw%>%filter(INDICATOR_NAME%in% c("Spring_Temperature_Surface_EGOA_Satellite", "Spring_Temperature_Surface_WCGOA_Satellite"))
head(jw)

require(AKmarineareas)
nmfs<-AK_marine_area()
lme<-AK_marine_area

#crab
#no temperature indices
data <- httr::content(httr::GET("https://apex.psmfc.org/akfin/data_marts/akmp/esp_indicators?"),
                      type = "application/json"
) %>%
  dplyr::bind_rows() 

unique(data$INTENDED_ESP_NAME)
unique((data%>%
  filter(INTENDED_ESP_NAME %in% c("BS Snow Crab", "Bristol Bay Red King Crab" , "St. Matthew Blue King Crab")))$INDICATOR_NAME)

