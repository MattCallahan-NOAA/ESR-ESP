# New indicator for arrowtooth
# identify best region:
library(akmarineareas2)
library(ggplot2)
library(sf)
library(httr)
library(jsonlite)
library(dplyr)

lkp <-   jsonlite::fromJSON(
    httr::content(
      httr::GET("https://apex.psmfc.org/akfin/data_marts/akmp/crw_spatial_lkp?"),
      as="text", encoding="UTF-8")) %>%
    dplyr::bind_rows() %>%
    rename_with(tolower)




ggplot()+geom_sf(data=ak_dd)+
  geom_sf(data=nmfs_dd, fill=NA, color="red")+
  #geom_sf(data=lme_dd, fill=NA, color="blue")+
  #geom_point(data=lkp %>%filter(depth>=-200 & depth<=-100 & ecosystem == "Gulf of Alaska"), 
  #           aes(x=longitude, y=latitude), color="black", size=1)+
  geom_point(data=lkp %>%filter(depth>=-900 & depth<=-150 & ecosystem == "Gulf of Alaska"), 
             aes(x=longitude, y=latitude), color="gold", size=1)+
  xlim(c(-165, -145))+ylim(c(52,62))+
  ggtitle("red=nmfsareas, gold=150-900")+
  theme_bw()
