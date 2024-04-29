library(tidyverse)
library(sf)
library(akmarineareas2)

ak_dd <- ak %>%
  st_transform(crs=4326)

lkp_fp <- "data/viirs/viirs_lkp_04222024.RDS"

lkp <- readRDS(lkp_fp)

plotdat <- lkp %>%
  filter(ecosystem_area != "Arctic") %>%
  filter(if_else(ecosystem_area == "Aleutian Islands", depth <= -30 & depth >= -1000, 
                 if_else(ecosystem_area== "Gulf of Alaska", depth <= -30 & depth >= -500,
                         depth <= -30 & depth >= -200)))

plotdat2 <- lkp %>%
  filter(if_else(ecosystem_area == "Aleutian Islands", depth <= -30, depth <= -30 & depth >= -500)) %>%
  filter(ecosystem_area == "Gulf of Alaska")


ggplot()+
  #geom_point(data=plotdat2, aes(x=lon360, y=latitude), size = 0.1, color="red")+
  geom_point(data=plotdat, aes(x=lon360, y=latitude), size = 0.1, color="red")+
  geom_sf(data=ak_dd%>%st_shift_longitude()) +
  geom_sf(data=esr_dd%>%st_shift_longitude(), fill=NA)+
  xlim(c(167,198))+ylim(c(48,59))+
  theme_bw()

