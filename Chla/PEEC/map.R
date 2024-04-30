library(tidyverse)
library(sf)
library(akmarineareas2)

ak_dd <- ak %>%
  st_transform(crs=4326)

lkp_fp <- "data/viirs/viirs_lkp_04222024.RDS"

lkp <- readRDS(lkp_fp)

plotdat <- lkp %>%
  filter(ecosystem_area != "Arctic") %>%
  filter(if_else(ecosystem_area == "Aleutian Islands", depth <= -30, 
                 if_else(ecosystem_area== "Gulf of Alaska", depth <= -30 & depth >= -500,
                         depth <= -30 & depth >= -200))) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326)%>%
  st_transform(crs=3338)

plotdat2 <- lkp %>%
  filter(if_else(ecosystem_area == "Aleutian Islands", depth <= -30, depth <= -30 & depth >= -500)) %>%
  filter(ecosystem_area == "Gulf of Alaska")

png(filename="PEEC/www/reference_map.png", width=1500, height=750)
ggplot()+
  geom_sf(data=plotdat, color="light gray", size=0.5)+
  geom_sf(data=ak, fill="dark gray") +
  geom_sf(data=esr, fill=NA)+
  coord_sf(crs=3338)+
  theme_bw()
dev.off()
