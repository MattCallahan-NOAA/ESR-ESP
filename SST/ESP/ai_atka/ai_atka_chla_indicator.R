# ai chla indicator
library(rerddap)
library(tidync)
library(sf)
library(ggplot2)
library(akgfmaps)
library(dplyr)
library(lubridate)
# from SST script
# seguam <- define_box(x_min=52, x_max=52.3, y_min=-173, y_max=-172.2)
# tanaga <- define_box(x_min=51.4, x_max=51.8, y_min=-179, y_max=-178)
# attu <- define_box(x_min=52.3, x_max=52.9, y_min=173, y_max=174)

# specify product
info_NR_chla<-rerddap::info(datasetid = "pmlEsaCCI60OceanColor8Day", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

# download chla
seguam_chla <- griddap(info_NR_chla, latitude = c(52, 52.3), longitude = c(-173, -172.2), time = c('1997-09-04','last'), fields = 'chlor_a')

seguam_chla <- as.data.frame(seguam_chla$data)
seguam_chla$dates<-as.Date(seguam_chla$time) 
seguam_chla$ai_atka <- "seguam"

tanaga_chla <- griddap(info_NR_chla, latitude = c(51.4, 51.8), longitude = c(-179, -178), time = c('1997-09-04','last'), fields = 'chlor_a')

tanaga_chla <- as.data.frame(tanaga_chla$data)
tanaga_chla$dates<-as.Date(tanaga_chla$time) 
tanaga_chla$ai_atka <- "tanaga"

attu_chla <- griddap(info_NR_chla, latitude = c(52.3, 52.9), longitude = c(173, 174), time = c('1997-09-04','last'), fields = 'chlor_a')

attu_chla <- as.data.frame(attu_chla$data)
attu_chla$dates<-as.Date(attu_chla$time)
attu_chla$ai_atka <- "attu"

atka_chla <-seguam_chla %>%
  bind_rows(tanaga_chla) %>%
  bind_rows(attu_chla) %>%
  mutate(month=month(dates),
         year=year(dates))

# calculate chla indicator
amj_meanchla <- atka_chla %>%
  filter(month %in% c(4,5,6)) %>%
  group_by(ai_atka, year) %>%
  summarize(n=n(),
            na_count=sum(is.na(chlor_a)),
            chla_mean=mean(chlor_a, na.rm=T),
            chla_sd = sd(chlor_a, na.rm=T)) %>%
  mutate(coarse_coverage=(n-na_count)/n)

ggplot()+
  geom_line(data=amj_meanchla, aes(x=year, y=chla_mean, color=ai_atka), size=2)+
  geom_line(data=amj_meanchla, aes(x=year, y=chla_mean-chla_sd, color=ai_atka), lty=2)+
  geom_line(data=amj_meanchla, aes(x=year, y=chla_mean+chla_sd, color=ai_atka), lty=2)+
  ylim(c(0,4.5))+
  ggtitle("AI Atka area AMJ mean chla")+
  theme_bw()
ggsave("ai_atka/atka_area_amj_chla.PNG")


ggplot()+
  geom_line(data=amj_meanchla, aes(x=year, y=coarse_coverage, color=ai_atka), size=2)+
  ylab("coverage: (total count - na count)/total count")+
  ggtitle("AI Atka area AMJ proportion of data available")+
  theme_bw()
ggsave("ai_atka/atka_area_amj_chla_coverage.PNG")
