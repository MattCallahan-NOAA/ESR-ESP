library(dplyr)
library(tidyverse)
library(zoo)
library(data.table)
library(cmocean)


# 
f23 <- readRDS("inter_jens_datafiles/bloomTimingGlob_1998_2023.RDS")

head(f23)
f23s<-subset(f23,bsierp_super_region %in% c("South inner shelf","South middle shelf","South outer shelf"))


head(f23s)
avg_timing_SEBS<-f23s %>%
  group_by(year) %>%
  summarise(mean = mean(peak_timing_all_log,na.rm=T), sd = sd(peak_timing_all_log,na.rm=T))


d22<-read.csv('Chla/ESP/2022_indicators/jens/ESP_Spline_BloomTiming_new_SEBS_22.csv')


plot(avg_timing_SEBS$year,avg_timing_SEBS$mean,type='b',col='red3',pch=19,ylim=c(100,160))
points(d22$YEAR,d22$DATA_VALUE,col='black',type='b')

avg_timing_SEBS$INDICATOR_NAME<-rep('Spring_Chlorophylla_Peak_SEBS_Satellite',nrow(avg_timing_SEBS))
avg_timing_SEBS_final<-avg_timing_SEBS[c(1,4,2,3)]
colnames(avg_timing_SEBS_final)<-c('YEAR','INDICATOR_NAME','DATA_VALUE','SD')
head(avg_timing_SEBS_final)

write.csv(x=avg_timing_SEBS_final, file='Chla/ESP/2023_indicators/ESP_Spline_BloomTiming_new_SEBS_23.csv')


###
###
###
b<-readRDS('inter_jens_datafiles/globcolour_23augSQL.RDS')
head(b)

b$month=month(b$mid_date)
b$year=year(b$mid_date)
b$doy=yday(b$mid_date)

bs<-subset(b,bsierp_super_region %in% c("South inner shelf","South middle shelf","South outer shelf"))
bs5<- bs %>% filter(month==5)
head(bs)

bs5_avg<- bs5 %>% group_by(year,bsierp_super_region) %>%  summarise(sd_chla = sd(meanchla,na.rm=TRUE),
                                                                      mean_chla = mean(meanchla,na.rm=TRUE))


bs5_avg2<- bs5_avg %>% group_by(year,) %>%  summarise(sd_chla = sd(sd_chla,na.rm=TRUE),
                                                                    mean_chla = mean(mean_chla,na.rm=TRUE))


# old product MODIS
bio22<-read.csv('Chla/ESP/2022_indicators/jens/Spring_Chlorophylla_Biomass_SEBS_Satellite.csv')


plot(bs5_avg2$year,bs5_avg2$mean_chla,type='b',col='red3',pch=19,ylim=c(0,6))
points(bio22$YEAR,bio22$DATA_VALUE,col='black',type='b')
abline(v=2012)

bs5_avg2$INDICATOR_NAME<-rep('Spring_Chlorophylla_Biomass_SEBS_Satellite',nrow(bs5_avg2))
bs5_avg2_final<-bs5_avg2[,c(1,4,2,3)]
colnames(bs5_avg2_final)<-c('YEAR','INDICATOR_NAME','DATA_VALUE','SD')
head(bs5_avg2_final)

write.csv(x=bs5_avg2_final, file='Chla/ESP/2023_indicators/ESP_average_CHLA_biomass_May_new_SEBS_22.csv')





