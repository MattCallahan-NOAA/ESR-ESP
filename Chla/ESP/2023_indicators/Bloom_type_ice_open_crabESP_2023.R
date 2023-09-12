library(dplyr)
library(tidyverse)
library(zoo)
library(data.table)
library(cmocean)


# 
f23 <- readRDS("inter_jens_datafiles/bloomTimingGlob_1998_2023.RDS")
head(bs)
head(f23)
# crop for Erins paper - we might still need to fix this # 
E<- f23[f23$jens_grid   %in% c(39,62:64,84:88,103:112,125:135,147:153, 170:175,193:196,215:218,237:241,
                         260:264,281:288,304:311,327:334,350:358,374:381,398:405,422:428,446:450,470:473,494:495),]


E$north_south<-'north'
E$north_south[E$lat<60.0001]<-'south'

library(data.table)
setDT(E)
agg_text<-E[ !is.na(gridid_MS), lapply(.SD, function(x) {
  if(is.numeric(x)) mean(x, na.rm = TRUE) else x[!is.na(x)][1L]}), by = list(gridid_MS)]



data_mapH<-map_data("world2") # map_data from ggmap mapping package
breaks_w2<-c(185,190,195,200) 
labels_w2<-breaks_w2-360
head(agg_text)
# #

#windows(24,20)
map_bloomtype<- ggplot()+
  coord_equal(xlim=c(181,203),ylim=c(54.8,66),ratio = 1.8)+ # big map coordinates
  geom_text(data = agg_text, aes(x = lon+360, y =lat,label=as.character(gridid_MS),col=north_south),size=5)+
  geom_polygon(data = data_mapH, aes(x=long, y = lat, group = group),colour="black", fill="darkgrey")+
  ylab("Latitude")+
  scale_x_continuous("Longitude", breaks=breaks_w2, labels=labels_w2, limits=c(140,250))+
  ggtitle('')+
  theme(plot.title = element_text(size = 24),
        strip.text = element_text(size=20,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill='dodgerblue'), # Add the NOAA color blue to the facet strips.
        axis.title = element_text(size=26,family="sans"),
        axis.text = element_text(size=18,family="sans"),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black"))


# save as png

png(filename="Chla/ESP/2023_indicators/Fig1_bloomtype_mapESP_crab_missing2023.png",width = 1600, height = 1200,res=120)
plot(map_bloomtype)
dev.off()


###
### bloom type metrics
### 
head(E)
E$ice_retr_roll15 [is.na(E$ice_retr_roll15 )]<-0
E$glob_bloom_ice_diff<-E$peak_timing_all_log -E$ice_retr_roll15
E$gl_type<-'ice_full'
E$gl_type[E$glob_bloom_ice_diff>20]<-'ice_free'





ice_free_sum<- E %>% group_by(year, north_south,gl_type) %>% summarize(count = n())
table(ice_free_sum$year)
head(ice_free_sum)
tail(ice_free_sum)
colnames(ice_free_sum)[3]<-'type'

wide_type<-spread(ice_free_sum, key = type, value = count)
wide_type$ice_free[is.na(wide_type$ice_free)]<-0
wide_type$ice_full[is.na(wide_type$ice_full)]<-0

wide_type$perc_open_water<- (wide_type$ice_free/ (wide_type$ice_free+wide_type$ice_full))*100


head(wide_type)

### note that 2023 is a dummy of 2019 (or 2018)

bloomtype_TS<-ggplot(data=wide_type, aes(x = year, y = perc_open_water,color=north_south)) + 
  geom_line(linewidth = 3) + 
  scale_fill_manual(values=c('red3','dodgerblue'),na.value = 'grey90')+
  scale_color_manual(values=c('red3','dodgerblue'),na.value = 'grey90')+
  xlab("")+
  ylab("Chl-a bloom peak (day of year)")+
  #scale_y_continuous( limits=c(0,200))+
  facet_wrap(~ north_south,ncol=1)+
  theme(plot.title = element_text(size = 24),
        strip.text = element_text(size=20,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill='dodgerblue'), # Add the NOAA color blue to the facet strips.
        axis.title = element_text(size=26,family="sans"),
        axis.text = element_text(size=18,family="sans"),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black"))


png(filename="Chla/ESP/2023_indicators/Fig2_bloomtype_timeseriesESP_crab_missing2023.png",width = 1600, height = 1200,res=120)
plot(bloomtype_TS)
dev.off()


write_csv(wide_type, file='Chla/ESP/2023_indicators/bloom_type_ESP_contribution_missing2023.csv')

