library(dplyr)
library(tidyverse)
library(zoo)
library(data.table)
library(cmocean)
# 
f25 <- readRDS("inter_jens_datafiles/bloomTimingOCCCI_1998_2024.RDS")
i25 <- readRDS("inter_jens_datafiles/iceRetreatTiming_1998_2025.RDS")

head(f25)
head(i25)

head(f25)
# crop for Erins paper - we might still need to fix this # 
E<- f25[f25$jens_grid   %in% c(39,62:64,84:88,103:112,125:135,147:153, 170:175,193:196,215:218,237:241,
                               260:264,281:288,304:311,327:334,350:358,374:381,398:405,422:428,446:450,470:473,494:495),]

E2<- i25[i25$jens_grid   %in% c(39,62:64,84:88,103:112,125:135,147:153, 170:175,193:196,215:218,237:241,
                                260:264,281:288,304:311,327:334,350:358,374:381,398:405,422:428,446:450,470:473,494:495),]




E_data<- E2 %>% full_join(E, by=c('jens_grid','year'))

head(E_data)


grid_ll <- readRDS("inter_jens_datafiles/glob_bloom_type_DECISION_tree_data_feb2023.RDS")
head(grid_ll)
grid<-grid_ll %>% group_by(gridid) %>% summarise(lat = mean(gl_lat,na.rm=TRUE),
                                                 lon = mean(gl_lon,na.rm=TRUE))


colnames(grid)[1]<-'jens_grid'

grid$jens_grid<-as.character(grid$jens_grid)

Edf<- E_data %>% left_join(grid, by=c('jens_grid'))

head(Edf)


###
###
###
head(Edf)
Edf$bloom_ice_diff<-Edf$peak_timing_all_log -Edf$ice_retr_roll15
Edf$bloomtype<-'ice_full'
Edf$bloomtype[Edf$bloom_ice_diff>20]<-'ice_free'


Edf$north_south<-'north'
Edf$north_south[Edf$lat<60.0001]<-'south'


head(Edf)


ice_free_sum<- Edf %>% group_by(year, north_south,bloomtype) %>% summarize(count = n())
table(ice_free_sum$year)
head(ice_free_sum)
tail(ice_free_sum)
colnames(ice_free_sum)[3]<-'type'

wide_type<-spread(ice_free_sum, key = type, value = count)
wide_type$ice_free[is.na(wide_type$ice_free)]<-0
wide_type$ice_full[is.na(wide_type$ice_full)]<-0

wide_type$perc_open_water<- (wide_type$ice_free/ (wide_type$ice_free+wide_type$ice_full))*100

tail(wide_type)

check24 <- read.csv('Chla/ESP/2024_indicators/bloom_type_ESP_crab_middle_outer_with2024.csv')

head(check24)

###
###
###

bloomtype_compare<-ggplot(data=wide_type[wide_type$year<2025,], aes(x = year, y = perc_open_water,color=north_south)) + 
  geom_line(linewidth = 3) + 
  scale_fill_manual(values=c('red3','dodgerblue'),na.value = 'grey90')+
  scale_color_manual(values=c('red3','dodgerblue'),na.value = 'grey90')+
  geom_line(data=check24,aes(x = year, y = perc_open_water),color='black',linewidth=1.5) + 
  
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


png(filename="Chla/ESP/2025_indicators/OCCCI_bloomtype_timeseriesESP_crab_middle_outer.png",width = 1600, height = 1200,res=120)
plot(bloomtype_compare)
dev.off()


write_csv(wide_type, file='Chla/ESP/2025_indicators/OCCCI_bloom_type_ESP_crab_middle_outer_with2024.csv')

############################
### compare bloom timing ###
############################
colnames(Edf)
bloomTiming_occci <- Edf %>% group_by(year, north_south) %>% summarize(mean_peak=mean(peak_timing_all_log,na.rm=TRUE),
                                                                 sd_peak=sd(peak_timing_all_log,na.rm=TRUE))

# old data 
old_opie_timing<- read.csv('Chla/ESP/2025_indicators/bloom_timing_NS_OPIE_2024.csv')
head(old_opie_timing)
windows()
bloomtype_compare_shade<-ggplot() +
  geom_ribbon(data=bloomTiming_occci, aes(x = year, y = mean_peak        ,ymin = mean_peak         - sd_peak, ymax = mean_peak         + sd_peak), alpha = 0.3,fill='red3') +
  geom_line(data=bloomTiming_occci, aes(x = year, y = mean_peak       ),color='red3')+  
  geom_ribbon(data=old_opie_timing, aes(x = year , y = mean_peak  ,ymin = mean_peak   - sd, ymax = mean_peak   + sd), alpha = 0.3) +
  geom_line(data=old_opie_timing, aes(x = year , y = mean_peak ))+  
  xlab('Years')+
  ylab('peak timing (day of year)')+
  ggtitle('new (red), old (black)') +
  theme(plot.title = element_text(size = 10),
        strip.text = element_text(size=10,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill='dodgerblue'), # Add the NOAA color blue to the facet strips.
        axis.title = element_text(size=10,family="sans"),
        axis.text = element_text(size=10,family="sans",color='black'),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black"),
        legend.title = element_text(size=10))+
  facet_wrap(.~north_south,ncol=1)


png(filename="Chla/ESP/2025_indicators/OCCCI_bloomtype_timeseriesESP_crab_middle_outer_shaded.png",width = 1600, height = 1200,res=120)
plot(bloomtype_compare_shade)
dev.off()


bloomtype_compare<-ggplot() +
  #geom_ribbon(data=bloomTiming_occci, aes(x = year, y = mean_peak        ,ymin = mean_peak         - sd_peak, ymax = mean_peak         + sd_peak), alpha = 0.3,fill='red3') +
  geom_line(data=bloomTiming_occci, aes(x = year, y = mean_peak       ),color='red3')+  
  #geom_ribbon(data=old_opie_timing, aes(x = year , y = mean_peak  ,ymin = mean_peak   - sd, ymax = mean_peak   + sd), alpha = 0.3) +
  geom_line(data=old_opie_timing, aes(x = year , y = mean_peak ))+  
  xlab('Years')+
  ylab('peak timing (day of year)')+
  ggtitle('new (red), old (black)') +
  theme(plot.title = element_text(size = 10),
        strip.text = element_text(size=10,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill='dodgerblue'), # Add the NOAA color blue to the facet strips.
        axis.title = element_text(size=10,family="sans"),
        axis.text = element_text(size=10,family="sans",color='black'),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black"),
        legend.title = element_text(size=10))+
  facet_wrap(.~north_south,ncol=1)



png(filename="Chla/ESP/2025_indicators/OCCCI_bloomtype_timeseriesESP_crab_middle_outer.png",width = 1600, height = 1200,res=120)
plot(bloomtype_compare)
dev.off()

write_csv(bloomTiming_occci, file='Chla/ESP/2025_indicators/OCCCI_bloomTiming_occci_ESP_crab_middle_outer_with2024.csv')
