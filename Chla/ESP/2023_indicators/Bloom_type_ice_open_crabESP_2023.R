library(dplyr)
library(tidyverse)
library(zoo)
library(data.table)
library(cmocean)


# 
f23 <- readRDS("inter_jens_datafiles/bloomTimingGlob_1998_2023.RDS")
i23 <- readRDS("inter_jens_datafiles/iceRetreatTiming_1998_2023.RDS")

head(f23)
head(i23)

head(f23)
# crop for Erins paper - we might still need to fix this # 
E<- f23[f23$jens_grid   %in% c(39,62:64,84:88,103:112,125:135,147:153, 170:175,193:196,215:218,237:241,
                         260:264,281:288,304:311,327:334,350:358,374:381,398:405,422:428,446:450,470:473,494:495),]

E2<- i23[i23$jens_grid   %in% c(39,62:64,84:88,103:112,125:135,147:153, 170:175,193:196,215:218,237:241,
                               260:264,281:288,304:311,327:334,350:358,374:381,398:405,422:428,446:450,470:473,494:495),]




E_data<- E2 %>% full_join(E, by=c('jens_grid','year'))



## getting  lat / longs from the jens grid

grid_ll <- readRDS("inter_jens_datafiles/glob_bloom_type_DECISION_tree_data_feb2023.RDS")
head(grid_ll)
grid<-grid_ll %>% group_by(gridid) %>% summarise(lat = mean(gl_lat,na.rm=TRUE),
                                                 lon = mean(gl_lon,na.rm=TRUE))


colnames(grid)[1]<-'jens_grid'

grid$jens_grid<-as.character(grid$jens_grid)

Edf<- E_data %>% left_join(grid, by=c('jens_grid'))

head(Edf)



Edf$north_south<-'north'
Edf$north_south[Edf$lat<60.0001]<-'south'

library(data.table)
setDT(Edf)
agg_text<-Edf[ !is.na(jens_grid), lapply(.SD, function(x) {
  if(is.numeric(x)) mean(x, na.rm = TRUE) else x[!is.na(x)][1L]}), by = list(jens_grid)]



data_mapH<-map_data("world2") # map_data from ggmap mapping package
breaks_w2<-c(185,190,195,200) 
labels_w2<-breaks_w2-360
head(agg_text)
# #

#windows(24,20)
map_bloomtype<- ggplot()+
  coord_equal(xlim=c(181,203),ylim=c(54.8,66),ratio = 1.8)+ # big map coordinates
  geom_text(data = agg_text, aes(x = lon+360, y =lat,label=as.character(jens_grid),col=north_south),size=5)+
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
head(Edf)
Edf$bloom_ice_diff<-Edf$peak_timing_all_log -Edf$ice_retr_roll15
Edf$bloomtype<-'ice_full'
Edf$bloomtype[Edf$bloom_ice_diff>20]<-'ice_free'



ice_free_sum<- Edf %>% group_by(year, north_south,bloomtype) %>% summarize(count = n())
table(ice_free_sum$year)
head(ice_free_sum)
tail(ice_free_sum)
colnames(ice_free_sum)[3]<-'type'

wide_type<-spread(ice_free_sum, key = type, value = count)
wide_type$ice_free[is.na(wide_type$ice_free)]<-0
wide_type$ice_full[is.na(wide_type$ice_full)]<-0

wide_type$perc_open_water<- (wide_type$ice_free/ (wide_type$ice_free+wide_type$ice_full))*100


check <- read.csv('inter_jens_datafiles/test_bloom.csv')
check
check
head(wide_type)
tail(check)
check[51:52,5]<-NA



### note that 2023 is a dummy of 2019 (or 2018)

bloomtype_TS<-
ggplot(data=wide_type, aes(x = year, y = perc_open_water,color=north_south)) + 
  geom_line(linewidth = 3) + 
  scale_fill_manual(values=c('red3','dodgerblue'),na.value = 'grey90')+
  scale_color_manual(values=c('red3','dodgerblue'),na.value = 'grey90')+
  #geom_line(data=check,aes(x = year, y = perc_open_water),color='black',size=1.5) + 
  
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


png(filename="Chla/ESP/2023_indicators/Fig2_bloomtype_timeseriesESP_crab_missing2023_final.png",width = 1600, height = 1200,res=120)
plot(bloomtype_TS)
dev.off()


#write_csv(wide_type, file='Chla/ESP/2023_indicators/bloom_type_ESP_contributio_with2023.csv')











###
### bloom type by super region
###
ff23 <- readRDS("inter_jens_datafiles/bloomTimingGlob_1998_2023.RDS")
ii23 <- readRDS("inter_jens_datafiles/iceRetreatTiming_1998_2023.RDS")


bl<- ff23 %>% full_join(ii23, by=c('jens_grid','year'))


bl<-bl[complete.cases(bl$peak_timing_all_log),]
bl<-bl[complete.cases(bl$bsierp_super_region),]


bl$bloom_ice_diff<-bl$peak_timing_all_log -bl$ice_retr_roll15
bl$bloomtype<-'ice_full'
bl$bloomtype[bl$bloom_ice_diff>20]<-'ice_free'



bl_sum<- bl %>% group_by(year, bsierp_super_region,bloomtype) %>% summarize(count = n())


table(bl_sum$year)
head(bl_sum)
tail(bl_sum)
colnames(bl_sum)[3]<-'type'

wide_type2<-spread(bl_sum, key = type, value = count)
wide_type2$ice_free[is.na(wide_type2$ice_free)]<-0
wide_type2$ice_full[is.na(wide_type2$ice_full)]<-0

wide_type2$perc_open_water<- (wide_type2$ice_free/ (wide_type2$ice_free+wide_type2$ice_full))*100




bloomtype_region<-
  ggplot(data=wide_type2, aes(x = year, y = perc_open_water,color=bsierp_super_region)) + 
  geom_line(linewidth = 3) + 
  #scale_fill_manual(values=c('red3','dodgerblue'),na.value = 'grey90')+
  #scale_color_manual(values=c('red3','dodgerblue'),na.value = 'grey90')+
  #geom_line(data=check,aes(x = year, y = perc_open_water),color='black',size=1.5) + 
  
  xlab("")+
  ylab("Percent open water blooms")+
  #scale_y_continuous( limits=c(0,200))+
  facet_wrap(~ bsierp_super_region,ncol=3)+
  theme(plot.title = element_text(size = 24),
        strip.text = element_text(size=20,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill='dodgerblue'), # Add the NOAA color blue to the facet strips.
        axis.title = element_text(size=26,family="sans"),
        axis.text = element_text(size=18,family="sans"),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black"))



windows(14,8)
bloomtype_region


write.csv(wide_type2,file='inter_jens_datafiles/bloom_type_superregion_forDaveSept2023.csv') 

