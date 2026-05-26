library(ggplot2)
library(rerddap)
library(tidync)
library(dplyr)
library(lubridate)
library(scales)
library(cmocean)
library(ncdf4)
library(mapdata)
rm(list = ls()) 

# getting info for the analysis 
info_NR_chla<-rerddap::info(datasetid = "nesdisVHNchlaWeekly", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

min_long<- (-179.99)
max_long<- (-130)
min_lat<- (47.5)
max_lat<- (66)

#basin_dims_w
# min_long_w<- (167)
# max_long_w<- (179.99)
# min_lat_w<- (47.5)
# max_lat_w<- (60)

# 

all_ts_chla_2026 <- griddap(info_NR_chla, latitude = c(min_lat, max_lat), longitude = c(min_long, max_long), time = c('2026-02-10','last'), fields = 'chlor_a')

data_ts26<-as.data.frame(all_ts_chla_2026$data) %>%
  filter(chlor_a > 0)
data_ts26$dates<-as.Date(data_ts26$time)
max(data_ts26$dates)

saveRDS(data_ts26,file='inter_jens_datafiles/data_NR_chla_spring2026.RDS')

gc()
df<-readRDS('inter_jens_datafiles/data_NR_chla_spring2026.RDS')
range(df$dates)


###
###
df$chlor_a[df$chlor_a>30]<-NA

head(df)
tail(df)
hm<- df %>% group_by(dates) %>% summarise(non_na_count = sum(!is.na(chlor_a)))

plot(hm$dates,hm$non_na_count)


##
## 4 dates 
##
max1<- hm$dates[which.max(hm$non_na_count)]
day_excluder1<- seq(max1+days(-7),max1+days(7),by=1) 
hm2<-hm[!hm$dates %in% day_excluder1,] 

# getting max 2 day 
max2<- hm2$dates[which.max(hm2$non_na_count)]
day_excluder2<- seq(max2+days(-7),max2+days(7),by=1) 
hm3<-hm2[!hm2$dates %in% day_excluder2,] 

# getting max 3 day 
max3<- hm3$dates[which.max(hm3$non_na_count)]
day_excluder3<- seq(max3+days(-7),max3+days(7),by=1) 
hm4<-hm3[!hm3$dates %in% day_excluder3,] 

# getting max 4 day 
max4<- hm4$dates[which.max(hm4$non_na_count)]
rm(hm,hm2,hm3)


date_for_plot<-(c(max1,max2,max3,max4))
date_for_plot<-date_for_plot[order(date_for_plot)]
str(date_for_plot)

abline(v=date_for_plot)
abline(v=as.Date('2026-04-14'),col='red')

abline(v=as.Date('2026-05-07'),col='red')


date_for_plot<-as.Date(c("2026-03-29","2026-04-14","2026-05-07","2026-05-16"))

plot(hm$dates,hm$non_na_count)
abline(v=date_for_plot)


##
## ice 
##
info_coral<-rerddap::info(datasetid = "NOAA_DHW", url = "https://coastwatch.pfeg.noaa.gov/erddap/")
min_ice_long<- (-179)
max_ice_long<- (-155)
min_ice_lat<- (55)
max_ice_lat<- (66)

str(date_for_plot[1])

ice1 <- griddap(info_coral, latitude = c(min_ice_lat, max_ice_lat), longitude = c(min_ice_long, max_ice_long), time = c(paste(date_for_plot[1]),paste(date_for_plot[1])), fields = 'CRW_SEAICE')
ice2 <- griddap(info_coral, latitude = c(min_ice_lat, max_ice_lat), longitude = c(min_ice_long, max_ice_long), time = c(paste(date_for_plot[2]),paste(date_for_plot[2])), fields = 'CRW_SEAICE')
ice3 <- griddap(info_coral, latitude = c(min_ice_lat, max_ice_lat), longitude = c(min_ice_long, max_ice_long), time = c(paste(date_for_plot[3]),paste(date_for_plot[3])), fields = 'CRW_SEAICE')
ice4 <- griddap(info_coral, latitude = c(min_ice_lat, max_ice_lat), longitude = c(min_ice_long, max_ice_long), time = c(paste(date_for_plot[4]),paste(date_for_plot[4])), fields = 'CRW_SEAICE')

ice<-data.frame(rbind(ice1$data,ice2$data,ice3$data,ice4$data))
ice$dates<-as.Date(ice$time)
head(ice)

saveRDS(ice, "inter_jens_datafiles/ice_latest.RDS")

ice<-readRDS('inter_jens_datafiles/ice_latest.RDS')

all_sub<-df[df$dates %in% date_for_plot,]


##
##
##
# plot can be improved # 
w <- map_data("world2", ylim = c(55, 66), xlim = c(-175+360, -155+360))
breaks_w2<-c(185,190,195,200) # working with 0-360 lons - helpful across the dateline. We can change that.
labels_w2<-breaks_w2-360 #

# making map plots - we can tweak this
maps_plot1<- ggplot() +
  geom_point(data = all_sub, aes(x = longitude+360, y = latitude, color =sqrt(chlor_a)),pch=15) +
  scale_color_gradientn(colours = (cmocean('thermal')(200)),name = "",limits=c(0,4.5)) +
  geom_raster(data = ice, aes(x = longitude+360, y = latitude, fill =(CRW_SEAICE)),interpolate = FALSE) +
  scale_fill_gradientn(colours = (cmocean('ice')(200)),name = "",na.value="transparent") +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  scale_x_continuous("Longitude", breaks=breaks_w2, labels=labels_w2, limits=c(140,250))+
  theme_bw() + ylab("latitude") + xlab("longitude") +
  facet_wrap(.~dates,ncol=2)+
  coord_fixed(1.3, xlim = c(-179+360, -155+360),  ylim = c(55, 66)) + ggtitle('Chla Bering Sea')


windows(8,10)
maps_plot1

###############
## goa plots ##
###############
w3 <- map_data("world", ylim = c(45, 60), xlim = c(-179, -140))
breaks_w3<-c(-175,-170,-160,-155,-150,-145,-140) # working with 0-360 lons - helpful across the dateline. We can change that.
labels_w3<-breaks_w3

# making map plots - we can tweak this
maps_plot3<- ggplot() +
  geom_point(data = all_sub, aes(x = longitude, y = latitude, color =sqrt(chlor_a)),pch=15) +
  scale_color_gradientn(colours = (cmocean('thermal')(200)),name = "") +
  geom_raster(data = ice, aes(x = longitude, y = latitude, fill =(CRW_SEAICE)),interpolate = FALSE) +
  scale_fill_gradientn(colours = (cmocean('ice')(200)),name = "",na.value="transparent") +
  geom_polygon(data = w3, aes(x = long, y = lat, group = group), fill = "grey80") +
  scale_x_continuous("Longitude", breaks=breaks_w3, labels=labels_w3, limits=c(-179,-140))+
  theme_bw() + ylab("latitude") + xlab("longitude") +
  facet_wrap(.~dates,ncol=2)+
  coord_fixed(1.2, xlim = c(-179, -140),  ylim = c(50, 60)) + ggtitle('GOA')


windows(12,8)
maps_plot3



# change to M2 etc 

df$N_S<-'north'
df$N_S[df$latitude<60.00]<-'south'

#avg_chla<- df %>% group_by(N_S,dates) %>%arrange(dates) %>% summarize(mean_chl = mean(chlor_a,na.rm=TRUE),
                                                                   #    sd_chl = sd(chlor_a,na.rm=TRUE))
# (\(56.87^{\circ }\text{N}\), \(-164.06^{\circ }\text{W}\)). 

m2_area<- df %>% group_by(dates) %>%arrange(dates) %>% filter(latitude>56.5 & latitude <57.5 & longitude >(-165.0) & longitude <(-163.0))  %>%
                                                      summarize(mean_chl = mean(chlor_a,na.rm=TRUE),
                                                                     sd_chl = sd(chlor_a,na.rm=TRUE))
head(ice)

icem2 <- griddap(info_coral, latitude = c(56.5, 57.5), longitude = c(-165.0, -163.0), time = c('2026-01-01','last'), fields = 'CRW_SEAICE')

ice_df_m2<-icem2$data
ice_df_m2$dates<-as.Date(ice_df_m2$time)


m2_ice<- ice_df_m2 %>% group_by(dates) %>%arrange(dates) %>% #filter(latitude>55.0 & latitude <58.0 & longitude >(-165) & longitude <(-163.0))  %>%
  summarize(CRW_SEAICE_m = mean(CRW_SEAICE,na.rm=TRUE),
            CRW_SEAICE_sd = sd(CRW_SEAICE,na.rm=TRUE))


head(all)

head(m2_area)
ggplot() +
  geom_ribbon(data=m2_area, aes(x = dates,  ymin = mean_chl  - sd_chl, ymax = mean_chl  + sd_chl), alpha = 0.3) +
  geom_line(data=m2_area, aes(x = dates,  y = mean_chl))+  
  geom_line(data=m2_ice, aes(x = dates,  y = CRW_SEAICE_m),col='red3')+  
  
  xlab('Dates')+
  scale_x_date(date_breaks = "1 month", 
               labels=date_format("%b-%Y"),
               limits = as.Date(c('2026-01-01','2026-06-01')))+
  ylab('Chla [ug / l]')+
  theme(plot.title = element_text(size = 10),
        strip.text = element_text(size=10,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill='dodgerblue'), # Add the NOAA color blue to the facet strips.
        axis.title = element_text(size=10,family="sans"),
        axis.text = element_text(size=10,family="sans",color='black'),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black"),
        legend.title = element_text(size=10))#+
 # facet_wrap(.~N_S,ncol=1,scales = "free") # 

windows(8,10)
timeseries_chlaNR


# M2 peak 
# date ~ 2026-04-17 when ice is gone - day of year 107
# yearly 


##
##

# east_path <- data.frame(LON = c(-152, -151, -151, -152, -152),
#                         LAT = c(58.25, 58.25, 57.75, 57.75, 58.25))
#
# south_path <- data.frame(LON = c(-157, -156, -156, -157, -157),
#                         LAT = c(56.5, 56.5, 56, 56, 56.5))
#
# west_path <- data.frame(LON = c(-155, -154, -154, -154.5, -154.5, -155.5, -155.5, -155, -155),
#                         LAT = c(58, 58, 57.75, 57.75, 57.5, 57.5, 57.75, 57.75, 58))


goa_shel<- df %>% group_by(dates) %>%arrange(dates) %>% filter(latitude>57.0 & latitude <58.0 & longitude >(-155.5) & longitude <(-154.0)) %>%
  summarize(mean_chl = mean(chlor_a,na.rm=TRUE),
            sd_chl = sd(chlor_a,na.rm=TRUE))




ggplot(goa_shel, aes(x = dates, y = mean_chl )) +
  geom_ribbon(aes(ymin = mean_chl  - sd_chl, ymax = mean_chl  + sd_chl), alpha = 0.3) +
  geom_line()+  
  xlab('Dates')+
  scale_x_date(date_breaks = "1 month", 
               labels=date_format("%b-%Y"),
               limits = as.Date(c('2026-01-01','2026-06-01')))+
  geom_vline(xintercept = as.Date("2026-04-21"), linetype = "dashed", color = "red")+
  ylab('Chla [ug / l]')+
  theme(plot.title = element_text(size = 10),
        strip.text = element_text(size=10,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill='dodgerblue'), # Add the NOAA color blue to the facet strips.
        axis.title = element_text(size=10,family="sans"),
        axis.text = element_text(size=10,family="sans",color='black'),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black"),
        legend.title = element_text(size=10))#+
# facet_wrap(.~N_S,ncol=1,scales = "free") # 

windows(8,10)
timeseries_chlaNR




goa_bl<-read.csv('inter_jens_datafiles/bloom_timingGOA.csv')


goa_bl_west<-goa_bl[goa_bl$region=='west',]

df_2026<-as.data.frame(cbind(year=c(2026), doy=112))

windows(9,6)
ggplot() +
 geom_line(data=goa_bl_west[goa_bl_west$method=='threshold1',], aes(x = year, y = doy ),size=1.4)+
  geom_point(data=goa_bl_west[goa_bl_west$method=='threshold1',], aes(x = year, y = doy ),size=4)+

  geom_point(data=df_2026,aes(x=2026,y=doy),col='red3',size=5)+
  xlab('Year')+
  #scale_x_date(date_breaks = "1 month", 
  #             labels=date_format("%b-%Y"),
  #             limits = as.Date(c('2026-01-01','2026-06-01')))+
  xlim(1998,2026)+
  ylab('Chla [ug / l]')+
  theme(plot.title = element_text(size = 16),
        strip.text = element_text(size=16,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill='dodgerblue'), # Add the NOAA color blue to the facet strips.
        axis.title = element_text(size=16,family="sans"),
        axis.text = element_text(size=16,family="sans",color='black'),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black"),
        legend.title = element_text(size=16))

