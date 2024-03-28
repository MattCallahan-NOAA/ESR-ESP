library(ggplot2)
library(mapdata)
library(slider)
library(rerddap)
library(ncdf4)
library(tidync)
library(dplyr)
library(lubridate)
library(scales)
library(cmocean)



rm(list = ls()) 

# getting info for the analysis 
info_NR_chla<-rerddap::info(datasetid = "nesdisVHNchlaWeekly", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

#basin_dims
min_long<- (-179)
max_long<- (-155)
min_lat<- (55)
max_lat<- (66)


# 
recent_day_chl <- griddap(info_NR_chla, latitude = c(min_lat, max_lat), longitude = c(min_long, max_long), time = c('last','last'), fields = 'chlor_a')

df<-as.data.frame(recent_day_chl$data)
head(df)
df$dates<-as.Date(df$time)

# setting up the long-term data to include all dates until 2nd last days. Once its updates automateically we should change this
dummy_2nd_last_day<-df$dates[1]-days(1)
# getting all data for 2024 (This should be just once)
# note i set the last date 

# matt you need to run this obvuiously once 
#all_ts_chla_2024 <- griddap(info_NR_chla, latitude = c(min_lat, max_lat), longitude = c(min_long, max_long), time = c('2024-02-10',paste(dummy_2nd_last_day)), fields = 'chlor_a')
#data_ts24<-as.data.frame(all_ts_chla_2024$data)
#data_ts24$dates<-as.Date(data_ts24$time)
#saveRDS(data_ts24,file='inter_jens_datafiles/data_NR_chla_spring2024.RDS')

data_ts24<-readRDS('inter_jens_datafiles/data_NR_chla_spring2024.RDS')
range(data_ts24$dates)

# once setup we should just store all_ts_chla_2024 internally and call it as RDS?
table(is.na(all$time))


# combining "long-terrm data" and "last day data" 
all<-data.frame(rbind(data_ts24,df) )
#nrow(all)
#nrow(df)+nrow(data_ts24)
range(all$dates)
#table(is.na(all$chlor_a),all$dates)

head(all)
# removing crzay high chl values #
all$chlor_a[all$chlor_a>30]<-NA

head(all)
tail(all)
hm<- all %>% group_by(dates) %>% summarise(non_na_count = sum(!is.na(chlor_a)))


## Matt this - can be nicer I think. But I had trouble automizing picking max pixel dates - that were at least x nb of days apart
max1<- hm$dates[which.max(hm$non_na_count)]
day_excluder1<- seq(max1+days(-3),max1+days(3),by=1) 
hm2<-hm[!hm$dates %in% day_excluder1,] 

# getting max 2 day 
max2<- hm2$dates[which.max(hm2$non_na_count)]
day_excluder2<- seq(max2+days(-3),max2+days(3),by=1) 
hm3<-hm2[!hm2$dates %in% day_excluder2,] 

# getting max 3 day 
max3<- hm3$dates[which.max(hm3$non_na_count)]
day_excluder3<- seq(max3+days(-3),max3+days(3),by=1) 
hm4<-hm3[!hm3$dates %in% day_excluder3,] 

# getting max 4 day 
max4<- hm4$dates[which.max(hm4$non_na_count)]
rm(hm,hm2,hm3)


# picking and ordering the dates for plotting
# we could alter this to plot the most recent data
date_for_plot<-(c(max1,max2,max3,max4))
date_for_plot<-date_for_plot[order(date_for_plot)]
str(date_for_plot)

########################################
### getting ice data for select dates 
########################################
info_coral<-rerddap::info(datasetid = "NOAA_DHW", url = "https://coastwatch.pfeg.noaa.gov/erddap/")


str(date_for_plot[1])

ice1 <- griddap(info_coral, latitude = c(min_lat, max_lat), longitude = c(min_long, max_long), time = c(paste(date_for_plot[1]),paste(date_for_plot[1])), fields = 'CRW_SEAICE')
ice2 <- griddap(info_coral, latitude = c(min_lat, max_lat), longitude = c(min_long, max_long), time = c(paste(date_for_plot[2]),paste(date_for_plot[2])), fields = 'CRW_SEAICE')
ice3 <- griddap(info_coral, latitude = c(min_lat, max_lat), longitude = c(min_long, max_long), time = c(paste(date_for_plot[3]),paste(date_for_plot[3])), fields = 'CRW_SEAICE')
ice4 <- griddap(info_coral, latitude = c(min_lat, max_lat), longitude = c(min_long, max_long), time = c(paste(date_for_plot[4]),paste(date_for_plot[4])), fields = 'CRW_SEAICE')

ice<-data.frame(rbind(ice1$data,ice2$data,ice3$data,ice4$data))
ice$dates<-as.Date(ice$time)
head(ice)


# setting up plot 
latest_date<-df$dates[1]
all_sub<-all[all$dates %in% date_for_plot,]

# plot can be improved # 
w <- map_data("world2Hires", ylim = c(55, 66), xlim = c(-175+360, -155+360))
breaks_w2<-c(185,190,195,200) # working with 0-360 lons - helpful across the dateline. We can change that.
labels_w2<-breaks_w2-360 #

# making map plots - we can tweak this
maps_plot1<- ggplot() +
  geom_point(data = all_sub, aes(x = longitude+360, y = latitude, color =sqrt(chlor_a)),pch=15) +
  scale_color_gradientn(colours = (cmocean('algae')(200)),name = "") +
  geom_raster(data = ice, aes(x = longitude+360, y = latitude, fill =(CRW_SEAICE)),interpolate = FALSE) +
  scale_fill_gradientn(colours = (cmocean('ice')(200)),name = "",na.value="transparent") +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  scale_x_continuous("Longitude", breaks=breaks_w2, labels=labels_w2, limits=c(140,250))+
  theme_bw() + ylab("latitude") + xlab("longitude") +
  facet_wrap(.~dates,ncol=2)+
  coord_fixed(1.3, xlim = c(-179+360, -155+360),  ylim = c(55, 66)) + ggtitle('Chla Bering Sea')


windows(8,10)
maps_plot1

# time series #
# dummy N and S
all$N_S<-'north'
all$N_S[all$latitude<60.00]<-'south'

avg_chla<- all %>% group_by(N_S,dates) %>%arrange(dates) %>% summarize(mean_chl = mean(chlor_a,na.rm=TRUE),
                                                                       sd_chl = sd(chlor_a,na.rm=TRUE))

head(all)

head(avg_chla)
timeseries_chlaNR<-ggplot(avg_chla, aes(x = dates, y = mean_chl )) +
  geom_ribbon(aes(ymin = mean_chl  - sd_chl, ymax = mean_chl  + sd_chl), alpha = 0.3) +
  geom_line()+  
  xlab('Dates')+
  scale_x_date(date_breaks = "1 month", 
               labels=date_format("%b-%Y"),
               limits = as.Date(c('2024-01-01','2024-06-01')))+
  ylab('Chla [ug / l]')+
  theme(plot.title = element_text(size = 10),
        strip.text = element_text(size=10,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill='dodgerblue'), # Add the NOAA color blue to the facet strips.
        axis.title = element_text(size=10,family="sans"),
        axis.text = element_text(size=10,family="sans",color='black'),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black"),
        legend.title = element_text(size=10))+
  facet_wrap(.~N_S,ncol=1,scales = "free") # 

windows(8,10)
timeseries_chlaNR


gc()
