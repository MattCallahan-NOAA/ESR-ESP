library(ggplot2)
library(mapdata)
library(slider)
library(rerddap)
library(ncdf4)
library(tidync)
library(dplyr)
library(lubridate)

rm(list = ls()) 

# getting info for the analysis 
info_NR_chla<-rerddap::info(datasetid = "nesdisVHNchlaWeekly", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

#basin_dims
min_long<- (-173)
max_long<- (-165)
min_lat<- (58)
max_lat<- (62)


# 
recent_day_chl <- griddap(info_NR_chla, latitude = c(min_lat, max_lat), longitude = c(min_long, max_long), time = c('last','last'), fields = 'chlor_a')

df<-as.data.frame(recent_day_chl$data)
head(df)
df$dates<-as.Date(df$time)

# setting up the long-term data to include all dates until 2nd last days. Once its updates automateically we should change this
dummy_2nd_last_day<-df$dates[1]-days(1)
# getting all data for 2024 (This should be just once)
# note i set the last date 
all_ts_chla_2024 <- griddap(info_NR_chla, latitude = c(min_lat, max_lat), longitude = c(min_long, max_long), time = c('2024-02-10',paste(dummy_2nd_last_day)), fields = 'chlor_a')
data_ts24<-as.data.frame(all_ts_chla_2024$data)
data_ts24$dates<-as.Date(data_ts24$time)
range(data_ts24$dates)

# once setup we should just store all_ts_chla_2024 internally and call it as RDS?



# combining "long-terrm data" and "last day data" 
all<-data.frame(rbind(data_ts24,df) )
#nrow(all)
#nrow(df)+nrow(data_ts24)
range(all$dates)
#table(is.na(all$chlor_a),all$dates)

# removing crzay high chl values #
all<-all[all$chlor_a<30,]

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

latest_date<-df$dates[1]

all_sub<-all[all$dates %in% date_for_plot,]

# plot can be improved # 

mycolor <- colors$temperature
w <- map_data("worldHires", ylim = c(55, 66), xlim = c(-170, -155))

maps_plot1<- ggplot(data = all_sub, aes(x = longitude, y = latitude, fill =sqrt(chlor_a))) +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colours = mycolor, na.value = NA) +
  theme_bw() + ylab("latitude") + xlab("longitude") +
  facet_wrap(.~dates,ncol=2)+
  coord_fixed(1.3, xlim = c(-170, -155),  ylim = c(55, 66)) + ggtitle('Chla Bering Sea')

windows(10,8)
maps_plot1

# time series #
# I have not grouped this into north and south. And we want to mask out the inner shelf. Could just pick certain bsierp regions? We can talk

# dummy N and S
all$N_S<-'north'
all$N_S[all$latitude<60.00]<-'south'

avg_chla<- all %>% group_by(N_S,dates) %>%arrange(dates) %>% summarize(mean_chl = mean(chlor_a,na.rm=TRUE),
                                                                       sd_chl = sd(chlor_a,na.rm=TRUE))






