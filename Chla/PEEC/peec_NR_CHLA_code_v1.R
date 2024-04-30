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

#basin_dims_e
# min_long<- (-179)
# max_long<- (-155)
# min_lat<- (55)
# max_lat<- (66)

min_long<- (-179.99)
max_long<- (-130)
min_lat<- (47.5)
max_lat<- (66)

#basin_dims_w
min_long_w<- (167)
max_long_w<- (179.99)
min_lat_w<- (47.5)
max_lat_w<- (60)

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
all_ts_chla_2024 <- griddap(info_NR_chla, latitude = c(min_lat, max_lat), longitude = c(min_long, max_long), time = c('2024-02-10','last'), fields = 'chlor_a')
data_ts24<-as.data.frame(all_ts_chla_2024$data) %>%
  filter(chlor_a > 0)
data_ts24$dates<-as.Date(data_ts24$time)

all_ts_chla_2024_w <- griddap(info_NR_chla, latitude = c(min_lat_w, max_lat_w), longitude = c(min_long_w, max_long_w), time = c('2024-02-10','last'), fields = 'chlor_a')
data_ts24_w<-as.data.frame(all_ts_chla_2024_w$data) %>%
  filter(chlor_a> 0)
data_ts24_w$dates<-as.Date(data_ts24_w$time)

data_ts24 <- data_ts24 %>%
  bind_rows(data_ts24_w) 

saveRDS(data_ts24,file='data/viirs/data_NR_chla_spring2024.RDS')

data_ts24<-readRDS('data/viirs/data_NR_chla_spring2024.RDS')
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

saveRDS(ice, "data/ice_latest.RDS")


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


#### Matt edits ####
#### get weekly averages for 2012-2023 ####
# updated to get whole AK EEZ
min_long<- (167)
max_long<- (230)
min_lat<- (47.5)
max_lat<- (66)
#download by year
myyear <- 2012:2023

options(timeout = 600)

for(i in myyear){
  file_name <- paste0("data/viirs/viirs_sq_",i,".nc")
  download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQchlaWeekly_Lon0360.nc?chlor_a%5B(",
                             i,"-02-10T00:00:00Z):(", i,"-10-31T12:00:00Z)%5D%5B(0.0):1:(0.0)%5D%5B(",min_lat,"):1:(",max_lat,")%5D%5B(",min_long,"):1:(",max_long,")%5D"),
                method = "libcurl", mode="wb",destfile = file_name)
}




# function to bring into data frame
tidy_chl<-function(file) {
  tidync(file) %>% 
    hyper_tibble() %>% 
    filter(chlor_a >0) %>%
    mutate(date=as_datetime(time),
           read_date=as.character(date),
           year=year(date),
           # month=month(date),
           # lon360=longitude,
           longitude=ifelse(longitude<180, longitude, longitude-360),
           join_lon=as.numeric(ifelse(longitude<0, #ifelse statement keeps +- lons the same length
                                  substr(longitude,1,8),
                                  substr(longitude,1,7))), 
           join_lat=as.numeric(substr(latitude,1,6)), 
           chlorophyll=round(chlor_a,3)) %>%
    dplyr::select(read_date, year, join_lon, join_lat, chlorophyll)
}
# join with lookup table (year by year)
# add domain
viirs_lkp <-readRDS("data/viirs/viirs_lkp_122623.RDS")

viirs_lkp <- viirs_lkp %>%
  mutate(domain = ifelse(ecosystem_area != "Eastern Bering Sea", NA,
                         ifelse(depth >= -50, "Inner Domain",
                                ifelse(depth >= -100, "Middle Domain", "Outer Domain"))))

saveRDS(viirs_lkp, "data/viirs/viirs_lkp_04222024.RDS")

join_fun <- function(data) {
  data %>%
    inner_join(viirs_lkp %>% dplyr::select(join_latitude, join_longitude, depth, ecosystem_subarea, ecosystem_area, domain), by=c("join_lat"="join_latitude", "join_lon"="join_longitude"))
}

# filter for shelf (30-200m) except in the AI
# average for ESR subregions
aggregate_fun <- function(data) {
  data %>%
    filter(if_else(ecosystem_area == "Aleutian Islands", depth <= -30, 
                   if_else(ecosystem_area == "Gulf of Alaska", depth <=-30 & depth >= -500, depth <= -30 & depth >= -200))) %>%
    group_by(read_date, year, ecosystem_area, ecosystem_subarea, domain) %>%
    summarize(mean_chla=mean(chlorophyll, na.rm=TRUE),
              n_chla = n())
}

# combine averages
# test with 2013
t13 <- tidy_chl("data/viirs/viirs_sq_2013.nc")
max(t13$join_lat)
nrow(t13 %>% filter(chlorophyll >0))
t13 %>%
  group_by(read_date) %>%
  summarize(n=n())
t13 <- t13 %>% join_fun()
t13 <- t13 %>% aggregate_fun()

weekly_avg_chl <- lapply(myyear, FUN=function(x) 
  tidy_chl(paste0("data/viirs/viirs_sq_",x,".nc")) %>%
    join_fun() %>%
    aggregate_fun()) %>%
  bind_rows()
         
saveRDS(weekly_avg_chl, "data/viirs/viirs_weekly_avg_2012_2023.RDS")

#### add 2024 to the time series ####
weekly_avg_chl <- readRDS("data/viirs/viirs_weekly_avg_2012_2023.RDS")

data_ts24 <-readRDS("data/viirs/data_NR_chla_spring2024.RDS")

data_ts24_sum <- data_ts24 %>%
  mutate(read_date = as.character(dates),
         year = year(dates),
         join_lon=as.numeric(ifelse(longitude<0, #ifelse statement keeps +- lons the same length
                                    substr(longitude,1,8),
                                    substr(longitude,1,7))), 
         join_lat=as.numeric(substr(latitude,1,6)),
         chlorophyll=round(chlor_a,3)) %>%
  join_fun()%>%
  aggregate_fun()

latest_ts <-weekly_avg_chl %>%
  bind_rows(data_ts24_sum)

saveRDS(latest_ts, "data/viirs/viirs_latest_ts.RDS")

  
# Is there a lag in the R package

#This pulls later data than the R package!
download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNchlaWeekly.nc?chlor_a%5B(last)%5D%5B(0.0):1:(0.0)%5D%5B(",min_lat,"):1:(",max_lat,")%5D%5B(",min_long,"):1:(",max_long,")%5D"),
              method = "libcurl", mode="wb",destfile = "data/viirs/viirs_latest.nc")

viirs_latest_nc <-tidy_chl("data/viirs/viirs_latest.nc")


# redo 2024 pull using the url?
download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNchlaWeekly.nc?chlor_a%5B(2024-02-10T00:00:00Z):(2024-03-23T12:00:00Z)%5D%5B(0.0):1:(0.0)%5D%5B(",min_lat,"):1:(",max_lat,")%5D%5B(",min_long,"):1:(",max_long,")%5D"),
              method = "libcurl", mode="wb",destfile = "data/viirs/viirs_2024_initial.nc")


# Huh as of April 4 the values match so I will stick with the pacage.
