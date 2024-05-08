#### automatically update data ####
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
library(sf)
library(akmarineareas2)

# Define filepaths. 
# The shiny server uses a different file structure.
# By defining these up here I can update the rest of the app without having to change these each update
nr_ts_fp <- 'data/viirs/data_NR_chla_spring2024.RDS'
ts_avg_fp <- "data/viirs/viirs_latest_ts.RDS"
map_fp <- "PEEC/www/chla_maps.png"
lkp_fp <- "data/viirs/viirs_lkp_04222024.RDS"
lineplot_fp <- "PEEC/www/Chla_annual_lines.png"
coverplot_fp <- "PEEC/www/chla_coverage.png"

map_goa_fp <- "PEEC/www/chla_maps_goa.png"
lineplot_goa_fp <- "PEEC/www/Chla_annual_lines_goa.png"
coverplot_goa_fp <- "PEEC/www/chla_coverage_goa.png"

map_ai_fp <- "PEEC/www/chla_maps_ai.png"
lineplot_ai_fp <- "PEEC/www/Chla_annual_lines_ai.png"
coverplot_ai_fp <- "PEEC/www/chla_coverage_ai.png"


# set download extent
min_long<- (-179.99)
max_long<- (-130)
min_lat<- (47.5)
max_lat<- (66)

#basin_dims_w
min_long_w<- (167)
max_long_w<- (179.99)
min_lat_w<- (47.5)
max_lat_w<- (60)

# load previous 2024 data
data_2024 <- readRDS(nr_ts_fp)
#data_2024 <- readRDS('inter_jens_datafiles/data_NR_chla_spring2024.RDS') # //  sorry needed this to be able to load olld plots

# identify max date
max_date <- max(data_2024$dates)
new_start <-as.character(max_date+1)

# download new data
# download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNchlaWeekly.nc?chlor_a%5B(last)%5D%5B(0.0):1:(0.0)%5D%5B(",min_lat,"):1:(",max_lat,")%5D%5B(",min_long,"):1:(",max_long,")%5D"),
#               method = "libcurl", mode="wb",destfile = "data/viirs/viirs_latest.nc")

# getting info for the analysis 
info_NR_chla<-rerddap::info(datasetid = "nesdisVHNchlaWeekly", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

# 
recent_day_chl <- griddap(info_NR_chla, latitude = c(min_lat, max_lat), longitude = c(min_long, max_long), time = c(new_start,'last'), fields = 'chlor_a')

df<-as.data.frame(recent_day_chl$data)
df$dates<-as.Date(df$time)

recent_day_chl_w <- griddap(info_NR_chla, latitude = c(min_lat_w, max_lat_w), longitude = c(min_long_w, max_long_w), time = c(new_start,'last'), fields = 'chlor_a')

dfw<-as.data.frame(recent_day_chl_w$data)
dfw$dates<-as.Date(dfw$time)

df <- df %>%
  bind_rows(dfw) %>%
  filter(chlor_a>0)

#function to update if data has been updated
update_fun <- function(old_df, new_df) {
  new_date <- max(new_df$dates)
  old_date <- max(old_df$dates)
  if(new_date <= old_date) {stop()}
  return(old_df %>%
           bind_rows(new_df))
}

#update
data_2024 <- update_fun(old_df=data_2024, new_df = df)

#save
saveRDS(data_2024, nr_ts_fp)

# jens internal save 
#saveRDS(data_2024, "inter_jens_datafiles/data_NR_chla_spring2024.RDS")
unique(data_2024$dates)
# choose days to plot
new_max_date <- max(data_2024$dates)
# filter to last 6 weeks
pixel_counts <- data_2024 %>%
  filter(dates > new_max_date-42) %>%
  filter(chlor_a >0) %>%
  group_by(dates) %>%
  summarize(pixel_count=n())

# One day will be within the last four days
pixel_counts_latest <- pixel_counts %>%
  filter(dates > new_max_date-4)
d1<-pixel_counts_latest[which.max(pixel_counts_latest$pixel_count),]$dates

#define desired spacing (on either side)
dx <- 5

pixel_counts <- pixel_counts %>%
  filter(dates < d1-dx)

d2 <- pixel_counts[which.max(pixel_counts$pixel_count),]$dates

pixel_counts <- pixel_counts %>%
  filter(dates > d2+dx | dates < d2-dx)

d3 <- pixel_counts[which.max(pixel_counts$pixel_count),]$dates

pixel_counts <- pixel_counts %>%
  filter(dates > d3+dx | dates < d3-dx)

d4 <- pixel_counts[which.max(pixel_counts$pixel_count),]$dates

date_for_plot <- c(d1,d2,d3,d4)

plot_chla <- data_2024 %>%
  filter(dates %in% date_for_plot)

## filter plot chla by region
viirs_lkp <-readRDS(lkp_fp)

join_fun <- function(data) {
  data %>%
    inner_join(viirs_lkp %>% dplyr::select(join_latitude, join_longitude, depth, ecosystem_subarea, ecosystem_area, domain), by=c("join_lat"="join_latitude", "join_lon"="join_longitude"))
}

plot_chla<-plot_chla %>%
  mutate(join_lon=as.numeric(ifelse(longitude<0, #ifelse statement keeps +- lons the same length
                                    substr(longitude,1,8),
                                    substr(longitude,1,7))), 
         join_lat=as.numeric(substr(latitude,1,6)))%>%
  join_fun()

 plot_chla_bs<-plot_chla%>% filter(ecosystem_area=="Eastern Bering Sea")
 plot_chla_goa<-plot_chla%>% filter(ecosystem_area=="Gulf of Alaska")
 plot_chla_ai<-plot_chla%>% filter(ecosystem_area=="Aleutian Islands")%>%
   mutate(lon360=ifelse(longitude>0, longitude, longitude+360))

# download ice
min_ice_long<- (-179)
max_ice_long<- (-155)
min_ice_lat<- (55)
max_ice_lat<- (66)

info_coral<-rerddap::info(datasetid = "NOAA_DHW", url = "https://coastwatch.pfeg.noaa.gov/erddap/")
ice1 <- griddap(info_coral, latitude = c(min_ice_lat, max_ice_lat), longitude = c(min_ice_long, max_ice_long), time = c(paste(date_for_plot[1]),paste(date_for_plot[1])), fields = 'CRW_SEAICE')
ice2 <- griddap(info_coral, latitude = c(min_ice_lat, max_ice_lat), longitude = c(min_ice_long, max_ice_long), time = c(paste(date_for_plot[2]),paste(date_for_plot[2])), fields = 'CRW_SEAICE')
ice3 <- griddap(info_coral, latitude = c(min_ice_lat, max_ice_lat), longitude = c(min_ice_long, max_ice_long), time = c(paste(date_for_plot[3]),paste(date_for_plot[3])), fields = 'CRW_SEAICE')
ice4 <- griddap(info_coral, latitude = c(min_ice_lat, max_ice_lat), longitude = c(min_ice_long, max_ice_long), time = c(paste(date_for_plot[4]),paste(date_for_plot[4])), fields = 'CRW_SEAICE')


# ice1 <- griddap(info_coral, latitude = c(min_lat, max_lat), longitude = c(min_long, max_long), time = c(paste(date_for_plot[1]),paste(date_for_plot[1])), fields = 'CRW_SEAICE')
# ice2 <- griddap(info_coral, latitude = c(min_lat, max_lat), longitude = c(min_long, max_long), time = c(paste(date_for_plot[2]),paste(date_for_plot[2])), fields = 'CRW_SEAICE')
# ice3 <- griddap(info_coral, latitude = c(min_lat, max_lat), longitude = c(min_long, max_long), time = c(paste(date_for_plot[3]),paste(date_for_plot[3])), fields = 'CRW_SEAICE')
# ice4 <- griddap(info_coral, latitude = c(min_lat, max_lat), longitude = c(min_long, max_long), time = c(paste(date_for_plot[4]),paste(date_for_plot[4])), fields = 'CRW_SEAICE')

ice<-data.frame(rbind(ice1$data,ice2$data,ice3$data,ice4$data))
ice$dates<-as.Date(ice$time)
unique(ice$dates)
# world
# plot can be improved # 
w <- map_data("world2Hires", ylim = c(55, 66), xlim = c(-175+360, -155+360))
breaks_w2<-c(185,190,195,200) # working with 0-360 lons - helpful across the dateline. We can change that.
labels_w2<-breaks_w2-360 #

table(plot_chla$dates,is.na(plot_chla$chlor_a))


ice_s<-ice[ice$CRW_SEAICE>0.0999,] # test - cutting out pixels with less than 10 % ice percentage. Might make sense - as this is normally our cutoff for Chl-a. I.e. Chla removed if ice is higher than 10 %
ice_s<-ice_s[complete.cases(ice_s$dates),]

map_theme <- theme(plot.title = element_text(size = 20),
                   strip.text = element_text(size=20,color="black",family="sans"),
                   axis.title = element_text(size=20,family="sans"),
                   axis.text = element_text(size=18,family="sans"),
                   panel.background=element_blank(),
                   panel.border=element_rect(color="black",fill=NA),
                   axis.text.x=element_text(color="black"))
# new plot 
#windows(20,15)
maps_plot1 <- ggplot() +
  geom_point(data = plot_chla_bs, aes(x = longitude+360, y = latitude, color =(chlor_a)),pch=19) +
  scale_color_gradientn(colours = (cmocean('thermal')(200)),name = "Chl-a [ug/l]", trans = "pseudo_log",limits=c(0.01,20)) +
  geom_raster(data = ice_s, aes(x = longitude+360, y = latitude, fill =(CRW_SEAICE)),interpolate = TRUE,color='white') +
  scale_fill_gradientn(colours = (cmocean('ice')(200)),name = "Ice fraction",na.value="transparent") +
  geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
  scale_x_continuous("Longitude", breaks=breaks_w2, labels=labels_w2, limits=c(140,250))+
  theme_bw() + ylab("Latitude")  +
  facet_wrap(.~dates,ncol=2)+
  coord_fixed(1.7, xlim = c(-175+360, -157+360),  ylim = c(55.2, 65.6)) + ggtitle('Chlorophyll-a Bering Sea')+
  map_theme




# making map plots - we can tweak this
# maps_plot1<- ggplot() +
#   geom_point(data = plot_chla, aes(x = longitude+360, y = latitude, color =sqrt(chlor_a)),pch=15) +
#   scale_color_gradientn(colours = (cmocean('algae')(200)),name = "") +
#   geom_raster(data = ice, aes(x = longitude+360, y = latitude, fill =(CRW_SEAICE)),interpolate = FALSE) +
#   scale_fill_gradientn(colours = (cmocean('ice')(200)),name = "",na.value="transparent") +
#   geom_polygon(data = w, aes(x = long, y = lat, group = group), fill = "grey80") +
#   scale_x_continuous("Longitude", breaks=breaks_w2, labels=labels_w2, limits=c(140,250))+
#   theme_bw() + ylab("latitude") + xlab("longitude") +
#   facet_wrap(.~dates,ncol=2)+
#   coord_fixed(1.3, xlim = c(-179+360, -155+360),  ylim = c(55, 66)) + ggtitle('Chla Bering Sea')

#save
png(map_fp, height=14.25, width=18, units="in", res=300)
print(maps_plot1)
dev.off()

#### Average data ####
# load previous years data
ts_avg <- readRDS(ts_avg_fp)
  

# filter for shelf (20-200m)
# average for ESR subregions
aggregate_fun <- function(data) {
  data %>%
    filter(if_else(ecosystem_area == "Aleutian Islands", depth <= -30, 
            if_else(ecosystem_area == "Gulf of Alaska", depth <=-30 & depth >= -500, depth <= -30 & depth >= -200))) %>%
    group_by(read_date, year, ecosystem_area, ecosystem_subarea, domain) %>%
    summarize(mean_chla=mean(chlorophyll, na.rm=TRUE),
              n_chla = n())
}

df2 <-df %>%
  mutate(read_date = as.character(dates),
         year = year(dates),
         join_lon=as.numeric(ifelse(longitude<0, #ifelse statement keeps +- lons the same length
                                    substr(longitude,1,8),
                                    substr(longitude,1,7))), 
         join_lat=as.numeric(substr(latitude,1,6)),
         chlorophyll=round(chlor_a,3)) %>%
  join_fun()%>%
  aggregate_fun()

#join with previous year time series
update_fun2 <- function(old_df, new_df) {
  new_date <- max(as.Date(new_df$read_date))
  old_date <- max(as.Date(old_df$read_date))
  if(new_date <= old_date) {stop()}
  
  return(old_df %>%
           bind_rows(new_df))
}

ts_avg <- update_fun2(old_df=ts_avg, new_df = df2)
  
saveRDS(ts_avg, ts_avg_fp)

#process for plotting
data <-ts_avg %>%
  mutate(week_date = as.Date(read_date),
    month=month(week_date),
         doy=yday(week_date))
# plot definitions
OceansBlue1='#0093D0'
OceansBlue2='#0055A4' # darker blue used for strip.background
current.year <- max(data$year)
last.year <- current.year-1
mean.years <- 2012:last.year
mean.lab <- paste0("Mean 2012-",last.year)#load 508 complient colors
current.year.color <- "black"
last.year.color <- '#0093D0'
mean.color <- '#7F7FFF'
old.years.color<-'#D0D0D0'


#  Specify legend position coordinates
mylegx <- 0.2
mylegy <- 0.865

bsdata <- data %>% filter(ecosystem_area == "Eastern Bering Sea")

# set up latest date label
# ann_text <- data.frame(doy = 100, mean_chla = 6, 
#                        lab = paste0("Last date: ", new_max_date),
#                        ecosystem_subarea = factor("Southeastern Bering Sea"), 
#                        levels = c("Northern Bering Sea", "Southeastern Bering Sea"),
#                        domain = factor("Outer Domain"),
#                        levels = c("Inner Domain", "Middle Domain", "Outer Domain"))

bsdata <- bsdata %>%
  mutate(domain= factor(domain, levels = c("Outer Domain", "Middle Domain", "Inner Domain")))

ts_theme <-   theme_bw()+
  theme(legend.position=c(mylegx,mylegy),
        legend.text = element_text(size=8,family="sans"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=10,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title = element_text(size=10,family="sans"),
        axis.text = element_text(size=10,family="sans"),
        legend.key.size = unit(0.35,"cm"),
        plot.margin=unit(c(0.65,0,0.65,0),"cm")) 

# plot
png(lineplot_fp,width=7,height=5,units="in",res=300)
ggplot() +
  geom_line(data=bsdata %>% filter(year<last.year),
            aes(doy,mean_chla,group=factor(year),col='old.years.color'),size=0.3) +
  geom_line(data=bsdata %>% filter(year==last.year),
            aes(doy,mean_chla,color='last.year.color'),size=0.75) +
  geom_line(data=bsdata %>% 
              filter(year%in%mean.years) %>% 
              group_by(ecosystem_subarea, domain, doy) %>% 
              summarise(meanchla=mean(mean_chla,na.rm=TRUE)),
            aes(doy,meanchla,col='mean.color'),size=0.75) +
  geom_line(data=bsdata %>% filter(year==current.year),
            aes(doy,mean_chla,color='current.year.color'),size=0.95) +
  #geom_text(data=ann_text, aes(doy,mean_chla), label = paste0("Last date: ", new_max_date))+
  facet_grid(rows=vars(ecosystem_subarea), cols=vars(domain)) + 
  scale_color_manual(name="",
                     breaks=c('current.year.color','last.year.color','old.years.color','mean.color'),
                     values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'old.years.color'=old.years.color,'mean.color'=mean.color),
                     labels=c(current.year,last.year,paste0('2012-',last.year-1),mean.lab)) +
  ylab("mean Chlorophyll-a (ug/L)") + 
  xlab("") +
  ggtitle(paste0("Bering Sea Chlorophyll-a through: ", new_max_date))+
  scale_x_continuous(limits=c(40, 274), breaks=c(60,121, 182,244), labels=c("Mar",  "May",  "Jul", "Sep"))+
  #scale_x_continuous(breaks=c(60,91,121,152, 182,213,244,274), labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))+
  ts_theme

dev.off()


## coverage plot
domain_sum <- viirs_lkp %>%
  filter(depth<= -30 & depth >= -200 & ecosystem_area == "Eastern Bering Sea") %>%
  group_by(ecosystem_subarea, domain) %>%
  summarize(pixel_count = n())

bsdata <- bsdata %>%
  left_join(domain_sum, by= c("ecosystem_subarea"="ecosystem_subarea", "domain"="domain")) %>%
  mutate(coverage = n_chla/pixel_count,
         domain= factor(domain, levels = c("Outer Domain", "Middle Domain", "Inner Domain")))
  

png(coverplot_fp,width=7,height=5,units="in",res=300)
ggplot() +
  geom_line(data=bsdata %>% filter(year==current.year),
            aes(x=doy,y=coverage),size=0.95) +
  #geom_text(data=ann_text, aes(doy,mean_chla), label = paste0("Last date: ", new_max_date))+
  facet_grid(rows=vars(ecosystem_subarea), cols=vars(domain)) + 
  ylab("proportion Chlorophyll-a available") + 
  xlab("") +
  #scale_x_continuous(limits=c(40, 274), breaks=c(60,91,121,152, 182,213,244,274), labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))+
  scale_x_continuous(limits=c(40, 274), breaks=c(60,121, 182,244), labels=c("Mar",  "May",  "Jul", "Sep"))+
  ts_theme
dev.off()

################################################################################
#### GOA ####
################################################################################

wg <- map_data("world2Hires", ylim = c(50, 61), xlim = c(196, 230))
breaks_wg<-c(190,200,210,220) # working with 0-360 lons - helpful across the dateline. We can change that.
labels_wg<-breaks_wg-360 #

# new plot 
maps_plot_goa <- ggplot() +
  geom_point(data = plot_chla_goa, aes(x = longitude+360, y = latitude, color =(chlor_a)),pch=19) +
  scale_color_gradientn(colours = (cmocean('thermal')(200)),name = "Chl-a [ug/l]", trans = "pseudo_log",limits=c(0.01,20)) +
  #geom_polygon(data = wg, aes(x = long, y = lat, group = group), fill = "grey80") +
  geom_sf(data=ak %>% st_transform(crs=4326)%>%st_shift_longitude(),fill = "grey80")+
  #scale_x_continuous("Longitude", breaks=breaks_wg, labels=labels_wg, limits=c(140,250))+
  scale_x_continuous("Longitude", limits=c(196,227.5), expand=c(0,0))+
  scale_y_continuous("Latitude",  limits=c(50,61), expand=c(0,0))+
  theme_bw() + 
  #ylab("Latitude")  + xlab("Latitude")+
  facet_wrap(.~dates,ncol=2)+
  #coord_sf(xlim = c(196, 228),  ylim = c(50, 61)) + 
  ggtitle('Chlorophyll-a Gulf of Alaska')+
  map_theme

# save
png(map_goa_fp, height=14.25, width=18, units="in", res=300)
maps_plot_goa
dev.off()

x#  line plots
goadata <- data %>% filter(ecosystem_area == "Gulf of Alaska")
goadata <- goadata %>%
  mutate(ecoystem_subarea= factor(ecosystem_subarea, levels = c("Western Gulf of Alaska", "Eastern Gulf of Alaska")))

png(lineplot_goa_fp,width=7,height=5,units="in",res=300)
ggplot() +
  geom_line(data=goadata %>% filter(year<last.year),
            aes(doy,mean_chla,group=factor(year),col='old.years.color'),size=0.3) +
  geom_line(data=goadata %>% filter(year==last.year),
            aes(doy,mean_chla,color='last.year.color'),size=0.75) +
  geom_line(data=goadata %>% 
              filter(year%in%mean.years) %>% 
              group_by(ecosystem_subarea, domain, doy) %>% 
              summarise(meanchla=mean(mean_chla,na.rm=TRUE)),
            aes(doy,meanchla,col='mean.color'),size=0.75) +
  geom_line(data=goadata %>% filter(year==current.year),
            aes(doy,mean_chla,color='current.year.color'),size=0.95) +
  facet_grid(cols=vars(ecosystem_subarea)) + 
  scale_color_manual(name="",
                     breaks=c('current.year.color','last.year.color','old.years.color','mean.color'),
                     values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'old.years.color'=old.years.color,'mean.color'=mean.color),
                     labels=c(current.year,last.year,paste0('2012-',last.year-1),mean.lab)) +
  ylab("mean Chlorophyll-a (ug/L)") + 
  xlab("") +
  ylim(c(0,6))+
  ggtitle(paste0("Gulf of Alaska Chlorophyll-a through ", new_max_date))+
  #scale_x_continuous(limits=c(40, 274), breaks=c(60,121, 182,244), labels=c("Mar",  "May",  "Jul", "Sep"))+
  scale_x_continuous(breaks=c(60,91,121,152, 182,213,244,274), labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))+
  theme_bw()+
  ts_theme
dev.off()

#coverage
goa_esr_sum <- viirs_lkp %>%
  filter(depth<= -30 & depth >= -500 & ecosystem_area == "Gulf of Alaska") %>% # change to 500
  group_by(ecosystem_subarea) %>%
  summarize(pixel_count = n())

goadata <- goadata %>%
  left_join(goa_esr_sum, by= c("ecosystem_subarea"="ecosystem_subarea")) %>%
  mutate(coverage = n_chla/pixel_count,
         ecoystem_subarea= factor(ecosystem_subarea, levels = c("Western Gulf of Alaska", "Eastern Gulf of Alaska")))


png(coverplot_goa_fp,width=7,height=5,units="in",res=300)
ggplot() +
  geom_line(data=goadata %>% filter(year==current.year),
            aes(x=doy,y=coverage),size=0.95) +
  #geom_text(data=ann_text, aes(doy,mean_chla), label = paste0("Last date: ", new_max_date))+
  facet_grid(cols=vars(ecosystem_subarea)) + 
  ylab("proportion Chlorophyll-a available") + 
  xlab("") +
  #scale_x_continuous(limits=c(40, 274), breaks=c(60,91,121,152, 182,213,244,274), labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))+
  scale_x_continuous(limits=c(40, 274), breaks=c(60,121, 182,244), labels=c("Mar",  "May",  "Jul", "Sep"))+
  ts_theme
dev.off()

################################################################################
#### AI ####
################################################################################
wa <- map_data("world2Hires", ylim = c(47.9, 56.2), xlim = c(167, 196))
breaks_wa<-c(170,180,190) # working with 0-360 lons - helpful across the dateline. We can change that.
labels_wa<-breaks_wa-360 #

# new plot 
maps_plot_ai <- ggplot() +
  geom_point(data = plot_chla_ai, aes(x = lon360, y = latitude, color =(chlor_a)),pch=19) +
  scale_color_gradientn(colours = (cmocean('thermal')(200)),name = "Chl-a [ug/l]", trans = "pseudo_log",limits=c(0.01,20)) +
  geom_polygon(data = wa, aes(x = long, y = lat, group = group), fill = "grey80") +
  scale_x_continuous("Longitude", breaks=breaks_wa, labels=labels_wa, limits=c(140,250))+
  theme_bw() + ylab("Latitude")  +
  facet_wrap(.~dates,ncol=2)+
  coord_fixed(1.7, xlim = c(167, 196),  ylim = c(47.9, 56.2)) + ggtitle('Chlorophyll-a Aleutian Islands')+
  map_theme

# save
png(map_ai_fp, height=14.25, width=18, units="in", res=300)
maps_plot_ai
dev.off()

#  line plots
aidata <- data %>% filter(ecosystem_area == "Aleutian Islands")
aidata <- aidata %>%
  mutate(ecosystem_subarea= factor(ecosystem_subarea, levels = c("Western Aleutians","Central Aleutians", "Eastern Aleutians")))

png(lineplot_ai_fp,width=7,height=5,units="in",res=300)
ggplot() +
  geom_line(data=aidata %>% filter(year<last.year),
            aes(doy,mean_chla,group=factor(year),col='old.years.color'),size=0.3) +
  geom_line(data=aidata %>% filter(year==last.year),
            aes(doy,mean_chla,color='last.year.color'),size=0.75) +
  geom_line(data=aidata %>% 
              filter(year%in%mean.years) %>% 
              group_by(ecosystem_subarea, doy) %>% 
              summarise(meanchla=mean(mean_chla,na.rm=TRUE)),
            aes(doy,meanchla,col='mean.color'),size=0.75) +
  geom_line(data=aidata %>% filter(year==current.year),
            aes(doy,mean_chla,color='current.year.color'),size=0.95) +
  facet_grid(cols=vars(ecosystem_subarea)) + 
  scale_color_manual(name="",
                     breaks=c('current.year.color','last.year.color','old.years.color','mean.color'),
                     values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'old.years.color'=old.years.color,'mean.color'=mean.color),
                     labels=c(current.year,last.year,paste0('2012-',last.year-1),mean.lab)) +
  ylab("mean Chla (ug/L)") + 
  xlab("") +
  ggtitle(paste0("Aleutian Chlorophyll-a through ", new_max_date))+
  #scale_x_continuous(breaks=c(60,91,121,152, 182,213,244,274), labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))+
  scale_x_continuous(limits=c(40, 274), breaks=c(60,121, 182,244), labels=c("Mar",  "May",  "Jul", "Sep"))+
  theme_bw()+
  ts_theme
dev.off()

#coverage
ai_esr_sum <- viirs_lkp %>%
  filter(depth<= -30  & ecosystem_area == "Aleutian Islands") %>% 
  group_by(ecosystem_subarea) %>%
  summarize(pixel_count = n())

aidata <- aidata %>%
  left_join(ai_esr_sum, by= c("ecosystem_subarea"="ecosystem_subarea")) %>%
  mutate(coverage = n_chla/pixel_count,
         ecosystem_subarea= factor(ecosystem_subarea, levels = c("Western Aleutians","Central Aleutians", "Eastern Aleutians"))) %>%
  data.frame()


png(coverplot_ai_fp, width=7,height=5,units="in",res=300)
ggplot() +
  geom_line(data=aidata %>% filter(year==current.year),
            aes(x=doy,y=coverage)) +
  facet_grid(cols=vars(ecosystem_subarea)) + 
  ylab("proportion Chlorophyll-a available") + 
  xlab("") +
  scale_x_continuous(limits=c(40, 274), breaks=c(60,121, 182,244), labels=c("Mar",  "May",  "Jul", "Sep"))+
  ts_theme
dev.off()


