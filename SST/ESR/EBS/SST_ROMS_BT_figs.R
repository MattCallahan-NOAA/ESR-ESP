####Matt Callahan and Jordan Watson####
#ROMS Wrangling
#6/22/21
#matt.callahan@noaa.gov

library(RNetCDF)
library(tidync)
require(tidyverse)
require(lubridate)
library(sf)
library(AKmarineareas)

#  Access netcdfs from THREDDS server (https://data.pmel.noaa.gov/aclim/thredds/catalog.html)

#  We are interested in bottom temperature for the Bering Sea ecosystem regions, which are much smaller than the ROMS extent. 
#  Read in our lookup table for the Bering Sea to find some spatial bounds.
lkp <- readRDS("EBS/Data/crwsst_spatial_lookup_table.RDS") %>% 
  filter(Ecosystem=="Eastern Bering Sea") %>% 
  dplyr::select(longitude,latitude) %>% 
  summarise(maxlat=max(latitude),
            minlat=min(latitude),
            maxlon=max(longitude),
            minlon=min(longitude))

#  We need to access the file containing the extended spatial grids, which contains different transformations of the spatial coordinates.
grid<-tidync("https://data.pmel.noaa.gov/aclim/thredds/dodsC/extended_grid/Bering10K_extended_grid.nc")

#  One of the native grids contains the columns we need to match the bottom temperature data to latitudes and longitudes.
grid_lkp <- grid %>% 
  activate("D8,D2") %>% 
  hyper_tibble() %>% 
  dplyr::select(lat_rho,lon_rho,xi_rho,eta_rho) %>% 
  filter(lon_rho>180) %>% 
  mutate(lon_rho=lon_rho-360,
         latitude=lat_rho,      # Make a duplicate column for the point-in-polygon operation
         longitude=lon_rho) %>%       # Make a duplicate column for the point-in-polygon operation
  filter(lon_rho>=lkp$minlon & 
           lon_rho<=lkp$maxlon & 
           lat_rho>=lkp$minlat & 
           lat_rho<=lkp$maxlat)

# Right now it's just a rectangular grid.
grid_lkp %>% 
  ggplot(aes(lon_rho,lat_rho)) + 
  geom_point()

#  Now load load the ROMS data file (note that this does not extract the data so it is quick).
ROMS <- tidync("https://data.pmel.noaa.gov/aclim/thredds/dodsC/Level2/B10K-K20_CORECFS_bottom5m.nc")

# We can create a data frame of dates. To match our SST data, we only need bottom temperatures since 1985-01-01.
# So we identify which dates to filter our from our subsequent query. 
datevec <- ROMS %>% 
  activate("D4") %>% 
  hyper_tibble() %>% 
  mutate(date=as_date(as_datetime(ocean_time,origin="1900-01-01 00:00:00", tz = "UTC")),
         date_index=1:n()) %>% 
  filter(date>=as.Date("1985-01-01"))

#  This is a beast. Extract all the temperature across the lookup grid since 1985. 
#  Ends up being about 27 million rows. Save directly to RDS.
ROMS %>% 
  activate("D8,D1,D4") %>% 
  hyper_filter(ocean_time=ocean_time>=ocean_time[min(datevec$date_index)],    # Filter dates based on the datevec index for 1985-01-01
               xi_rho=xi_rho>=min(grid_lkp$xi_rho) & xi_rho<=max(grid_lkp$xi_rho), # Filter xi_rho based on the spatial lookup
               eta_rho=eta_rho>=min(grid_lkp$eta_rho) & eta_rho<=max(grid_lkp$eta_rho)) %>%  # Filter eta_rho based on the spatial lookup
  hyper_tibble(select_var="temp") %>% 
  saveRDS("EBS/Data/ROMS_bottom_temp_EBS_1985_2022.RDS")# Only extract the temperature variable


#  Read in the ESR shapefile and subset for the Bering areas
esr_shp <- AK_marine_area(area="Ecosystem Subarea") %>% 
  filter(Ecosystem_Subarea%in%c("Northern Bering Sea","Southeastern Bering Sea"))

# Convert the lookup grid to a sf object with CRS and then transform to that of the shapefile and 
# perform the point-in-polygon operation, removing unmatched coordinates.
#turn off spherical geometry first
sf::sf_use_s2(FALSE)
esr_pts = st_join(
  st_as_sf(grid_lkp, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")  %>% # Use the duplicated lat/lon columns for matching to avoid rounding issues.
    st_transform(st_crs(esr_shp)$proj4string),
  esr_shp) %>% 
  filter(!is.na(Ecosystem_Subarea)) %>% 
  data.frame %>% # THis will drop the latitude and longitude point geometry column
  dplyr::select(Ecosystem_Subarea,lat_rho,lon_rho,xi_rho,eta_rho,BSIERP_ID)

#  Make sure it looks alright
esr_pts %>% 
  ggplot(aes(lon_rho,lat_rho)) + 
  geom_point()


#  Join and save!
readRDS("EBS/Data/ROMS_bottom_temp_EBS_1985_2022.RDS") %>% 
  inner_join(esr_pts) %>% 
  mutate(date=as_date(as_datetime(ocean_time,origin="1900-01-01 00:00:00", tz = "UTC"))) %>% 
  saveRDS("EBS/Data/ROMS_bottom_temp_1985_2022_merged_ESR.RDS")



#------------------------------------------------------------------------------#


#ROMS SST and bottom temperature in the Bering Sea
#ROMS Bering Sea "hot topic" for 2022 ESR
#Matt Callahan 
#9/2/2022

#install packages
library(tidyverse)
library(lubridate)
library(cowplot)
library(magick)
library(httr)
library(gridExtra)
library(scales)
library(marmap)
library(getPass)
library(odbc)
library(DBI)

#SST
#  Load 508 compliant NOAA colors
OceansBlue1='#0093D0'
OceansBlue2='#0055A4' # rebecca dark blue
Crustacean1='#FF8300'
UrchinPurple1='#7F7FFF'
SeagrassGreen4='#D0D0D0' # This is just grey
#  Assign colors to different time series.
current.year.color <- "black"#CoralRed1 #OceansBlue1
last.year.color <- OceansBlue1#WavesTeal1
mean.color <- UrchinPurple1
#  Set default plot theme
theme_set(theme_cowplot())

#  Specify legend position coordinates (top panel)
mylegx <- 0.625
mylegy <- 0.865


####-------------------------------------------------------------####

#  Define the Bering Sea dataset
BSupdateddata <- httr::content(httr::GET('https://apex.psmfc.org/akfin/data_marts/akmp/ecosystem_sub_crw_avg_sst?ecosystem_sub=Southeastern%20Bering%20Sea,Northern%20Bering%20Sea&start_date=19850101&end_date=20220807'), type = "application/json") %>% 
  bind_rows %>% 
  mutate(date=as_date(READ_DATE)) %>% 
  data.frame %>% 
  dplyr::select(date,meansst=MEANSST,Ecosystem_sub=ECOSYSTEM_SUB)


SSTdata <- 
  BSupdateddata %>% 
  rename_all(tolower) %>% 
  mutate(read_date=date,
         esr_region=ecosystem_sub,
         month=month(read_date),
         day=day(read_date),
         year=year(read_date),
         newdate=as.Date(ifelse(month>=9,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),#  Create a dummy year so that each year can more easily be overlain
                                as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
         year2=ifelse(month>=9,year+1,year)) %>% # To have our years go from Sep-Aug, force Sep-Dec to be part of the subsequent year.
  arrange(read_date) 
#  Set year criteria to automatically identify the current and previous years
current.year <- max(SSTdata$year2)
last.year <- current.year-1
mean.years <- 1986:2016 # We use the oldest 30-year time series as our climatological baseline.
mean.lab <- "Mean 1986-2016"

####---------------------------------------------------####
#Plots for P1
#  Create plotting function that will allow selection of 2 ESR regions
pb1 <- ggplot() +
  geom_line(data=SSTdata %>% filter(year2<last.year), # Older years are grey lines.
            aes(newdate,meansst,group=factor(year2),col='mygrey'),size=0.3) +
  geom_line(data=SSTdata %>% filter(year2==last.year), # The previous year
            aes(newdate,meansst,color='last.year.color'),size=0.75) +
  geom_line(data=SSTdata %>% 
              filter(year%in%mean.years) %>% # The mean from 1986-2015
              group_by(esr_region,newdate) %>% 
              summarise(meantemp=mean(meansst,na.rm=TRUE)),
            aes(newdate,meantemp,col='mean.color'),size=0.65,linetype="solid") +
  geom_line(data=SSTdata %>% filter(year2==current.year), # This year
            aes(newdate,meansst,color='current.year.color'),size=0.75) +
  facet_wrap(~esr_region,ncol=2) + 
  scale_color_manual(name="",
                     breaks=c('current.year.color','last.year.color','mygrey','mean.color'),
                     values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
                     labels=c(current.year,last.year,paste0('1986-',last.year-1),mean.lab)) +
  scale_linetype_manual(values=c("solid","solid","solid","dashed")) +
  ylab("Sea Surface Temperature (°C)") + 
  xlab("") +
  scale_x_date(limits=c(as_date("1999-09-01"),as_date("2000-08-31")),date_breaks="1 month",date_labels = "%b",expand=c(0.01,0)) +
  theme(legend.position=c(mylegx,mylegy),
        legend.text = element_text(size=20,family="sans"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=24,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title.y = element_text(size=20,family="sans"),
        axis.text.y = element_text(size=16,family="sans"),
        panel.border=element_rect(colour="black",size=0.75),
        axis.text.x=element_blank(),
        legend.key.size = unit(0.35,"cm"),
        plot.margin=unit(c(0,0.5,0.5,0.5),"cm")) 
pb1

#ROMS
#extracted via ROMS_experiment_bottom_temp.R script
#depth from marmap
lkp <- readRDS("EBS/Data/crwsst_spatial_lookup_table.RDS") %>% 
  filter(Ecosystem=="Eastern Bering Sea") %>% 
  dplyr::select(longitude,latitude) %>% 
  summarise(maxlat=max(latitude),
            minlat=min(latitude),
            maxlon=max(longitude),
            minlon=min(longitude))
r.ak <- marmap::as.raster(getNOAA.bathy(lon1=lkp$minlon,lon2=lkp$maxlon,lat1=lkp$minlat,lat2=lkp$maxlat, resolution=1))
ROMS<-readRDS("EBS/Data/ROMS_bottom_temp_1985_2022_merged_ESR.RDS")%>%
  mutate(depth=round(raster::extract(r.ak,cbind(lon_rho,lat_rho),method="bilinear"),0))

#Group by date
ROMSdata<-ROMS %>% 
  filter(between(depth,-200,-10)) %>% 
  group_by(Ecosystem_Subarea, date)%>%
  summarize(temp=mean(temp))%>%
  complete(date = seq.Date(min(date), max(date), by="day"))%>%
  fill(temp, Ecosystem_Subarea) %>% #fill in the values
  mutate(year=year(date),
         month=month(date),
         week=week(date),
         day=day(date),
         esr_region=Ecosystem_Subarea,
         newdate=as.Date(ifelse(month>=9,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),#  Create a dummy year so that each year can more easily be overlain
                                as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
         year2=ifelse(month>=9,year+1,year))



######
####plot with the same code from the shiny plots
pb2<-ggplot() +
  geom_line(data=ROMSdata %>% filter(year2<last.year), # Older years are grey lines.
            aes(newdate,temp,group=factor(year2),col='mygrey'),size=0.3) +
  geom_line(data=ROMSdata %>% filter(year2==last.year), # The previous year
            aes(newdate,temp,color='last.year.color'),size=1) +
  geom_line(data=ROMSdata %>% 
              filter(year2%in%mean.years) %>% # The mean from 1986-2015
              group_by(esr_region,newdate) %>% 
              summarise(meantemp=mean(temp,na.rm=TRUE)),
            aes(newdate,meantemp, col='mean.color'), size=1,linetype="solid") +
  geom_line(data=ROMSdata %>% filter(year2==current.year), # the current year
            aes(newdate,temp,group=factor(year2),color='current.year.color'),size=0.75) +
  facet_wrap(~esr_region,ncol=2) +
  scale_color_manual(name="",
                     breaks=c('current.year.color','last.year.color','mygrey','mean.color'),
                     values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
                     labels=c(current.year,last.year,paste0('1985-',last.year-1),mean.lab)) +
  scale_linetype_manual(values=c("solid","solid","solid","dashed")) +
  # scale_y_continuous(labels=scaleFUN)+
  scale_x_date(limits=c(as_date("1999-09-01"),as_date("2000-08-31")),date_breaks="1 month",date_labels = "%b",expand=c(0.01,0)) +
  ylab("ROMS Bottom Temperature (°C)") + 
  #xlab("Week") +
  theme(legend.position=c(mylegx,mylegy),
        legend.text = element_text(size=20,family="sans"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=24,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title.y = element_text(size=20,family="sans"),
        axis.text.y = element_text(size=16,family="sans"),
        panel.border=element_rect(colour="black",size=0.75),
        #axis.text.x=element_text(size=20,family="sans"),
        legend.key.size = unit(0.35,"cm"),
        axis.text.x=element_text(size=20, color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
        axis.title.x=element_blank(),
        plot.margin=unit(c(0,0.5,0.5,0.5),"cm")) 

pb2

#combine plots
pb3<-plot_grid(pb1,pb2,ncol=1)
png("EBS/2022/hottopic_sst_bt.png", height=24, width=24, units="cm", res=300)
pb3
dev.off()


min(ROMS$depth)
hist(ROMS$depth)
hist((ROMS%>%filter(depth>=-200))$depth)


#Now run the same thing with new regions

#Just use Jordan's RDS for previous years
SSTdata2<-readRDS("EBS/Data/ESR_sst_depthbins.RDS")
#need to query raw SST for new aggregation
#connect to db
con<- dbConnect(odbc::odbc(),dsn="AKFIN",uid=getPass(),pwd=getPass())
#download sst
SSTupdate<- dbFetch(
  dbSendQuery(con,"select read_date, crw_id, temp, ecosystem_sub, depth from AFSC.erddap_crw_sst sst
inner join afsc.erddap_crw_sst_spatial_lookup lkp
on sst.CRW_ID=lkp.id
where ecosystem='Eastern Bering Sea'
and read_date > to_date('09 sep 2021 12:00:00', 'dd mon yyyy hh:mi:ss')
and read_date < to_date('08 aug 2022 12:00:00', 'dd mon yyyy hh:mi:ss')")) 
saveRDS(SSTupdate, "EBS/Data/2022update_raw.RDS")
#oops forgot a few rows
bonus<- dbFetch(
  dbSendQuery(con,"select read_date, crw_id, temp, ecosystem_sub, depth from AFSC.erddap_crw_sst sst
inner join afsc.erddap_crw_sst_spatial_lookup lkp
on sst.CRW_ID=lkp.id
where ecosystem='Eastern Bering Sea'
and read_date > to_date('02 sep 2021 12:00:00', 'dd mon yyyy hh:mi:ss')
and read_date < to_date('09 sep 2021 12:00:00', 'dd mon yyyy hh:mi:ss')"))
SSTupdate<-SSTupdate%>%bind_rows(bonus)
#download a test day to match with last year's
test<- dbFetch(
  dbSendQuery(con,"select read_date, crw_id, temp, ecosystem_sub, depth from AFSC.erddap_crw_sst sst
inner join afsc.erddap_crw_sst_spatial_lookup lkp
on sst.CRW_ID=lkp.id
where ecosystem='Eastern Bering Sea'
and read_date = to_date('02 sep 2021 12:00:00', 'dd mon yyyy hh:mi:ss')"))

#this math looks right.
test%>%
  filter(DEPTH>-200 & DEPTH< -10)%>%
  mutate(depth2=ifelse(DEPTH>=-50, "inner", "outer/middle"),
              eco2=paste(ECOSYSTEM_SUB,depth2))%>%
  group_by(eco2)%>%
  summarise(sst=mean(TEMP),
            n=n())

SSTdata2%>% 
  mutate(date=as.Date(READ_DATE))%>%
           filter(date==date("2021-09-02"))

#calculate 2022
SSTupdate2<-SSTupdate%>%
  filter(DEPTH>-200 & DEPTH< -10)%>%
  mutate(depth2=ifelse(DEPTH>=-50, "inner", "outer/middle"),
         Ecosystem_sub=ECOSYSTEM_SUB,
         eco2=paste(ECOSYSTEM_SUB,depth2))%>%
  group_by(READ_DATE, Ecosystem_sub, eco2, )%>%
  summarise(SST=mean(TEMP))%>%
  dplyr::select(READ_DATE, SST, Ecosystem_sub, eco2)

#process data
SSTdata2<-SSTdata2%>%bind_rows(SSTupdate2) %>%
  rename_all(tolower) %>% 
  mutate(month=month(read_date),
         day=day(read_date),
         year=year(read_date),
         newdate=as.Date(ifelse(month>=9,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),#  Create a dummy year so that each year can more easily be overlain
                                as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
         year2=ifelse(month>=9,year+1,year)) %>% # To have our years go from Sep-Aug, force Sep-Dec to be part of the subsequent year.
  arrange(read_date)

SSTdata2$eco2<-recode_factor(SSTdata2$eco2, 
                             "Northern Bering Sea (0-50m)"="NBS (Inner)",
                             "Northern Bering Sea (51-200m)"="NBS (Middle/Outer)",
                             "Southeastern Bering Sea (0-50m)"="SEBS (Inner)",
                             "Southeastern Bering Sea (51-200m)"="SEBS (Middle/Outer)",
                             "Northern Bering Sea inner"="NBS (Inner)",
                             "Northern Bering Sea outer/middle"="NBS (Middle/Outer)",
                             "Southeastern Bering Sea inner"="SEBS (Inner)",
                             "Southeastern Bering Sea outer/middle"="SEBS (Middle/Outer)")
#legend position
mylegx <- 0.27
mylegy <- 0.95

pb4 <- ggplot() +
  geom_line(data=SSTdata2 %>% filter(year2<last.year), # Older years are grey lines.
            aes(newdate,sst,group=factor(year2),col='mygrey'),size=0.3) +
  geom_line(data=SSTdata2 %>% filter(year2==last.year), # The previous year
            aes(newdate,sst,color='last.year.color'),size=0.75) +
  geom_line(data=SSTdata2 %>% 
              filter(year%in%mean.years) %>% # The mean from 1986-2015
              group_by(eco2,newdate) %>% 
              summarise(meantemp=mean(sst,na.rm=TRUE)),
            aes(newdate,meantemp,col='mean.color'),size=0.65,linetype="solid") +
  geom_line(data=SSTdata2 %>% filter(year2==current.year), # This year
            aes(newdate,sst,color='current.year.color'),size=0.75) +
  facet_wrap(~eco2,ncol=1) + 
  scale_color_manual(name="",
                     breaks=c('current.year.color','last.year.color','mygrey','mean.color'),
                     values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
                     labels=c(current.year,last.year,paste0('1986-',last.year-1),mean.lab)) +
  scale_linetype_manual(values=c("solid","solid","solid","dashed")) +
  ylab("Sea Surface Temperature (°C)") + 
  ylim(c(-2,13))+
  scale_x_date(limits=c(as_date("1999-09-01"),as_date("2000-08-31")),date_breaks="1 month",date_labels = "%b",expand=c(0.01,0)) +
  theme(legend.position=c(mylegx,mylegy),
        legend.text = element_text(size=15,family="sans"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=24,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title.y = element_text(size=20,family="sans"),
        axis.text.y = element_text(size=16,family="sans"),
        panel.border=element_rect(colour="black",size=0.75),
        axis.text.x=element_text(size=20, color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
        #axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        legend.key.size = unit(0.35,"cm"),
        plot.margin=unit(c(0,0.5,0.5,0.5),"cm")) 
pb4


ROMSdata2<-ROMS %>% 
  filter(between(depth,-200,-10)) %>% 
  mutate(eco2=ifelse(depth<(-50),paste0(Ecosystem_Subarea," (51-200m)"),paste0(Ecosystem_Subarea," (0-50m)"))) %>% 
  group_by(eco2,date) %>% 
  summarize(temp=mean(temp))%>%
  complete(date = seq.Date(min(date), max(date), by="day"))%>%
  fill(temp, eco2) %>% #fill in the values
  mutate(year=year(date),
         month=month(date),
         week=week(date),
         day=day(date),
         newdate=as.Date(ifelse(month>=9,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),#  Create a dummy year so that each year can more easily be overlain
                                as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
         year2=ifelse(month>=9,year+1,year))

ROMSdata2$eco2<-recode_factor(ROMSdata2$eco2, 
                              "Northern Bering Sea (0-50m)"="NBS (Inner)",
                              "Northern Bering Sea (51-200m)"="NBS (Middle/Outer)",
                              "Southeastern Bering Sea (0-50m)"="SEBS (Inner)",
                              "Southeastern Bering Sea (51-200m)"="SEBS (Middle/Outer)")


####plot with the same code from the shiny plots
pb5<-ggplot() +
  geom_line(data=ROMSdata2 %>% filter(year2<last.year), # Older years are grey lines.
            aes(newdate,temp,group=factor(year2),col='mygrey'),size=0.3) +
  geom_line(data=ROMSdata2 %>% filter(year2==last.year), # The previous year
            aes(newdate,temp,color='last.year.color'),size=1) +
  geom_line(data=ROMSdata2 %>% 
              filter(year2%in%mean.years) %>% # The mean from 1986-2015
              group_by(eco2,newdate) %>% 
              summarise(meantemp=mean(temp,na.rm=TRUE)),
            aes(newdate,meantemp, col='mean.color'), size=1,linetype="solid") +
  geom_line(data=ROMSdata2 %>% filter(year2==current.year), # the current year
            aes(newdate,temp,group=factor(year2),color='current.year.color'),size=0.75) +
  facet_wrap(~eco2,ncol=1) +
  scale_color_manual(name="",
                     breaks=c('current.year.color','last.year.color','mygrey','mean.color'),
                     values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
                     labels=c(current.year,last.year,paste0('1985-',last.year-1),mean.lab)) +
  scale_linetype_manual(values=c("solid","solid","solid","dashed")) +
  #scale_y_continuous(labels=scaleFUN)+
  ylim(c(-2,13))+
  scale_x_date(limits=c(as_date("1999-09-01"),as_date("2000-08-31")),date_breaks="1 month",date_labels = "%b",expand=c(0.01,0)) +
  ylab("ROMS Bottom Temperature (°C)") + 
  #xlab("Week") +
  theme(legend.position=c(mylegx,mylegy),
        legend.text = element_text(size=15,family="sans"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=24,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title.y = element_text(size=20,family="sans"),
        axis.text.y = element_text(size=16,family="sans"),
        panel.border=element_rect(colour="black",size=0.75),
        #axis.text.x=element_text(size=20,family="sans"),
        legend.key.size = unit(0.35,"cm"),
        axis.text.x=element_text(size=20, color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
        axis.title.x=element_blank(),
        plot.margin=unit(c(0,0.5,0.5,0.5),"cm")) 

pb5

pb6<-plot_grid(pb4,pb5,ncol=2)
png("EBS/2022/hottopic_sst_bt_depthbin.png", height=24, width=24, units="cm", res=300)
pb6
dev.off()

#------------------------------------------------------------------------------#
####compare middle and outer domains####

#ROMS
#extracted via ROMS_experiment_bottom_temp.R script
#depth from marmap
lkp <- readRDS("EBS/Data/crwsst_spatial_lookup_table.RDS") %>% 
  filter(Ecosystem=="Eastern Bering Sea") %>% 
  dplyr::select(longitude,latitude) %>% 
  summarise(maxlat=max(latitude),
            minlat=min(latitude),
            maxlon=max(longitude),
            minlon=min(longitude))
r.ak <- marmap::as.raster(getNOAA.bathy(lon1=lkp$minlon,lon2=lkp$maxlon,lat1=lkp$minlat,lat2=lkp$maxlat, resolution=1))
ROMS<-readRDS("EBS/Data/ROMS_bottom_temp_1985_2022_merged_ESR.RDS")%>%
  mutate(depth=round(raster::extract(r.ak,cbind(lon_rho,lat_rho),method="bilinear"),0))


SSTupdate<-readRDS("EBS/Data/2022update_raw.RDS")


#calculate 2022
SSTupdate2<-SSTupdate%>%
  filter(DEPTH>-200 & DEPTH< -50)%>%
  mutate(depth2=ifelse(DEPTH>=-100, "middle", "outer"),
         Ecosystem_sub=ECOSYSTEM_SUB,
         eco2=paste(ECOSYSTEM_SUB,depth2))%>%
  group_by(READ_DATE, Ecosystem_sub, eco2, )%>%
  summarise(SST=mean(TEMP))%>%
  dplyr::select(READ_DATE, SST, Ecosystem_sub, eco2)


sstupdate3<-SSTupdate2%>%rename_all(tolower) %>% 
  mutate(month=month(read_date),
         day=day(read_date),
         year=year(read_date),
         newdate=as.Date(ifelse(month>=9,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),#  Create a dummy year so that each year can more easily be overlain
                                as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
         year2=ifelse(month>=9,year+1,year)) %>% # To have our years go from Sep-Aug, force Sep-Dec to be part of the subsequent year.
  arrange(read_date)


ROMSdata2<-ROMS %>% 
  filter(between(depth,-200,-50)) %>% 
  mutate(eco2=ifelse(depth<(-100),paste0(Ecosystem_Subarea," outer"),paste0(Ecosystem_Subarea," middle"))) %>% 
  group_by(eco2,date) %>% 
  summarize(temp=mean(temp))%>%
  complete(date = seq.Date(min(date), max(date), by="day"))%>%
  fill(temp, eco2) %>% #fill in the values
  mutate(year=year(date),
         month=month(date),
         week=week(date),
         day=day(date),
         newdate=as.Date(ifelse(month>=9,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),#  Create a dummy year so that each year can more easily be overlain
                                as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
         year2=ifelse(month>=9,year+1,year))




png("EBS/2022/hottopic_bt_middleouter.png", height=36, width=36, units="cm", res=300)
ROMSdata2%>%
  filter(year2==current.year)%>%
  ggplot()+
  geom_line(aes(x=date, y=temp, color=eco2))+
  facet_wrap(~substr(eco2, 1, 5),ncol=1) 
dev.off()

png("EBS/2022/hottopic_sst_middleouter.png", height=36, width=36, units="cm", res=300)
sstupdate3%>%
  filter(year2==current.year)%>%
  ggplot()+
  geom_line(aes(x=read_date, y=sst, color=eco2))+
  facet_wrap(~substr(eco2, 1, 5),ncol=1) 
dev.off()


#Calculate avg SST for inner middle outer domains
start<-Sys.time()
sst_domains<-dbFetch(
  dbSendQuery(con,"with lkp as 
(select 
id,
case when depth between -50 and -10 then 'inner'
when depth between -100 and -51 then 'middle'
when depth between -200 and -101 then 'outer'
else null
end as domain,
ecosystem_sub
from afsc.erddap_crw_sst_spatial_lookup a
where depth < -10 and depth > -200
and ecosystem = 'Eastern Bering Sea'
),
sst as 
(select crw_id, read_date, temp
from afsc.erddap_crw_sst)
select ecosystem_sub, domain, read_date, round(avg(temp), 2) meansst
from lkp
left join sst
on lkp.id = sst.crw_id
group by ecosystem_sub, domain, read_date"))
end<-Sys.time()
end-start
#10 min

#save
sst_domains%>%
  rename_with(tolower)%>%
  saveRDS("EBS/Data/sst_in_min_out_2022.RDS")
#prepare data for plotting
sst_domains<-readRDS("EBS/Data/sst_in_min_out_2022.RDS")%>%
  filter(read_date<="2022-08-31 12:00:00")%>%
  mutate(eco_short=ifelse(ecosystem_sub=="Northern Bering Sea", "NBS", "SEBS"),
         eco2=paste(eco_short, domain),
         month=month(read_date),
         day=day(read_date),
         year=year(read_date),
         newdate=as.Date(ifelse(month>=9,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),#  Create a dummy year so that each year can more easily be overlain
                                as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
         year2=ifelse(month>=9,year+1,year)) %>% # To have our years go from Sep-Aug, force Sep-Dec to be part of the subsequent year.
  arrange(read_date)

sst_domains$domain<-fct_relevel(sst_domains$domain, c("outer", "middle", "inner"))

#plot
pb4 <- ggplot() +
  geom_line(data=sst_domains %>% filter(year2<last.year), # Older years are grey lines.
            aes(newdate,meansst,group=factor(year2),col='mygrey'),size=0.3) +
  geom_line(data=sst_domains %>% filter(year2==last.year), # The previous year
            aes(newdate,meansst,color='last.year.color'),size=0.75) +
  geom_line(data=sst_domains %>% 
              filter(year%in%mean.years) %>% # The mean from 1986-2015
              #group_by(eco2,newdate) %>%
               group_by(eco_short, domain,newdate) %>% 
              summarise(meantemp=mean(meansst,na.rm=TRUE)),
            aes(newdate,meantemp,col='mean.color'),size=0.65,linetype="solid") +
  geom_line(data=sst_domains %>% filter(year2==current.year), # This year
            aes(newdate,meansst,color='current.year.color'),size=0.75) +
  #facet_wrap(~eco2,ncol=1)+
  facet_grid(rows=vars(eco_short), cols=vars(domain)) + 
  scale_color_manual(name="",
                     breaks=c('current.year.color','last.year.color','mygrey','mean.color'),
                     values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
                     labels=c(current.year,last.year,paste0('1986-',last.year-1),mean.lab)) +
  scale_linetype_manual(values=c("solid","solid","solid","dashed")) +
  ylab("Sea Surface Temperature (°C)") + 
  ylim(c(-2,13))+
  scale_x_date(limits=c(as_date("1999-09-01"),as_date("2000-08-31")),date_breaks="1 month",date_labels = "%b",expand=c(0.01,0)) +
  theme(legend.position=c(0.08,0.9),
        legend.text = element_text(size=15,family="sans"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=24,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title.y = element_text(size=20,family="sans"),
        axis.text.y = element_text(size=16,family="sans"),
        panel.border=element_rect(colour="black",size=0.75),
        #axis.text.x=element_text(size=20, color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        legend.key.size = unit(0.35,"cm"),
        plot.margin=unit(c(0,0.5,0.5,0.5),"cm")) 
pb4

#ROMS
ROMSdata3<-ROMS %>% 
  filter(between(depth,-200,-10)) %>% 
  mutate(domain=ifelse(depth<(-100),"outer", ifelse(depth< -50, "middle", "inner")),
         eco_short=ifelse(Ecosystem_Subarea=="Northern Bering Sea", "NBS", "SEBS"),
         eco2=paste(eco_short, domain)) %>% 
  #group_by(eco2,date) %>% 
  group_by(eco_short,domain,date) %>% 
  summarize(temp=mean(temp))%>%
  complete(date = seq.Date(min(date), max(date), by="day"))%>%
  #fill(temp, eco2) %>% #fill in the values
  fill(temp, eco_short, domain)%>%
  mutate(year=year(date),
         month=month(date),
         week=week(date),
         day=day(date),
         newdate=as.Date(ifelse(month>=9,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),#  Create a dummy year so that each year can more easily be overlain
                                as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
         year2=ifelse(month>=9,year+1,year))
#ROMS 
#ROMSdata3$eco2<-fct_relevel(ROMSdata3$eco2, c("NBS inner", "NBS middle", "NBS outer", "SEBS inner", "SEBS middle", "SEBS outer"))
ROMSdata3$domain<-fct_relevel(ROMSdata3$domain, c("outer", "middle", "inner"))

pb5<-ggplot() +
  geom_line(data=ROMSdata3 %>% filter(year2<last.year), # Older years are grey lines.
            aes(newdate,temp,group=factor(year2),col='#ACDDF1'),size=0.3) +
  geom_line(data=ROMSdata3 %>% filter(year2==last.year), # The previous year
            aes(newdate,temp,color='#FF8300'),size=1) +
  geom_line(data=ROMSdata3 %>% 
              filter(year2%in%mean.years) %>% # The mean from 1986-2015
              #group_by(eco2,newdate) %>% 
              group_by(eco_short, domain,newdate) %>% 
              summarise(meantemp=mean(temp,na.rm=TRUE)),
            aes(newdate,meantemp, col='#007934'), size=1,linetype="solid") +
  geom_line(data=ROMSdata3 %>% filter(year2==current.year), # the current year
            aes(newdate,temp,group=factor(year2),color='#B2292E'),size=0.75) +
  #facet_wrap(~eco2,ncol=1) +
  facet_grid(rows=vars(eco_short), cols=vars(domain)) +
  scale_color_manual(name="",
                     breaks=c('#B2292E','#FF8300','#ACDDF1','#007934'),
                     values=c('#B2292E','#FF8300','#ACDDF1','#007934'),
                     #values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
                     labels=c(current.year,last.year,paste0('1985-',last.year-1),mean.lab)) +
  scale_linetype_manual(values=c("solid","solid","solid","dashed")) +
  #scale_y_continuous(labels=scaleFUN)+
  ylim(c(-2,13))+
  scale_x_date(limits=c(as_date("1999-09-01"),as_date("2000-08-31")),date_breaks="1 month",date_labels = "%b",expand=c(0.01,0)) +
  ylab("ROMS Bottom Temperature (°C)") + 
  #xlab("Week") +
  theme(legend.position=c(0.08,0.9),
        legend.text = element_text(size=15,family="sans"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        strip.text.x=element_blank(),
        strip.text.y = element_text(size=24,color="white",family="sans",face="bold"),
        strip.background.y = element_rect(fill=OceansBlue2),
        axis.title.y = element_text(size=20,family="sans"),
        axis.text.y = element_text(size=16,family="sans"),
        panel.border=element_rect(colour="black",size=0.75),
        #axis.text.x=element_text(size=20,family="sans"),
        legend.key.size = unit(0.35,"cm"),
        axis.text.x=element_text(size=20, color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
        axis.title.x=element_blank(),
        plot.margin=unit(c(0,0.5,0.5,0.5),"cm")) 

pb5

pb6<-plot_grid(pb4,pb5,ncol=1)
png("EBS/2022/hottopic_sst_bt_inmidout.png", height=24, width=30, units="cm", res=300)
pb6
dev.off()


#Maps
library(AKmarineareas)
library(raster)


depth.colors<-c("#ACDDF1","#1ECAD3","#0093D0","#00467F")
sf_use_s2(FALSE)
esr<-AK_marine_area(area="Ecosystem Subarea")%>%
  filter(Ecosystem_Area=="Eastern Bering Sea")
ak<-AK_basemap()
depth_c<-rasterToContour(r.ak, levels=c(-10,-50,-100,-200))%>%
  st_as_sf()%>%
  st_intersection(esr)
depth_c$level<-fct_relevel(depth_c$level, c("-10", "-50", "-100", "-200"))



png("EBS/2022/domain_map.png", height=23, width=24, units="cm", res=300)
ggplot()+
  geom_sf(data= ak, color="gray", fill="gray")+
  geom_sf(data= esr, color="#FF4438", fill=NA)+
  
  geom_sf(data= depth_c, aes(color=level, fill=level))+
  coord_sf(xlim=c(-180,-155), ylim=c(66, 54))+
  scale_color_manual(values=depth.colors)+
  scale_fill_manual(values=depth.colors)+
  theme_void()+
  theme(legend.title=element_blank(),
        legend.position=c(0.9, 0.7))
dev.off()          
          
