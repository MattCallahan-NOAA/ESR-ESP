# Height of surface boundary layer

library(RNetCDF)
library(tidync)
require(tidyverse)
require(lubridate)
library(sf)
library(akmarineareas2)
library(marmap)
library(cowplot)

#  Access netcdfs from THREDDS server (https://data.pmel.noaa.gov/aclim/thredds/catalog.html)

#  Read in our lookup table for the Bering Sea to find some spatial bounds.
lkp <- readRDS("EBS/Data/crwsst_spatial_lookup_table.RDS") %>% 
  filter(Ecosystem=="Eastern Bering Sea") %>% 
  dplyr::select(longitude,latitude) %>% 
  summarise(maxlat=max(latitude),
            minlat=min(latitude),
            maxlon=max(longitude),
            minlon=min(longitude))

#  We need to access the file containing the extended spatial grids, which contains different transformations of the spatial coordinates.
#grid<-tidync("https://data.pmel.noaa.gov/aclim/thredds/dodsC/extended_grid/Bering10K_extended_grid.nc")
#2023 update
grid<-tidync("https://data.pmel.noaa.gov/aclim/thredds/dodsC/ancillary/Bering10K_extended_grid.nc") #new
#grid<-tidync("https://data.pmel.noaa.gov/aclim/thredds/dodsC/B10K-K20_CORECFS/Level2/2020-2024/B10K-K20_CORECFS_2020-2024_average_temp_bottom5m.nc")

#  One of the native grids contains the columns we need to match the bottom temperature data to latitudes and longitudes.
#This didn't work 2023
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


# specify years because for some reason ROMS data is in 5 year bins
myyears<-c("2020-2024", "2015-2019", "2010-2014", "2005-2009", "2000-2004", "1995-1999", "1990-1994", "1985-1989")


# loop across years to download data 
hsbl<- lapply(myyears, FUN=function(x) paste0(
  "https://data.pmel.noaa.gov/aclim/thredds/dodsC/B10K-K20_CORECFS/Level1/",x,"/B10K-K20_CORECFS_",x,"_average_Hsbl.nc") %>%
    hyper_tibble() %>% 
    mutate(date=as_date(as_datetime(ocean_time,origin="1900-01-01 00:00:00", tz = "UTC")))) %>% 
  bind_rows()

  saveRDS(hsbl, "EBS/Data/hsbl.RDS")
  hsbl<-readRDS("EBS/Data/hsbl.RDS")

# point in polygon operation for lookup table
  sf_use_s2(FALSE)
  
 #  Read in the ESR shapefile and subset for the Bering areas
  esr_shp <- esr_dd %>% 
    filter(Ecosystem_Subarea%in%c("Northern Bering Sea","Southeastern Bering Sea"))
  
  # convert to spatial object
  esr_pts <- st_as_sf(grid_lkp, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
  
  # join to  esr
  esr_pts <- esr_pts %>% st_join(esr_shp, join = st_intersects)
  
  esr_pts <- esr_pts %>%
    filter(!is.na(Ecosystem_Subarea)) %>% 
    data.frame %>% # THis will drop the latitude and longitude point geometry column
    dplyr::select(Ecosystem_Subarea,lat_rho,lon_rho,xi_rho,eta_rho,BSIERP_ID)
  
  esr_pts %>% 
    ggplot(aes(lon_rho,lat_rho)) + 
    geom_point()
  
  # add depth
  r.ak <- marmap::as.raster(getNOAA.bathy(lon1=lkp$minlon,lon2=lkp$maxlon,lat1=lkp$minlat,lat2=lkp$maxlat, resolution=0.25))
  
  esr_pts <-esr_pts %>%
    mutate(depth=round(raster::extract(r.ak,cbind(lon_rho,lat_rho),method="bilinear"),0))
  
  #filter hslb to lookup table and add interperatable latlon
  hsbl <- hsbl %>%
    inner_join(esr_pts, by = c("xi_rho"="xi_rho","eta_rho"="eta_rho"))

  # adjust for plotting
  hsbl <-hsbl %>%
    filter(between(depth,-200,-10)) %>% 
    mutate(domain=ifelse(depth<(-100),"outer", ifelse(depth< -50, "middle", "inner")),
           eco_short=ifelse(Ecosystem_Subarea=="Northern Bering Sea", "NBS", "SEBS"),
           eco2=paste(eco_short, domain)) %>% 
    #group_by(eco2,date) %>% 
    group_by(eco_short,domain,date) %>% 
    summarize(hsbl=mean(Hsbl))%>%
    complete(date = seq.Date(min(date), max(date), by="day"))%>%
    #fill(temp, eco2) %>% #fill in the values
    fill(hsbl, eco_short, domain)%>%
    mutate(year=year(date),
           month=month(date),
           week=week(date),
           day=day(date),
           newdate=as.Date(ifelse(month>=9,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),#  Create a dummy year so that each year can more easily be overlain
                                  as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
           year2=ifelse(month>=9,year+1,year))
  
  #plot
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
  
  current.year <- max(hsbl$year2)
  last.year <- current.year-1
  mean.years <- 1986:2016 # We use the oldest 30-year time series as our climatological baseline.
  mean.lab <- "Mean 1986-2016"
  
  hsbl$domain<-factor(hsbl$domain, c("outer", "middle", "inner"))

  
  pb5<-ggplot() +
    geom_line(data=hsbl %>% filter(year2<last.year), # Older years are grey lines.
              aes(newdate,hsbl,group=factor(year2),col='mygrey'),size=0.3) +
    geom_line(data=hsbl %>% filter(year2==last.year), # The previous year
              aes(newdate,hsbl,color='last.year.color'),size=1) +
    geom_line(data=hsbl %>% 
                filter(year2%in%mean.years) %>% # The mean from 1986-2015
                #group_by(eco2,newdate) %>% 
                group_by(eco_short, domain,newdate) %>% 
                summarise(meanhsbl=mean(hsbl,na.rm=TRUE)),
              aes(newdate,meanhsbl, col='mean.color'), size=1,linetype="solid") +
    geom_line(data=hsbl %>% filter(year2==current.year), # the current year
              aes(newdate,hsbl,group=factor(year2),color='current.year.color'),size=0.75) +
    #facet_wrap(~eco2,ncol=1) +
    facet_grid(rows=vars(eco_short), cols=vars(domain)) +
    scale_color_manual(name="",
                       breaks=c('current.year.color','last.year.color','mygrey','mean.color'),
                       values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
                       labels=c(current.year,last.year,paste0('1986-',last.year-1),mean.lab)) +
    scale_linetype_manual(values=c("solid","solid","solid","dashed")) +
    #scale_y_continuous(labels=scaleFUN)+
    scale_x_date(limits=c(as_date("1999-09-01"),as_date("2000-08-31")),date_breaks="1 month",date_labels = "%b",expand=c(0.01,0)) +
    ylab("HSBL") + 
    #xlab("Week") +
    theme(legend.position=c(0.7,0.2),
          legend.text = element_text(size=15,family="sans"),
          legend.background = element_blank(),
          legend.title = element_blank(),
          #strip.text=element_blank(),
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

  png("EBS/2024/hsbl.png", height=16, width=24, units="cm", res=300)
  pb5
  dev.off()  
  