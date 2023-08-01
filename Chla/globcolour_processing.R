# process globcolour data
library(tidyverse)
library(tidync)
library(sf)
library(akmarineareas2)
library(marmap)
library(lubridate)
library(odbc)
library(getPass)


# 1 Create and validate spatial lookup table
  # skip to line ____ if you already have the lookup table
  # a create grid of lat and lon including w of the dateline, have code already
glob98_e<-readRDS("Data/2022_e/1998_GlobColour_AV.RDS")
glob98_w<-readRDS("Data/2023_w/1998_glob_w.RDS")
#Identify max and min values
max(glob98_w$lat);min(glob98_w$lat)
max(glob98_w$lon);min(glob98_w$lon)

#create new vectors with 4 decimal places
#The excessively long numbers were used because smaller fewer decimals led to a missmatch in the join
lon_lkp<-as.numeric(c(substr(seq(from=-179.97916666666666667, to=-128.0208, by=0.0416666666666667),  1, 9),#east
           substr(seq(from=166.979197916666666666667, to=179.9792, by=0.0416666666666667), 1, 8)))#west

lat_lkp<-as.numeric(substr(seq(from=47.020826666666666666666666667, to=69.02083, by=0.04166666666666666666666667), 1, 6))

#create grid
glob_grid<-expand.grid(lon_lkp, lat_lkp)%>%
  rename(join_lat=Var2, join_lon=Var1)

  # b Test joins, record counts, etc. 
glob98<-glob98_e %>%
  bind_rows(glob98_w)

#join chla data with lookup table.   
test_join2<-glob98%>%
  mutate(join_lat=as.numeric(substr(lat, 1, 6)),
         join_lon=ifelse(lon>0, as.numeric(substr(lon, 1, 8)), as.numeric(substr(lon, 1, 9))))%>%
  inner_join(glob_grid, by=c("join_lon"="join_lon", "join_lat"="join_lat")) 

#save chlorophyll lat and lon as chl_lat and chl_lon, and do the spatial operations on those.
glob_grid2<-test_join2 %>%
  group_by(join_lat, join_lon) %>%
  summarize(chla_lon=mean(lon),
            chla_lat=mean(lat))

#make sure holes are just around the edges 
test3<-glob_grid%>%
  left_join(glob_grid2, by=c("join_lon"="join_lon", "join_lat"="join_lat"))

ggplot()+
  geom_point(data=test3[is.na(test3$chla_lat),], mapping=aes(x=join_lon, y=join_lat))

  # c Add spatial stata
#use the AK marine areas package to bring in spatial strata
adfg<-adfg %>%
  dplyr::select(! c(Area_Type,AreaID,Area_Name,BSIERP_ID,BSIERP_Region_Name,Ecosystem_Area,
                    Ecosystem_Subarea,NMFS_REP_AREA,Shape_Length,Shape_Area)) %>%
  st_make_valid()
nmfs<-nmfs%>%
  dplyr::select(! c(Area_Type,AreaID,Area_Name,BSIERP_ID,BSIERP_Region_Name,Ecosystem_Area,
                    Ecosystem_Subarea,STAT_AREA,WATERS_COD,Shape_Length,Shape_Area)) %>%
  st_make_valid()
esr<-esr%>%
  dplyr::select(! c(Area_Type,AreaID,Area_Name,BSIERP_ID,BSIERP_Region_Name,Ecosystem_Area,
                  NMFS_REP_AREA,STAT_AREA,WATERS_COD,Shape_Length,Shape_Area)) %>%
  st_make_valid()
lme<-lme%>%
  dplyr::select(! c(Area_Type,AreaID,Area_Name,BSIERP_ID,BSIERP_Region_Name,
                    Ecosystem_Subarea,NMFS_REP_AREA,STAT_AREA,WATERS_COD,Shape_Length,Shape_Area)) %>%
  st_make_valid()
bsierp<-bsierp%>%
  dplyr::select(! c(Area_Type,AreaID,Area_Name,Ecosystem_Area,
                    Ecosystem_Subarea,NMFS_REP_AREA,STAT_AREA,WATERS_COD,Shape_Length,Shape_Area)) %>%
  st_make_valid()

#Turn off spherical geometry
sf::sf_use_s2(FALSE)

#make grid into a spatial object
grid <- glob_grid2%>%
  mutate(LAT=chla_lat, LON=chla_lon)%>%
  st_as_sf(coords = c('LON', 'LAT'), crs = 4326, agr = 'constant') %>%
  st_transform(crs=6393)


#ADFG
grid<-st_join(grid, adfg, join = st_within)


ggplot()+
  geom_sf(data=grid, aes(color=STAT_AREA))

#brings from 800k to <400k
grid<-grid%>%
  filter(STAT_AREA>0)

#NMFS
grid<-grid%>%
  st_join(nmfs, join = st_within)


#Ecosystem Area
grid<-grid%>%
  st_join(lme, join = st_within)

#Ecosystem subarea
grid<-grid%>%
  st_join(esr, join = st_within)


#BSIERP
grid<-grid%>%
  st_join(bsierp, join = st_within)

#Also load marmap
r.ak <- getNOAA.bathy(lon1=-180,lon2=-128,lat1=46.5,lat2=69, resolution=1)
#Positive longitudes
r.ak_w <- getNOAA.bathy(lon1=167,lon2=180,lat1=47.5,lat2=69, resolution=1)

#converting to a raster
r.ak<-marmap::as.raster( r.ak)
r.ak_w <- marmap::as.raster( r.ak_w)

r.ak2<-raster::merge(r.ak, r.ak_w)

#merge combined raster and remove positive depths
grid<-grid%>%mutate(depth=round(raster::extract(r.ak2,cbind(chla_lon,chla_lat),method="bilinear"),0))  %>% filter(depth<=0) 

#that processing was slow so save here
saveRDS(grid, "data/grid1.RDS")

# add crab areas.
#load fgdb files and reproject
nbs_crab <- st_read(dsn="data/gis/AK_Crab_Management_Areas.gdb", layer="NBS_Crab_dd") %>%
  dplyr::select(!c(Shape_Length, Shape_Area))%>%
  st_transform(crs=6393)

bs_tanner_crab <- st_read(dsn="data/gis/AK_Crab_Management_Areas.gdb", layer="BS_Tanner_Crab_dd")%>%
  dplyr::select(!c(Shape_Length, Shape_Area))%>%
  st_transform(crs=6393)

bs_king_crab <- st_read(dsn="data/gis/AK_Crab_Management_Areas.gdb", layer="BS_King_Crab_dd")%>%
  dplyr::select(!c(Shape_Length, Shape_Area))%>%
  st_transform(crs=6393)

#NBS crab
grid<-grid%>%
  st_join(nbs_crab, join = st_within)

#BS King crab
grid<-grid%>%
  st_join(bs_king_crab, join = st_within)

#BS Tanner crab
grid<-grid%>%
  st_join(bs_tanner_crab, join = st_within)
 
  # add jens grid
# set extent
ymax <- 65.8
ymin <- 54.5
xmin <- -180
xmax <- -157

  # Run jens grid code
jens_grid <- st_make_grid(
  as(raster::extent(xmin,xmax,ymin,ymax), "SpatialPolygons") %>%
    st_as_sf(),
  cellsize = c(1, 0.5)) %>%
  st_set_crs(4326) %>% # Set projection CRS as 4326
  st_sf(gridid_MS=1:529,.) # Number the 529 squares (numbers by row, first left to right, then bottom to top)

#reproject
jens_grid<-jens_grid %>%
  st_transform(crs=6393)

#rename
jens_grid<- jens_grid %>%
  rename(jens_grid=gridid_MS)

#join
grid<-grid %>%
  st_join(jens_grid, join = st_within)

  # d Add crw ID
crw<-readRDS("data/crw_spatial_lookup.RDS") %>%
  rename_with(tolower) %>%
  mutate(crw_id=id) %>%
  dplyr::select(crw_id, latitude, longitude)%>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326, agr = 'constant') %>%
  st_transform(crs=6393)

grid<-grid %>%
  st_join(crw, join = st_nearest_feature)

#create glob_id
grid<-grid %>%
  mutate(glob_id=seq(from=1, to=nrow(grid), by=1))
  
#convert to dataframe
grid<-grid %>%
  data.frame() %>%
  dplyr::select(-geometry)

#save
saveRDS(grid%>%rename_with(tolower), "data/globcolour_spatial_lookup.RDS")


## QA lookup table with a different year. 
 # load spatial lookup table
lkp<-readRDS("data/globcolour_spatial_lookup.RDS")

  # load chla data
#east
glob23e<-readRDS("data/2023_e/globcolour_2023_e_update.RDS")%>%
  rename_with(tolower) %>%
  rename(chla=chl1_mean) %>%
  filter(!is.na(chla)) %>%
    mutate(start_date=ymd(substr(id, 5, 12))) %>% 
  dplyr::select(start_date, lon, lat, chla)
#west
glob23w<-readRDS("data/2023_w/2023_glob_w.RDS")%>%
  rename_with(tolower) %>%
  rename(chla=chl1_mean) %>%
  filter(!is.na(chla)) %>%
  mutate(start_date=ymd(substr(id, 5, 12))) %>% 
  dplyr::select(start_date, lon, lat, chla)
#combine
glob23<-glob23e%>%
  bind_rows(glob23w)
rm(glob23e);rm(glob23w)

# join
glob23_strata<-glob23 %>%
mutate(lat=as.numeric(substr(lat, 1, 6)),
       lon=ifelse(lon>0, as.numeric(substr(lon, 1, 8)), as.numeric(substr(lon, 1, 9))))%>%
  inner_join(lkp, by=c("lon"="join_lon", "lat"="join_lat")) 
#3975048 record counts

# QA 
# point in polygon with adfg
glob23s<-glob23 %>%
  mutate(LON=lon,
         LAT=lat) %>%
  st_as_sf(coords=c("LON","LAT"), crs = 4326, agr = 'constant') %>%
  st_transform(crs=6393)

#ADFG
glob23s<-st_join(glob23s, adfg, join = st_within) 

#
glob23s<-glob23s%>%
  filter(STAT_AREA>0)
# filter to depth <0
#merge combined raster and remove positive depths
glob23s<-glob23s%>%mutate(depth=round(raster::extract(r.ak2,cbind(lon,lat),method="bilinear"),0))  %>% filter(depth<=0) 

#3975048 ... same record counts. Sweet.

# QA code to track down missmatch from when I originally did the look up table with join_lon and join_lat
#join to figure out which missmatched
#pare down fields
# glob23s<-glob23s %>%
#   data.frame() %>%
#   dplyr::select(!c(geometry, WATERS_COD))
# 
# glob23_strata<-glob23_strata%>%
#   dplyr::select(start_date, lon, lat, chla, stat_area, glob_id, ecosystem_subarea, depth)
# 
# test<-glob23_strata %>%
#   left_join(glob23s%>%
#               mutate(lat=as.numeric(substr(lat, 1, 6)),
#                      lon=ifelse(lon>0, as.numeric(substr(lon, 1, 8)), as.numeric(substr(lon, 1, 9))))%>%
#   mutate(XX=1) %>%
#   dplyr::select(start_date, lon, lat, XX, depth), by=c("lon"="lon", "lat"="lat", "start_date"="start_date"))
# 
# miss<-test[is.na(test$XX),]
# summary(miss)
# #519 misses
# 
# #looks like a close to shore, depth related issue
# ggplot()+
#   geom_point(data=miss, aes(x=ifelse(lon<0, lon, lon-360), y=lat), color="red")+
#   geom_sf(data=ak%>%st_transform(crs=4326))+
#   xlim(c(-190, -129))
# 

# 2 Create annual files with ID, startdate, and chla

#create annual files
#I just moved 2023 to 2022 and named it 
merge_ew<-function(year) {
readRDS(paste0("data/2022_e/",year,"_GlobColour_AV.RDS"))%>%
    rename_with(tolower) %>%
    dplyr::select(chl1_mean, lon, lat, id)%>%
    bind_rows(readRDS(paste0("data/2023_w/",year,"_glob_w.RDS"))%>%
                rename_with(tolower) %>%
                dplyr::select(chl1_mean, lon, lat, id))%>%
    saveRDS(paste0("data/2023_combined/",year, "glob_ew.RDS"))
}

lapply(1998:2023, FUN=merge_ew)

#check positive lons present
readRDS("data/2023_combined/2005glob_ew.RDS")%>%
  summary()

#create files to push to akfin db
#shorten lookup table
lkp_trim<-lkp%>%
  dplyr::select(join_lat, join_lon, glob_id)

glob_trim<-function(year) {
readRDS(paste0("data/2023_combined/",year,"glob_ew.RDS"))%>%
    rename(chla=chl1_mean) %>%
    filter(!is.na(chla)) %>% #should have done this in the last function. oh well
    mutate(start_date=ymd(substr(id, 5, 12)),
          join_lat=as.numeric(substr(lat, 1, 6)),
          join_lon=ifelse(lon>0, as.numeric(substr(lon, 1, 8)), as.numeric(substr(lon, 1, 9)))) %>% 
    dplyr::select(start_date, join_lon, join_lat, chla)%>%
    inner_join(lkp_trim, by=c("join_lon"="join_lon", "join_lat"="join_lat"))%>%
    dplyr::select(glob_id, start_date, chla)%>%
    saveRDS(paste0("data/2023_combined/",year,"_for_akfin.RDS"))
    }
  
lapply(1998:2023, FUN=glob_trim)


#create massive object
glob_data <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(glob_data) <- c("glob_id",  "start_date", "chla")
glob_data<-glob_data %>%
  mutate(across(c(glob_id, chla), as.numeric),
         start_date=as.character(start_date))

for (i in 1998:2023) {
  glob_data<-glob_data%>%
    bind_rows(readRDS(paste0("data/2023_combined/",i,"_for_akfin.RDS"))%>%
                mutate(start_date=as.character(start_date)))
}

glob_data<-glob_data %>%
  mutate(start_date=as.character(start_date))

# 3 Upload globcolour lkp and data to AKFIN
  con <- dbConnect(odbc::odbc(), "akfin", UID=getPass(msg="USER NAME"), PWD=getPass()) 
#write to env_data schema
dbWriteTable(con, "GLOBCOLOUR_SPATIAL_LOOKUP", lkp %>% rename_with(toupper))
  

  
# write all globcolour data
dbWriteTable(con, "GLOBCOLOUR", glob_data %>% rename_with(toupper))
