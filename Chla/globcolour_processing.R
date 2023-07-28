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
  rename(latitude=Var2, longitude=Var1)

  # b Test joins, record counts, etc. 
glob98<-glob98_e %>%
  bind_rows(glob98_w)

#join chla data with lookup table.   
test_join2<-glob98%>%
  mutate(latitude=as.numeric(substr(lat, 1, 6)),
         longitude=ifelse(lon>0, as.numeric(substr(lon, 1, 8)), as.numeric(substr(lon, 1, 9))))%>%
  inner_join(glob_grid, by=c("longitude"="longitude", "latitude"="latitude")) 

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
grid <- glob_grid%>%
  mutate(LAT=latitude, LON=longitude)%>%
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
grid<-grid%>%mutate(depth=round(raster::extract(r.ak2,cbind(longitude,latitude),method="bilinear"),0))  %>% filter(depth<=0) 

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

# 2 Create annual files with ID, startdate, and chla
######This needs work
  # a join data
glob23e<-readRDS("data/2023_e/globcolour_2023_e_update.RDS")
big_glob<-function(year)

# 3 Upload globcolour lkp and data to AKFIN
  con <- dbConnect(odbc::odbc(), "akfin", UID=getPass(msg="USER NAME"), PWD=getPass()) 
#write to env_data schema
dbWriteTable(con, "GLOBCOLOUR_SPATIAL_LOOKUP", grid %>% rename_with(toupper))
  

