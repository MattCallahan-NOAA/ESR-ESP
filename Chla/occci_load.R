# Download OCCCI and upload to AKFIN database
library(tidync)
library(tidyverse)
library(lubridate)
library(sf)
library(httr)
library(marmap)
library(akgfmaps)
library(RJDBC)
library(getPass)
library(akfinupload)


options(timeout=6000)
myyear <- 1998:2024
for(i in myyear){
  file_name <- paste0("occci/occci_",i,".nc")
  download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/pmlEsaCCI60OceanColor8Day_Lon0360.nc?chlor_a%5B(",i,"-01-01T00:00:00Z):1:(",i,"-12-31T00:00:00Z)%5D%5B(69):1:(47)%5D%5B(167):1:(230)%5D"),
                method = "libcurl", mode="wb",destfile = file_name)
}            
                             
# create lookup table
# function to import .nc data
tidy_chl<-function(file) {
  tidync(file) %>% 
    hyper_tibble()%>% 
    mutate(date=as_datetime(time),
           latitude=round(as.numeric(latitude),4),
           longitude=round(as.numeric(longitude),4),
           chlorophyll=round(chlor_a,3))
  
}

oc2019 <- tidy_chl("occci/occci_2019.nc")

#Identify max and min values
lat_lkp <- sort(unique(oc2019$latitude))
lon_lkp <- sort(unique(oc2019$longitude))

# create grid
occ_grid<-expand.grid(lon_lkp, lat_lkp)%>%
  rename(latitude=Var2, longitude=Var1)

test_join<-oc2019 %>%
  inner_join(occ_grid %>% mutate(test=1), by=c("longitude"="longitude", "latitude"="latitude"))  
any(is.na(test_join$test))
rm(test_join)

# get areas from akgfmaps
adfg <- get_adfg_areas() %>%
  st_transform(4326) %>%
  dplyr::select(STAT_AREA, WATERS_COD, geometry)
nmfs <- get_nmfs_areas(set.crs=4326) %>%
  mutate(NMFS_AREA=REP_AREA) %>%
  dplyr::select(NMFS_AREA, geometry)
esr <- get_esr_regions(select.region='esr_subarea', set.crs=4326) %>%
  mutate(ECOSYSTEM_SUBAREA=AREA_NAME) %>%
  dplyr::select(ECOSYSTEM_SUBAREA, geometry)
lme <- get_esr_regions(select.region='esr_area', set.crs=4326)%>%
  mutate(ECOSYSTEM_AREA=AREA_NAME) %>%
  dplyr::select(ECOSYSTEM_AREA, geometry)
bsierp <- get_bsierp_regions(set.crs=4326) %>%
  mutate(BSIERP_NAME=BSIERP_Region_Name) %>%
  dplyr::select(BSIERP_ID, BSIERP_NAME, geometry)
bbrkc <- akgfmaps::get_crab_strata(select.region = "bs.all", select.stock="bbrkc", set.crs = 4326) %>%
  mutate(BBRKC=STOCK) %>%
  dplyr::select(BBRKC, geom)


# convert lkp to spatial object
grid <- occ_grid%>%
  mutate(LAT=latitude,
         lon360=longitude,
         longitude = ifelse(longitude<180, longitude, longitude-360),
         LON=longitude)%>%
  st_as_sf(coords = c('LON', 'LAT'), crs = 4326, agr = 'constant')

# joins
grid<-st_join(grid, adfg, join = st_within)
grid<-st_join(grid, nmfs, join = st_within)
grid<-st_join(grid, lme, join = st_within)
grid<-st_join(grid, esr, join = st_within)
grid<-st_join(grid, bsierp, join = st_within)
grid<-st_join(grid, bbrkc, join = st_within)


# depth
#Also load marmap
r.ak <- getNOAA.bathy(lon1=-180,lon2=-128,lat1=46.5,lat2=69, resolution=0.25)
#Positive longitudes
r.ak_w <- getNOAA.bathy(lon1=167,lon2=180,lat1=47.5,lat2=69, resolution=0.25)

#converting to a raster
r.ak<-marmap::as.raster( r.ak)
r.ak_w <- marmap::as.raster( r.ak_w)

r.ak2<-raster::merge(r.ak, r.ak_w)
rm(r.ak)
rm(r.ak_w)
#merge combined raster and remove positive depths
grid<-grid%>%mutate(depth=round(raster::extract(r.ak2,cbind(longitude,latitude),method="bilinear"),0))  %>% filter(depth<=0) 

# also remove rows without stat area 
grid<-grid%>%
  filter(STAT_AREA>0)

#add jens grid
# Run jens grid code
# set extent
ymax <- 65.8
ymin <- 54.5
xmin <- -180
xmax <- -157

jens_grid <- st_make_grid(
  as(raster::extent(xmin,xmax,ymin,ymax), "SpatialPolygons") %>%
    st_as_sf(),
  cellsize = c(1, 0.5)) %>%
  st_set_crs(4326) %>% # Set projection CRS as 4326
  st_sf(gridid_MS=1:529,.) # Number the 529 squares (numbers by row, first left to right, then bottom to top)


#rename
jens_grid<- jens_grid %>%
  rename(jens_grid=gridid_MS)

#join
grid<-grid %>%
  st_join(jens_grid, join = st_within)


#plot
ggplot()+
  geom_point(data=grid, aes(x=lon360, y=latitude), size=0.1)
ggplot()+
  geom_point(data=grid, aes(x=lon360, y=latitude, color=NMFS_AREA), size=0.1)
ggplot()+
  geom_point(data=grid, aes(x=lon360, y=latitude, color=ECOSYSTEM_SUBAREA), size=0.1)
ggplot()+
  geom_point(data=grid, aes(x=lon360, y=latitude, color=BSIERP_NAME), size=0.1)
ggplot()+
  geom_point(data=grid, aes(x=lon360, y=latitude, color=jens_grid), size=0.1)

# create ID
#create glob_id
grid<-grid %>%
  mutate(occci_id=seq(from=1, to=nrow(grid), by=1))

# save the grid
occ_grid<-grid%>%data.frame()%>%dplyr::select(-geometry) %>% rename_with(toupper)
saveRDS(occ_grid, "occci/occ_grid.RDS")
# upload to AKFIN

jdbcDriver <- RJDBC::JDBC(driverClass="oracle.jdbc.OracleDriver",
                          classPath=system.file("driver", "ojdbc8.jar", package = "akfinupload") )
server<-"jdbc:oracle:thin:@akfin"

con<-DBI::dbConnect(jdbcDriver, server,
                    getPass(), # enter user name
                    getPass())
# upload lookup table

meta<-generate_metadata(occ_grid, "OCCCI_SPATIAL_LOOKUP")
create_akfin_table(con, occ_grid, "OCCCI_SPATIAL_LOOKUP", meta)
#dbWriteTable(con, "OCCCI_SPATIAL_LOOKUP", occ_grid)


# now upload all of the occci data
occ_grid<-readRDS("occci/occ_grid.RDS")
# we will start with 1998 to create the table
trim_chl<-function(file) {
  tidync(file) %>% 
    hyper_tibble()%>% 
    mutate(read_date=time,
           latitude=round(as.numeric(latitude),4),
           longitude=round(as.numeric(longitude),4),
           longitude = ifelse(longitude<180, longitude, longitude-360),
           chlorophyll=round(chlor_a,3)) %>%
    inner_join(occ_grid%>%
                 rename_with(tolower) %>%
                 dplyr::select(longitude, latitude, occci_id), 
                by=c("longitude"="longitude", "latitude"="latitude")) %>%
    dplyr::select(occci_id, read_date,  chlorophyll)
  
}

oc1998 <- trim_chl("occci/occci_1998.nc")

ggplot()+
  geom_point(data=oc1998 %>% 
               left_join(occ_grid, by=c("occci_id"="OCCCI_ID")) %>% 
               filter(read_date == "1998-05-09"), 
             aes(x=LON360, y=LATITUDE), size=0.1)

#meta<-generate_metadata(oc1998, "OCCCI_CHLA")
#create_akfin_table(con, oc1998, "OCCCI_CHLA", meta)

# Make sure that update works
# originally goofed up the lon360 and only uploaded west of the meridian
update_akfin_table(con, oc1998, "OCCCI_CHLA", overwrite=FALSE)

options(java.parameters="-Xmx8g")
# apply accross all years
# myyears <- 1999:2024 ran out of memory
myyears <- 2002:2024
lapply(myyears, FUN=function(x) {
  file_name <- paste0("occci/occci_",x,".nc")
  oc_data <- trim_chl(file_name)
  update_akfin_table(con, oc_data, "OCCCI_CHLA", overwrite=FALSE)})


dbDisconnect(con)

#### 2025 update
myyear <- 2025
for(i in myyear){
  file_name <- paste0("occci/occci_",i,".nc")
  download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/pmlEsaCCI60OceanColor8Day_Lon0360.nc?chlor_a%5B(",i,"-01-01T00:00:00Z):1:(",i,"-06-26T00:00:00Z)%5D%5B(69):1:(47)%5D%5B(167):1:(230)%5D"),
                method = "libcurl", mode="wb",destfile = file_name)
}    


# run the same process with 2025 as before
oc2025 <- trim_chl("occci/occci_2025.nc")


# ensure we're not uploading duplicate data
min(oc2025$read_date)
dbGetQuery(con, "SELECT MAX(READ_DATE) AS MAX_DATE FROM OCCCI_CHLA")

# Huh both datasets have a 2025-01-01.
db0101<- dbGetQuery(con, "SELECT * from OCCCI_CHLA where read_date = '2025-01-01'") %>%
  rename_with(tolower) %>%
  mutate(occci_id = as.numeric(occci_id),
         chlorophyll = as.numeric(chlorophyll))
oc0101 <- oc2025 %>% filter(read_date == "2025-01-01") 
setdiff(db0101, oc0101)

# since they're the same we'll filter it out from the data to upload
oc2025 <- oc2025 %>% filter(read_date != "2025-01-01")

# upload
update_akfin_table(con, oc2025, "OCCCI_CHLA", overwrite=FALSE)
