library(tidyverse)
library(lubridate)
library(odbc)
library(getPass)
library(sf)



# pull data from akfin
#connect to akfin
con <- dbConnect(odbc::odbc(), "akfin", UID=getPass(msg="USER NAME"), PWD=getPass()) 

#load data with depth, season, and region filters
glob<-dbFetch(dbSendQuery(con, "select round(avg(chla),2) meanchla, to_date(start_date,'YYYY-MM-DD')+4 mid_date, jens_grid
from env_data.globcolour_2023 a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and ecosystem_area = ('Eastern Bering Sea')
and waters_cod = 'FED'
and depth>(-200)
and depth<(-10)
and jens_grid>=0
group by to_date(start_date,'YYYY-MM-DD')+4, jens_grid"))%>%
  rename_with(tolower)

##Get sea ice data
# # this is the code used to add Jens' grid to the crw lookup table
# # download crw lookup table
# crw<-dbFetch(dbSendQuery(con, "select * from afsc.erddap_crw_sst_spatial_lookup"))%>%
#   rename_with(tolower)
# # convert to sf object
# crw<-crw %>%
#   mutate(LAT=latitude, LON=longitude)%>%
#   st_as_sf(coords = c('LON', 'LAT'), crs = 4326, agr = 'constant') %>%
#   st_transform(crs=6393)
# 
# #code for jens grid
# # Run jens grid code
# ymax <- 65.8
# ymin <- 54.5
# xmin <- -180
# xmax <- -157
# jens_grid <- st_make_grid(
#   as(raster::extent(xmin,xmax,ymin,ymax), "SpatialPolygons") %>%
#     st_as_sf(),
#   cellsize = c(1, 0.5)) %>%
#   st_set_crs(4326) %>% # Set projection CRS as 4326
#   st_sf(jens_grid=1:529,.) %>% # Number the 529 squares (numbers by row, first left to right, then bottom to top)
#   st_transform(crs=6393) #reproject
# 
# crw<-crw %>%
#   st_join(jens_grid, join = st_within)
# 
# 
# #convert to dataframe
# crw<-crw %>%
#   data.frame() %>%
#   dplyr::select(-geometry)
# 
# crw<-crw%>%
#   rename(crw_id=id)
# 
# 
# #write to akfin db
# # write as csv for now... still haven't solved uploading from R
# write.csv(crw, "crw_lkp_with_jens_grid.csv", row.names = FALSE)
# 

#Download ice by jens area with the same filters as chla
#this may take 10-15 min
ice<-dbFetch(dbSendQuery(con, "select round(avg(a.sea_ice_fraction),2) ice_fraction, a.read_date, b.jens_grid
from afsc.erddap_crw_sst a
left join env_data.crw_lookup_with_jens_grid b on a.crw_id=b.crw_id
where extract(month from a.read_date) in (4, 5, 6)
and extract(year from a.read_date) >1997
and b.ecosystem = 'Eastern Bering Sea'
and b.state_fed = 'FED'
and b.depth>(-200)
and b.depth<(-10)
and b.jens_grid>=0
group by read_date, jens_grid "))

