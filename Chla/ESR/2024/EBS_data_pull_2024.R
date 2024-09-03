library(tidyverse)
library(lubridate)
library(odbc)
library(getPass)
library(sf)



# pull data from akfin
#connect to akfin
#con <- dbConnect(odbc::odbc(), "akfin", UID=getPass(msg="USER NAME"), PWD=getPass()) 
con <- dbConnect(odbc::odbc(), "akfin",  UID="JNIELSEN", PWD=getPass())

#load data with depth, season, and region filters
glob<-dbFetch(dbSendQuery(con, "select round(avg(chla),2) meanchla, to_date(start_date,'YYYY-MM-DD')+4 mid_date, jens_grid, bsierp_id,bsierp_super_region 
from env_data.globcolour_2023 a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (1,2,3,4, 5, 6,7,8,9,10,11,12)
and ecosystem_area = ('Eastern Bering Sea')
and waters_cod = 'FED'
and depth>(-200)
and depth<(-10)
and jens_grid>=0
group by to_date(start_date,'YYYY-MM-DD')+4, jens_grid,bsierp_id,bsierp_super_region "))%>%
  rename_with(tolower)

head(glob)
saveRDS(glob,file='inter_jens_datafiles/globcolour_23augSQL.RDS')


table(glob$bsierp_id)
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
where extract(month from a.read_date) in (1,2,3,4, 5, 6,7)
and extract(year from a.read_date) >1997
and b.ecosystem = 'Eastern Bering Sea'
and b.depth>(-200)
and b.depth<(-10)
and b.jens_grid>=0
group by read_date, jens_grid "))%>%
  rename_with(tolower)

head(ice)
saveRDS(ice,file='inter_jens_datafiles/icedata_for_retreat_timing_23augSQL.RDS')



#seq(from=as.Date(paste0(2018,"-01-01")), to=as.Date(paste0(2018,"-06-30")), by=1)
#c(rep(as.Date(paste0(2018, "-04-03")), 6))



#merge based on nearest date
#create a lookup datable of daily vs 8 day data
glob_day_week_lookup <- function(year) {data.frame(
  seq(from=as.Date(paste0(year,"-04-01")), to=as.Date(paste0(year,"-06-30")), by=1),
  c(rep(as.Date(paste0(year, "-04-03")), 6), #
    rep(seq(from=as.Date(paste0(year,"-04-11")), to=as.Date(paste0(year,"-06-22")), by=8), each=8),
    rep(as.Date(paste0(year, "-06-30")),5)
  ))}



hm<-glob_day_week_lookup(2018)


#test<-glob_day_week_lookup(2023)
maxyear<-max(year(glob$mid_date))

dates_lkp<-lapply(1998:maxyear, FUN=function(x) {glob_day_week_lookup(x)})%>%
  bind_rows()

colnames(dates_lkp) <- c("date", "day8")

# Assign each date in daily ice data a corresponding 8 day date from globclour data
ice2<-ice%>%
  mutate(date=as.Date(read_date))%>%
  left_join(dates_lkp, by="date")


# Average ice faction by globcolour date
ice2<-ice2%>%
  group_by(day8, jens_grid)%>%
  summarize(mean_ice=round(mean(ice_fraction),2))

# More ice2 than glob is expected since ice data isn't limited by cloud cover

# Join average ice to chla data
glob2<-glob%>%
  mutate(mid_date=as.Date(mid_date))%>%
  left_join(ice2, by=c("mid_date"="day8", "jens_grid"="jens_grid"))


## QA
#let's check this for a random date and jens_area: May 13 2006 in jens 400
glob_ice_test<-dbFetch(dbSendQuery(con, "select round(avg(a.sea_ice_fraction),2) ice_fraction, a.read_date, b.jens_grid
from afsc.erddap_crw_sst a
left join env_data.crw_lookup_with_jens_grid b on a.crw_id=b.crw_id
where extract(month from a.read_date) =5
and extract(year from a.read_date) =2006
and b.ecosystem = 'Eastern Bering Sea'
and b.state_fed = 'FED'
and b.depth>(-200)
and b.depth<(-10)
and b.jens_grid = '400'
group by read_date, jens_grid "))%>%
  rename_with(tolower)

test_5_13<-glob_ice_test%>%
  filter(read_date>=as.Date("2006-05-09") & read_date<as.Date("2006-05-17"))
mean(test_5_13$ice_fraction) #0.17

glob2%>%
  filter(jens_grid=='400' & mid_date==as.Date("2006-05-13")) #0.17
#matches up
