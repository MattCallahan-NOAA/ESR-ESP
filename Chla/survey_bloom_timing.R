# chlorophyll for Erin/Louise/Jens

library(RJDBC)
library(tidyverse)
library(keyring)
library(sf)
library(akgfmaps)
library(scales)
library(cowplot)
library(heatwaveR)

jdbcDriver <- RJDBC::JDBC(driverClass="oracle.jdbc.OracleDriver",
                          classPath=system.file("driver", "ojdbc8.jar", package = "akfinupload") )
#server<-"jdbc:oracle:thin:@akfin"
server<-"jdbc:oracle:thin:@//tiger:2045/akfin.psmfc.org"

con<-dbConnect(jdbcDriver, 
               server, 
               key_list("akfin_oracle_db")$username, 
               keyring::key_get("akfin_oracle_db", keyring::key_list("akfin_oracle_db")$username))

# Tasks
# 1) Associate chla bloom and ice retreat with each bottom trawl grid station
# # download chla averaged for each station (including a count). 
# # Calculate bloom timing
# # download ice for each station
# # determine ice extent at peak bloom. 
# # run Jens code to determin whether it's ice-associated or not 
# # 


### get occci and crw id for each station
# pull lookup table
occci_lkp<-dbGetQuery(con,"select * from env_data.occci_spatial_lookup") %>%
  rename_with(tolower)

sg<-get_base_layers(select.region = "ebs")
sgg<-sg$survey.grid


occci_ebs <- occci_lkp %>%
  filter(ecosystem_area=="Eastern Bering Sea") %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_transform(st_crs(sgg))

occci_sg <- occci_ebs %>%
  st_join(sgg, join=st_intersects)

occci_sg2<-occci_sg %>%
  data.frame() %>%
  dplyr::select(occci_id, STATION) %>%
  rename_with(toupper) %>%
  filter(!is.na(STATION))

dbWriteTable(con, "SURVEY_GRID_OCCCI", occci_sg2 )

### crw
crw_lkp<-dbGetQuery(con,"select * from afsc.erddap_crw_sst_spatial_lookup
                      where ecosystem='Eastern Bering Sea'") %>%
  rename_with(tolower)

crw_ebs <- crw_lkp %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>%
  st_transform(st_crs(sgg))

crw_sg <- crw_ebs %>%
  st_join(sgg, join=st_intersects)

crw_sg2<-crw_sg %>%
  data.frame() %>%
  dplyr::select(id, STATION) %>%
  rename_with(toupper) %>%
  filter(!is.na(STATION))

dbWriteTable(con, "SURVEY_GRID_CRW", crw_sg2 )


# download amj chl averaged by survey area
occci_chla_sg<-dbGetQuery(con,"select a.read_date, b.station, avg(chlorophyll) meanchla 
from env_data.occci_chla a
inner join mcallahan.survey_grid_occci b on a.occci_id = b.occci_id
group by read_date, station
")

# take only the top chla for each station and year.
occci_chla_sg2<-occci_chla_sg %>%
  rename_with(tolower) %>%
  mutate(date=as.Date(read_date)+4,
         month=month(date),
         year=year(date),
         doy=yday(date)) %>%
  filter(month %in% 4:6) %>%
  group_by(year, station) %>%
  arrange(date, .by_group = TRUE) %>%
  slice_max(meanchla, n=1, with_ties=FALSE) %>%
  ungroup()

# Download ice
crw_ice_sg<-dbGetQuery(con,"SELECT
    a.read_date,
    b.station,
    AVG(a.sea_ice_fraction) AS mean_sea_ice
FROM afsc.erddap_crw_sst a
INNER JOIN (SELECT to_number(id) CRW_ID, STATION FROM mcallahan.survey_grid_crw ) b
    ON a.crw_id = b.CRW_ID
WHERE EXTRACT(MONTH FROM read_date) BETWEEN 4 AND 6
AND EXTRACT(YEAR FROM read_date) >1997
GROUP BY
    a.read_date,
    b.station
ORDER BY
    a.read_date,
    b.station
")

# get last ice date
sea_ice_latest <- crw_ice_sg %>%
  rename_with(tolower) %>%
  mutate(
    year = lubridate::year(read_date),
    doy = lubridate::yday(read_date)
  ) %>%
  filter(mean_sea_ice >= 0.15) %>%
  summarise(
    last_ice_doy = max(doy),
    .by = c(year, station)
  )

  saveRDS(sea_ice_latest, "../../Chla/data/sea_ice_latest.RDS")
  saveRDS(occci_chla_sg2, "../../Chla/data/staton_bloom_peak.RDS")
  saveRDS(crw_ice_sg, "../../Chla/data/station_sea_ice.RDS")
  saveRDS(occci_chla_sg, "../../Chla/data/staton_chlorophyll.RDS")
  

# plot bloom timing 2025
c26<-sgg %>%
  left_join(occci_chla_sg2 %>% 
  filter(year==2026), by=c("STATION"="station"))%>%
ggplot()+
  geom_sf(aes(fill=doy))+
  scale_fill_viridis_c()

i26<-sgg %>%
  left_join(sea_ice_latest %>% 
              filter(year==2026), by=c("STATION"="station"))%>%
  ggplot()+
  geom_sf(aes(fill=last_ice_doy))
  
gridExtra::grid.arrange(c26,i26,nrow=1)

# join the two datasets and look at 2026 as an example

# 2) associate chla and ice retreat timing with each BSIERP region (confirm that we don't want to do crab regions instead)
bsierp<-get_bsierp_regions(set.crs=st_crs(sgg))
crab <- get_crab_strata(set.crs=st_crs(sgg))

ggplot()+
  #geom_sf(data=occci_sg, aes(color=jens_grid), size=0.1)+
  geom_sf(data=sgg, fill=NA)+
  geom_sf(data=bsierp%>%filter(BSIERP_ID!=15), fill=NA, color="blue")

ggplot()+
  #geom_sf(data=occci_sg, aes(color=jens_grid), size=0.1)+
  geom_sf(data=sgg, fill=NA)+
  #geom_sf(data=bsierp%>%filter(BSIERP_ID!=15), fill=NA, color="blue")+
  geom_sf(data=crab, fill=NA, color="red")+
  theme_bw()