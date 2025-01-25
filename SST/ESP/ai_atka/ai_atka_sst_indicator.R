library(RJDBC)
library(sf)
library(ggplot2)
library(akgfmaps)

#connect to akfin
jdbcDriver <-jdbcDriver <- RJDBC::JDBC(driverClass="oracle.jdbc.OracleDriver",
                                       classPath="java/ojdbc8.jar")

con<-DBI::dbConnect(jdbcDriver, "jdbc:oracle:thin:@//tiger:2045/akfin.psmfc.org",
                    keyring::key_list("akfin_oracle_db")$username,
                    keyring::key_get("akfin_oracle_db", keyring::key_list("akfin_oracle_db")$username))

# download avg sst by year for whole ai. 
ai_sst_combined <- dbFetch(dbSendQuery(con, "select extract(year from a.read_date) year, round(avg(temp), 2) as sst_mean, round(stddev(temp), 2) as sst_sd
from afsc.erddap_crw_sst a
left join afsc.erddap_crw_sst_spatial_lookup b on a.crw_id = b.id
where b.nmfsarea in (541, 542, 543)
and depth between -100 and 0
and extract(month from a.read_date) * 100 + extract(day from a.read_date) between 715 and 1015
group by extract(year from a.read_date)"))

ai_sst_combined <- ai_sst_combined %>%
 # rename("year" = "EXTRACT(YEARFROMA.READ_DATE)") %>% # originally forgot to rename in sql
  rename_with(tolower)

saveRDS(ai_sst_combined, "ai_atka/ai_sst_combined.RDS")

# download avg sst by year and nmfs area. 
ai_sst_nmfs <- dbFetch(dbSendQuery(con, "select extract(year from a.read_date) year, b.nmfsarea, round(avg(temp), 2) as sst_mean, round(stddev(temp), 2) as sst_sd
from afsc.erddap_crw_sst a
left join afsc.erddap_crw_sst_spatial_lookup b on a.crw_id = b.id
where b.nmfsarea in (541, 542, 543)
and depth between -100 and 0
and extract(month from a.read_date) * 100 + extract(day from a.read_date) between 715 and 1015
group by extract(year from a.read_date), b.nmfsarea
order by extract(year from a.read_date), b.nmfsarea"))

ai_sst_nmfs <- ai_sst_nmfs %>%
  # rename("year" = "EXTRACT(YEARFROMA.READ_DATE)") %>% # originally forgot to rename in sql
  rename_with(tolower)

saveRDS(ai_sst_nmfs, "ai_atka/ai_sst_nmfs.RDS")

# plot time series 
ggplot()+
  geom_line(data=ai_sst_combined, aes(x=year, y=sst_mean), size=2)+
  geom_line(data=ai_sst_combined, aes(x=year, y=sst_mean-sst_sd), lty=2)+
  geom_line(data=ai_sst_combined, aes(x=year, y=sst_mean+sst_sd), lty=2)+
  ylim(c(0,11))+
  theme_bw()
ggsave("ai_atka/combined_timeseries.PNG")

ggplot()+
  geom_line(data=ai_sst_nmfs, aes(x=year, y=sst_mean, color=nmfsarea), size=2)+
  geom_line(data=ai_sst_nmfs, aes(x=year, y=sst_mean-sst_sd, color=nmfsarea), lty=2)+
  geom_line(data=ai_sst_nmfs, aes(x=year, y=sst_mean+sst_sd, color=nmfsarea), lty=2)+
  ylim(c(0,11))+
  theme_bw()
ggsave("ai_atka/nmfs_timeseries.PNG")

# map areas included
sst <- dbFetch(dbSendQuery(con, "select * from afsc.erddap_crw_sst_spatial_lookup
where nmfsarea in (541, 542, 543)
and depth between -100 and 0"))

sst <- sst |>
  dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE),
                LATITUDE = as.numeric(LATITUDE))
 
sst <- st_as_sf(sst, coords=c('LONGITUDE','LATITUDE'), crs=4326) %>%
  st_transform(crs=3338)

ak <- get_base_layers(select.region = "ai", set.crs = "EPSG:3338")
nmfs <- get_nmfs_areas(set.crs = "EPSG:3338")

ext <-st_bbox(sst)
xmin<-ext[1]
xmax<-ext[3]
ymin<-ext[2]
ymax<-ext[4]

ggplot()+
  geom_sf(data = ak$akland) +
  geom_sf(data = nmfs, fill=NA) +
  geom_sf(data = sst) +
  coord_sf(xlim=c(xmin, xmax), ylim=c(ymin,ymax))+
  theme_bw()
ggsave("ai_atka/ai_100m_map.PNG")

