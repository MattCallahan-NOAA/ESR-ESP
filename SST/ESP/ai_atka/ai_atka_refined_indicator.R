library(RJDBC)
library(sf)
library(ggplot2)
library(akgfmaps)
library(dplyr)
library(lubridate)

# pull GAP strata from akgfmaps
ai_gis <- get_base_layers(select.region="ai", set.crs=3338)

ext <-st_bbox(ai_gis$survey.strata)
xmin<-ext[1]
xmax<-ext[3]
ymin<-ext[2]
ymax<-ext[4]

strata<-ai_gis$survey.strata

ggplot()+
  geom_sf(data=ai_gis$akland)+
  geom_sf(data=strata %>% dplyr::filter(STRATUM==570), aes(fill=STRATUM))+
  coord_sf(xlim=c(xmin, xmax), ylim=c(ymin,ymax))+
  theme_bw()

# define boxes for sst and chla subsetting
define_box <- function(x_min, x_max, y_min, y_max){
  coords <- matrix(c(
    y_max, x_min,  # Bottom-left corner
    y_min, x_min,  # Bottom-right corner
    y_min, x_max,  # Top-right corner
    y_max, x_max,  # Top-left corner
    y_max, x_min  # Closing the polygon
  ), ncol = 2, byrow = TRUE)
  
  # Create an sf polygon
  st_polygon(list(coords))
  # polygon <- st_polygon(list(coords))
  # 
  # st_sfc(polygon, crs = 4326) %>%
  #   st_transform(crs=3338)
}


# Convert to an sf object with a coordinate reference system (WGS 84)
seguam <- define_box(x_min=52, x_max=52.3, y_min=-173, y_max=-172.2)
tanaga <- define_box(x_min=51.4, x_max=51.8, y_min=-179, y_max=-178)
attu <- define_box(x_min=52.3, x_max=52.9, y_min=173, y_max=174)

ai_atka_areas <- st_sf(AI_ATKA = c("seguam", "tanaga", "attu"),
                       geometry=st_sfc(seguam, tanaga, attu, crs=4326)) %>%
  st_transform(crs=3338)

png("ai_atka/atka_area_map.PNG", width=6,height=2.5,units="in",res=300)
ggplot()+
  geom_sf(data=strata, fill="orange")+
  # geom_sf(data=seguam, color="purple", fill=NA, lwd=2)+
  # geom_sf(data=tanaga, color="purple", fill=NA, lwd=2)+
  # geom_sf(data=attu, color="purple", fill=NA, lwd=2)+
  geom_sf(data=ai_atka_areas, color="purple", fill=NA, lwd=1)+
  geom_sf(data=ai_gis$akland)+
  coord_sf(xlim=c(xmin, xmax), ylim=c(ymin,ymax))+
  scale_x_continuous(breaks = seq(0, 360, 5))+
  theme_bw()
dev.off()

# Pull SST
# connect to akfin
jdbcDriver <-jdbcDriver <- RJDBC::JDBC(driverClass="oracle.jdbc.OracleDriver",
                                       classPath="java/ojdbc8.jar")

con<-DBI::dbConnect(jdbcDriver, "jdbc:oracle:thin:@//tiger:2045/akfin.psmfc.org",
                    keyring::key_list("akfin_oracle_db")$username,
                    keyring::key_get("akfin_oracle_db", keyring::key_list("akfin_oracle_db")$username))

# download sst lookup table
sst_lkp <- dbFetch(dbSendQuery(con, "select * from afsc.erddap_crw_sst_spatial_lookup"))

# convert to spatial object
sst_lkp <- sst_lkp %>%
  st_as_sf(coords=c("LONGITUDE","LATITUDE"), crs=4326) %>%
  st_transform(crs=3338)
  


sst_lkp <- sst_lkp %>%
  st_join(ai_atka_areas, join=st_intersects)

ggplot()+
  #geom_sf(data=strata, fill="orange")+
  geom_sf(data=sst_lkp, aes(color=AI_ATKA))+
  geom_sf(data=ai_atka_areas, color="purple", fill=NA, lwd=2)+
  geom_sf(data=ai_gis$akland)+
  coord_sf(xlim=c(xmin, xmax), ylim=c(ymin,ymax))+
  scale_x_continuous(breaks = seq(0, 360, 5))+
  theme_bw()

sst_lkp %>%
  group_by(AI_ATKA) %>%
  summarize(n=n())

# upload back to the database
sst_atka_lkp <- sst_lkp %>%
  filter(!is.na(AI_ATKA)) %>%
  data.frame() %>%
  dplyr::select(ID, DEPTH, AI_ATKA)

dbWriteTable(con, "SST_ATKA_AREAS", sst_atka_lkp, overwrite=TRUE)

dbGetQuery(con, "select * from SST_ATKA_AREAS")

# join to SST table and download
# once I sort out what averageing we want it might be more efficient to do the averaging in sql
atka_sst <- dbGetQuery(con, "select a.read_date, a.temp, b.depth, b.ai_atka from afsc.erddap_crw_sst a
                       inner join SST_ATKA_AREAS b on a.crw_id = b.id")

atka_sst<-atka_sst %>%
  mutate(YEAR=year(READ_DATE),
         MONTH=month(READ_DATE))

amj_meansst <- atka_sst %>%
  filter(MONTH %in% c(4,5,6)) %>%
  group_by(YEAR, AI_ATKA) %>%
  summarize(sst_mean=mean(TEMP),
            sst_sd=sd(TEMP))

# plot time series
ggplot()+
  geom_line(data=amj_meansst, aes(x=YEAR, y=sst_mean, color=AI_ATKA), size=2)+
  geom_line(data=amj_meansst, aes(x=YEAR, y=sst_mean-sst_sd, color=AI_ATKA), lty=2)+
  geom_line(data=amj_meansst, aes(x=YEAR, y=sst_mean+sst_sd, color=AI_ATKA), lty=2)+
  ylim(c(0,7))+
  ggtitle("AI Atka area AMJ mean SST")+
  theme_bw()
ggsave("ai_atka/atka_area_amj_sst.PNG")


# calculare fall indicator
# Aug 15-Nov 15
fall_meansst <- atka_sst %>%
  mutate(day=yday(READ_DATE)) %>%
  filter(day >=227 & day <= 319) %>%
  group_by(YEAR, AI_ATKA) %>%
  summarize(sst_mean=mean(TEMP),
            sst_sd=sd(TEMP))
ggplot()+
  geom_line(data=fall_meansst, aes(x=YEAR, y=sst_mean, color=AI_ATKA), size=2)+
  geom_line(data=fall_meansst, aes(x=YEAR, y=sst_mean-sst_sd, color=AI_ATKA), lty=2)+
  geom_line(data=fall_meansst, aes(x=YEAR, y=sst_mean+sst_sd, color=AI_ATKA), lty=2)+
  ggtitle("AI Atka area Aug 15 - Nov 15 mean SST")+
  ylim(0,11)+
  theme_bw()
ggsave("ai_atka/atka_area_fall_sst.PNG")

# save SST data
amj_meansst %>%
  rename_with(tolower) %>%
  mutate(indicator_name="Atka_amj_sst") %>%
  write.csv("ai_atka/atka_amj_sst.csv", row.names=F)

fall_meansst %>%
  rename_with(tolower) %>%
  mutate(indicator_name="Atka_fall_sst") %>%
  write.csv("ai_atka/atka_fall_sst.csv", row.names=F)
