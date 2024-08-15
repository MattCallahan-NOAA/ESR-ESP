library(RJDBC)
library(magrittr)
library(dplyr)
library(keyring)
library(sf)
library(ggplot2)
library(akmarineareas2)

# connect to AKFIN
options(java.parameters="-Xmx8g")

jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="../../snippets/dbconnect/java/ojdbc8.jar")

con_j <- dbConnect(jdbcDriver, 
                   "jdbc:oracle:thin:@//tiger:2045/akfin.psmfc.org", 
                   key_list("akfin_oracle_db")$username, 
                   keyring::key_get("akfin_oracle_db", keyring::key_list("akfin_oracle_db")$username))

# plot GOA
GOA24 <- dbGetQuery(con_j, "select round(chla,2) chla, chla_lat, chla_lon, to_date(start_date,'YYYY-MM-DD')+4 mid_date, ecosystem_subarea
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and extract(year from to_date(start_date,'YYYY-MM-DD')+4) = 2024
and ecosystem_area = ('Gulf of Alaska')
and waters_cod = 'FED'
and depth>(-200)
and depth<(-10)
")

GOA23 <- dbGetQuery(con_j, "select round(chla,2) chla, chla_lat, chla_lon, to_date(start_date,'YYYY-MM-DD')+4 mid_date, ecosystem_subarea
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and extract(year from to_date(start_date,'YYYY-MM-DD')+4) = 2023
and ecosystem_area = ('Gulf of Alaska')
and waters_cod = 'FED'
and depth>(-200)
and depth<(-10)
")

GOA24 = GOA24 %>%
  mutate(CHLA_LAT=as.numeric(CHLA_LAT),
         CHLA_LON = as.numeric(CHLA_LON))

GOA23 = GOA23 %>%
  mutate(CHLA_LAT=as.numeric(CHLA_LAT),
         CHLA_LON = as.numeric(CHLA_LON))

ggplot()+
  geom_sf(data=ak_dd)+
  geom_point(data=GOA24 %>%
            filter(MID_DATE == "2024-04-10"),
          aes(x=CHLA_LON, y=CHLA_LAT))+
  geom_point(data=GOA23 %>%
               filter(MID_DATE == "2023-04-03"),
             aes(x=CHLA_LON, y=CHLA_LAT), color="red", alpha=0.02)+
  xlim(c(-170, -129))+ylim(c(47, 62))

# plot Bering Sea

# Plot AI

AI24 <- dbGetQuery(con_j, "select round(chla,2) chla, chla_lat, chla_lon, to_date(start_date,'YYYY-MM-DD')+4 mid_date, ecosystem_subarea
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and extract(year from to_date(start_date,'YYYY-MM-DD')+4) = 2024
and ecosystem_area = ('Aleutian Islands')
and waters_cod = 'FED'
") %>%
  mutate(CHLA_LAT=as.numeric(CHLA_LAT),
         CHLA_LON = as.numeric(CHLA_LON),
         lon360 = ifelse(CHLA_LON >0, CHLA_LON, CHLA_LON+360))

AI23 <- dbGetQuery(con_j, "select round(chla,2) chla, chla_lat, chla_lon, to_date(start_date,'YYYY-MM-DD')+4 mid_date, ecosystem_subarea
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and extract(year from to_date(start_date,'YYYY-MM-DD')+4) = 2023
and ecosystem_area = ('Aleutian Islands')
and waters_cod = 'FED'
") %>%
  mutate(CHLA_LAT=as.numeric(CHLA_LAT),
         CHLA_LON = as.numeric(CHLA_LON),
         lon360 = ifelse(CHLA_LON >0, CHLA_LON, CHLA_LON+360))

ggplot()+
  geom_sf(data=ak_dd%>%st_shift_longitude())+
  geom_point(data=AI24 %>%
               filter(MID_DATE == "2024-04-10"),
             aes(x=lon360, y=CHLA_LAT))+
  geom_point(data=AI23 %>%
               filter(MID_DATE == "2023-04-03"),
             aes(x=lon360, y=CHLA_LAT), color="red", alpha=0.02)+
  xlim(c(166, 200))+ylim(c(47, 58))

# Plot ESP indicator time series

STM<-dbGetQuery(con_j, "select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year, 'AMJ_Chlorophylla_Biomass_SEBS_Satellite' as indicator_name, round(avg(chla),2) data_value
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and bsierp_region_name in ('St. Matthew','Pribilofs')
group by extract(year from to_date(start_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(start_date,'YYYY-MM-DD')+4) asc")

GOA<-dbGetQuery(con_j, "select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_GOA_Satellite' as indicator_name, round(avg(chla),2) data_value
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and NMFS_REP_AREA in ('610', '620', '630', '640', '650')
and depth <= (-10)
group by extract(year from to_date(start_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(start_date,'YYYY-MM-DD')+4) asc")

library(gridExtra)
p1<-ggplot()+
  geom_line(data=STM, aes(x=YEAR, y=DATA_VALUE))+
  ggtitle("St Matt + Pribs AMJ globcolour")

p2<-ggplot()+
  geom_line(data=GOA, aes(x=YEAR, y=DATA_VALUE))+
  ggtitle("GOA AMJ globcolour")

grid.arrange(p1,p2, nrow=2)

GOAS<-dbGetQuery(con_j, "
select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_WCGOA_Satellite' as indicator_name, round(avg(chla),2) data_value, count(*) n_values
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) = 5 
and NMFS_REP_AREA in ('610', '620', '630')
and depth <= (-10)
and depth > (-200)
group by extract(year from to_date(start_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(start_date,'YYYY-MM-DD')+4) asc")

p3<-ggplot()+
  geom_line(data=GOAS, aes(x=YEAR, y=DATA_VALUE))+
  ggtitle("WCGOA shelf AMJ globcolour")

grid.arrange(p1,p3, nrow=2)


