library(httr)
library(dplyr)
library(ggplot2)
library(RJDBC)
library(keyring)
library(akfinupload)
#pak::pkg_install("MattCallahan-NOAA/akfinupload")

jdbcDriver <- RJDBC::JDBC(driverClass="oracle.jdbc.OracleDriver",
                          classPath=system.file("driver", "ojdbc8.jar", package = "akfinupload") )
#server<-"jdbc:oracle:thin:@akfin"

con<-dbConnect(jdbcDriver, "jdbc:oracle:thin:@//tiger:2045/akfin.psmfc.org", key_list("akfin_oracle_db")$username, keyring::key_get("akfin_oracle_db", keyring::key_list("akfin_oracle_db")$username))


# crab occci globcolour comparison
snow25 <- read.csv("ESP/2025_indicators/AMJ_Chlorophylla_Biomass_SEBS_Satellite.csv")
bbrkc25 <- read.csv("ESP/2025_indicators/Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite.csv")

snow24q<-"-- AMJ_Chlorophylla_Biomass_SEBS_Satellite
select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year, 'AMJ_Chlorophylla_Biomass_SEBS_Satellite' as indicator_name, round(avg(chla),2) data_value
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and bsierp_region_name in ('St. Matthew','Pribilofs')
group by extract(year from to_date(start_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(start_date,'YYYY-MM-DD')+4) asc"

bbrkc24q<-"
-- Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite
select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite' as indicator_name, round(avg(chla),2) data_value
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and bs_king='BBRKC'
group by extract(year from to_date(start_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(start_date,'YYYY-MM-DD')+4) asc"

snow24 <- dbGetQuery(con, snow24q)
bbrkc24 <- dbGetQuery(con, bbrkc24q)

p1<-ggplot()+
  geom_line(data=snow24%>%rename_with(tolower), aes(x=year, y=data_value), color="red")+
  geom_line(data=snow25%>%rename_with(tolower), aes(x=year, y=data_value), color="blue")+
  ggtitle("AMJ_Chlorophylla_Biomass_SEBS_Satellite: red=globcolour, blue=occci")
p1
ggsave("ESP/2025_indicators/snow_crab_glob_occci.png",
       width = 8, height = 6, dpi = 300)

p2<-ggplot()+
  geom_line(data=bbrkc24%>%rename_with(tolower), aes(x=year, y=data_value), color="red")+
  geom_line(data=bbrkc25%>%rename_with(tolower), aes(x=year, y=data_value), color="blue")+
  ggtitle("Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite: red=globcolour, blue=occci")
p2
ggsave("ESP/2025_indicators/bbrkc_glob_occci.png",
       width = 8, height = 6, dpi = 300)

# Groundfish
goabq<-"--Spring_Chlorophylla_Biomass_GOA_Satellite
select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_GOA_Satellite' as indicator_name, round(avg(chla),2) data_value
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and NMFS_REP_AREA in ('610', '620', '630', '640', '650')
and depth <= (-10)
group by extract(year from to_date(start_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(start_date,'YYYY-MM-DD')+4) asc"

goapq<-"--Spring_Chlorophylla_Peak_GOA_Satellite
WITH ranked_data AS (
    SELECT
         year,
         doy,
        meanchla,
        ROW_NUMBER() OVER (PARTITION BY year ORDER BY meanchla DESC) AS rn
        from (select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year,
        to_number(to_char(to_date(start_date,'YYYY-MM-DD')+4,'DDD')) doy, 
        round(avg(chla),2) meanchla 
    FROM env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and NMFS_REP_AREA in ('610', '620', '630', '640', '650')
and depth <= (-10)
        group by  extract(year from to_date(start_date,'YYYY-MM-DD')+4),
        to_number(to_char(to_date(start_date,'YYYY-MM-DD')+4,'DDD'))
))
SELECT
    year,
    'Spring_Chlorophylla_Peak_GOA_Satellite' as indicator_name,
    doy data_value
FROM ranked_data
WHERE rn = 1"


wgoabq<-"--Spring_Chlorophylla_Biomass_WCGOA_Satellite
select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_WCGOA_Satellite' as indicator_name, round(avg(chla),2) data_value, count(*) n_values
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) = 5 
and NMFS_REP_AREA in ('610', '620', '630')
and depth <= (-10)
and depth > (-200)
group by extract(year from to_date(start_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(start_date,'YYYY-MM-DD')+4) asc"

wgoapq<-"--Spring_Chlorophylla_Peak_WCGOA_Satellite
WITH ranked_data AS (
    SELECT
         year,
         doy,
        meanchla,
        ROW_NUMBER() OVER (PARTITION BY year ORDER BY meanchla DESC) AS rn
        from (select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year,
        to_number(to_char(to_date(start_date,'YYYY-MM-DD')+4,'DDD')) doy, 
        round(avg(chla),2) meanchla 
    FROM env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and NMFS_REP_AREA in ('610', '620', '630')
and depth <= (-10)
and depth >= (-200)
        group by  extract(year from to_date(start_date,'YYYY-MM-DD')+4),
        to_number(to_char(to_date(start_date,'YYYY-MM-DD')+4,'DDD'))
))
SELECT
    year,
    'Spring_Chlorophylla_Peak_WCGOA_Satellite' as indicator_name,
    doy data_value
FROM ranked_data
WHERE rn = 1"

gf_glob<-dbGetQuery(con, goabq) %>%
  bind_rows(dbGetQuery(con, goapq), dbGetQuery(con, wgoabq),dbGetQuery(con, wgoapq)) %>%
              rename_with(tolower)

# now with occci
goabq<-"--Spring_Chlorophylla_Biomass_GOA_Satellite
select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_GOA_Satellite' as indicator_name, round(avg(chlorophyll),2) data_value
from env_data.OCCCI_CHLA a
left join env_data.OCCCI_SPATIAL_LOOKUP b on a.occci_id=b.occci_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and NMFS_AREA in ('610', '620', '630', '640', '650')
and depth <= (-10)
group by extract(year from to_date(read_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(read_date,'YYYY-MM-DD')+4) asc"

goapq<-"--Spring_Chlorophylla_Peak_GOA_Satellite
WITH ranked_data AS (
    SELECT
         year,
         doy,
        meanchla,
        ROW_NUMBER() OVER (PARTITION BY year ORDER BY meanchla DESC) AS rn
        from (select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year,
        to_number(to_char(to_date(read_date,'YYYY-MM-DD')+4,'DDD')) doy, 
        round(avg(chlorophyll),2) meanchla 
    from env_data.OCCCI_CHLA a
left join env_data.OCCCI_SPATIAL_LOOKUP b on a.occci_id=b.occci_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and NMFS_AREA in ('610', '620', '630', '640', '650')
and depth <= (-10)
        group by  extract(year from to_date(read_date,'YYYY-MM-DD')+4),
        to_number(to_char(to_date(read_date,'YYYY-MM-DD')+4,'DDD'))
))
SELECT
    year,
    'Spring_Chlorophylla_Peak_GOA_Satellite' as indicator_name,
    doy data_value
FROM ranked_data
WHERE rn = 1"


wgoabq<-"--Spring_Chlorophylla_Biomass_WCGOA_Satellite
select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_WCGOA_Satellite' as indicator_name, round(avg(chlorophyll),2) data_value, count(*) n_values
from env_data.OCCCI_CHLA a
left join env_data.OCCCI_SPATIAL_LOOKUP b on a.occci_id=b.occci_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) = 5 
and NMFS_AREA in ('610', '620', '630')
and depth <= (-10)
and depth > (-200)
group by extract(year from to_date(read_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(read_date,'YYYY-MM-DD')+4) asc"

wgoapq<-"--Spring_Chlorophylla_Peak_WCGOA_Satellite
WITH ranked_data AS (
    SELECT
         year,
         doy,
        meanchla,
        ROW_NUMBER() OVER (PARTITION BY year ORDER BY meanchla DESC) AS rn
        from (select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year,
        to_number(to_char(to_date(read_date,'YYYY-MM-DD')+4,'DDD')) doy, 
        round(avg(chlorophyll),2) meanchla 
    from env_data.OCCCI_CHLA a
left join env_data.OCCCI_SPATIAL_LOOKUP b on a.occci_id=b.occci_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and NMFS_AREA in ('610', '620', '630')
and depth <= (-10)
and depth >= (-200)
        group by  extract(year from to_date(read_date,'YYYY-MM-DD')+4),
        to_number(to_char(to_date(read_date,'YYYY-MM-DD')+4,'DDD'))
))
SELECT
    year,
    'Spring_Chlorophylla_Peak_WCGOA_Satellite' as indicator_name,
    doy data_value
FROM ranked_data
WHERE rn = 1"

gf_occci <- dbGetQuery(con, goabq) %>%
  bind_rows(dbGetQuery(con, goapq), dbGetQuery(con, wgoabq),dbGetQuery(con, wgoapq)) %>%
  rename_with(tolower)

gf <- gf_glob %>%
  mutate(globcolour=data_value) %>%
  dplyr::select(year, indicator_name, globcolour) %>%
  full_join(gf_occci %>% mutate(occci = data_value) %>%
              dplyr::select(year, indicator_name, occci),
            by=c("year", "indicator_name"))


ggplot()+
  geom_line(data=gf, aes(x=year, y=globcolour), color="red")+
  geom_line(data=gf, aes(x=year, y=occci), color="blue")+
  facet_wrap(~indicator_name, scales="free")

ggsave("ESP/2025_indicators/goa_glob_occci.png",
       width = 8, height = 6, dpi = 300)
saveRDS(gf, "ESP/2025_indicators/gf_glob_occci.RDS")
