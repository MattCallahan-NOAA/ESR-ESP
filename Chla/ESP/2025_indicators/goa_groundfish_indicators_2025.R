#### groundfish ESP indicators #####

library(tidyverse)
library(lubridate)
library(RJDBC)
library(keyring)

options(java.parameters="-Xmx8g")

jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="../../snippets/dbconnect/java/ojdbc8.jar")

con_j <- dbConnect(jdbcDriver, 
                   "jdbc:oracle:thin:@//tiger:2045/akfin.psmfc.org", 
                   key_list("akfin_oracle_db")$username, 
                   keyring::key_get("akfin_oracle_db", keyring::key_list("akfin_oracle_db")$username))

# add_submission year
add_submission_year <- function(x) {
  x<- x %>%
    mutate(SUBMISSION_YEAR=lubridate::year(Sys.Date())) %>%
    dplyr::select(SUBMISSION_YEAR, YEAR, INDICATOR_NAME, DATA_VALUE)
  x
}

# Spring_Chlorophylla_Biomass_GOA_Satellite
Spring_Chlorophylla_Biomass_GOA_Satellite<-dbGetQuery(con_j, "select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_GOA_Satellite' as indicator_name, round(avg(chlorophyll),2) data_value
from env_data.occci_chla a
left join env_data.occci_spatial_lookup b on a.occci_id=b.occci_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and NMFS_AREA in ('610', '620', '630', '640', '650')
and depth <= (-10)
group by extract(year from to_date(read_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(read_date,'YYYY-MM-DD')+4) asc")

write.csv(Spring_Chlorophylla_Biomass_GOA_Satellite %>% add_submission_year() ,"ESP/2025_indicators/Spring_Chlorophylla_Biomass_GOA_Satellite.csv",  row.names = FALSE)

# Spring_Chlorophylla_Peak_GOA_Satellite 
Spring_Chlorophylla_Peak_GOA_Satellite <-dbGetQuery(con_j, "WITH ranked_data AS (
    SELECT
         year,
         doy,
        meanchla,
        ROW_NUMBER() OVER (PARTITION BY year ORDER BY meanchla DESC) AS rn
        from (select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year,
        to_number(to_char(to_date(read_date,'YYYY-MM-DD')+4,'DDD')) doy, 
        round(avg(chlorophyll),2) meanchla 
    from env_data.occci_chla a
left join env_data.occci_spatial_lookup b on a.occci_id=b.occci_id
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
WHERE rn = 1")

write.csv(Spring_Chlorophylla_Peak_GOA_Satellite%>% add_submission_year() ,"ESP/2025_indicators/Spring_Chlorophylla_Peak_GOA_Satellite.csv",  row.names = FALSE)

# Spring_Chlorophylla_Biomass_WCGOA_Satellite
Spring_Chlorophylla_Biomass_WCGOA_Satellite<-dbGetQuery(con_j, " select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_WCGOA_Satellite' as indicator_name, round(avg(chlorophyll),2) data_value, count(*) n_values
from env_data.occci_chla a
left join env_data.occci_spatial_lookup b on a.occci_id=b.occci_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) = 5 
and NMFS_AREA in ('610', '620', '630')
and depth <= (-10)
and depth > (-200)
group by extract(year from to_date(read_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(read_date,'YYYY-MM-DD')+4) asc")

write.csv(Spring_Chlorophylla_Biomass_WCGOA_Satellite%>% add_submission_year() ,"ESP/2025_indicators/Spring_Chlorophylla_Biomass_WCGOA_Satellite.csv",  row.names = FALSE)
