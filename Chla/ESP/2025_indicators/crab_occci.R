# 2025 crab indicators
# calculated with data through 2024 on 8/20/2025

library(RJDBC)
library(keyring)
library(dplyr)
library(akfinupload)

jdbcDriver <- RJDBC::JDBC(driverClass="oracle.jdbc.OracleDriver",
                          classPath=system.file("driver", "ojdbc8.jar", package = "akfinupload") )
server<-"jdbc:oracle:thin:@akfin"

con<-dbConnect(jdbcDriver, "jdbc:oracle:thin:@//tiger:2045/akfin.psmfc.org", key_list("akfin_oracle_db")$username, keyring::key_get("akfin_oracle_db", keyring::key_list("akfin_oracle_db")$username))


# AMJ_Chlorophylla_Biomass_SEBS_Satellite
snow<- "select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year, 'AMJ_Chlorophylla_Biomass_SEBS_Satellite' as indicator_name, round(avg(chlorophyll),2) data_value
from env_data.OCCCI_CHLA a
left join env_data.OCCCI_SPATIAL_LOOKUP b on a.occci_id=b.occci_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and bsierp_name in ('St. Matthew','Pribilofs')
group by extract(year from to_date(read_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(read_date,'YYYY-MM-DD')+4) asc"

dbGetQuery(con, snow) %>%
  write.csv("AMJ_Chlorophylla_Biomass_SEBS_Satellite.csv", row.names=FALSE)


# Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite

bbrkc<-"select extract(year from to_date(read_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite' as indicator_name, round(avg(chlorophyll),2) data_value
from env_data.gOCCCI_CHLA a
left join env_data.OCCCI_SPATIAL_LOOKUP b on a.glob_id=b.glob_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and bs_king='Bristol Bay RKC'
group by extract(year from to_date(read_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(read_date,'YYYY-MM-DD')+4) asc"

dbGetQuery(con, bbrkc) %>%
  write.csv("Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite.csv", row.names=FALSE)
