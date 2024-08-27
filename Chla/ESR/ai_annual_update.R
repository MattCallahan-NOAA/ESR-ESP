library(tidyverse)
library(lubridate)
library(RJDBC)
library(getPass)

options(java.parameters="-Xmx12g")

# connect to AKFIN
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="../../snippets/dbconnect/java/ojdbc8.jar")

con <- dbConnect(jdbcDriver, 
                   "jdbc:oracle:thin:@//tiger:2045/akfin.psmfc.org", 
                   getPass(msg="USER NAME"), getPass())


current_year <- 2024
last_year <- 2023


#spring
start<-Sys.time()
spring<-dbFetch(dbSendQuery(con, paste0("select round(chla,2) chla, 
                          to_date(start_date,'YYYY-MM-DD')+4 mid_date, 
                          chla_lat,
                          chla_lon,
                          ecosystem_subarea, 
                          depth,
                          waters_cod state_fed,
                          stat_area
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and extract(year from to_date(start_date,'YYYY-MM-DD')+4) = ", current_year, "
and ecosystem_area = ('Aleutian Islands')")))%>%
  rename_with(tolower)
end<-Sys.time()
end-start
#1.5 minute for one year


#fall 
#took 30 min!
start<-Sys.time()
fall<-dbFetch(dbSendQuery(con, paste0("select round(chla,2) chla, 
                          to_date(start_date,'YYYY-MM-DD')+4 mid_date, 
                          chla_lat,
                          chla_lon,
                          ecosystem_subarea, 
                          depth,
                          waters_cod state_fed,
                          stat_area
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (8, 9, 10)
and extract(year from to_date(start_date,'YYYY-MM-DD')+4) = ", last_year, "
and ecosystem_area = ('Aleutian Islands')")))%>%
  rename_with(tolower)
end<-Sys.time()
end-start


fall <- fall %>%
  mutate(year=year(mid_date))

spring <- spring %>%
  mutate(year=year(mid_date))


    write.csv(fall, paste0("data/Aleutians/ai_glob_fall_",last_year,".csv"), row.names=FALSE)
    write.csv(spring, paste0("data/Aleutians/ai_glob_spring_",current_year,".csv"), row.names=FALSE)

