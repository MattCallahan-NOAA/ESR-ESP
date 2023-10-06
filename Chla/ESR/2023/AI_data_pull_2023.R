library(tidyverse)
library(lubridate)
library(odbc)
library(getPass)

#connect to akfin
con <- dbConnect(odbc::odbc(), "akfin", UID=getPass(msg="USER NAME"), PWD=getPass()) 

#spring
start<-Sys.time()
data<-dbFetch(dbSendQuery(con, "select round(chla,2) chla, 
                          to_date(start_date,'YYYY-MM-DD')+4 mid_date, 
                          chla_lat,
                          chla_lon,
                          ecosystem_subarea, 
                          depth,
                          waters_cod state_fed,
                          stat_area
from env_data.globcolour_2023 a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and ecosystem_area = ('Aleutian Islands')"))%>%
  rename_with(tolower)
end<-Sys.time()
end-start
#15 minute download

#This csv is too big to open or upload to google drive
write.csv(data, "data/ai_chla_2023.csv", row.names=FALSE)


data<-data%>%
  mutate(year=year(mid_date))

myyears<-1998:2023

for (i in myyears) {
  data%>%
    filter(year==i)%>%
    write.csv(paste0("data/ai2023/ai_glob_spring_",i,".csv"), row.names=FALSE)
}

#fall 
#took 30 min!
start<-Sys.time()
data<-dbFetch(dbSendQuery(con, "select round(chla,2) chla, 
                          to_date(start_date,'YYYY-MM-DD')+4 mid_date, 
                          chla_lat,
                          chla_lon,
                          ecosystem_subarea, 
                          depth,
                          waters_cod state_fed,
                          stat_area
from env_data.globcolour_2023 a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (8, 9, 10)
and ecosystem_area = ('Aleutian Islands')"))%>%
  rename_with(tolower)
end<-Sys.time()
end-start


#This csv is too big to open or upload to google drive
#write.csv(data, "data/ai_chla_2023.csv", row.names=FALSE)


data<-data%>%
  mutate(year=year(mid_date))

myyears<-1998:2023

for (i in myyears) {
  data%>%
    filter(year==i)%>%
    write.csv(paste0("data/ai2023/ai_glob_fall_",i,".csv"), row.names=FALSE)
}

