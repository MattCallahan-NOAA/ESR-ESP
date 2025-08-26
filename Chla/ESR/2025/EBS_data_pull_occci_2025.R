library(tidyverse)
library(lubridate)
library(odbc)
library(getPass)
library(sf)



# pull data from akfin
#connect to akfin
#con <- dbConnect(odbc::odbc(), "akfin", UID=getPass(msg="USER NAME"), PWD=getPass()) 
con <- dbConnect(odbc::odbc(), "akfin",  UID="JNIELSEN", PWD=getPass())


occci<-dbFetch(dbSendQuery(con, "select round(avg(chlorophyll),2) meanchla, to_date(read_date,'YYYY-MM-DD')+4 read_date, jens_grid, bsierp_id 
from env_data.OCCCI_CHLA a
left join env_data.OCCCI_spatial_lookup b on a.occci_id=b.occci_id
where extract(month from to_date(read_date,'YYYY-MM-DD')+4) in (1,2,3,4, 5, 6,7,8,9,10,11,12)
and ecosystem_area = ('Eastern Bering Sea')
and waters_cod = 'FED'
and depth>(-200)
and depth<(-10)
and jens_grid>=0
group by to_date(read_date,'YYYY-MM-DD')+4, jens_grid,bsierp_id "))%>%
  rename_with(tolower)

head(occci)
tail(occci)
saveRDS(occci,file='inter_jens_datafiles/occci_25augSQL.RDS')





ice<-dbFetch(dbSendQuery(con, "select round(avg(a.sea_ice_fraction),2) ice_fraction, a.read_date, b.jens_grid
from afsc.erddap_crw_sst a
left join env_data.crw_lookup_with_jens_grid b on a.crw_id=b.crw_id
where extract(month from a.read_date) in (1,2,3,4, 5, 6,7)
and extract(year from a.read_date) >1997
and b.ecosystem = 'Eastern Bering Sea'
and b.depth>(-200)
and b.depth<(-10)
and b.jens_grid>=0
group by read_date, jens_grid"))%>%
  rename_with(tolower)

head(ice)
tail(ice)

saveRDS(ice,file='inter_jens_datafiles/icedata_for_retreat_timing_25augSQL.RDS')
