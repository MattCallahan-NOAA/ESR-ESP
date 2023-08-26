library(tidyverse)
library(lubridate)
library(odbc)
library(getPass)



# pull data from akfin
#connect to akfin
con <- dbConnect(odbc::odbc(), "akfin", UID=getPass(msg="USER NAME"), PWD=getPass()) 

#load data with depth, season, and region filters
data<-dbFetch(dbSendQuery(con, "select round(avg(chla),2) meanchla, to_date(start_date,'YYYY-MM-DD')+4 mid_date, jens_grid
from env_data.globcolour_2023 a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and ecosystem_area = ('Eastern Bering Sea')
and waters_cod = 'FED'
and depth>(-200)
and depth<(-10)
and jens_grid>=0
group by to_date(start_date,'YYYY-MM-DD')+4, jens_grid"))%>%
  rename_with(tolower)

