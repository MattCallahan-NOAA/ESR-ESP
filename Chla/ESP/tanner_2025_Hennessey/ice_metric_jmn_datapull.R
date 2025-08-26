library(tidyverse)
library(lubridate)
library(odbc)
library(getPass)
library(sf)



# pull data from akfin
#con <- dbConnect(odbc::odbc(), "akfin",  UID="JNIELSEN", PWD=getPass())



# ice<-dbFetch(dbSendQuery(con, "select round(avg(a.sea_ice_fraction),2) ice_fraction, a.read_date, b.jens_grid,bsierp_id
# from afsc.erddap_crw_sst a
# left join env_data.crw_lookup_with_jens_grid b on a.crw_id=b.crw_id
# where extract(month from a.read_date) in (1,2,3,4, 5, 6,7)
# and extract(year from a.read_date) >1984
# and b.ecosystem = 'Eastern Bering Sea'
# and b.depth>(-200)
# and b.depth<(-10)
# and b.jens_grid>=0
# group by read_date, jens_grid,bsierp_id"))%>%
#   rename_with(tolower)

head(ice)
tail(ice)

#saveRDS(ice,file='inter_jens_datafiles/tanner_ice_metric_25augSQL.RDS')
library(pracma) # findpeak calc
library(lubridate)
library(dplyr)
library(tidyverse)
library(zoo)
library(data.table)
library(cmocean)
library(forecast)
is<-readRDS("inter_jens_datafiles/tanner_ice_metric_25augSQL.RDS")


is$jens_grid<-as.character(is$jens_grid)
is$bsierp_id<-as.character(is$bsierp_id)


head(is) # is means ice in Danish
is$a_ice<-is$ice_fraction           
table(is.na(is$ice_fraction))

is$month=month(is$read_date )
is$year=year(is$read_date )
is$doy=yday(is$read_date )

table(is$year)

# 

smoother_value_forecast<-8 # note here this is the daily data 


head(is)


is<-is %>% group_by(jens_grid,year)  %>% arrange(doy) %>% mutate(a_ice = ifelse(a_ice>0.001, replace(a_ice, duplicated(a_ice), NA), 0))

# interpolating the ice data
is<-is %>% group_by(jens_grid,year)  %>% arrange(doy) %>% mutate(a_ice=ifelse(row_number()==1, mean(na.omit(a_ice)[1]), a_ice)) # first point
is<-is %>% group_by(jens_grid,year)  %>% arrange(doy) %>% mutate(a_ice=ifelse(row_number()==n(), last(na.omit(a_ice)), a_ice)) # last point
is<-is %>% group_by(jens_grid,year)  %>% arrange(doy) %>%   mutate(a_ice_int = na.approx(a_ice, na.rm=FALSE))
is<-is %>% group_by(jens_grid,year)  %>% arrange(doy) %>%   filter(!all(is.na(a_ice_int)))  %>%   mutate(a_ice_roll14 = forecast::ma(a_ice_int,order=smoother_value_forecast))

# spline estimator #

##########################
### ice retreat timing ###
##########################
ice_ret_15 <- is %>% group_by(jens_grid,year)  %>% arrange(doy,decreasing = TRUE)  %>% filter(doy<181) %>%  filter(a_ice_roll14 > 0.15)%>%
  summarize(ice_retr_roll15 = max(doy))

unique_jens_grids<-unique(is$jens_grid)
dummy_fill<-expand.grid(unique_jens_grids,c(1998:2025))
colnames(dummy_fill)<-c('jens_grid','year')


ice_df<-is %>%
  full_join(ice_ret_15, by=c('jens_grid','year')) 


ave_ice_spring_toPeak <- ice_df %>% group_by(jens_grid,year)   %>% filter(doy>29 & doy<ice_retr_roll15) %>%  summarize(mean_ice_frac = mean(a_ice_roll14,na.rm=T))



comb_df<- ave_ice_spring_toPeak  %>% 
  full_join(ice_ret_15,by=c('jens_grid','year')) %>% 
  full_join(dummy_fill, by=c('jens_grid','year')) # this adds all the na ice retreat / ice frac - we can them give them a zero


comb_df$mean_ice_frac[is.na(comb_df$mean_ice_frac)]<-0
comb_df$ice_retr_roll15[is.na(comb_df$ice_retr_roll15)]<-0
table(is.na(comb_df$mean_ice_frac))
table(is.na(comb_df$ice_retr_roll15))
head(comb_df)
tail(comb_df)
saveRDS(comb_df,file='inter_jens_datafiles/Tanner_iceRetreatTiming_1985_2025.RDS')



###

