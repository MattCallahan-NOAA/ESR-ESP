library(pracma) # findpeak calc
library(lubridate)
library(dplyr)
library(tidyverse)
library(zoo)
library(data.table)
library(cmocean)
library(forecast)


p <- readRDS("inter_jens_datafiles/globcolour_24augSQL.RDS")

table(p$bsierp_id)

#p<-readRDS('inter_jens_datafiles/dummy2023_grid_chla_forESR_prep.RDS')
head(p)
tail(p)


p$month=month(p$mid_date)
p$year=year(p$mid_date)
p$doy=yday(p$mid_date)


##
## calculation of bloom timing - still need to fix the data - but here is the code
## perhaps we can just calculate this for 2023 - and merge to the data I have alreayd made (1998-2022)
## method follow Nielsen et al. 20XX. 

# here would be the input file for 2023 # 
# assign data to the "gridid_MS grid first and then calc bloom timing 
# p would be the input data. 

grid_dummy<-data.frame(expand.grid(c(unique(p$jens_grid)), doy=unique(p$doy),year=unique(p$year)))
colnames(grid_dummy)<-c('jens_grid','doy','year')
head(grid_dummy)
tail(grid_dummy)
##################################
### Cleaning up the cycle data ###
##################################
df<-merge(p, grid_dummy,by=c('doy','jens_grid','year'), all=TRUE)
df<-df[order(as.numeric(df$doy)),]
head(df)

####
###
###
head(df)
smoother_value_forecast<-1


df$log_chl<-log(df$meanchla+1)
# log all values #
df$a_chlorophyll_log<-df$log_chl
df$a_chlorophyll_log[df$doy<60]<-NA # set

# interpolating the chla data
df<-df %>%   group_by(jens_grid,year)  %>% arrange(doy) %>% mutate(a_chlorophyll_log=ifelse(row_number()==1, 0.1, a_chlorophyll_log)) # first point
df<-df %>%   group_by(jens_grid,year) %>% arrange(doy) %>% mutate(a_chlorophyll_log=ifelse(row_number()==n(), 0.1, a_chlorophyll_log)) # last point
df<-df %>%   group_by(jens_grid,year) %>% arrange(doy)%>%   mutate(a_chlorophyll_log_int = na.approx(a_chlorophyll_log, na.rm=FALSE))
## # 14 day roll average - not weighted
df<-df %>%   group_by(jens_grid,year) %>% arrange(doy) %>%   dplyr::filter(!all(is.na(a_chlorophyll_log_int)))  %>%   mutate(a_chlorophyll_log_14roll = forecast::ma(a_chlorophyll_log_int,order=smoother_value_forecast))
df<-df %>%   group_by(jens_grid,year)  %>% arrange(doy) %>% mutate(a_chlorophyll_log_14roll=ifelse(row_number()==1,0.1, a_chlorophyll_log_14roll)) # first point set to zero
df<-df %>%   group_by(jens_grid,year) %>% arrange(doy) %>% mutate(a_chlorophyll_log_14roll=ifelse(row_number()==n(), 0.1, a_chlorophyll_log_14roll)) # last point set to zero
df<-df %>%   group_by(jens_grid,year) %>% arrange(doy)%>%   mutate(a_chlorophyll_log_14roll_int = na.approx(a_chlorophyll_log_14roll, na.rm=FALSE)) # interpolation of smoothed 3 day roll data


###
### peak timing estimate
###

n_peaks_set_log<-2 # allow for 2 peaks to be identified (useful for depicting 2nd spring peak)
#threshold_set<-0 # keep at zero / not used
sort_TRUE_log=TRUE # sorting to make primary peak the first one in pracma::findpeaks
set_minpeakheight_log<-log(1+1) # minimum peak height set to 1ug/l (of the smoothed  averaged data)



peakall_log<-df %>%   group_by(jens_grid,year) %>% arrange(doy)%>% filter(doy>59 & doy<181) %>% # spring peak estimated prior to 180 day of year (Sigler 2014)
  mutate(peak_timing_all_log=ifelse(is.null(doy[pracma::findpeaks(a_chlorophyll_log_14roll_int,sortstr=sort_TRUE_log,minpeakheight=set_minpeakheight_log)[,2][1]]),
                                    NA, doy[pracma::findpeaks(a_chlorophyll_log_14roll_int,sortstr=sort_TRUE_log,minpeakheight=set_minpeakheight_log)[,2][1]]))


setDT(peakall_log)
timing_peak_all_log8<-peakall_log[  !is.na(year) &  !is.na(jens_grid), lapply(.SD, function(x) {
  if(is.numeric(x)) mean(x, na.rm = TRUE) else x[!is.na(x)][1L]}), by = list(year,jens_grid)]
timing_peak_all_log8<-data.frame(timing_peak_all_log8)

head(timing_peak_all_log8)
tail(timing_peak_all_log8)


saveRDS(timing_peak_all_log8,file='inter_jens_datafiles/bloomTimingGlob_1998_2024.RDS')

# plot 2023 here- just checking that it worked







# 
# bloom_df <- prevBL_df %>% full_join(timing_peak_all_log8, by=c('gridid_MS','year')) 
# head(bloom_df)
# tail(bloom_df)

#################################
### Calculation with ice data ###
#################################
gc()
is<-readRDS("inter_jens_datafiles/icedata_for_retreat_timing_24augSQL.RDS")
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
dummy_fill<-expand.grid(unique_jens_grids,c(1998:2024))
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

saveRDS(comb_df,file='inter_jens_datafiles/iceRetreatTiming_1998_2024.RDS')


#####
##### merging everything 
# merge with previous years here 
head(timing_peak_all_log8)
# figure this out
df_chl23_bloom_sub<-subset(timing_peak_all_log8,select = c( gridid_MS, year,lon,lat, depth,peak_timing_all_log))
head(df_chl23_bloom_sub)
str(df_chl23_bloom_sub)

prevBL_df<- readRDS("inter_jens_datafiles/GLOBCOLOUR_1_smooth_8_day_composite_TIMING_jan2028.RDS")
head(prevBL_df)
prevBL_df_sub<-subset(prevBL_df,select = c( gridid_MS, year,lon,lat, depth,peak_timing_all_log))
str(prevBL_df_sub)

# rbind full bloom timnig data 
bloom_full<-  data.frame(rbind(prevBL_df_sub,df_chl23_bloom_sub )) # prevBL_df_sub %>% full_join(df_chl23_bloom_sub, by=c('gridid_MS','year'))
head(bloom_full)
tail(bloom_full)


# 
previce_df<- readRDS("inter_jens_datafiles/all_years_ice_sst_timing_anomaly_data_created_3jan2023_8daysmooth.RDS")

previce_df<-data.frame(previce_df)
head(previce_df)
str(previce_df)
ice_full<-  data.frame(rbind(previce_df,comb_df )) # prevBL_df_sub %>% full_join(df_chl23_bloom_sub, by=c('gridid_MS','year'))
head(ice_full)
tail(ice_full)

f23<-merge(bloom_full,ice_full,by=c('gridid_MS','year'))

head(f23)
tail(f23)

saveRDS(f23, file='Chla/bloom_ice_retreat_timing_upto23.RDS')


