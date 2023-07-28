library(pracma) # findpeak calc
library(lubridate)
library(dplyr)
library(tidyverse)
library(zoo)
library(data.table)
library(cmocean)


p<-readRDS('inter_jens_datafiles/dummy2023_grid_chla_forESR_prep.RDS')
head(p)

p$year<-rep(2023,nrow(p)) # hack to create 2023
p<-p[p$depth<(-19.9),]
p<-p[p$depth>(-200.01),]

##
## calculation of bloom timing - still need to fix the data - but here is the code
## perhaps we can just calculate this for 2023 - and merge to the data I have alreayd made (1998-2022)
## method follow Nielsen et al. 20XX. 

# here would be the input file for 2023 # 
# assign data to the "gridid_MS grid first and then calc bloom timing 
# p would be the input data. 

grid_dummy<-data.frame(expand.grid(c(unique(p$gridid_MS)), doy=unique(p$doy),year=unique(p$year)))
colnames(grid_dummy)<-c('gridid_MS','doy','year')
head(grid_dummy)
##################################
### Cleaning up the cycle data ###
##################################
df<-merge(p, grid_dummy,by=c('doy','gridid_MS','year'), all=TRUE)
df<-df[order(as.numeric(df$doy)),]
head(df)

####
###
###
head(df)
smoother_value_forecast<-1

# log all values #
df$a_chlorophyll_log<-df$log_chl
df$a_chlorophyll_log[df$doy<60]<-NA # set

# interpolating the chla data
df<-df %>%   group_by(gridid_MS,year)  %>% arrange(doy) %>% mutate(a_chlorophyll_log=ifelse(row_number()==1, 0.1, a_chlorophyll_log)) # first point
df<-df %>%   group_by(gridid_MS,year) %>% arrange(doy) %>% mutate(a_chlorophyll_log=ifelse(row_number()==n(), 0.1, a_chlorophyll_log)) # last point
df<-df %>%   group_by(gridid_MS,year) %>% arrange(doy)%>%   mutate(a_chlorophyll_log_int = na.approx(a_chlorophyll_log, na.rm=FALSE))
## # 14 day roll average - not weighted
df<-df %>%   group_by(gridid_MS,year) %>% arrange(doy) %>%   dplyr::filter(!all(is.na(a_chlorophyll_log_int)))  %>%   mutate(a_chlorophyll_log_14roll = forecast::ma(a_chlorophyll_log_int,order=smoother_value_forecast))
df<-df %>%   group_by(gridid_MS,year)  %>% arrange(doy) %>% mutate(a_chlorophyll_log_14roll=ifelse(row_number()==1,0.1, a_chlorophyll_log_14roll)) # first point set to zero
df<-df %>%   group_by(gridid_MS,year) %>% arrange(doy) %>% mutate(a_chlorophyll_log_14roll=ifelse(row_number()==n(), 0.1, a_chlorophyll_log_14roll)) # last point set to zero
df<-df %>%   group_by(gridid_MS,year) %>% arrange(doy)%>%   mutate(a_chlorophyll_log_14roll_int = na.approx(a_chlorophyll_log_14roll, na.rm=FALSE)) # interpolation of smoothed 3 day roll data


###
### peak timing estimate
###

n_peaks_set_log<-2 # allow for 2 peaks to be identified (useful for depicting 2nd spring peak)
#threshold_set<-0 # keep at zero / not used
sort_TRUE_log=TRUE # sorting to make primary peak the first one in pracma::findpeaks
set_minpeakheight_log<-log(1+1) # minimum peak height set to 1ug/l (of the smoothed  averaged data)



peakall_log<-df %>%   group_by(gridid_MS,year) %>% arrange(doy)%>% filter(doy>59 & doy<181) %>% # spring peak estimated prior to 180 day of year (Sigler 2014)
  mutate(peak_timing_all_log=ifelse(is.null(doy[pracma::findpeaks(a_chlorophyll_log_14roll_int,sortstr=sort_TRUE_log,minpeakheight=set_minpeakheight_log)[,2][1]]),
                                    NA, doy[pracma::findpeaks(a_chlorophyll_log_14roll_int,sortstr=sort_TRUE_log,minpeakheight=set_minpeakheight_log)[,2][1]]))


setDT(peakall_log)
timing_peak_all_log8<-peakall_log[  !is.na(year) &  !is.na(gridid_MS), lapply(.SD, function(x) {
  if(is.numeric(x)) mean(x, na.rm = TRUE) else x[!is.na(x)][1L]}), by = list(year,gridid_MS)]
timing_peak_all_log8<-data.frame(timing_peak_all_log8)

head(timing_peak_all_log8)

# plot 2023 here- just checking that it worked
ggplot()+
  coord_equal(xlim=c(181,203),ylim=c(54.8,66),ratio = 1.8)+
  geom_point(data = timing_peak_all_log8, aes(x = lon+360, y = lat,fill=(peak_timing_all_log)),pch=21,size=7,color='black')+
  scale_fill_gradientn(colours = (cmocean('haline')(200)),name = "") +
  #geom_polygon(data = data_mapH, aes(x=long, y = lat, group = group),colour="black", fill="darkgrey")+
  ylab("Latitude")+
  scale_x_continuous("Longitude", breaks=breaks_w2, labels=labels_w2, limits=c(140,250))+
  ggtitle('Bloom timing (1998-2022)')+
  theme(plot.title = element_text(size = 18),
        strip.text = element_text(size=18,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill='dodgerblue'), # Add the NOAA color blue to the facet strips.
        axis.title = element_text(size=18,family="sans"),
        axis.text = element_text(size=18,family="sans",color='black'),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black"),
        legend.title = element_text(size=14))#+









bloom_df <- prevBL_df %>% full_join(timing_peak_all_log8, by=c('gridid_MS','year')) 
head(bloom_df)
tail(bloom_df)

#################################
### Calculation with ice data ###
#################################
is_23df<-readRDS("inter_jens_datafiles/Dummy_2023_ice_sst_ice_forGlob2019.RDS")
head(is_23df)
is_23df$a_ice<-is_23df$CRW_SEAICE
is_23df$a_ice[is.na(is_23df$a_ice)]<-0 #
is_23df$a_sst<-is_23df$CRW_SST

# dummy create 2023
is_23df$year<-rep(2023,nrow(is_23df))


smoother_value_forecast<-8 # note here this is the daily data 





is_23df<-is_23df %>% group_by(gridid_MS,year)  %>% arrange(doy) %>% mutate(a_ice = ifelse(a_ice>0.001, replace(a_ice, duplicated(CRW_SEAICE), NA), 0))

# interpolating the ice data
is_23df<-is_23df %>% group_by(gridid_MS,year)  %>% arrange(doy) %>% mutate(a_ice=ifelse(row_number()==1, mean(na.omit(a_ice)[1]), a_ice)) # first point
is_23df<-is_23df %>% group_by(gridid_MS,year)  %>% arrange(doy) %>% mutate(a_ice=ifelse(row_number()==n(), last(na.omit(a_ice)), a_ice)) # last point
is_23df<-is_23df %>% group_by(gridid_MS,year)  %>% arrange(doy) %>%   mutate(a_ice_int = na.approx(a_ice, na.rm=FALSE))
is_23df<-is_23df %>% group_by(gridid_MS,year)  %>% arrange(doy) %>%   filter(!all(is.na(a_ice_int)))  %>%   mutate(a_ice_roll14 = forecast::ma(a_ice_int,order=smoother_value_forecast))

# spline estimator #

# interpolating the temperature data
is_23df<-is_23df %>% group_by(gridid_MS,year) %>% arrange(doy) %>% mutate(a_sst=ifelse(row_number()==1, mean(na.omit(a_sst)[1]), a_sst)) # first point
is_23df<-is_23df %>% group_by(gridid_MS,year) %>% arrange(doy) %>% mutate(a_sst=ifelse(row_number()==n(), last(na.omit(a_sst)), a_sst)) # last point
is_23df<-is_23df %>% group_by(gridid_MS,year) %>% arrange(doy)%>%   mutate(a_sst_int = na.approx(a_sst, na.rm=FALSE))
is_23df<-is_23df %>% group_by(gridid_MS,year) %>% arrange(doy) %>%   filter(!all(is.na(a_sst_int)))  %>%   mutate(a_sst_roll14 = forecast::ma(a_sst_int,order=smoother_value_forecast))


##########################
### ice retreat timing ###
##########################
ice_ret_15 <- is_23df %>% group_by(gridid_MS,year)  %>% arrange(doy,decreasing = TRUE)  %>% filter(doy<181) %>%  filter(a_ice_roll14 > 0.15)%>%
  summarize(ice_retr_roll15 = max(doy))


ice_df<-is_23df %>%
  full_join(ice_ret_15, by=c('gridid_MS','year')) 


ave_ice_spring_toPeak <- ice_df %>% group_by(gridid_MS,year)   %>% filter(doy>29 & doy<ice_retr_roll15) %>%  summarize(mean_ice_frac = mean(a_ice_roll14,na.rm=T))


###
### ssst this can be skipped # 
###
sst_df<-is_23df %>% group_by(gridid_MS,doy) %>% arrange(doy)%>% filter(doy>90 & doy<152) %>% mutate(avg_grid_SST = mean(a_sst_int,na.rm=T))
sst_df_sum<- sst_df %>%   group_by(gridid_MS,year) %>% arrange(doy)%>% filter(doy>90 & doy<152) %>% summarise(cum_anomSST =sum( (a_sst_int-avg_grid_SST),na.rm=T))

comb_df<- sst_df_sum %>% full_join(ave_ice_spring_toPeak,by=c('gridid_MS','year')) %>% 
  full_join(ice_ret_15,by=c('gridid_MS','year'))

head(comb_df)


#####
##### merging everything 
# merge with previous years here 
head(timing_peak_all_log8)
# figure this out
df_chl23_bloom_sub<-subset(timing_peak_all_log8,select = c( gridid_MS, year,lon,lat, depth,peak_timing_all_log))
head(df_chl23_bloom_sub)
str(df_chl23_bloom_sub)

prevBL_df<- readRDS("inter_jens_datafiles/GLOBCOLOUR_1_smooth_8_day_composite_TIMING_jan2023.RDS")
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




