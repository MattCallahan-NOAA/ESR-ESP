#2022 figures for GOA ESR
library(tidyverse)
library(heatwaveR)
library(lubridate)
library(viridis)
library(cowplot)
library(ggplottimeseries)
library(odbc)
library(getPass)
library(zoo)


#  Load 508 compliant NOAA colors
OceansBlue1='#0093D0'
OceansBlue2='#0055A4' # rebecca dark blue
CoralRed1='#FF4438'
Crustacean1='#FF8300'
SeagrassGreen1='#93D500'
SeagrassGreen4='#D0D0D0' # This is just grey
UrchinPurple1='#7F7FFF'
WavesTeal1='#1ECAD3'

####This is the data pull####

newdat <- httr::content(httr::GET('https://apex.psmfc.org/akfin/data_marts/akmp/ecosystem_sub_crw_avg_sst?ecosystem_sub=Eastern%20Gulf%20of%20Alaska&start_date=19850401&end_date=20221231'), type = "application/json") %>% 
  bind_rows %>% 
  mutate(date=as_date(READ_DATE)) %>% 
  data.frame %>% 
  dplyr::select(date,meansst=MEANSST,Ecosystem_sub=ECOSYSTEM_SUB) %>% 
  mutate(doy=yday(date),
         year=year(date),
         month=month(date),
         day=day(date),
         newdate=as.Date(ifelse(month>=12,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),
                                as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
         year2=ifelse(month>=12,year+1,year),
         Ecosystem_sub=fct_relevel(Ecosystem_sub,"Western Gulf of Alaska")) %>% 
  arrange(date) 


#  Assign colors to different time series.
current.year.color <- "black"
last.year.color <- OceansBlue1
mean.color <- UrchinPurple1

#  Set default plot theme
theme_set(theme_cowplot())

#  Specify legend position coordinates
mylegx <- 0.625
mylegy <- 0.865

current.year <- max(newdat$year)
last.year <- current.year-1
mean.years <- 1985:2014
mean.lab <- "Mean 1985-2014"




#--------------------------------------------------------------------------------------------------------------------------
#  Figure 2. Marine heatwave (MHW) status during the last three years. Filled (yellow) areas depict MHW events. 
#  Black lines represent the 30-year baseline (smoothed line) and observed daily sea surface temperatures (jagged line). 
#  Faint grey dotted lines illustrate the MHW severity thresholds in increasing order (moderate, strong). 
#--------------------------------------------------------------------------------------------------------------------------

#  Create figure that shows MHW status
mhw <- detect_event(ts2clm(newdat %>%
                                   rename(t=date,temp=meansst) %>% 
                                   arrange(t), climatologyPeriod = c("1985-12-01", "2014-11-30")))$clim 


clim_cat <- mhw %>%
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff,
                year=year(t))



####This is the figure that shows seasons####
mytheme <- theme(strip.text = element_text(size=10,color="white",family="sans",face="bold"),
                 strip.background = element_rect(fill=OceansBlue2),
                 axis.title = element_text(size=10,family="sans",color="black"),
                 axis.text = element_text(size=10,family="sans",color="black"),
                 panel.border=element_rect(colour="black",fill=NA,size=0.5),
                 panel.background = element_blank())


#--------------------------------------------------------------------------------------------------------------------------
# Figure 3. Number of days during which marine heatwave conditions persisted in a given year. 
# Seasons are summer (Jun - Aug), fall (Sept – Nov), winter (Dec – Feb), spring (Mar – Jun). 
# Years are shifted to include complete seasons so December of a calendar year is grouped with the 
# following year to aggregate winter data (e.g., Dec 2020 occurs with winter of 2021).
#--------------------------------------------------------------------------------------------------------------------------



mhw_egoa <- ((detect_event(ts2clm(newdat %>%
                                   rename(t=date,temp=meansst) %>% 
                                   arrange(t), climatologyPeriod = c("1985-12-01", "2015-11-30")))))$event %>% 
  data.frame

annualevents <-   lapply(1:nrow(mhw_egoa),function(x)data.frame(date=seq.Date(as.Date(mhw_egoa[x,"date_start"]),as.Date(mhw_egoa[x,"date_end"]),"days"))) %>% 
  bind_rows() %>% 
  mutate(year=year(date),
         month=month(date),
         year2=ifelse(month>=12,year+1,year)) %>% 
  dplyr::select(year,month,year2) %>% 
  group_by(year2) %>% 
  summarise(Fall=length(month[month%in%c(9,10,11)]),
            Winter=length(month[month%in%c(12,1,2)]),
            Spring=length(month[month%in%c(3,4,5)]),
            Summer=length(month[month%in%c(6,7,8)])) %>% 
  right_join(data.frame(year2=1985:2022)) %>% 
  replace_na(list(Fall=0,Winter=0,Spring=0,Summer=0)) %>% 
  arrange(year2) 

png("GOA/2022/Callahan_Figure3_MHW_days_season_2022.png",width=6,height=3.375,units="in",res=300)
annualevents %>% 
  gather(Period,Duration,-c(year2)) %>% 
  data.frame %>% 
  mutate(Period=fct_relevel(Period,"Summer","Fall","Winter","Spring")) %>% 
  #filter(Period!="totaldays") %>% 
  ggplot() +
  geom_bar(aes(year2,Duration,fill=Period),stat="identity") + 
  scale_fill_manual(name="",labels=c("Summer","Fall","Winter","Spring"),values=c(OceansBlue2,Crustacean1,UrchinPurple1,WavesTeal1)) +
  #geom_bar(aes(year2,totaldays),stat="identity",fill=OceansBlue2) + 
  #geom_bar(aes(year2,winterdays),stat="identity",fill=Crustacean1) + 
  mytheme + 
  scale_x_continuous(expand=c(0,0.5)) +
  scale_y_continuous(limits=c(0,370),expand=c(0.0,0)) +
  xlab("Year") + 
  ylab("Number of Marine Heatwave Days") +
  theme(plot.margin=unit(c(0.15,0.25,0.05,0),"cm"),
        legend.position = c(0.1,0.85))
dev.off()
#--------------------------------------------------------------------------------------------------------------------------

