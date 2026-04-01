# SST analysis for Johanna
# Confirming that averaging was done correctly:

library(tidyverse)
library(heatwaveR)
library(lubridate)
library(viridis)
library(cowplot)
library(ggplottimeseries)

current.year <- 2025
last.year <- current.year-1
climatology_start_year <- 1985
climatology_start_date <- "1985-01-01"
climatology_end_year <- 2014
climatology_end_date <- "2014-12-31"
mean.years <- climatology_start_year:climatology_end_year 
mean.lab <- paste0("Mean ",climatology_start_year,"-",climatology_end_year)

newdat <- httr::content(httr::GET('https://apex.psmfc.org/akfin/data_marts/akmp/ecosystem_sub_crw_avg_sst?ecosystem_sub=Southeastern%20Bering%20Sea,Northern%20Bering%20Sea&start_date=19850101&end_date=20260101'), type = "application/json") %>% 
  bind_rows %>% 
  mutate(date=as_date(READ_DATE)) %>% 
  data.frame %>% 
  dplyr::select(date,meansst=MEANSST,Ecosystem_sub=ECOSYSTEM_SUB) %>% 
  mutate(doy=yday(date),
         year=year(date),
         month=month(date),
         day=day(date),
         newdate=as.Date(ifelse(month>=9,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),
                                as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
         year2=ifelse(month>=9,year+1,year)) %>% 
  filter(year2<=current.year)%>%
  arrange(date) 

mymean <- newdat %>% 
  filter(!year2%in%c(1985)) %>% 
  group_by(year2,Ecosystem_sub) %>% 
  arrange(newdate) %>% 
  summarise(cumheat=sum(meansst),
            dailymean1=mean(meansst)) %>% 
  group_by(Ecosystem_sub) %>% 
  mutate(meanheat=mean(cumheat[between(year2,1986,climatology_end_year)]), # Mannually change
         sdheat=sd(cumheat[between(year2,1986,climatology_end_year)]), # Mannually change
         anomaly=cumheat-meanheat,
         meantemp1=mean(dailymean1),
         temp_anom_1 = dailymean1-meantemp1)

mean2 <- newdat %>% 
  group_by(year,Ecosystem_sub) %>% 
  arrange(newdate) %>% 
  summarise(cumheat2=sum(meansst),
            dailymean2=mean(meansst)) %>% 
  group_by(Ecosystem_sub) %>% 
  mutate(meanheat2=mean(cumheat2), # Mannually change
         sdheat2=sd(cumheat2), # Mannually change
         anomaly2=cumheat2-meanheat2,
         meantemp2=mean(dailymean2),
         temp_anom_2 = dailymean2-meantemp2)

compare <- left_join(mymean,mean2,by=c("year2"="year","Ecosystem_sub"="Ecosystem_sub"))
ggplot(compare)+
  geom_col(aes(x=year2,y=dailymean2), fill="darkred", alpha=0.5)+
  geom_col(aes(x=year2,y=dailymean1), fill="darkblue", alpha=0.5)+
  facet_wrap(~Ecosystem_sub)+
  theme_minimal()


compare <- compare %>%
  mutate(warm1=ifelse(temp_anom_1>0,"WARM","COLD"),
         warm2=ifelse(temp_anom_2>0,"WARM","COLD")) 



