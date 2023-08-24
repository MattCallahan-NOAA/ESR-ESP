library(tidyverse)
library(lubridate)
library(forcats)
library(readxl)
library(viridis)
library(magrittr)
library(ggridges)
library(odbc)
library(getPass)

#  Load 508 compliant NOAA colors
OceansBlue1='#0093D0'
OceansBlue2='#0055A4' # darker blue used for strip.background

# pull data from akfin
#connect to akfin
con <- dbConnect(odbc::odbc(), "akfin", UID=getPass(msg="USER NAME"), PWD=getPass()) 

#load data with depth, season, and region filters
data<-dbFetch(dbSendQuery(con, "select round(avg(chla),2) meanchla, to_date(start_date,'YYYY-MM-DD')+4 mid_date, ecosystem_subarea
from env_data.globcolour_2023 a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and ecosystem_area = ('Gulf of Alaska')
and waters_cod = 'FED'
and depth>(-200)
and depth<(-10)
group by to_date(start_date,'YYYY-MM-DD')+4, ecosystem_subarea"))%>%
  rename_with(tolower)

data <-  data%>%
  mutate(month=month(mid_date),
         year=year(mid_date),
         doy=yday(mid_date),
         ecosystem_subarea=fct_relevel(ecosystem_subarea,"Western Gulf of Alaska","Eastern Gulf of Alaska"))

#  Prepare data for chlorophyll spring average plot.
#  Save this as a separate object so we can calculate the means by ecosystem_subarea area.
plotdat <- data %>% 
  filter(month%in%(4:6)) %>% # Filter for April-June
  group_by(year,ecosystem_subarea) %>% 
  summarise(springchl=mean(meanchla,na.rm=TRUE)) %>% 
  mutate(iseven=ifelse(year%in%(seq(1998,2023,by=2)),"even","odd")) %>% # Create an even/odd year flag for salmon models
  data.frame

#  Are means significantly different across regions? 
summary(lm(springchl~ecosystem_subarea,data=plotdat)) #no
t.test(springchl~ecosystem_subarea,data=plotdat) #no

#  Create a data frame that has the time series means for spring chlorophyll
plotmean <- plotdat %>% 
  group_by(ecosystem_subarea) %>% 
  summarise(mymean=mean(springchl))

#  Create Figure 1 for the ESR. 
#  Time series of spring chlorophyll concentrations.
png("ESR/2023/Figure_1_GOA.png",width=8,height=6,units="in",res=300)
plotdat %>% 
  ggplot() + 
  geom_bar(aes(year,springchl),stat="identity",fill=OceansBlue1) +
  geom_hline(data=plotmean,aes(yintercept=mymean),linetype=2) + #add horizontal mean values
  facet_wrap(~ecosystem_subarea) + 
  theme(strip.text = element_text(size=11,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title = element_text(size=12,family="sans"),
        axis.text = element_text(size=11,family="sans",color="black"),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black"),
        legend.position="top",
        legend.text=element_text(size=11,family="sans"),
        legend.title = element_text(size=12,family="sans")) +  xlab("Year") + 
  ylab("Chlorophyll-a [ug/L]") + 
  scale_x_continuous(expand = c(0.02,0.0)) + # Specify a boundary and expansion to set bars on bottom black line
scale_y_continuous(limits=c(0,5.5),expand = c(0,0.0)) # Specify a boundary and expansion to set bars on bottom black line
dev.off()


png("ESR/2023/Figure_2_GOA.png",width=8,height=6,units="in",res=300)
data %>% 
  #filter(month%in%(3:6)) %>% 
  filter(doy>=50 & doy<=180) %>% 
  group_by(year,ecosystem_subarea) %>% 
  mutate(mymax=doy[meanchla==max(meanchla)][1]) %>% 
  ungroup %>% 
  group_by(ecosystem_subarea) %>% 
  mutate(meandoy=mean(mymax)) %>% 
  data.frame %>% 
  ggplot() +
  geom_tile(aes(doy,year,fill=log(meanchla))) + 
  geom_vline(aes(xintercept = meandoy),linetype=2,color="white") +
  scale_fill_viridis(option="C",name="Chlorophyll-a (ug/L)") + 
  facet_wrap(~ecosystem_subarea) + 
  theme(strip.text = element_text(size=11,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title = element_text(size=12,family="sans"),
        axis.text = element_text(size=11,family="sans",color="black"),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black"),
        legend.position="top",
        legend.text=element_text(size=11,family="sans"),
        legend.title = element_text(size=12,family="sans"),
        legend.margin=margin(t = 0.1, unit='cm')) +
  ylab("Year") + 
  xlab("Day of the year") +
  scale_y_continuous(expand=c(0,0.2)) + 
  scale_x_continuous(expand=c(0,6)) 
dev.off()

data %>% 
  filter(doy>=50 & doy<=180 & year==2022) %>% 
  group_by(ecosystem_subarea) %>% 
  mutate(mymax=doy[meanchla==max(meanchla)][1])%>%
  print(n=Inf)

data %>% 
  filter(doy>=50 & doy<=180) %>% 
  group_by(ecosystem_subarea,year) %>% 
  summarise(mymax=doy[meanchla==max(meanchla)][1]) %>% 
  group_by(ecosystem_subarea) %>% 
  summarise(mean(mymax))

data %>% 
  filter(doy%in%c(189,117) & year==2022) %>% 
  group_by(ecosystem_subarea) %>% 
  mutate(mymax=doy[meanchla==max(meanchla)][1])

data%>%
  filter(month %in% (4:6))%>%
  group_by(year,ecosystem_subarea)%>%
  summarize(annual_meanchla=mean(meanchla))%>%
  print(n=Inf)


#  Rescaled version of heatmap
dailydat <- data %>% 
  filter(month%in%(3:8)) %>% 
  group_by(doy,year,ecosystem_subarea) %>% 
  summarise(springchl=mean(meanchla,na.rm=TRUE)) %>% 
  mutate(iseven=ifelse(year%in%(seq(1998,2023,by=2)),"even","odd")) %>% 
  data.frame

png("ESR/2023/goa_chl_daily_heatmap.png",width=8,height=6,units="in",res=300)
dailydat %>% 
  ggplot() + 
  geom_tile(aes(doy,year,fill=springchl)) + 
  scale_fill_viridis(option="C",name="Chlorophyll-a (ug/L)") + 
  facet_wrap(~ecosystem_subarea) + 
  theme(strip.text = element_text(size=10,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title = element_text(size=10,family="sans"),
        axis.text = element_text(size=10,family="sans"),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black"),
        legend.position="top") +
  ylab("Year") + 
  xlab("Day of the year")
dev.off()


#add april 19 to data and rerun from line 17, see if it makes a difference...
#ultimately it doesn't...
data<-readRDS("../../chla-indicator-comparison/Data/MODIS/ESR/mod_goa.RDS")%>%
  rename_with(tolower)
april<-readRDS("../../chla-indicator-comparison/Data/MODIS/ESR/mod_apr19.RDS")%>%
  dplyr::select(!c(lonc, latc, BSIERP_ID, BSIERP_Region_Name, NBS_CRAB, BS_KING, BS_TANNER))%>%
  rename_with(tolower)%>%
  filter(ecosystem_area=="Gulf of Alaska")
  
data<-data%>%bind_rows(april)





#  Create a monthly dataset for the heatmap. 
#  I save this as a separate object because I explored models with it. However you could instead just pipe all of this
#  directly into the ggplot call that follows it and avoid saving the intermediate (monthdat) object.
monthdat <- data %>% 
  filter(month%in%(3:8)) %>% 
  group_by(month,year,ecosystem_subarea) %>% 
  summarise(springchl=mean(meanchl,na.rm=TRUE)) %>% 
  mutate(iseven=ifelse(year%in%(seq(2003,2020,by=2)),"even","odd")) %>% 
  data.frame

png("goa_chl_monthly_heatmap.png",width=8,height=6,units="in",res=300)
monthdat %>% 
  ggplot() + 
  geom_tile(aes(factor(month),factor(year),fill=springchl)) + 
  scale_fill_viridis(option="C",name="Chlorophyll-a (ug/L)") + 
  facet_wrap(~ecosystem_subarea) + 
  theme(strip.text = element_text(size=10,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title = element_text(size=10,family="sans"),
        axis.text = element_text(size=10,family="sans"),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black"),
        legend.position="top") +
  ylab("Year") + 
  xlab("Month")
dev.off()


####2021 SST####
sst <- httr::content(httr::GET('https://apex.psmfc.org/akfin/data_marts/akmp/ecosystem_subarea_crw_avg_sst?ecosystem_subarea=Eastern%20Gulf%20of%20Alaska,Western%20Gulf%20of%20Alaska&start_date=19850101&end_date=20211231'), type = "application/json") %>% 
  bind_rows %>% 
  mutate(date=as_date(READ_DATE)) %>% 
  data.frame %>% 
  dplyr::select(date,meansst=MEANSST,ecosystem_subarea=ecosystem_subarea) %>% 
# sst <- readRDS("Data/crwsst_goaesr_19850401_to_2020_0824.RDS") %>% 
#   filter(ecosystem_subarea %in% c("Eastern Gulf of Alaska","Western Gulf of Alaska")) %>% 
  mutate(month=month(date),
         year=year(date),
         ecosystem_subarea=fct_relevel(ecosystem_subarea,"Western Gulf of Alaska","Eastern Gulf of Alaska")) %>% 
  filter(month%in%(4:6)) %>% 
  group_by(year,ecosystem_subarea) %>% 
  summarise(meansst=mean(meansst,na.rm=TRUE)) %>% 
  data.frame

data %>% 
  filter(month%in%(4:6)) %>% 
  group_by(year,ecosystem_subarea) %>% 
  summarise(springchl=mean(meanchl,na.rm=TRUE)) %>% 
  mutate(iseven=ifelse(year%in%(seq(2003,2021,by=2)),"even","odd")) %>% 
  data.frame %>% 
  inner_join(sst) %>%
  ggplot() + 
  geom_point(aes(year,springchl,color=iseven),size=2) + 
  geom_line(aes(year,springchl)) + 
  geom_point(aes(year,meansst),size=2,color="black") + 
  geom_line(aes(year,meansst),linetype=2) +
  facet_wrap(~ecosystem_subarea)

data %>% 
  filter(month%in%(4:6)) %>% 
  group_by(year,ecosystem_subarea) %>% 
  summarise(springchl=mean(meanchl,na.rm=TRUE)) %>% 
  mutate(iseven=ifelse(year%in%(seq(2003,2020,by=2)),"even","odd")) %>% 
  data.frame %>% 
  inner_join(sst) %>%
  ggplot() + 
  geom_point(aes(meansst,springchl),size=2) +
  facet_wrap(~ecosystem_subarea)

library(broom)

#  Does temperature drive chlorophyll? No.
data %>% 
  filter(month%in%(4:6)) %>% 
  group_by(year,ecosystem_subarea) %>% 
  summarise(springchl=mean(meanchl,na.rm=TRUE)) %>% 
  mutate(iseven=ifelse(year%in%(seq(2003,2021,by=2)),"even","odd")) %>% 
  data.frame %>% 
  inner_join(sst) %>% 
  group_by(ecosystem_subarea) %>% 
  do(glance(lm(springchl~meansst+iseven,data=.)))


data %>% 
  filter(doy>=50 & doy<=180) %>% 
  group_by(ecosystem_subarea,year) %>% 
  summarise(mymax=doy[meanchl==max(meanchl)][1]) %>% 
  inner_join(sst) %>% 
  ggplot(aes(mymax,meansst)) + 
  geom_point() + 
  facet_wrap(~ecosystem_subarea)


data %>% 
  filter(doy>=50 & doy<=180) %>% 
  group_by(ecosystem_subarea,year) %>% 
  summarise(mymax=doy[meanchl==max(meanchl)][1]) %>% 
  inner_join(sst) %>% 
group_by(ecosystem_subarea) %>% 
  do(glance(lm(mymax~meansst,data=.)))
