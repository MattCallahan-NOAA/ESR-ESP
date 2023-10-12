library(tidyverse)
library(lubridate)
library(forcats)
library(readxl)
library(viridis)
library(magrittr)
library(ggridges)
library(odbc)
library(getPass)
library(cowplot)

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

####------------------------------------------------------####
# plot AMJ time series

#load 508 complient colors
current.year.color <- "black"
last.year.color <- '#0093D0'
mean.color <- '#7F7FFF'
old.years.color<-'#D0D0D0'


#  Specify legend position coordinates
mylegx <- 0.3
mylegy <- 0.865

current.year <- max(data$year)
last.year <- current.year-1
mean.years <- 1998:last.year
mean.lab <- paste0("Mean 1998-",last.year)

theme_set(theme_cowplot())

png("ESR/2023/Chla_annual_lines.png",width=7,height=5,units="in",res=300)
ggplot() +
  geom_line(data=data %>% filter(year<last.year),
            aes(doy,meanchla,group=factor(year),col='old.years.color'),size=0.3) +
  geom_line(data=data %>% filter(year==last.year),
            aes(doy,meanchla,color='last.year.color'),size=0.75) +
  geom_line(data=data %>% 
              filter(year%in%mean.years) %>% 
              group_by(ecosystem_subarea, doy) %>% 
              summarise(meanchla=mean(meanchla,na.rm=TRUE)),
            aes(doy,meanchla,col='mean.color'),size=0.75) +
  geom_line(data=data %>% filter(year==current.year),
            aes(doy,meanchla,color='current.year.color'),size=0.95) +
  facet_wrap(~ecosystem_subarea ,ncol=2) + 
  scale_color_manual(name="",
                     breaks=c('current.year.color','last.year.color','old.years.color','mean.color'),
                     values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'old.years.color'=old.years.color,'mean.color'=mean.color),
                     labels=c(current.year,last.year,paste0('1998-',last.year-1),mean.lab)) +
  ylab("mean Chla (ug/L)") + 
  xlab("") +
  scale_x_continuous(breaks=c(91,121,152), labels=c("Apr", "May", "Jun"))+
  # scale_x_date(date_breaks="1 month",
  #              date_labels = "%b",
  #              expand = c(0.025,0.025)) + 
  theme(legend.position=c(mylegx,mylegy),
        legend.text = element_text(size=8,family="sans"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=10,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title = element_text(size=10,family="sans"),
        axis.text = element_text(size=10,family="sans"),
        #panel.border=element_rect(colour="black",size=0.75),
       # axis.text.x=element_text(color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
        legend.key.size = unit(0.35,"cm"),
        plot.margin=unit(c(0.65,0,0.65,0),"cm")) 
dev.off()


#plot smoothed version
png("ESR/2023/Chla_annual_lines_smoothed.png",width=7,height=5,units="in",res=300)
ggplot() +
  geom_smooth(data=data %>% filter(year<last.year),
            aes(doy,meanchla,group=factor(year),col='old.years.color'),size=0.3, method="loess", se=FALSE) +
  geom_smooth(data=data %>% filter(year==last.year),
            aes(doy,meanchla,color='last.year.color'),size=0.75, method="loess", se=FALSE) +
  geom_smooth(data=data %>% 
              filter(year%in%mean.years) %>% 
              group_by(ecosystem_subarea, doy) %>% 
              summarise(meanchla=mean(meanchla,na.rm=TRUE)),
            aes(doy,meanchla,col='mean.color'),size=0.75, method="loess", se=FALSE) +
  geom_smooth(data=data %>% filter(year==current.year),
            aes(doy,meanchla,color='current.year.color'),size=0.95, method="loess", se=FALSE) +
  facet_wrap(~ecosystem_subarea ,ncol=2) + 
  scale_color_manual(name="",
                     breaks=c('current.year.color','last.year.color','old.years.color','mean.color'),
                     values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'old.years.color'=old.years.color,'mean.color'=mean.color),
                     labels=c(current.year,last.year,paste0('1998-',last.year-1),mean.lab)) +
  ylab("mean Chla (ug/L)") + 
  xlab("") +
  scale_x_continuous(breaks=c(91,121,152), labels=c("Apr", "May", "Jun"))+
  theme(legend.position=c(mylegx,mylegy),
        legend.text = element_text(size=8,family="sans"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=10,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title = element_text(size=10,family="sans"),
        axis.text = element_text(size=10,family="sans"),
        legend.key.size = unit(0.35,"cm"),
        plot.margin=unit(c(0.65,0,0.65,0),"cm")) 
dev.off()

# plot all years
png("ESR/2023/Chla_yearbyyear_lines.png",width=7,height=20,units="in",res=300)
ggplot() +
  geom_line(data=data,
            aes(doy,meanchla),color="red",size=0.95) +
  facet_grid(rows=vars(year), cols=vars(ecosystem_subarea)) + 
  ylab("mean Chla (ug/L)") + 
  xlab("") +
  scale_x_continuous(breaks=c(91,121,152), labels=c("Apr", "May", "Jun"))+
  theme(
        strip.text = element_text(size=12,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title = element_text(size=12,family="sans"),
        axis.text = element_text(size=10,family="sans"),
        panel.border=element_rect(colour="black",size=0.25),
        # axis.text.x=element_text(color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
        legend.key.size = unit(0.35,"cm"),
        plot.margin=unit(c(0.65,0,0.65,0),"cm")) 
dev.off()


####------------------------------------------------------####
# extra code for adding value to text

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
