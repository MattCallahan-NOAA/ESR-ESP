library(tidyverse)
library(heatwaveR)
library(lubridate)
library(cowplot)
library(viridis)
#devtools::install_github("brisneve/ggplottimeseries")
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
redsnapper = "#D02C2F"

mytheme <- theme(strip.text = element_text(size=10,color="white",family="sans",face="bold"),
                 strip.background = element_rect(fill=OceansBlue2),
                 axis.title = element_text(size=10,family="sans",color="black"),
                 axis.text = element_text(size=10,family="sans",color="black"),
                 panel.border=element_rect(colour="black",fill=NA,size=0.5),
                # panel.background = element_blank(),
                 plot.margin=unit(c(0.65,0,0.65,0),"cm"),
                 legend.position=c(0.6,0.7),
                 legend.background = element_blank(),
                 legend.key.size = unit(1,"line"))


data <- httr::content(httr::GET('https://apex.psmfc.org/akfin/data_marts/akmp/ecosystem_sub_crw_avg_sst?ecosystem_sub=Western%20Aleutians,Central%20Aleutians,Eastern%20Aleutians&start_date=19850101&end_date=20251231'), type = "application/json") %>% 
  bind_rows %>% 
  mutate(date=as_date(READ_DATE)) %>% 
  data.frame %>% 
  dplyr::select(date,meansst=MEANSST,esr_region=ECOSYSTEM_SUB) %>% 
  mutate(doy=yday(date),
         year=year(date),
         month=month(date),
         day=day(date),
         newdate=as.Date(ifelse(month>=12,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),
                                as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
         year2=ifelse(month>=12,year+1,year),
         esr_region=fct_relevel(esr_region,"Western Aleutians")) %>% 
  rename(read_date=date) %>% 
  arrange(read_date) 


#----------------------------------------------------------------------------------------------------------------------------
#  Figure 1. Temperature time series with years overlain NOW JUST FROM SHINY
#----------------------------------------------------------------------------------------------------------------------------

#  Assign colors to different time series.
current.year.color <- Crustacean1#CoralRed1 #OceansBlue1
last.year.color <- OceansBlue1#WavesTeal1
mean.color <- "black"

#  Set default plot theme
#theme_set(theme_cowplot())

#  Specify legend position coordinates
mylegx <- 0.35
mylegy <- 0.865

#  Specify NOAA logo position coordinates
mylogox <- 0.045
mylogoy <- 0.285

#  I copied and pasted previous code and of course, I'd used a different name for the dataset.
#  I'm too lazy to make things consistent throughout so I just make a copy.
#data <- newdat

#  Set year criteria to automatically identify the current and previous years
current.year <- max(data$year)
last.year <- current.year-1
climatology_start_year <- 1985
climatology_start_date <- "1985-01-01"
climatology_end_year <- 2014
climatology_end_date <- "2014-12-31"
mean.years <- climatology_start_year:climatology_end_year 
mean.lab <- paste0("Mean ",climatology_start_year,"-",climatology_end_year)



#----------------------------------------------------------------------------------------------------------------------------
#  Figure 2. Explore time series trend
#  Time series trend (i.e., seasonality and noise removed) of sea surface temperatures. Horizontal dashed 
#  lines represent the mean (black) and standard deviation from the mean (red) during the earliest complete 30-yr baseline period (1985-2014). 
#----------------------------------------------------------------------------------------------------------------------------

wai <- data %>% 
  filter(esr_region=="Western Aleutians")

dfwai <- dts1(wai$read_date,wai$meansst,365.25, type = "additive") %>% 
  mutate(ecosystem_sub="Western Aleutians")

cai <- data %>% 
  filter(esr_region=="Central Aleutians")

dfcai <- dts1(cai$read_date,cai$meansst,365.25, type = "additive") %>% 
  mutate(ecosystem_sub="Central Aleutians")

eai <- data %>% 
  filter(esr_region=="Eastern Aleutians")

dfeai <- dts1(eai$read_date,eai$meansst,365.25, type = "additive") %>% 
  mutate(ecosystem_sub="Eastern Aleutians")


df <- dfwai %>% 
  bind_rows(dfcai) %>% 
  bind_rows(dfeai) %>% 
  mutate(ecosystem_sub=factor(ecosystem_sub,c("Western Aleutians", "Central Aleutians", "Eastern Aleutians")),
         year=year(date))

dfmean <- df %>% 
  group_by(ecosystem_sub) %>% 
  summarise(meantrend=mean(trend[between(year,climatology_start_year,climatology_end_year)],na.rm=TRUE), # uses 1985-2014 since visualization uses calendar year
            sdtrend=sd(trend[between(year,climatology_start_year,climatology_end_year)],na.rm=TRUE))

png(paste0("AI/",current.year,"/Figure_2_SST_AI_ESR_TimeSeries_updated_test.png"),width=6,height=4,units="in",res=300)
df %>% 
  ggplot(aes(x = date, y = trend)) + 
  geom_line() + 
  geom_hline(data=dfmean,aes(yintercept=meantrend),linetype=2) +
  geom_hline(data=dfmean,aes(yintercept=meantrend+sdtrend),linetype=2,color=redsnapper) +
  geom_hline(data=dfmean,aes(yintercept=meantrend-sdtrend),linetype=2,color=redsnapper) +
  facet_wrap(~ecosystem_sub) + 
  theme_bw() + 
  theme(strip.text = element_text(size=10,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title = element_text(size=10,family="sans"),
        axis.text = element_text(size=10,family="sans"),
        panel.border=element_rect(colour="black",size=0.5),
        plot.margin=unit(c(0.15,0.05,0.15,0),"cm"),
        panel.grid = element_line()) + 
  ylab("Sea surface temperature (C)") + 
  xlab("Date")
dev.off()
#---------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------
# Figure 3. Marine heatwave (MHW) status during the last three years. 
# Now taken from shiny app
# Filled (yellow) areas depict MHW events.
# Black lines represent the 30-year baseline (smoothed line) and observed daily sea surface temperatures (jagged line). 
# Faint grey dotted lines illustrate the MHW severity thresholds in increasing order (moderate, strong).
#---------------------------------------------------------------------------------------------

#  Create figure that shows MHW status
# mhw <- (detect_event(ts2clm((data) %>%
#                               filter(esr_region=="Western Aleutians") %>% 
#                               rename(t=read_date,temp=meansst) %>% 
#                               arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>% # uses 1986-2015 year2 baseline
#   mutate(region="Western Aleutians") %>% 
#   bind_rows((detect_event(ts2clm((data) %>%
#                                    filter(esr_region=="Eastern Aleutians") %>% 
#                                    rename(t=read_date,temp=meansst) %>% 
#                                    arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>% 
#               mutate(region="Eastern Aleutians")) %>% 
#   bind_rows((detect_event(ts2clm((data) %>%
#                                    filter(esr_region=="Central Aleutians") %>% 
#                                    rename(t=read_date,temp=meansst) %>% 
#                                    arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>% 
#               mutate(region="Central Aleutians"))
# 
# clim_cat <- mhw %>%
#   mutate(region=fct_relevel(region,"Western Aleutians")) %>% 
#   group_by(region) %>% 
#   dplyr::mutate(diff = thresh - seas,
#                 thresh_2x = thresh + diff,
#                 thresh_3x = thresh_2x + diff,
#                 thresh_4x = thresh_3x + diff,
#                 year=year(t))
# 
# # Set line colours
# lineColCat <- c(
#   "Temperature" = "black",
#   "Climatology" = "gray20",
#   "Moderate" = "gray60",
#   "Strong" = "gray60"#,
#   #"Severe" = "gray60"#,
#   #"Extreme" = "gray60"
# )
# 
# fillColCat <- c(
#   "Moderate" = "#ffc866",
#   "Strong" = "#ff6900",
#   "Severe" = "#9e0000",
#   "Extreme" = "#2d0000"
# )
# 
# mytheme2 <- theme(strip.text = element_text(size=10,color="white",family="sans",face="bold"),
#                  strip.background = element_rect(fill=OceansBlue2),
#                  axis.title = element_text(size=10,family="sans",color="black"),
#                  axis.text = element_text(size=10,family="sans",color="black"),
#                  panel.border=element_rect(colour="black",fill=NA,size=0.5))
#         
# 
# png(paste0("AI/",current.year,"/Figure_3_Flames.png"),width=7,height=5,units="in",res=300)
# ggplot(data = clim_cat %>% filter(t>=as.Date(paste0(current.year-3,"-12-01"))), aes(x = t, y = temp)) +
#   geom_flame(aes(y2 = thresh, fill = "Moderate")) +
#   geom_flame(aes(y2 = thresh_2x, fill = "Strong")) +
#   geom_flame(aes(y2 = thresh_3x, fill = "Severe")) +
#   geom_flame(aes(y2 = thresh_4x, fill = "Extreme")) +
#   geom_line(aes(y = thresh_2x, col = "Strong"), size = 0.5, linetype = "dotted") +
#   #geom_line(aes(y = thresh_3x, col = "Severe"), size = 0.5, linetype = "dotted") +
#   #geom_line(aes(y = thresh_4x, col = "Extreme"), size = 0.5, linetype = "dotted") +
#   geom_line(aes(y = seas, col = "Climatology"), size = 0.5) +
#   geom_line(aes(y = thresh, col = "Moderate"), size = 0.5,linetype= "dotted") +
#   geom_line(aes(y = temp, col = "Temperature"), size = 0.5) +
#   scale_colour_manual(name = NULL, values = lineColCat,
#                       breaks = c("Temperature", "Climatology", 
#                                  "Moderate","Strong"#, "Severe"
#                       )) +
#   scale_fill_manual(name = "Heatwave\nIntensity", values = fillColCat, labels=c("Moderate","Strong","Severe","Extreme")) +
#   scale_x_date(date_labels = "%b %Y",expand=c(0.01,0)) +
#   guides(color="none")+
#  # guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "dotted",
#  #                                                               "dotted"#, "dotted", "dotted"
#   #),
#  # size = c(0.6, 0.7, 0.7, 0.7#, 0.7, 0.7
#  # )),
#   #ncol=6)) +
#   labs(y = "Sea Surface Temperature (°C)", x = NULL) + 
#  # theme(legend.position="none") +
#   facet_wrap(~region,ncol=1,scales="free_y") +
#   mytheme2 + 
#   theme(legend.position=c(0.05,0.88),
#     #legend.key=element_blank(),
#     legend.text = element_text(size=7),
#     legend.key.size = unit(0.8,"line"),
#     legend.title = element_text(size=8),
#     axis.title.x=element_blank()
#    # legend.margin=margin(l=-6.25,t = -8.5, unit='cm'),
#    # plot.margin=unit(c(0.65,0,0.0,0),"cm")
#    )
# dev.off()

#---------------------------------------------------------------------------------------------
# Figure 4. Number of days during which marine heatwave conditions persisted in a given year. 
# Seasons are summer (Jun - Aug), fall (Sept – Nov), winter (Dec – Feb), spring (Mar – Jun). 
# Years are shifted to include complete seasons so December of a calendar year is grouped with 
# the following year to aggregate winter data (e.g., Dec 2019 occurs with winter of 2020).

mhw_wai <- (detect_event(ts2clm((data) %>%
                                  filter(esr_region=="Western Aleutians") %>% 
                                  rename(t=read_date,temp=meansst) %>% 
                                  arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$event %>% 
  mutate(region="WesternAleutians") %>% 
  data.frame

mhw_eai <- ((detect_event(ts2clm((data) %>%
                                   filter(esr_region=="Eastern Aleutians") %>% 
                                   rename(t=read_date,temp=meansst) %>% 
                                   arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$event %>% 
              mutate(region="Eastern Aleutians")) %>% 
  data.frame

mhw_cai <- ((detect_event(ts2clm((data) %>%
                                   filter(esr_region=="Central Aleutians") %>% 
                                   rename(t=read_date,temp=meansst) %>% 
                                   arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$event %>% 
              mutate(region="Central Aleutians")) %>% 
  data.frame

annualevents <- lapply(1:nrow(mhw_wai),function(x)data.frame(date=seq.Date(as.Date(mhw_wai[x,"date_start"]),as.Date(mhw_wai[x,"date_end"]),"days"))) %>% 
  bind_rows() %>% 
  mutate(region="Western Aleutians") %>% 
  bind_rows(lapply(1:nrow(mhw_eai),function(x)data.frame(date=seq.Date(as.Date(mhw_eai[x,"date_start"]),as.Date(mhw_eai[x,"date_end"]),"days"))) %>% 
              bind_rows() %>% 
              mutate(region="Eastern Aleutians")) %>% 
  bind_rows(lapply(1:nrow(mhw_cai),function(x)data.frame(date=seq.Date(as.Date(mhw_cai[x,"date_start"]),as.Date(mhw_cai[x,"date_end"]),"days"))) %>% 
              bind_rows() %>% 
              mutate(region="Central Aleutians")) %>% 
  #distinct() %>% 
  mutate(year=year(date),
         month=month(date),
         year2=ifelse(month>=12,year+1,year)) %>% 
  dplyr::select(region,year,month,year2) %>% 
  group_by(year2,region) %>% 
  summarise(Fall=length(month[month%in%c(9,10,11)]),
            Winter=length(month[month%in%c(12,1,2)]),
            Spring=length(month[month%in%c(3,4,5)]),
            Summer=length(month[month%in%c(6,7,8)])) %>% 
  right_join(data.frame(year2=1985:current.year)) %>% 
  replace_na(list(Fall=0,Winter=0,Spring=0,Summer=0)) %>% 
  arrange(year2) %>% 
  filter(!is.na(region))

png(paste0("AI/",current.year,"/Figure_4_MHW_days_season_updated_test.png"),width=6,height=3.375,units="in",res=300)
annualevents %>% 
  gather(Period,Duration,-c(year2,region)) %>% 
  data.frame %>% 
  mutate(Period=factor(Period, c("Fall","Summer","Spring","Winter")),
         region=factor(region,c("Western Aleutians","Central Aleutians","Eastern Aleutians"))) %>% 
  #filter(Period!="totaldays") %>% 
  ggplot() +
  geom_bar(aes(year2,Duration,fill=Period),stat="identity") + 
  scale_fill_manual(name="",labels=c("Fall (Sep-Nov)","Summer (Jun-Aug)","Spring (Mar-May","Winter (Dec-Feb)"),values=c(OceansBlue2,Crustacean1,UrchinPurple1,WavesTeal1)) +
  #geom_bar(aes(year2,totaldays),stat="identity",fill=OceansBlue2) + 
  #geom_bar(aes(year2,winterdays),stat="identity",fill=Crustacean1) + 
  mytheme + 
  facet_wrap(~region) + 
  scale_x_continuous(expand=c(0,0.5)) +
  scale_y_continuous(limits=c(0,370),expand=c(0.0,0)) +
  xlab("Year") + 
  ylab("Number of Marine Heatwave Days") +
  #theme_bw() + 
  theme(plot.margin=unit(c(0.15,0.25,0.05,0),"cm"),
        legend.position = c(0.15,0.85),
        legend.text = element_text(size=9),
        panel.background = element_blank(),
        panel.grid = element_blank())
dev.off()




#Figure of proportion mhw
#connect to AKFIN
# This uses 1985-2014 baseline per CRW
con <- dbConnect(odbc::odbc(), "akfin", UID=getPass(msg="USER NAME"), PWD=getPass())

#query by heatwave category
mhw_ai2<- dbFetch(dbSendQuery(con,
                              paste0("select 
heatwave_category,
count(heatwave_category) mhw_count,
a.read_date, 
b.ecosystem_sub 
from (select
crw_id, read_date, heatwave_category
from afsc.erddap_crw_sst
where extract(year from read_date)=",current.year,") a
inner join (select id, ecosystem_sub 
from afsc.erddap_crw_sst_spatial_lookup
where ecosystem ='Aleutian Islands') b
on a.crw_id=b.id
group by a.read_date, b.ecosystem_sub, heatwave_category
order by a.read_date, b.ecosystem_sub, heatwave_category")))%>%
  rename_with(tolower)

#load ai mhw without heatwave category
mhw_ai<-readRDS("AI/Data/prop_mhw_ai.RDS")%>%
  rename_with(tolower)

#Join in total counts... I could do this in sql but it's easier for me in R since I already have the other results

mhw_ai2<-mhw_ai2%>%left_join(mhw_ai%>%dplyr::group_by(ecosystem_sub)%>%summarise(total_count=mean(total_count)),by="ecosystem_sub")
#calculate propmhw
mhw_ai2<-mhw_ai2%>%mutate(prop_mhw=mhw_count/total_count)

#save
saveRDS(mhw_ai2, "AI/Data/prop_mhw_ai2.RDS")

#start here on subsequent runs
mhw_ai2<-readRDS("AI/Data/prop_mhw_ai2.RDS")
#reorder ecosystems
mhw_ai2<-mhw_ai2%>%mutate(ecosystem_sub=fct_relevel(ecosystem_sub,
                                                    c("Western Aleutians", "Central Aleutians", "Eastern Aleutians")))

#reassign ice to no heatwave
mhw_ai2$heatwave_category<-recode(mhw_ai2$heatwave_category, "I"="0")
mhw_ai2$Intensity<-recode(mhw_ai2$heatwave_category, "0"="No heatwave", "1"="Moderate", "2"="Strong", "3"="Severe", "4"="Extreme")
mhw_ai2<-mhw_ai2%>%mutate(Intensity=fct_relevel(Intensity, c("No heatwave", "Moderate", "Strong", "Severe")))
#calculate 5 day averages
mhw_ai2_5<-mhw_ai2%>%
  group_by(ecosystem_sub, Intensity)%>%
  mutate(mean_5day = zoo::rollmean(prop_mhw, k = 5, fill=NA))%>%
  ungroup()

#function for a smoothed version
count_by_mhw_d<-function(x){
  mycolors=c("white", "#ffc866","#ff6900", "#9e0000", "#0093D0", "#2d0000", "#0093D0", "white")
  ggplot() +
    geom_histogram(data=x,
                   aes(read_date,mean_5day, fill=Intensity, color=Intensity), 
                   stat="identity") +
    facet_wrap(~ecosystem_sub,nrow=1) + 
    ylab("proportion in MHW") + 
    xlab("") +
    ylim(c(0,1))+
    scale_color_manual(values=mycolors)+
    scale_fill_manual(values=mycolors)+
    theme_bw()+
    theme( strip.text = element_text(size=10,color="white",family="sans",face="bold"),
           strip.background = element_rect(fill='#0055A4'),
           axis.title.y = element_text(size=10,family="sans"),
           axis.text.y = element_text(size=10,family="sans"),
           panel.grid.major = element_blank(), 
           panel.grid.minor = element_blank()
           #panel.border=element_rect(colour="black",, fill=NA, size=0.75)
    ) 
}

png(paste0("AI/",current.year,"/ai_mhw_by_status_5day.png"), width=9,height=4.5,units="in",res=300)
count_by_mhw_d(mhw_ai2_5)
dev.off()


#does temp ever go below seasonal in 2023?
any(with(clim_cat%>%filter(year==2023), temp-seas<0), na.rm=T)

#histogram of heatwave status
mhw_ai%>%ggplot()+
  geom_histogram(aes(x=prop_mhw))+
  facet_wrap(~ecosystem_sub)


#is the eastern region warmer
#Is this because the eastern region has had an overall higher temperature thereby "raising the bar" for a heat wave?
#plot baseline
ggplot(data=data%>%filter(year<2015)%>%group_by(esr_region, newdate)%>%
         summarise(avgsst=mean(meansst), sdsst=sd(meansst)))+
  geom_line(aes(x=newdate, y=avgsst, color=esr_region), lty=1, size=1)+
  geom_line(aes(x=newdate, y=avgsst+sdsst, color=esr_region), lty=2)+
  geom_line(aes(x=newdate, y=avgsst-sdsst, color=esr_region), lty=2)+
  xlab("date")+ylab("mean sst 1985-2014")+
  theme_bw()

#plot 2023
ggplot()+
  geom_line(data=data%>%filter(year==2023), 
            aes(x=read_date, y=meansst, color=esr_region))+
  xlab("date")+ylab("mean sst 2023")+
  theme_bw()
    