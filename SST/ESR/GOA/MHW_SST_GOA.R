#2024 figures for GOA ESR
# originally written in 2021
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

mytheme <- theme(strip.text = element_text(size=10,color="white",family="sans",face="bold"),
                 strip.background = element_rect(fill=OceansBlue2),
                 axis.title = element_text(size=10,family="sans",color="black"),
                 axis.text = element_text(size=10,family="sans",color="black"),
                 panel.border=element_rect(colour="black",fill=NA,size=0.5),
                 panel.background = element_blank(),
                 plot.margin=unit(c(0.65,0,0.65,0),"cm"),
                 legend.position=c(0.6,0.7),
                 legend.background = element_blank(),
                 legend.key.size = unit(1,"line"))

#  Make a function to combine calls to ts2clm() and detect_event()  
# mhwfun <- function(region){
#   detect_event(ts2clm(data %>% filter(Ecosystem_sub==region), climatologyPeriod = c("1986-01-01", "2015-12-31")))
# }

newdat <- httr::content(httr::GET('https://apex.psmfc.org/akfin/data_marts/akmp/ecosystem_sub_crw_avg_sst?ecosystem_sub=Eastern%20Gulf%20of%20Alaska,Western%20Gulf%20of%20Alaska&start_date=19850101&end_date=20251231'), type = "application/json") %>% 
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
         Ecosystem_sub=factor(Ecosystem_sub,"Western Gulf of Alaska")) %>% 
  arrange(date) 

#--------------------------------------------------------------------------------------------------------------------------
#  Figure 1. Seasonal sea surface temperatures (SST) for Gulf of Alaska ecosystem regions. 
#  Lines illustrate the 2020 SST (orange), 2019 SST (blue), 30-year mean SST (black), 
#  and each of the 1985-2018 SST (gray) time series
#--------------------------------------------------------------------------------------------------------------------------

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
climatology_start_year <- 1985
climatology_start_date <- "1985-01-01"
climatology_end_year <- 2014
climatology_end_date <- "2014-12-31"
mean.years <- climatology_start_year:climatology_end_year 
mean.lab <- paste0("Mean ",climatology_start_year,"-",climatology_end_year)


png(paste0("GOA/",current.year,"/Callahan_Fig1_test.png"),width=7,height=5,units="in",res=300)
ggplot() +
  geom_line(data=newdat %>% filter(year2<last.year),
            aes(newdate,meansst,group=factor(year2),col='mygrey'),size=0.3) +
  geom_line(data=newdat %>% filter(year2==last.year),
            aes(newdate,
                meansst,color='last.year.color'),size=0.75) +
  geom_line(data=newdat %>% 
              filter(year%in%mean.years) %>% 
              group_by(Ecosystem_sub ,newdate) %>% 
              summarise(meantemp=mean(meansst,na.rm=TRUE)),
            aes(newdate,meantemp,col='mean.color'),size=0.5) +
  geom_line(data=newdat %>% filter(year2==current.year),
            aes(newdate,meansst,color='current.year.color'),size=0.95) +
  facet_wrap(~Ecosystem_sub ,ncol=2) + 
  scale_color_manual(name="",
                     breaks=c('current.year.color','last.year.color','mygrey','mean.color'),
                     values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
                     labels=c(current.year,last.year,paste0('1985-',last.year-1),mean.lab)) +
  ylab("Mean Sea Surface Temperature (C)") + 
  xlab("") +
  scale_x_date(date_breaks="1 month",
               date_labels = "%b",
               expand = c(0.025,0.025)) + 
  theme(legend.position=c(mylegx,mylegy),
        legend.text = element_text(size=8,family="sans"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(size=10,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title = element_text(size=10,family="sans"),
        axis.text = element_text(size=10,family="sans"),
        #panel.border=element_rect(colour="black",size=0.75),
        axis.text.x=element_text(color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
        legend.key.size = unit(0.35,"cm"),
        plot.margin=unit(c(0.65,0,0.65,0),"cm")) 
dev.off()


#--------------------------------------------------------------------------------------------------------------------------
#  Figure 2. Marine heatwave (MHW) status during the last three years. Filled (yellow) areas depict MHW events. 
#  Black lines represent the 30-year baseline (smoothed line) and observed daily sea surface temperatures (jagged line). 
#  Faint grey dotted lines illustrate the MHW severity thresholds in increasing order (moderate, strong). 
#--------------------------------------------------------------------------------------------------------------------------

#  Create figure that shows MHW status
mhw <- (detect_event(ts2clm(newdat %>%
                              filter(Ecosystem_sub=="Western Gulf of Alaska") %>% 
                              rename(t=date,temp=meansst) %>% 
                              arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>% 
  mutate(region="Western Gulf of Alaska") %>% 
  bind_rows((detect_event(ts2clm(newdat %>%
                                   filter(Ecosystem_sub=="Eastern Gulf of Alaska") %>% 
                                   rename(t=date,temp=meansst) %>% 
                                   arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>% 
              mutate(region="Eastern Gulf of Alaska"))


clim_cat <- mhw %>%
  mutate(region=factor(region,"Western Gulf of Alaska")) %>% 
  group_by(region) %>% 
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff,
                year=year(t))

# Set line colours
lineColCat <- c(
  "Temperature" = "black",
  "Climatology" = "gray20",
  "Moderate" = "gray60",
  "Strong" = "gray60",
  "Severe" = "gray60",
  "Extreme" = "gray60"
)

fillColCat <- c(
  "Moderate" = "#ffc866",
  "Strong" = "#ff6900",
  "Severe" = "#9e0000",
  "Extreme" = "#2d0000"
)

#  Modified flame fill parameters
Moderate = "#ffc866"
Strong = "#ff6900"
Severe = "#9e0000"
Extreme = "#2d0000"
No_heatwave = "white"

mytheme <- theme(strip.text = element_text(size=10,color="white",family="sans",face="bold"),
                 strip.background = element_rect(fill=OceansBlue2),
                 axis.title = element_text(size=10,family="sans",color="black"),
                 axis.text = element_text(size=10,family="sans",color="black"),
                 panel.border=element_rect(colour="black",fill=NA,size=0.5),
                 panel.background = element_blank(),
                # plot.margin=unit(c(0.65,0,0.65,0),"cm"),
                 #legend.position=c(0.375,0.7),
                 #legend.background = element_blank(),
                 #legend.key.size = unit(1,"line")
                 )

png(paste0("GOA/",current.year,"/Callahan_Figure_2_Flames_GOA.png"),width=7,height=5,units="in",res=300)
ggplot(data = clim_cat %>% filter(t>=as.Date(paste0(current.year-3,"-09-01"))), aes(x = t, y = temp)) +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.85) +
  geom_flame(aes(y2 = thresh, fill = Moderate)) +
  geom_flame(aes(y2 = thresh_2x, fill = Strong)) +
  geom_flame(aes(y2 = thresh_3x, fill = Severe)) +
  geom_flame(aes(y2 = thresh_4x, fill = Extreme)) +
  geom_line(aes(y = thresh_2x, col = "Strong (2x Threshold)"), size = 0.5, linetype = "dotted") +
  geom_line(aes(y = thresh_3x, col = "Severe (3x Threshold)"), size = 0.5, linetype = "dotted") +
  geom_line(aes(y = thresh_4x, col = "Extreme (4x Threshold)"), size = 0.5, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Baseline"), size = 0.65,linetype="solid") +
  geom_line(aes(y = thresh, col = "Moderate (1x Threshold)"), size = 0.5,linetype= "dotted") +
  # geom_line(aes(y = thresh_2x, col = "Strong"), size = 0.5, linetype = "dotted") +
  # geom_line(aes(y = thresh_3x, col = "Severe"), size = 0.5, linetype = "dotted") +
  # geom_line(aes(y = thresh_4x, col = "Extreme"), size = 0.5, linetype = "dotted") +
  # geom_line(aes(y = seas, col = "Climatology"), size = 0.5) +
  # geom_line(aes(y = thresh, col = "Moderate"), size = 0.5,linetype= "dotted") +
  #geom_line(aes(y = temp, col = "Temperature"), size = 0.5) +
  scale_colour_manual(name = NULL, values = lineColCat,
                      breaks = c("Temperature", "Climatology", "Moderate",
                                 "Strong", "Severe", "Extreme")) +
  scale_colour_manual(name = NULL, values = lineColCat,
                      breaks = c("Temperature", "Baseline", "Moderate (1x Threshold)"),guide=FALSE) +
  scale_fill_manual(name = "Heatwave\nIntensity", values = c(Extreme,Severe,Strong,Moderate),labels=c("Extreme","Severe","Strong","Moderate")#, guide = FALSE
  ) +
  #scale_fill_manual(name = NULL, values = fillColCat, guide = FALSE) +
  # scale_fill_manual(name = "Heatwave\nIntensity", values = fillColCat, labels=c("Moderate","Strong","Severe","Extreme")) +
  # scale_x_date(date_labels = "%b %Y",expand=c(0.01,0)) +
#  scale_fill_manual(name = "Heatwave\nIntensity", values = c(Extreme,Severe,Strong,Moderate),labels=c("Extreme","Severe","Strong","Moderate")#, guide = FALSE
#  ) +
#  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "dotted",
#                                                                "dotted", "dotted", "dotted"),
#                                                   size = c(0.6, 0.7, 0.7, 0.7, 0.7, 0.7)),
#                               ncol=6)) +
  guides(color="none")+
  labs(y = "Sea Surface Temperature (°C)", x = NULL) + 
 # theme(legend.position="none") +
  facet_wrap(~region,ncol=1,scales="free_y") +
  mytheme + 
  theme(#legend.position="top",
    #legend.key=element_blank(),
    #legend.text = element_text(size=10),
    axis.title.x=element_blank(),
    #legend.margin=margin(l=-6,t = -8.5, unit='cm'),
    #plot.margin=unit(c(0.65,0,0.0,0),"cm"),
    legend.position=c(0.1,0.85),
    legend.background = element_blank(),
    legend.key.size = unit(1,"line"))
dev.off()
#--------------------------------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------------------------------
# Figure 3. Number of days during which marine heatwave conditions persisted in a given year. 
# Seasons are summer (Jun - Aug), fall (Sept – Nov), winter (Dec – Feb), spring (Mar – Jun). 
# Years are shifted to include complete seasons so December of a calendar year is grouped with the 
# following year to aggregate winter data (e.g., Dec 2020 occurs with winter of 2021).
#--------------------------------------------------------------------------------------------------------------------------

mhw_wgoa <- (detect_event(ts2clm(newdat %>%
                                  filter(Ecosystem_sub=="Western Gulf of Alaska") %>% 
                                  rename(t=date,temp=meansst) %>% 
                                  arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$event %>% 
  mutate(region="Western Gulf of Alaska") %>% 
  data.frame

mhw_egoa <- ((detect_event(ts2clm(newdat %>%
                                   filter(Ecosystem_sub=="Eastern Gulf of Alaska") %>% 
                                   rename(t=date,temp=meansst) %>% 
                                   arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$event %>% 
              mutate(region="Eastern Gulf of Alaska")) %>% 
  data.frame

annualevents <- lapply(1:nrow(mhw_wgoa),function(x)data.frame(date=seq.Date(as.Date(mhw_wgoa[x,"date_start"]),as.Date(mhw_wgoa[x,"date_end"]),"days"))) %>% 
  bind_rows() %>% 
  mutate(region="Western Gulf of Alaska") %>% 
  bind_rows(lapply(1:nrow(mhw_egoa),function(x)data.frame(date=seq.Date(as.Date(mhw_egoa[x,"date_start"]),as.Date(mhw_egoa[x,"date_end"]),"days"))) %>% 
              bind_rows() %>% 
              mutate(region="Eastern Gulf of Alaska")) %>% 
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

png(paste0("GOA/",current.year,"/Callahan_Figure3_MHW_days_season.png"),width=6,height=3.375,units="in",res=300)
annualevents %>% 
  gather(Period,Duration,-c(year2,region)) %>% 
  data.frame %>% 
  mutate(Period=factor(Period,"Summer","Fall","Winter","Spring")#,
         #region=fct_rev(region)
         ) %>% 
  #filter(Period!="totaldays") %>% 
  ggplot() +
  geom_bar(aes(year2,Duration,fill=Period),stat="identity") + 
  scale_fill_manual(name="",labels=c("Summer","Fall","Winter","Spring"),values=c(OceansBlue2,Crustacean1,UrchinPurple1,WavesTeal1)) +
  #geom_bar(aes(year2,totaldays),stat="identity",fill=OceansBlue2) + 
  #geom_bar(aes(year2,winterdays),stat="identity",fill=Crustacean1) + 
  mytheme + 
  facet_wrap(~region) + 
  scale_x_continuous(expand=c(0,0.5)) +
  scale_y_continuous(limits=c(0,370),expand=c(0.0,0)) +
  xlab("Year") + 
  ylab("Number of Marine Heatwave Days") +
  theme(plot.margin=unit(c(0.15,0.25,0.05,0),"cm"),
        legend.position = c(0.1,0.85))
dev.off()
#--------------------------------------------------------------------------------------------------------------------------

#Figure of proportion mhw
#connect to AKFIN
con <- dbConnect(odbc::odbc(), "akfin", UID=getPass(msg="USER NAME"), PWD=getPass())

#query by heatwave category
mhw_goa<- dbFetch(dbSendQuery(con,
                              paste0("select 
heatwave_category,
count(heatwave_category) mhw_count,
a.read_date, 
b.ecosystem_sub 
from (select
crw_id, read_date, heatwave_category
from afsc.erddap_crw_sst
where extract(year from read_date)=2024) a
inner join (select id, ecosystem_sub 
from afsc.erddap_crw_sst_spatial_lookup
where ecosystem ='Gulf of Alaska'
and depth<-10
and depth>-200) b
on a.crw_id=b.id
group by a.read_date, b.ecosystem_sub, heatwave_category
order by a.read_date, b.ecosystem_sub, heatwave_category")))%>%
  rename_with(tolower)

#load counts
goa_totals<-dbFetch(dbSendQuery(con,
                    paste0("select 
count(*) total_count,
ecosystem_sub
from afsc.erddap_crw_sst_spatial_lookup
where ecosystem ='Gulf of Alaska'
and depth<-10
and depth>-200
group by ecosystem_sub")))%>%
  rename_with(tolower)

#Join in total counts... I could do this in sql but it's easier for me in R 
#also calculate proportion and reorder ecosystems
mhw_goa2<-mhw_goa%>%left_join(goa_totals, by="ecosystem_sub")%>%
  mutate(prop_mhw=mhw_count/total_count,
         ecosystem_sub=factor(ecosystem_sub,
                                   c("Western Gulf of Alaska", "Eastern Gulf of Alaska")))
#reassign ice to no heatwave
mhw_goa2$heatwave_category<-recode(mhw_goa2$heatwave_category, "I"="0")
#save
saveRDS(mhw_goa2, "GOA/Data/prop_mhw_goa2024.RDS")
mhw_goa2<-readRDS("GOA/Data/prop_mhw_goa2024.RDS")


#calculate 5 day averages
mhw_goa2_5<-mhw_goa2%>%
  group_by(ecosystem_sub, heatwave_category)%>%
  mutate(mean_5day = zoo::rollmean(prop_mhw, k = 5, fill=NA))%>%
  ungroup()

#function for a smoothed version
count_by_mhw_d<-function(x){
  mycolors=c("white", "#ffc866","#ff6900", "#9e0000", "#0093D0", "#2d0000", "#0093D0", "white")
  ggplot() +
    geom_histogram(data=x%>%mutate(category=heatwave_category),
                   aes(read_date,mean_5day, fill=category, color=category), 
                   stat="identity") +
    facet_wrap(~ecosystem_sub,nrow=1) + 
    ylab("proportion in MHW") + 
    xlab("") +
    ylim(c(0,1))+
    #scale_color_manual(values=mycolors)+
    #scale_fill_manual(values=mycolors)+
     scale_color_manual(values=mycolors, guide = "none")+
    scale_fill_manual(name = "Heatwave\nIntensity",
                      values = c(mycolors),
                      labels=c("No heatwave", "Moderate","Strong"),
                      limits=c("#ffc866","#ff6900", "#9e0000", "#0093D0", "#2d0000")
                      #, guide = FALSE
    ) +
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

png(paste0("GOA/",current.year,"/goa_mhw_by_status_5day.png"), width=9,height=4.5,units="in",res=300)
count_by_mhw_d(mhw_goa2_5)
dev.off()


#what is the highest proportion of mhw?
min((mhw_goa2_5%>%filter(heatwave_category=='0'))$mean_5day, na.rm=T)

min((mhw_goa2_5%>%filter(heatwave_category=='0'))$prop_mhw, na.rm=T)

min((mhw_goa2_5%>%filter(heatwave_category!='0' & ecosystem_sub=="Eastern Gulf of Alaska"))$read_date, na.rm=T)

#histogram of heatwave status
mhw_goa2%>%ggplot()+
  geom_histogram(aes(x=prop_mhw))+
  facet_wrap(~ecosystem_sub)
    

#relabel
   # reassign ice to no heatwave
    mhw_goa2$heatwave_category<-recode(mhw_goa2$heatwave_category, "I"="0")
    mhw_goa2$Intensity<-recode(mhw_goa2$heatwave_category, "0"="No heatwave", "1"="Moderate", "2"="Strong", "3"="Severe", "4"="Extreme")
    mhw_goa2<-mhw_goa2%>%mutate(Intensity=factor(Intensity, c("No heatwave", "Moderate", "Strong"#, "Severe"
                                                                   )))
    #calculate 5 day averages
    mhw_goa2_5<-mhw_goa2%>%
      group_by(ecosystem_sub, Intensity)%>%
      mutate(mean_5day = zoo::rollmean(prop_mhw, k = 5, fill=NA))%>%
      ungroup()
    
    #function for a smoothed version
    count_by_mhw_d<-function(x){
      mycolors=c("white", "#ffc866","#ff6900", "#9e0000", "black", "#0093D0", "#2d0000", "#0093D0", "white")
      ggplot() +
        geom_histogram(data=x,
                       aes(read_date,prop_mhw, fill=Intensity, color=Intensity), 
                       #aes(read_date,mean_5day, fill=Intensity, color=Intensity), 
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
    
    png("GOA/2023/goa_mhw_by_status_5day.png", width=9,height=4.5,units="in",res=300)
    count_by_mhw_d(mhw_goa2_5)
    dev.off()
    
    png("GOA/2023/goa_mhw_by_status_1day.png", width=9,height=4.5,units="in",res=300)
    count_by_mhw_d(mhw_goa2_5)
    dev.off()
    
    #need to updarte legend
    scale_fill_manual(name = "Heatwave\nIntensity", values = c(Extreme,Severe,Strong,Moderate),labels=c("Extreme","Severe","Strong","Moderate")#, guide = FALSE
    ) +

