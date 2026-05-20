library(RJDBC)
library(tidyverse)
library(keyring)
library(sf)
library(akgfmaps)
library(scales)
library(cowplot)
library(heatwaveR)

jdbcDriver <- RJDBC::JDBC(driverClass="oracle.jdbc.OracleDriver",
                          classPath=system.file("driver", "ojdbc8.jar", package = "akfinupload") )
#server<-"jdbc:oracle:thin:@akfin"
server<-"jdbc:oracle:thin:@//tiger:2045/akfin.psmfc.org"

con<-dbConnect(jdbcDriver, 
               server, 
               key_list("akfin_oracle_db")$username, 
               keyring::key_get("akfin_oracle_db", keyring::key_list("akfin_oracle_db")$username))

# show catch distribution

ai_catch_depth<-dbGetQuery(con,
                                paste0("select bottom_depth_fathoms * 1.8288 depth_m, 
                                fmp_subarea,
official_total_catch
from norpac.debriefed_haul_mv
                                       where fmp_subarea = 'AI'
                                       and year = 2025"))%>%
  rename_with(tolower)

c25<-sum(ai_catch_depth$official_total_catch)

png(paste0("AI/","2026","/PEEC_fishing_depth.png"),width=6,height=3.375,units="in",res=300)
ggplot(ai_catch_depth%>%
         mutate(depth2=round(depth_m,-1))%>%
         group_by(depth2) %>%
         summarize(catch=sum(official_total_catch))%>%
         mutate(catch_prop_2025=catch/c25))+
  geom_line(aes(x=depth2, y=catch_prop_2025))+
  ylab("Proportion of observed 2025 AI catch")+
  xlab("Depth (m)")+
  theme_bw()
dev.off()
# show depth distribution

ai_pts<-dbGetQuery(con,
           paste0("select latitude, longitude, depth
            from afsc.erddap_crw_sst_spatial_lookup
            where ecosystem ='Aleutian Islands'
            ")) %>%
  rename_with(tolower)

ai_pts<-ai_pts %>%
  mutate(longitude=as.numeric(longitude),
         latitude=as.numeric(latitude),
         depth=as.numeric(depth),
    lon360=ifelse(longitude>0, longitude, longitude+360),
         depth_range=ifelse(depth<=-700, "deeper than 700m", "shallower than 700m"))

ai_base<-get_base_layers(select.region="AI", set.crs=4326)

png(paste0("AI/","2026","/PEEC_AI_depth.png"),width=6,height=3.375,units="in",res=300)
ggplot()+
  geom_point(data=ai_pts , 
             aes(x=lon360, y=latitude, color=depth_range), size=0.2)+
  geom_sf(data=ai_base$akland %>%st_shift_longitude())+
  geom_sf(data=ai_base$bathymetry %>%filter(DEPTH_M==700) %>%st_shift_longitude(), color="darkblue")+
  coord_sf(xlim=c(167, 197), ylim=c(48,57))+
  xlab("longitude")+
  theme_bw()
dev.off()
# Note that mhw per year figures were recalculated with anomaly added 

  
# create time series 
ai_ts<-dbGetQuery(con,
                   paste0("select a.read_date, 
                   avg(a.temp) meansst,
                   b.ecosystem_sub
            from afsc.erddap_crw_sst a
            inner join afsc.erddap_crw_sst_spatial_lookup b
            on a.crw_id=b.id
            where b.ecosystem ='Aleutian Islands'
            and b.depth >= -700
            group by a.read_date, b.ecosystem_sub
            ")) %>%
  rename_with(tolower)

# plot
#Load 508 compliant NOAA colors
OceansBlue1='#0093D0'
OceansBlue2='#0055A4' # rebecca dark blue
Crustacean1='#FF8300'
UrchinPurple1='#7F7FFF'
SeagrassGreen4='#D0D0D0' # This is just grey
#  Assign colors to different time series.
current.year.color <- "black"#CoralRed1 #OceansBlue1
last.year.color <- OceansBlue1#WavesTeal1
mean.color <- UrchinPurple1
#  Set default plot theme
theme_set(theme_cowplot())

#  Specify legend position coordinates (top panel)
mylegx <- 0.525
mylegy <- 0.865

#  Specify NOAA logo position coordinates (top panel)
mylogox <- 0.045
mylogoy <- 0.285

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


####-------------------------------------------------------------####
#Load Base data
AIdata<-ai_ts %>%
  mutate(esr_region=ecosystem_sub,
         date=date(read_date),
         month=month(read_date),
         day=day(read_date),
         year=year(read_date),
         newdate=as.Date(ifelse(month==12,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),#  Create a dummy year so that each year can more easily be overlain
                                as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
         year2=ifelse(month==12,year+1,year)) %>% # To have our years go from Dec-Nov, force December to be part of the subsequent year.
  arrange(read_date) 
#  Set year criteria to automatically identify the current and previous years
current.year <- max(AIdata$year2)
last.year <- current.year-1

climatology_start_year <- 1985
climatology_end_year <- 2014
mean.years <- climatology_start_year:climatology_end_year 
mean.lab <- paste0("Mean ",climatology_start_year,"-",climatology_end_year)

#change legend position
AIlegx <- 0.375
#  Create plotting function that will allow selection of 3 ESR regions
AIdata<-AIdata %>% 
  mutate(esr_region=factor(esr_region, levels=c("Western Aleutians","Central Aleutians","Eastern Aleutians"))) 
region1<-"Eastern Aleutians"
region2<-"Central Aleutians"
region3<-"Western Aleutians"
pa1 <- ggplot() +
    geom_line(data=AIdata %>% filter(year2<last.year & esr_region%in%(c(region1,region2,region3))), # Older years are grey lines.
              aes(newdate,meansst,group=factor(year2),col='mygrey'),size=0.3) +
    geom_line(data=AIdata %>% filter(year2==last.year & esr_region%in%(c(region1,region2,region3))), # The previous year
              aes(newdate,meansst,color='last.year.color'),size=0.75) +
    geom_line(data=AIdata %>% 
                filter(year%in%mean.years & esr_region%in%(c(region1,region2,region3))) %>% # The mean from 1986-2015
                group_by(esr_region,newdate) %>% 
                summarise(meantemp=mean(meansst,na.rm=TRUE)),
              aes(newdate,meantemp,col='mean.color'),size=0.65,linetype="solid") +
    geom_line(data=AIdata %>% filter(year2==current.year & esr_region%in%(c(region1,region2,region3))), # This year
              aes(newdate,meansst,color='current.year.color'),size=0.75) +
    facet_wrap(~esr_region,ncol=3) + 
    scale_color_manual(name="",
                       breaks=c('current.year.color','last.year.color','mygrey','mean.color'),
                       values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
                       labels=c(current.year,last.year,paste0('1985-',last.year-1),mean.lab)) +
    scale_linetype_manual(values=c("solid","solid","solid","dashed")) +
    scale_y_continuous(breaks=c(0,5,10))+
    ylab("Sea Surface Temperature (°C)") + 
    xlab("") +
    scale_x_date(date_breaks="1 month",
                 date_labels = "%b",
                 expand = c(0.025,0.025)) + 
  mytheme+
    theme(
      legend.position=c(AIlegx,mylegy),
          legend.text = element_text(size=20,family="sans"),
          legend.background = element_blank(),
          legend.title = element_blank(),
          strip.text = element_text(size=24,color="white",family="sans",face="bold"),
          strip.background = element_rect(fill=OceansBlue2),
          axis.title.y = element_text(size=20,family="sans"),
          axis.text.y = element_text(size=16,family="sans"),
      panel.background = element_blank(),
          panel.border=element_rect(colour="black",size=0.75),
          axis.text.x=element_blank(),
          legend.key.size = unit(0.35,"cm"),
          plot.margin=unit(c(-0.1,0.05,0,0),"cm")) 

pa1

# Use heatwaveR package to detect marine heatwaves.

pd <- list(start_date = "1985-01-01", end_date = "2014-12-31")

AImhw <-  (detect_event(ts2clm(AIdata %>% 
                         filter(ecosystem_sub=="Eastern Aleutians") %>% 
                         rename(t=date,temp=meansst) %>% 
                         arrange(t), climatologyPeriod = c(pd$start_date, pd$end_date))))$clim %>% 
    mutate(region=paste0(current.year," Eastern Aleutians Heatwaves")) %>% 
    bind_rows((detect_event(ts2clm(AIdata %>%
                                     filter(ecosystem_sub=="Central Aleutians") %>% 
                                     rename(t=date,temp=meansst) %>% 
                                     arrange(t), climatologyPeriod = c(pd$start_date, pd$end_date))))$clim %>% 
                mutate(region=paste0(current.year," Central Aleutians Heatwaves"))) %>%
    bind_rows((detect_event(ts2clm(AIdata %>%
                                     filter(ecosystem_sub=="Western Aleutians") %>% 
                                     rename(t=date,temp=meansst) %>% 
                                     arrange(t), climatologyPeriod = c(pd$start_date, pd$end_date))))$clim %>% 
                mutate(region=paste0(current.year," Western Aleutians Heatwaves")))

end_d <- as_date(paste0(current.year, "-11-30"))
start_d <- as_date(max(AImhw$t)) + 1
yearvec <- if (start_d > end_d) {
    as.Date(character(0))
  } else {
    seq.Date(start_d, end_d, "day")}

current.year.length<-ifelse(leap_year(current.year)==FALSE, 365,366)

#  Replace the current year with the previous year for our remaining days vector.
AIdummydat <-   data.frame(t=yearvec-current.year.length,newt=yearvec) %>%
    inner_join(AImhw %>% dplyr::select(region,thresh,seas,t)) %>% 
    dplyr::select(t=newt,region,thresh,seas) %>% 
    mutate(temp=NA)

# Calculate threshold values for heatwave categories. This code directly from Schegel & Smit
AIclim_cat <-  AImhw %>%
    bind_rows(AIdummydat) %>% 
    group_by(region) %>% 
    dplyr::mutate(diff = thresh - seas,
                  thresh_2x = thresh + diff,
                  thresh_3x = thresh_2x + diff,
                  thresh_4x = thresh_3x + diff,
                  year=year(t),
                  region2=factor(region, levels=c(paste0(current.year," Western Aleutians Heatwaves"),
                                                  paste0(current.year," Central Aleutians Heatwaves"),
                                                  paste0(current.year," Eastern Aleutians Heatwaves")))) %>% 
    arrange(t)


#  Create annotation text for plot
cc<-AIclim_cat
AImhw_lab <- data.frame(region2=factor(c(paste0(current.year," Western Aleutians Heatwaves"),paste0(current.year," Central Aleutians Heatwaves"),paste0(current.year," Eastern Aleutians Heatwaves"))),
             t=c(as_date(paste0(last.year,"-12-05")),as_date(paste0(last.year,"-12-05")),as_date(paste0(last.year,"-12-05"))),
             temp=rev(c((1*max(cc$temp,na.rm=TRUE)),(0.9*max(cc$temp,na.rm=TRUE)),(0.8*max(cc$temp,na.rm=TRUE)))),
             mylab=rev(c("Heatwave intensity increases\n(successive dotted lines)\nas waters warm.",
                         "Heatwaves occur when daily\nSST exceeds the 90th\npercentile of normal\n(lowest dotted line) for\n5 consecutive days.","")))
#  Create custom categories for lines
lineColCat <- c(
  "Temperature" = "black",
  "Baseline" = mean.color,
  "Moderate (1x Threshold)" = "gray60",
  "Strong (2x Threshold)" = "gray60",
  "Severe (3x Threshold)" = "gray60",
  "Extreme (4x Threshold)" = "gray60"
)

#  Create flame fill parameters
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

mytheme <- theme(strip.text = element_text(size=24,color="white",family="sans",face="bold"),
                 strip.background = element_rect(fill=OceansBlue2),
                 axis.title = element_text(size=20,family="sans",color="black"),
                 axis.text = element_text(size=16,family="sans",color="black"),
                 panel.border=element_rect(colour="black",fill=NA,size=0.5),
                 panel.background = element_blank(),
                 plot.margin=unit(c(0.65,0,0.65,0),"cm"),
                 legend.position=c(0.6,0.7),
                 legend.background = element_blank(),
                 legend.key.size = unit(1,"line"))

pa2 <- ggplot(data = AIclim_cat %>% filter(t>=as.Date(paste0(last.year,"-12-01"))), aes(x = t, y = temp)) +
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
                  geom_text(data=AImhw_lab,aes(x=t,y=temp,label=mylab),hjust=0,size=8,family="sans",lineheight=1) +
                  scale_colour_manual(name = NULL, values = lineColCat,
                                      breaks = c("Temperature", "Baseline", "Moderate (1x Threshold)"),guide=FALSE) +
                  scale_fill_manual(name = "Heatwave\nIntensity", values = c(Extreme,Severe,Strong,Moderate),labels=c("Extreme","Severe","Strong","Moderate")#, guide = FALSE
                  ) +
                  scale_x_date(limits=c(as_date(paste0(last.year,"-12-01")),as_date(paste0(current.year,"-11-30"))),date_breaks="1 month",date_labels = "%b",expand=c(0.01,0)) +
                  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
                  labs(y = "Sea Surface Temperature (°C)", x = NULL) + 
                  facet_wrap(~region2,ncol=3) +
                  mytheme + 
                  theme(strip.text=element_text(size=20),
                        legend.position=c(0.75,0.5),
                        legend.title = element_text(size=20),
                        legend.key.size = unit(0.75,"line"),
                        legend.text = element_text(size=16),
                        axis.title.x=element_blank(),
                        axis.text.x=element_text(color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
                        plot.margin=unit(c(-0.7,0.05,3,0),"cm"))
pa2b <- ggdraw(pa2 + 
                   annotate("text",x=0.5,y=0.065,label=paste0("NOAA Coral Reef Watch data, courtesy NOAA Pacific Islands Ocean Observing System (Updated: ",
                                                              format(max(AIdata$date),"%m-%d-%Y"),
                                                              ")\n Data are modeled satellite products and periodic discrepancies or gaps may exist across sensors and products.\n                                    Contact: matt.callahan@noaa.gov, Alaska Fisheries Science Center "),
                            
                            hjust=0.5, size=7,family="sans",fontface=1,color=OceansBlue2,lineheight=0.85) )
pa3<-plot_grid(pa1,pa2,ncol=1)
pa3

png(paste0("AI/","2026","/PEEC_700m_app.png"), height=14.25, width=18, units="in", res=300)
pa3
dev.off()

# MHW days
data<-ai_ts %>%
  mutate(date=as_date(read_date)) %>% 
  data.frame %>% 
  dplyr::select(date,meansst,esr_region=ecosystem_sub) %>% 
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

current.year <- max(data$year)
last.year <- current.year-1
climatology_start_year <- 1985
climatology_start_date <- "1985-01-01"
climatology_end_year <- 2014
climatology_end_date <- "2014-12-31"
mean.years <- climatology_start_year:climatology_end_year 
mean.lab <- paste0("Mean ",climatology_start_year,"-",climatology_end_year)


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

# add annual departure from baseline
mhw <- (detect_event(ts2clm((data) %>%
                              filter(esr_region=="Western Aleutians") %>%
                              rename(t=read_date,temp=meansst) %>%
                              arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>% # uses 1986-2015 year2 baseline
  mutate(region="Western Aleutians") %>%
  bind_rows((detect_event(ts2clm((data) %>%
                                   filter(esr_region=="Eastern Aleutians") %>%
                                   rename(t=read_date,temp=meansst) %>%
                                   arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>%
              mutate(region="Eastern Aleutians")) %>%
  bind_rows((detect_event(ts2clm((data) %>%
                                   filter(esr_region=="Central Aleutians") %>%
                                   rename(t=read_date,temp=meansst) %>%
                                   arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>%
              mutate(region="Central Aleutians"))
# 
annual_deviation<-mhw %>%
  mutate(dev=temp-seas,
         year=year(t),
         month=month(t),
         year2=ifelse(month>=12,year+1,year)) %>%
  group_by(region,year2) %>%
  summarize(mean_dev=round(mean(dev),1)) %>%
  mutate(region=factor(region,c("Western Aleutians","Central Aleutians","Eastern Aleutians")))
# Modified Figure 3 with annual deviations from the baseline
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
png(paste0("AI/",current.year,"/PEEC_700m_mhw_days.png"),width=6,height=3.375,units="in",res=300)

annualevents %>% 
  gather(Period, Duration, -c(year2, region)) %>% 
  data.frame() %>% 
  mutate(Period=factor(Period, c("Fall","Summer","Spring","Winter")),
         region=factor(region,c("Western Aleutians","Central Aleutians","Eastern Aleutians"))) %>% 
  ggplot() +
  geom_bar(
    aes(year2, Duration, fill = Period),
    stat = "identity"
  ) +
  
  # Small guide lines to make clear which label goes with which year
  geom_segment(
    data = annual_deviation,
    aes(x = year2, xend = year2, y = -8, yend = 0),
    inherit.aes = FALSE,
    linewidth = 0.25
  ) +
  
  
  geom_text(
    data = annual_deviation,
    aes(x = year2, y = -18, label = mean_dev),
    inherit.aes = FALSE,
    size = 1.2,
    angle = 90,
    hjust = 0.5,
    vjust = 0.5
  ) +
  
  scale_fill_manual(
    name = "",
    labels = c("Summer", "Fall", "Winter", "Spring"),
    values = c(OceansBlue2, Crustacean1, UrchinPurple1, WavesTeal1)
  ) +
  mytheme + 
  facet_wrap(~region) + 
  
  scale_x_continuous(
    expand = c(0, 0.5)
  ) +
  
  # Negative lower limit creates space for labels under the bars
  scale_y_continuous(
    limits = c(-35, 370),
    expand = c(0, 0)
  ) +
  
  coord_cartesian(clip = "off") +
  
  xlab("Year") + 
  ylab("Number of Marine Heatwave Days") +
  
  theme(
    plot.margin = unit(c(0.15, 0.25, 0.35, 0), "cm"),
    legend.position = c(0.1, 0.85)
  )

dev.off()

annual_deviation %>%
  filter(year2 >=2015) %>%
  group_by(region) %>%
  summarize(post_baseline_deviation=mean(mean_dev))
