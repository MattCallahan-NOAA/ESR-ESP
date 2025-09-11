#### EBS contribution code####
# Originally run in 2022
# modified in 2023

#  Create the 5 SST figures for the Bering Sea ESR.


library(tidyverse)
library(heatwaveR)
library(lubridate)
library(viridis)
library(cowplot)
library(ggplottimeseries)

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
                 axis.text.y = element_text(size=10,family="sans",color="black"),
                 axis.text.x = element_text(size=9,family="sans",color="black",hjust=0.75),
                 panel.border=element_rect(colour="black",fill=NA,size=0.5),
                 panel.background = element_blank(),
                 plot.margin=unit(c(0.65,0,0.65,0),"cm"),
                 legend.position=c(0.6,0.7),
                 legend.background = element_blank(),
                 legend.key.size = unit(1,"line"))

#specify ESR year
current.year <- 2025
last.year <- current.year-1
climatology_start_year <- 1985
climatology_start_date <- "1985-01-01"
climatology_end_year <- 2014
climatology_end_date <- "2014-12-31"
mean.years <- climatology_start_year:climatology_end_year 
mean.lab <- paste0("Mean ",climatology_start_year,"-",climatology_end_year)

newdat <- httr::content(httr::GET('https://apex.psmfc.org/akfin/data_marts/akmp/ecosystem_sub_crw_avg_sst?ecosystem_sub=Southeastern%20Bering%20Sea,Northern%20Bering%20Sea&start_date=19850101&end_date=20250901'), type = "application/json") %>% 
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

#  Figure 1; Anomaly of cumulative SST for years that go from Sept - Aug
mymean <- newdat %>% 
  filter(!year2%in%c(1985)) %>% 
  group_by(year2,Ecosystem_sub) %>% 
  arrange(newdate) %>% 
  summarise(cumheat=sum(meansst)) %>% 
  group_by(Ecosystem_sub) %>% 
  mutate(meanheat=mean(cumheat[between(year2,1986,climatology_end_year)]), # Mannually change
         sdheat=sd(cumheat[between(year2,1986,climatology_end_year)]), # Mannually change
         anomaly=cumheat-meanheat)

png("EBS/2025/Callahan_Fig1.png",width=6,height=3.375,units="in",res=300)
mymean %>% 
  ggplot(aes(year2,anomaly)) +
  geom_bar(stat="identity",fill=OceansBlue2) + 
  geom_hline(aes(yintercept=0),linetype=2) +
  geom_hline(aes(yintercept=sdheat),linetype=2) +
  geom_hline(aes(yintercept=-sdheat),linetype=2) +
  facet_wrap(~Ecosystem_sub) + 
  mytheme + 
  scale_x_continuous(expand=c(0.01,0.75), breaks= c(1990, 2000, 2010, 2020)) + 
  xlab("") + 
  ylab("Cumulative Annual SST Anomaly (°C)") +
  theme(plot.margin=unit(c(0.15,0.25,0.05,0),"cm"))
dev.off()

# writecsv for Johanna
#write.csv(mymean, "EBS/2025/ebs_sst_anomaly.csv", row.names=FALSE)

#  Create Figure 2. Total cumulative sea surface temperature (sum of daily temperatures) for each year, apportioned
#  by season: summer (Jun–Aug), fall (Sept–Nov), winter (Dec–Feb), spring (Mar–May). Negative
#  values are the result of sea surface temperatures below zero
# png("EBS/2024/Callahan_Fig2.png",width=6,height=4,units="in",res=300)
# newdat %>% 
#  # filter(year2>1985) %>% 
#   mutate(Season=case_when(
#     month%in%c(9,10,11)~"Fall",
#     month%in%c(12,1,2)~"Winter",
#     month%in%c(3,4,5)~"Spring",
#     month%in%c(6,7,8)~"Summer")) %>% 
#   data.frame %>% 
#   mutate(Season=factor(Season),
#          Season=fct_relevel(Season,"Fall","Winter","Spring","Summer")) %>% 
#   group_by(year2,Ecosystem_sub,Season) %>% 
#   summarise(cumheat=sum(meansst)) %>% 
#   data.frame %>% 
#   mutate(Season=fct_relevel(Season,"Summer","Fall","Winter","Spring")) %>% 
#   ggplot(aes(year2,cumheat,fill=Season)) + 
#   geom_bar(stat="identity") +
#   #geom_hline(data=mymean,aes(yintercept=meanheat),linetype=2) +
#   scale_fill_manual(name="",labels=c("Summer","Fall","Winter","Spring"),values=c(OceansBlue2,Crustacean1,UrchinPurple1,WavesTeal1)) +
#   facet_wrap(~Ecosystem_sub) + 
#   mytheme + 
#   scale_x_continuous(expand=c(0.01,0.75)) + 
#   xlab("") + 
#   ylab("Total Annual Cumulative Sea Surface Temperature (°C)") +
#   theme(plot.margin=unit(c(0.15,0.25,0.05,0),"cm"),
#         legend.position=c(0.1,0.9))
# dev.off()

#line for tyler
png("EBS/2025/Callahan_Fig2_line.png",width=6,height=4,units="in",res=300)
newdat %>% 
  filter(year2>1985) %>% 
  mutate(Season=case_when(
    month%in%c(9,10,11)~"Fall",
    month%in%c(12,1,2)~"Winter",
    month%in%c(3,4,5)~"Spring",
    month%in%c(6,7,8)~"Summer")) %>% 
  data.frame %>% 
  mutate(Season=factor(Season, c("Fall","Winter","Spring","Summer"))) %>% 
  group_by(year2,Ecosystem_sub,Season) %>% 
  summarise(meansst=mean(meansst)) %>% 
  data.frame %>% 
  mutate(Season=factor(Season, c("Summer","Fall","Winter","Spring"))) %>% 
  ggplot(aes(year2,meansst,color=Season)) + 
  geom_line(stat="identity") +
  geom_point(stat="identity")+
  #geom_hline(data=mymean,aes(yintercept=meanheat),linetype=2) +
  scale_color_manual(name="",labels=c("Summer","Fall","Winter","Spring"),values=c(OceansBlue2,Crustacean1,CoralRed1,WavesTeal1)) +
  facet_wrap(~Ecosystem_sub) + 
  mytheme + 
  scale_x_continuous(expand=c(0.01,0.75),breaks= c(1990, 2000, 2010, 2020)) + 
  xlab("") + 
  ylab("Seasonal mean Sea Surface Temperature (°C)") +
  theme(plot.margin=unit(c(0.15,0.25,0.05,0),"cm"),
        legend.position=c(0.07,0.86),
        legend.title = element_blank())
dev.off()

#### 2023 question Is fig 2 within 1 sd of seasonal mean? ####
 seasmean<-newdat %>% 
  filter(year2>1985) %>% 
  mutate(Season=case_when(
    month%in%c(9,10,11)~"Fall",
    month%in%c(12,1,2)~"Winter",
    month%in%c(3,4,5)~"Spring",
    month%in%c(6,7,8)~"Summer")) %>% 
  data.frame %>% 
  mutate(Season=factor(Season),
         Season=factor(Season,c("Fall","Winter","Spring","Summer"))) %>% 
  group_by(year2,Ecosystem_sub,Season) %>% 
  summarise(meansst=mean(meansst)) %>% 
  data.frame %>% 
  mutate(Season=factor(Season,c("Summer","Fall","Winter","Spring")))

seas_mean_sd<-seasmean%>%
  group_by(Ecosystem_sub, Season) %>%
  summarize(avg_sst=mean(meansst),
            sd_sst=sd(meansst))

png("EBS/2025/Callahan_Fig2_line_sd.png",width=6,height=4,units="in",res=300)
ggplot(data=seasmean, aes(year2,meansst,color=Season)) + 
  geom_hline(data=seas_mean_sd,aes(yintercept=avg_sst, color=Season)) +
  geom_hline(data=seas_mean_sd,aes(yintercept=avg_sst-sd_sst, color=Season),linetype=2) +
  geom_hline(data=seas_mean_sd,aes(yintercept=avg_sst+sd_sst, color=Season),linetype=2) +
  geom_line(stat="identity") +
  geom_point(stat="identity")+
  scale_color_manual(name="",labels=c("Summer","Fall","Winter","Spring"),values=c(OceansBlue2,Crustacean1,CoralRed1,WavesTeal1)) +
  facet_wrap(~Ecosystem_sub) + 
  mytheme + 
  #scale_x_continuous(expand=c(0.01,0.75)) + 
  xlab("") + 
  ylab("Seasonal mean Sea Surface Temperature (°C)") +
  theme(plot.margin=unit(c(0.15,0.25,0.05,0),"cm"),
        legend.position=c(0.07,0.86),
        legend.title = element_blank())
dev.off()


#  Figure 3. Marine heatwaves in the southeastern and northern Bering Sea since September 2018
mhw <- (detect_event(ts2clm(newdat %>%
                              filter(Ecosystem_sub=="Southeastern Bering Sea") %>% 
                              rename(t=date,temp=meansst) %>% 
                              arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>% 
  mutate(region="Southeastern Bering Sea") %>% 
  bind_rows((detect_event(ts2clm(newdat %>%
                                   filter(Ecosystem_sub=="Northern Bering Sea") %>% 
                                   rename(t=date,temp=meansst) %>% 
                                   arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim %>% 
              mutate(region="Northern Bering Sea"))


clim_cat <- mhw %>%
  #mutate(region=fct_relevel(region,"Western Gulf of Alaska")) %>% 
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

Moderate = "#ffc866"
Strong = "#ff6900"
Severe = "#9e0000"
Extreme = "#2d0000"

mytheme2 <- theme(strip.text = element_text(size=10,color="white",family="sans",face="bold"),
                 strip.background = element_rect(fill=OceansBlue2),
                 axis.title = element_text(size=10,family="sans",color="black"),
                 axis.text.y = element_text(size=10,family="sans",color="black"),
                 axis.text.x = element_text(size=9,family="sans",color="black",hjust=0.75),
                 panel.border=element_rect(colour="black",fill=NA,size=0.5),
                 panel.background = element_blank(),
                 legend.position=c(0.1,0.85),
                 legend.background = element_blank(),
                 legend.key.size = unit(1,"line"),
                 legend.key=element_blank(),
                 legend.text = element_text(size=10),
                 #legend.margin=margin(l=-9,t = -8.5, unit='cm'),
                 axis.title.x=element_blank())

#png("SST_ESR/2020/EBS/Watson_Fig5_010421.png",width=7,height=5,units="in",res=   300)
#Update date in this figure!
png("EBS/2025/Callahan_Fig3.png",width=7,height=5,units="in",res=300)
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
  
  #geom_text(data=BSmhw_lab,aes(x=t,y=temp,label=mylab),hjust=0,size=8,family="sans",lineheight=1) +
  scale_colour_manual(name = NULL, values = lineColCat,
                      breaks = c("Temperature", "Baseline", "Moderate (1x Threshold)"),guide=FALSE) +
  scale_fill_manual(name = "Heatwave\nIntensity", values = c(Extreme,Severe,Strong,Moderate),labels=c("Extreme","Severe","Strong","Moderate")#, guide = FALSE
  ) +
  scale_x_date(date_labels = "%b %Y",expand=c(0.01,0)) +
  guides(colour = "none") +
  labs(y = "Sea Surface Temperature (°C)", x = NULL) + 
 # theme(legend.position="none") +
  facet_wrap(~region,ncol=1,scales="free_y") +
  mytheme2
  #theme(legend.position=c(0.18,0.85))
dev.off()

####2023 question, is it accurate that there have been no heatwaves? ####
filter(clim_cat, year==2025 & threshCriterion==TRUE)%>%
  print(n=Inf)

#-------------------------------------------------------------------------------------


#  Figure 4. Mean SST for the northern (left) and southeastern (right) Bering Sea shelves.

# #  Create plotting function that will allow selection of 2 ESR regions
# #  Load 508 compliant NOAA colors
# OceansBlue1='#0093D0'
# OceansBlue2='#0055A4' # dark blue
# Crustacean1='#FF8300'
# SeagrassGreen4='#D0D0D0' # This is just grey
# 
# #  Assign colors to different time series.
# current.year.color <- "black"
# last.year.color <- OceansBlue1
# mean.color <- UrchinPurple1
# 
# #  Set default plot theme
# theme_set(theme_cowplot())
# 
# #  Specify legend position coordinates
# mylegx <- 0.625
# mylegy <- 0.865
# 
# 
# mean.years <- 1985:2014
# mean.lab <- "Mean 1985-2014"
# 
# png("EBS/2024/Callahan_Fig4.png",width=7,height=5,units="in",res=300)
# ggplot() +
#   geom_line(data=newdat %>% filter(year2<last.year),
#             aes(newdate,meansst,group=factor(year2),col='mygrey'),size=0.3) +
#   geom_line(data=newdat %>% filter(year2==last.year),
#             aes(newdate,meansst,color='last.year.color'),size=0.75) +
#   geom_line(data=newdat %>% 
#               filter(year%in%mean.years) %>% 
#               group_by(Ecosystem_sub ,newdate) %>% 
#               summarise(meantemp=mean(meansst,na.rm=TRUE)),
#             aes(newdate,meantemp,col='mean.color'),size=0.5) +
#   geom_line(data=newdat %>% filter(year2==current.year),
#             aes(newdate,meansst,color='current.year.color'),size=0.95) +
#   facet_wrap(~Ecosystem_sub ,ncol=2) + 
#   scale_color_manual(name="",
#                      breaks=c('current.year.color','last.year.color','mygrey','mean.color'),
#                      values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
#                      labels=c(current.year,last.year,paste0('1985-',last.year-1),mean.lab)) +
#   ylab("Mean Sea Surface Temperature (C)") + 
#   xlab("") +
#   scale_x_date(date_breaks="1 month",
#                date_labels = "%b",
#                expand = c(0.025,0.025)) + 
#   theme(legend.position=c(mylegx,mylegy),
#         legend.text = element_text(size=8,family="sans"),
#         legend.background = element_blank(),
#         legend.title = element_blank(),
#         strip.text = element_text(size=10,color="white",family="sans",face="bold"),
#         strip.background = element_rect(fill=OceansBlue2),
#         axis.title = element_text(size=10,family="sans"),
#         axis.text = element_text(size=10,family="sans"),
#         panel.border=element_rect(colour="black",size=0.75),
#         axis.text.x=element_text(color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
#         legend.key.size = unit(0.35,"cm"),
#         plot.margin=unit(c(0.65,0,0.65,0),"cm")) 
# dev.off()


# Figure 5. Time series decomposition

#devtools::install_github("brisneve/ggplottimeseries")


#  The following could all be combined but I have left it separated out to be more transparent.
df1 <- newdat %>% 
  filter(Ecosystem_sub=="Southeastern Bering Sea")

#  Perform the time series decomposition for the EGOA, setting the frequency as 365.25 because we have daily data with leap years.
#dts1 is a function from the ggplottimeseries package and is a wrapper for the decompose function from 
df1 <- dts1(df1$date,df1$meansst,365.25, type = "additive") %>% 
  mutate(Ecosystem_sub="Southeastern Bering Sea",
         year=year(date))

#  Repeat for the wgoa
df2 <- newdat %>% 
  filter(Ecosystem_sub=="Northern Bering Sea")

df2 <- dts1(df2$date,df2$meansst,365.25, type = "additive") %>% 
  mutate(Ecosystem_sub="Northern Bering Sea",
         year=year(date))

#  Combine the time series decompositions for each area and reorder the factors.
df <- df1 %>% 
  bind_rows(df2)

#  Create the horizontal mean and sd lines for the 30 year baseline period.
dfmean <- df %>% 
  group_by(Ecosystem_sub) %>% 
  summarise(meantrend=mean(trend[between(year,climatology_start_year,climatology_end_year)],na.rm=TRUE),
            sdtrend=sd(trend[between(year,climatology_start_year,climatology_end_year)],na.rm=TRUE))


png("EBS/2025/Callahan_Fig5.png",width=7,height=5,units="in",res=300)
df %>% 
  ggplot(aes(x = date, y = trend)) + 
  geom_line() + 
  geom_hline(data=dfmean,aes(yintercept=meantrend),linetype=2) +
  geom_hline(data=dfmean,aes(yintercept=meantrend+sdtrend),linetype=2,color="red") +
  geom_hline(data=dfmean,aes(yintercept=meantrend-sdtrend),linetype=2,color="red") +
  facet_wrap(~Ecosystem_sub) + 
  theme_bw()+
  theme(strip.text = element_text(size=10,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill=OceansBlue2),
        axis.title = element_text(size=10,family="sans"),
        axis.text = element_text(size=10,family="sans"),
        panel.border=element_rect(colour="black",size=0.5, fill=NA),
        plot.margin=unit(c(0.65,0,0.65,0),"cm")) + 
  ylab("Sea surface temperature (C)") + 
  xlab("Date")
dev.off()

