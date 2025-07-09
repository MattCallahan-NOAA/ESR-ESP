library(httr)
library(dplyr)
library(lubridate)
library(ggplot2)

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
current.year <- 2024
last.year <- current.year-1
climatology_start_year <- 1985
climatology_start_date <- "1985-01-01"
climatology_end_year <- 2014
climatology_end_date <- "2014-12-31"
mean.years <- climatology_start_year:climatology_end_year 
mean.lab <- paste0("Mean ",climatology_start_year,"-",climatology_end_year)

# download data and add time fields
newdat <- httr::content(httr::GET('https://apex.psmfc.org/akfin/data_marts/akmp/ecosystem_sub_crw_avg_sst?ecosystem_sub=Southeastern%20Bering%20Sea,Northern%20Bering%20Sea&start_date=19850101&end_date=20240901'), type = "application/json") %>% 
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

# calculate sd by

mymean <- newdat %>% 
  filter(!year2%in%c(1985)) %>% 
  group_by(year2,Ecosystem_sub) %>% 
  arrange(newdate) %>% 
  summarise(cumheat=sum(meansst)) %>% 
  group_by(Ecosystem_sub) %>% 
  mutate(meanheat=mean(cumheat), # Mannually change
         sdheat=sd(cumheat), # Mannually change
         anomaly=cumheat-meanheat)

png("fig_for_johanna_no_baseline.png",width=6,height=3.375,units="in",res=300)
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
