library(tidyverse)
library(lubridate)
library(forcats)
library(readxl)
library(viridis)
library(magrittr)
library(ggridges)
library(RJDBC)
library(getPass)
library(cowplot)
library(keyring)

#  Load 508 compliant NOAA colors
OceansBlue1='#0093D0'
OceansBlue2='#0055A4' # darker blue used for strip.background

# pull data from akfin
#connect to akfin
# connect to AKFIN
options(java.parameters="-Xmx8g")

jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="../../snippets/dbconnect/java/ojdbc8.jar")

con_j <- dbConnect(jdbcDriver, 
                   "jdbc:oracle:thin:@//tiger:2045/akfin.psmfc.org", 
                   key_list("akfin_oracle_db")$username, 
                   keyring::key_get("akfin_oracle_db", keyring::key_list("akfin_oracle_db")$username))

#load data with depth, season, and region filters
data<-dbGetQuery(con_j, "select round(avg(chla),2) meanchla, to_date(start_date,'YYYY-MM-DD')+4 mid_date, ecosystem_subarea
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and ecosystem_area = ('Gulf of Alaska')
and waters_cod = 'FED'
and depth>(-200)
and depth<(-10)
group by to_date(start_date,'YYYY-MM-DD')+4, ecosystem_subarea")%>%
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
  mutate(iseven=ifelse(year%in%(seq(1998,2024,by=2)),"even","odd")) %>% # Create an even/odd year flag for salmon models
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
png("ESR/2024/Figure_1_GOA.png",width=8,height=6,units="in",res=300)
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


png("ESR/2024/Figure_2_GOA.png",width=8,height=6,units="in",res=300)
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

png("ESR/2024/Chla_annual_lines.png",width=7,height=5,units="in",res=300)
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
png("ESR/2024/Chla_annual_lines_smoothed.png",width=7,height=5,units="in",res=300)
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
png("ESR/2024/Chla_yearbyyear_lines.png",width=7,height=20,units="in",res=300)
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


# extra code for adding peak timing to text

#get peak bloom for 2024
data %>% 
  filter(doy>=50 & doy<=180 & year==2024) %>% 
  group_by(ecosystem_subarea,year) %>% 
  summarise(mymax=doy[meanchla==max(meanchla)][1]) %>% 
  group_by(ecosystem_subarea) %>% 
  summarise(peak_bloom=mean(mymax))

data %>% 
  filter(doy>=50 & doy<=180 & year< current.year) %>% 
  group_by(ecosystem_subarea,year) %>% 
  summarise(mymax=doy[meanchla==max(meanchla)][1]) %>% 
  group_by(ecosystem_subarea) %>% 
  summarise(peak_bloom=mean(mymax))

#get values for 2024 at each of those dates
data %>% 
  filter(doy%in%c(125,133) & year==2024) %>% 
  group_by(ecosystem_subarea) %>% 
  mutate(mymax=doy[meanchla==max(meanchla)][1])

# get the mean chla for all years
data%>%
  filter(month %in% (4:6))%>%
  group_by(year,ecosystem_subarea)%>%
  summarize(annual_meanchla=mean(meanchla))%>%
  print(n=Inf)

