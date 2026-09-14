# Annoying that the netcdf structure changed 25-26, oh well
library(tidyverse)
library(tidync)
library(lubridate)
library(sf)
library(akgfmaps)
library(stringr)
# create grid with NBS/EBS and depth (from ecosystem subarea)
esr_dd <- get_esr_regions(select.region="esr_subarea", set.crs=4326)
gridebs <- grid %>%
  mutate(lat=geolat, lon=geolon-360) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326, agr = 'constant') %>%
  st_join(esr_dd, join = st_within) %>%
  filter(AREA_NAME %in% c("Southeastern Bering Sea", "Northern Bering Sea") & deptho >= 10 & deptho <= 200) %>%
  data.frame() %>%
  rename_with(tolower) %>%
  dplyr::select(deptho, geolon, geolat, ih, jh, area_name)

ggplot()+geom_point(data=gridebs, aes(x=geolon, y=geolat, color=area_name))

# inner join file to that grid on ih jh
# add year field from filename
# loop through and calculate domain averages

myyear<-1993:2026


dat<-lapply(myyear, FUN= function(x) 
       tidync(paste0("EBS/Data/MOM6/tob.nep.iq0-342jq446-743.hcast.daily.e202604.",x,"0101.nc")) %>%
         hyper_tibble() %>%
         inner_join(gridebs, by=c("jh"="jh", "ih"="ih")) %>%
         mutate(year=x,
                domain=case_when(deptho >= 10 & deptho <= 50 ~ "inner",
                                 deptho > 50 & deptho <= 100 ~ "middle",
                                 deptho > 100 & deptho <= 200~"outer",
                                 .default="you messed up your domain assignments")) %>%
         group_by(year, time, area_name, domain) %>%
         summarize(mean_bt = mean(tob))) %>% bind_rows()

# check for duplicates
sum(duplicated(dat))

# create date from time
dat<-dat %>%
  mutate(
    date = case_when(
      year == 2026 ~ as.Date("2026-01-01") + (time - 0.5),
      TRUE         ~ as.Date("1993-01-01") + (time - 0.5)
    )
  )

# format for plotting
dat<-dat %>%
mutate(eco_short=ifelse(area_name=="Northern Bering Sea", "NBS", "SEBS"),
       eco2=paste(eco_short, domain),
       day=day(date),
       month=month(date),
       newdate=as.Date(ifelse(month>=9,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),#  Create a dummy year so that each year can more easily be overlain
                              as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
       year2=ifelse(month>=9,year+1,year))

mom6<-dat
mom6$domain <- factor(mom6$domain, c("outer", "middle", "inner"))

# plot to make sure it looks the same as last year.
current.year<-year(Sys.Date())
last.year<-current.year-1
sst_start_year <- 1985
mom6_start_year <- 1993
mean.years.sst <- sst_start_year:last.year
mean.lab.sst <- paste0("Mean ",sst_start_year,"-",last.year)
mean.years.mom6 <- mom6_start_year:last.year
mean.lab.mom6 <- paste0("Mean ",mom6_start_year,"-",last.year)

#SST
#  Load 508 compliant NOAA colors
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
mylegx <- 0.625
mylegy <- 0.865


pb5<-ggplot() +
  geom_line(data=mom6 %>% filter(year2<last.year), # Older years are grey lines.
            aes(newdate,mean_bt,group=factor(year2),col='mygrey'),size=0.3) +
  geom_line(data=mom6 %>% filter(year2==last.year), # The previous year
            aes(newdate,mean_bt,color='last.year.color'),size=1) +
  geom_line(data=mom6 %>% 
              filter(year2%in%mean.years.mom6) %>% # The mean from 1986-2015
              #group_by(eco2,newdate) %>% 
              group_by(eco_short, domain,newdate) %>% 
              summarise(meantemp=mean(mean_bt,na.rm=TRUE)),
            aes(newdate,meantemp, col='mean.color'), size=1,linetype="solid") +
  geom_line(data=mom6 %>% filter(year2==current.year), # the current year
            aes(newdate,mean_bt,group=factor(year2),color='current.year.color'),size=0.75) +
  #facet_wrap(~eco2,ncol=1) +
  facet_grid(rows=vars(eco_short), cols=vars(domain)) +
  scale_color_manual(name="",
                     breaks=c('current.year.color','last.year.color','mygrey','mean.color'),
                     values=c('current.year.color'=current.year.color,'last.year.color'=last.year.color,'mygrey'=SeagrassGreen4,'mean.color'=mean.color),
                     labels=c(current.year,last.year,paste0(mom6_start_year,"-",last.year-1),mean.lab.mom6)) +
  scale_linetype_manual(values=c("solid","solid","solid","dashed")) +
  #scale_y_continuous(labels=scaleFUN)+
  ylim(c(-2,13))+
  scale_x_date(limits=c(as_date("1999-09-01"),as_date("2000-08-31")),date_breaks="1 month",date_labels = "%b",expand=c(0.01,0)) +
  ylab("MOM6 Bottom Temperature (°C)") +
  #xlab("Week") +
  theme(legend.position=c(0.08,0.9),
        legend.text = element_text(size=15,family="sans"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        strip.text.x=element_blank(),
        strip.text.y = element_text(size=24,color="white",family="sans",face="bold"),
        strip.background.y = element_rect(fill=OceansBlue2),
        axis.title.y = element_text(size=20,family="sans"),
        axis.text.y = element_text(size=16,family="sans"),
        panel.border=element_rect(colour="black",size=0.75),
        #axis.text.x=element_text(size=20,family="sans"),
        legend.key.size = unit(0.35,"cm"),
        axis.text.x=element_text(size=20, color=c("black",NA,NA,"black",NA,NA,"black",NA,NA,"black",NA,NA,NA)),
        axis.title.x=element_blank(),
        plot.margin=unit(c(0,0.5,0.5,0.5),"cm")) 

pb5
# looks right


saveRDS(mom6, "EBS/Data/MOM6/domain_averages_2026.RDS")
