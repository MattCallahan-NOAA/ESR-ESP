# here some code to make the ESR EBS stuff 
library(tidyverse)
library(tidync)
library(lubridate)
library(maps)
library(mapdata)
library(viridis)
library(data.table)
library(gridExtra)
library(RColorBrewer)
library(viridis)


gl <- readRDS("inter_jens_datafiles/bloomTimingGlob_1998_2024.RDS")
oc <- readRDS("inter_jens_datafiles/bloomTimingOCCCI_1998_2024.RDS")


head(gl)
table(gl$bsierp_id)
head(oc)
table(oc$bsierp_id)

table(gl$bsierp_id,gl$bsierp_super_region)

super<-gl %>% group_by(bsierp_super_region,bsierp_id) %>% summarize()
super <- super[complete.cases(super),]
super


oc2<- merge(oc,super,by='bsierp_id')



sum_bl<- gl %>% group_by(year,bsierp_super_region ) %>% summarize(avg_peak = mean(peak_timing_all_log,na.rm=TRUE),
                                                                  sd_peak = sd(peak_timing_all_log,na.rm=TRUE))

sum_oc<- oc2 %>% group_by(year,bsierp_super_region ) %>% summarize(avg_peak = mean(peak_timing_all_log,na.rm=TRUE),
                                                                  sd_peak = sd(peak_timing_all_log,na.rm=TRUE))




sum_oc<-subset(sum_oc,bsierp_super_region %in% c("South inner shelf","South middle shelf","South outer shelf","Offshelf")  )
sum_bl<-subset(sum_bl,bsierp_super_region %in% c("South inner shelf","South middle shelf","South outer shelf","Offshelf")  )

head(sum_oc)

###
### compare 

windows()
ggplot()+
  geom_point(data=sum_oc,aes(x=year, y=avg_peak))+
  geom_line(data=sum_oc,aes(x=year, y=avg_peak))+
  geom_point(data=sum_bl,aes(x=year, y=avg_peak),col='red')+
  geom_line(data=sum_bl,aes(x=year, y=avg_peak),col='red')+
  facet_wrap(.~bsierp_super_region)+
  theme(strip.text = element_text(size=18,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill='dodgerblue'),
        axis.title = element_text(size=20,family="sans"),
        axis.text = element_text(size=14,family="sans"),
        legend.text = element_text(size=16),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black"),
        axis.text.y=element_text(color="black"))+
  ylab("Year") + 
  xlab("Day of year")



###
### ice compare
###

i24 <- readRDS("inter_jens_datafiles/iceRetreatTiming_1998_2024.RDS")
i25 <- readRDS("inter_jens_datafiles/iceRetreatTiming_1998_2025.RDS")

head(i24)
head(i25)
colnames(i25)<-c("jens_grid","year","mean_ice_frac25","ice_retr_roll15_25")

ic<-merge(i24,i25,by=c('year','jens_grid'))
head(ic)
          


