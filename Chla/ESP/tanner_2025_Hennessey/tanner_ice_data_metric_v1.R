
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

# 
is <- readRDS("inter_jens_datafiles/Tanner_iceRetreatTiming_1985_2025.RDS")
head(is)

bs$month=month(bs$mid_date)
bs$year=year(bs$mid_date)
bs$doy=yday(bs$mid_date)

head(bs)
###
### dummy to get bsierp regions 
###
oc <- readRDS("inter_jens_datafiles/bloomTimingOCCCI_1998_2024.RDS")


head(oc)
table(oc$bsierp_id)

table(gl$bsierp_id,gl$bsierp_super_region)

super<-oc %>% group_by(jens_grid,bsierp_id) %>% summarize()
super <- super[complete.cases(super),]
super


is2<- merge(is,super,by='jens_grid')



head(is2)


is_sum<- is2 %>% group_by(year,bsierp_id ) %>% summarize(avg_iceret = mean(ice_retr_roll15 ,na.rm=TRUE),
                                                                  sd_iceret = sd(ice_retr_roll15 ,na.rm=TRUE))

# BSIERP regions 3,4,5,6,8 - sensu Shannon H

is_sub<-subset(is_sum,bsierp_id %in% c('3','4','5','6','8')  )


windows()
ggplot()+
  geom_point(data=is_sub,aes(x=year, y=avg_iceret))+
  geom_line(data=is_sub,aes(x=year, y=avg_iceret))+
  facet_wrap(.~bsierp_id)+
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




## all regions combined 
is_sub2<- is_sub %>% group_by(year ) %>% mutate(avg_iceall = mean(avg_iceret ,na.rm=TRUE),
                                                         sd_iceall = sd(avg_iceret ,na.rm=TRUE))
head(is_sub2)




tanner_ice_allregions<-ggplot()+
  geom_point(data=is_sub2,aes(x=year, y=avg_iceret))+
  geom_line(data=is_sub2,aes(x=year, y=avg_iceret))+
  geom_point(data=is_sub2,aes(x=year, y=avg_iceall),col='red')+
  geom_line(data=is_sub2,aes(x=year, y=avg_iceall),col='red')+
  
  facet_wrap(.~bsierp_id)+
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

png(filename="Chla/ESP/tanner_2025_Hennessey/ice_retreat_eachbsierp.png",width = 1600, height = 1200,res=120)
plot(tanner_ice_allregions)
dev.off()


write.csv(is_sub2, file='Chla/ESP/tanner_2025_Hennessey/iceretreat_eachbsierpregionTanner_ShannonHennessey.csv')


is_sub3<- is_sub %>% group_by(year ) %>% summarize(avg_iceall = mean(avg_iceret ,na.rm=TRUE),
                                                sd_iceall = sd(avg_iceret ,na.rm=TRUE))


write.csv(is_sub3, file='Chla/ESP/tanner_2025_Hennessey/iceretreat_average_allregions_ShannonHennessey.csv')
