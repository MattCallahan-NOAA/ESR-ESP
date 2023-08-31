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

# 
bs <- readRDS("inter_jens_datafiles/globcolour_23augSQL.RDS")
head(bs)

bs$month=month(bs$mid_date)
bs$year=year(bs$mid_date)
bs$doy=yday(bs$mid_date)

head(bs)

#########################
### prep for figure 2 ###
#########################
super_avg<- bs %>% group_by(year,bsierp_super_region) %>% filter(month>3 & month <7) %>% summarise(mean_chla = mean(meanchla,na.rm=TRUE),
                                                                                                   sd_chla = sd(meanchla,na.rm=TRUE))
head(super_avg)  
table(super_avg$bsierp_super_region)

# picking the super regions we focus on
super_avg_sub<-subset(super_avg,bsierp_super_region %in% c("South inner shelf","South middle shelf","South outer shelf","North inner shelf",
                                                                                    "North middle shelf","North outer shelf","Offshelf","Bering Strait & St Lawrence"))


# sets up the darker blue color of the most recent year
color_filler<-rep(c(rep('dodgerblue',25),'blue'),8) # fix here - when more years are added # 


## plot trick - for "controlling the SDs"###
super_avg_sub$sd_chla_max<- super_avg_sub$mean_chla+super_avg_sub$sd_chla
super_avg_sub$sd_chla_max[super_avg_sub$sd_chla_max>10.0]<-9.9
super_avg_sub$sd_chla_min<- super_avg_sub$mean_chla-super_avg_sub$sd_chla
super_avg_sub$sd_chla_min[super_avg_sub$sd_chla_min<0]<-0.01

# plot labels - made nice #
jens_names <- c("Bering Strait & St Lawrence" = "Bering Strait",
                "North outer shelf" = "north outer",
                "North middle shelf" = "north middle",
                "North inner shelf" = "north inner",
                "Offshelf"  = "off-shelf",
                "South outer shelf"  = "south outer",
                "South middle shelf" = "south middle",
                "South inner shelf" = "south inner")

# median line - Hollowed suggested the median rather than mean - but either works for me #
a_median<-aggregate(mean_chla~bsierp_super_region,data=super_avg_sub,median)

# aligning the subplots / arrange factor levels 
super_avg_sub$bsierp_super_region<-factor((as.character(super_avg_sub$bsierp_super_region)), levels= c("Bering Strait & St Lawrence","North outer shelf","North middle shelf","North inner shelf",
                                                                                       "Offshelf","South outer shelf",  "South middle shelf","South inner shelf"))
# aligning median data for the subplots / arrange factor levels 
a_median$bsierp_super_region<-factor((as.character(a_median$bsierp_super_region)), levels= c("Bering Strait & St Lawrence","North outer shelf","North middle shelf","North inner shelf",
                                                                                                       "Offshelf","South outer shelf",  "South middle shelf","South inner shelf"))
# saving plots 
fig2<-super_avg_sub %>% 
  ggplot() + 
  geom_bar(aes(year,mean_chla ),stat="identity",fill=color_filler) +
  facet_wrap(bsierp_super_region~.,ncol=4,labeller =as_labeller(jens_names) ) + 
  geom_hline(data=a_median,aes(yintercept=mean_chla),linetype=2) + #add horizontal mean values
  geom_errorbar( aes(x=year,ymin=sd_chla_min, ymax=sd_chla_max), width=.3,col='red',alpha=0.4)+
  theme(strip.text = element_text(size=20,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill='dodgerblue'), # Add the NOAA color blue to the facet strips.
        axis.title = element_text(size=20,family="sans"),
        axis.text = element_text(size=14,family="sans"),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black")) +
  xlab("") + 
  ylab("Chlorophyll-a [ug/L]")+
  scale_y_continuous(limits=c(0,10),expand = c(0,0.0)) # Specify a boundary and expansion to set bars on bottom black line

# look at plot 
windows(14,8)
fig2

# save plot 
png(filename="Chla/ESR/2023/Fig2_satellite_Chla_ESR_EBS.png",width = 1600, height = 1100,res=120)
plot(fig2)
dev.off()
# rejoice (or actually move on to Fig 3 #


