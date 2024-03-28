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
bs <- readRDS("inter_jens_datafiles/globcolour_23augSQL.RDS")
head(bs)

bs$month=month(bs$mid_date)
bs$year=year(bs$mid_date)
bs$doy=yday(bs$mid_date)

head(bs)

#########################
### prep for figure 2 ###
#########################
bs2<- bs %>% group_by(year,bsierp_super_region,jens_grid) %>% filter(month>3 & month <7) %>% summarise(mean_chla = mean(meanchla,na.rm=TRUE),
                                                                                                    sd_chla = sd(meanchla,na.rm=TRUE))
head(bs2)

table(bs2$bsierp_super_region,bs2$year)

super_avg<- bs2 %>% group_by(year,bsierp_super_region) %>%  summarise(sd_chla = sd(mean_chla,na.rm=TRUE),
                                                                      mean_chla = mean(mean_chla,na.rm=TRUE))
head(super_avg)  
table(super_avg$bsierp_super_region)

# picking the super regions we focus on
super_avg_sub<-subset(super_avg,bsierp_super_region %in% c("South inner shelf","South middle shelf","South outer shelf","North inner shelf",
                                                                                    "North middle shelf","North outer shelf","Offshelf","Bering Strait & St Lawrence"))



write.csv(super_avg_sub,file='inter_jens_datafiles/ESR_chla_forDave_superREGIONS_sep2023.csv') 

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
# png(filename="Chla/ESR/2023/newFig2_satellite_Chla_ESR_EBS.png",width = 1600, height = 1100,res=120)
# plot(fig2)
# dev.off()
# rejoice (or actually move on to Fig 3 )#

range(bs$doy)
# for tile plot 
super_tile<- bs %>% group_by(year,doy,bsierp_super_region) %>% filter(doy>55 & doy <302) %>% summarise(mean_chla = mean(meanchla,na.rm=TRUE))
range(super_tile$doy)

head(super_tile)  
table(super_tile$bsierp_super_region)

super_tile_sub<-subset(super_tile,bsierp_super_region %in% c("South inner shelf","South middle shelf","South outer shelf","North inner shelf",
                                                           "North middle shelf","North outer shelf","Offshelf","Bering Strait & St Lawrence"))

super_tile_sub$bsierp_super_region<-factor((as.character(super_tile_sub$bsierp_super_region)), levels= c("Bering Strait & St Lawrence","North outer shelf","North middle shelf","North inner shelf",
                                                                                                       "Offshelf","South outer shelf",  "South middle shelf","South inner shelf"))

###
###
###
OuterSpacing = unit(20, "pt")
range(super_tile_sub$mean_chla)
fig3<- super_tile_sub %>% 
  ggplot() + 
  geom_tile(aes((doy),factor(year),fill=(mean_chla))) + 
  #scale_fill_viridis(option="C",name="Chlorophyll-a (ug/L)",trans = "pseudo_log",limits = c(0.1,35)) + 
  scale_fill_gradientn(colours = viridis(50), na.value = NA,name='',trans = "pseudo_log",limits = c(0.0,31),breaks=c(0,1,2,5,10,20,30))+ # 
  guides(fill = guide_colourbar(barwidth = 15, barheight = 2.0))+
  scale_y_discrete(breaks=c(2005,2010,2015,2020))+ # breaks=c(2004,2006,2008,2010,2012,2014,2016,2018,2020)
  facet_wrap(bsierp_super_region~.,ncol=4,labeller =as_labeller(jens_names)) +
  xlim(60, 302)+
  theme(strip.text = element_text(size=18,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill='dodgerblue'),
        axis.title = element_text(size=20,family="sans"),
        axis.text = element_text(size=18,family="sans"),
        legend.text = element_text(size=16),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black"),
        legend.position="top",
        panel.spacing.x=OuterSpacing) +
  ylab("Year") + 
  xlab("Day of year")


windows(14,9)
fig3

png(filename="Chla/ESR/2023/Fig3_satellite_Chla_ESR_EBS_tileplot60_300.png",width = 1600, height = 1200,res=120)
plot(fig3)
dev.off()


###
# Figure 4 timing plot #
##
##
bl <- readRDS("inter_jens_datafiles/bloomTimingGlob_1998_2023.RDS")

sum_bl<- bl %>% group_by(year,bsierp_super_region ) %>% summarize(avg_peak = mean(peak_timing_all_log,na.rm=TRUE),
                                                                  sd_peak = sd(peak_timing_all_log,na.rm=TRUE))

sum_bl_sub<-subset(sum_bl,bsierp_super_region %in% c("South inner shelf","South middle shelf","South outer shelf","Offshelf")  )
head(sum_bl_sub)


#write.csv(sum_bl_sub,file='inter_jens_datafiles/bloom_timing_superRegions_forDaveSEP2023.csv') 




m2_glob<- bl[bl$jens_grid %in% c(108,109), ]

#plot(m2_glob$year, m2_glob$gap_sizeTS)

m2_glob_agg<- m2_glob %>% group_by(year) %>% summarize(avg_peak = mean(peak_timing_all_log,na.rm=TRUE),
                                                       sd_peak = sd(peak_timing_all_log,na.rm=TRUE))

m2_glob_agg$bsierp_super_region<-rep("M2 mooring",nrow(m2_glob_agg))

  
sat<-rbind(sum_bl_sub, m2_glob_agg)
head(sat)
sat$bsierp_super_region<-factor((as.character(sat$bsierp_super_region)), levels=c("South outer shelf","South middle shelf","South inner shelf",
                                                                                    "Offshelf","M2 mooring"))  


sigler<-read.csv("~/Arctic_IERP_Eisner_2019/nb13_spatial_chla_bloom_ms_BeringSea/r_script_satellite_bloom/mooring_m2_m4_m5_m8_data_workup/Sigler_2014_timing_data_modified_version.csv",header=TRUE, dec=".",sep=",",na.strings="NA")
sig_m2<-sigler[sigler$Moor=="M2",]

load("~/Arctic_IERP_Eisner_2019/nb13_spatial_chla_bloom_ms_BeringSea/r_script_satellite_bloom/mooring_m2_m4_m5_m8_data_workup/timing_data_M2_mooring_peaks_30_dec2021.RData")
mor_m2<-jnd_tbl_m2
dummy_year<-data.frame(1998:2023)
colnames(dummy_year)<-'year'
mor_m2<-merge(mor_m2,dummy_year,all=T,by='year')

mor_m2$prim_hybrid<-mor_m2$primary_peak

mor_m2$prim_hybrid[mor_m2$year==1998]<-sig_m2$peak_spring_bloom[sig_m2$Year==1998]
mor_m2$prim_hybrid[mor_m2$year==1999]<-sig_m2$peak_spring_bloom[sig_m2$Year==1999]
mor_m2$prim_hybrid[mor_m2$year==2000]<-sig_m2$peak_spring_bloom[sig_m2$Year==2000]
mor_m2$prim_hybrid[mor_m2$year==2001]<-sig_m2$peak_spring_bloom[sig_m2$Year==2001]
mor_m2$prim_hybrid[mor_m2$year==2002]<-sig_m2$peak_spring_bloom[sig_m2$Year==2002]


mor_m2$prim_hybrid[mor_m2$year==2003]<-sig_m2$peak_spring_bloom[sig_m2$Year==2003]
mor_m2$prim_hybrid[mor_m2$year==2004]<-sig_m2$peak_spring_bloom[sig_m2$Year==2004]
mor_m2$prim_hybrid[mor_m2$year==2005]<-sig_m2$peak_spring_bloom[sig_m2$Year==2005]
mor_m2$prim_hybrid[mor_m2$year==2010]<-sig_m2$peak_spring_bloom[sig_m2$Year==2010]
mor_m2$prim_hybrid[mor_m2$year==2014]<-NA # adjustment done by looking at mooring data (highest near sat peak)
mor_m2$prim_hybrid[mor_m2$year==2021]<-141 # adjustment done by looking at mooring data (highest near sat peak)

head(mor_m2)  

mooring<- rbind(mor_m2[,c(1,5)],mor_m2[,c(1,5)])

mooring$bsierp_super_region<-c(rep('M2 mooring',nrow(mor_m2)),rep('South middle shelf',nrow(mor_m2)))

sat$bsierp_super_region<-factor((as.character(sat$bsierp_super_region)), levels=c("South outer shelf","South middle shelf","South inner shelf",
                                                                                  "Offshelf","M2 mooring"))  


mooring$bsierp_super_region<-factor((as.character(mooring$bsierp_super_region)), levels=c("South outer shelf","South middle shelf","South inner shelf",
                                                                                  "Offshelf","M2 mooring"))  


# personal M2 mooring inspection - peak estimation
mooring$prim_hybrid[25]<-141 # day of year - based on Prawler data
mooring$prim_hybrid[51]<-141 # day of year - based on Prawler data

###
### average long-term data 
###
long_termavg<- sat %>% group_by(bsierp_super_region ) %>% summarize(avg_peak = mean(avg_peak,na.rm=TRUE),
                                                                  sd_peak = sd(avg_peak,na.rm=TRUE))


jens_names_fig4 <- c("South outer shelf"  = "south outer",
                     "South middle shelf" = "south middle",
                     "South inner shelf" = "south inner",
                     "Offshelf"  = "off-shelf",
                     "M2 mooring" = "M2 mooring")

windows(11,9)  
ggplot(sat,aes(avg_peak, year)) + 
  geom_vline(data= long_termavg, aes(xintercept=avg_peak), linetype="dashed", color = "black",size=1)+
  facet_wrap(bsierp_super_region~.,ncol=3,labeller =as_labeller(jens_names_fig4)) +
  xlim(70, 190)+
  scale_y_continuous(limits = c(1998,2023), breaks = seq(1998,2023,2))+
  geom_point(data= sat, aes(x=avg_peak,y=year), color = "black",size=3,pch=15,alpha=1)+
  geom_errorbar(data= sat, aes(y=year,xmin=avg_peak-sd_peak, xmax=avg_peak+sd_peak), width=.3,col='black',alpha=1)+
  geom_point(data= mooring, aes(x=prim_hybrid,y=year), color = "dodgerblue",size=3,pch=19,alpha=0.7)+
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
