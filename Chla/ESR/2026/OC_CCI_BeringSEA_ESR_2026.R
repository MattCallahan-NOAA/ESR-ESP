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

# General note. You need to add additional years when updating to 2026



# super region
dummy_region<-readRDS('inter_jens_datafiles/super_bsierp_lkp.RDS') %>%
  mutate(bsierp_super_region=case_match(bsierp_super_region, "Central inner shelf" ~ "South inner shelf",  .default = bsierp_super_region))

# 

# data is created from the sql pull using the script "EBS_data_pull_occci_2026.R" in the 2026 folder.
# and data is stored in "inter_jens_datafiles" which is gitignore.

bs <- readRDS("inter_jens_datafiles/occci_26.RDS")
head(bs)
bs$bsierp_id<-as.character(bs$bsierp_id)
tail(bs)
bs$month=month(bs$read_date)
bs$year=year(bs$read_date)
bs$doy=yday(bs$read_date)

bs<-bs %>% full_join(dummy_region,by='bsierp_id')
bs<-bs[complete.cases(bs$jens_grid),]
# assigning bsierp super region #
table(bs$month,bs$year)
#rm(bs24)


## here should be a simple file that gets the jens grid stations - super bsierp regions and  the lat /lon
## to plot Fig 1 - this is not automated! Example fig is from a depreciatd script in 2023. Fig1_ESR_map.png
# test generating from lkp table
lkp<-readRDS('inter_jens_datafiles/occci_spatial_lkp.RDS')
lkp<-lkp %>% full_join(dummy_region,by='bsierp_id')

lkp<-lkp %>% filter(!is.na(bsierp_super_region) & ecosystem_area=="Eastern Bering Sea")
ggplot(lkp)+geom_point(aes(x=lon360, y=latitude, color=bsierp_super_region))
#Note-need to make central inner shelf into South inner shelf

data_mapH<-map_data("world2Hires") # map_data from ggmap mapping package
breaks_w2<-c(185,190,195,200)
labels_w2<-breaks_w2-360


col_jens_custom<-c('red2','#874F6F' ,'red2','blue','red2','red2','#874F6F','#E1C62F',
                   'dodgerblue',"dodgerblue",'orange','darkgreen','darkgreen','white','grey33','grey33')




windows(8,8)
ggplot()+
  coord_equal(xlim=c(181,203),ylim=c(54.8,66),ratio = 1.8)+
  #xlim(c(140,250))+
  ylim(c(40,80))+
  geom_tile(data = plot_single, aes(x = lon2, y = latc,fill=factor(bsierp_name_nb)))+
  #scale_fill_gradientn(colours = viridis(17), na.value = NA,limits = peak_limits2,trans = "pseudo_log")+
  scale_fill_manual(values = col_jens_custom,name='Bsierp regions')+
  geom_polygon(data = data_mapH, aes(x=long, y = lat, group = group),colour="black", fill="darkgrey")+
  #geom_text(data = avg_dummy_text, aes(x = lon2, y = latc,label=factor(bsierp_name_nb)),size = 2)+
  ylab("Latitude")+
  scale_x_continuous("Longitude", breaks=breaks_w2, labels=labels_w2, limits=c(140,250))+
  ggtitle('')+
  theme_bw()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=20),
        plot.title = element_text(size=22),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"))




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



# sets up the darker blue color of the most recent year
color_filler<-rep(c(rep('dodgerblue',28),'blue'),8) # fix here - when more years are added # 


## plot trick - for "controlling the SDs"###
super_avg_sub$sd_chla_max<- super_avg_sub$mean_chla+super_avg_sub$sd_chla
#super_avg_sub$sd_chla_max[super_avg_sub$sd_chla_max>10.0]<-9.9
super_avg_sub$sd_chla_min<- super_avg_sub$mean_chla-super_avg_sub$sd_chla
#super_avg_sub$sd_chla_min[super_avg_sub$sd_chla_min<0]<-0.01

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
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2020,2025))+ # breaks=c(2004,2006,2008,2010,2012,2014,2016,2018,2020)
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
png(filename="ESR/2026/newFig2_satellite_Chla_ESR_EBS.png",width = 1600, height = 1100,res=120)
plot(fig2)
dev.off()
# rejoice (or actually move on to Fig 3 )#


##
##fig 3 - which I think we might not be making for the ESR anymore. But its still helpful to look at 
##


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
  scale_y_discrete(breaks=c(2005,2010,2015,2020,2025))+ # breaks=c(2004,2006,2008,2010,2012,2014,2016,2018,2020)
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
# 
png(filename="Chla/ESR/2025/Fig3_satellite_Chla_ESR_EBS_tileplot60_300.png",width = 1600, height = 1200,res=120)
plot(fig3)
dev.off()


###
# Figure 4 bloom timing plot #
##
##

# this data is created in a different chla script  "chla_bloomTiming_ice_retreat_calculations_2025_occci.R"
# which is located in the Chla folder. That is takes data to calculate bloom timing - and stores those data 
# in an internal folder "inter_jens_datafiles" (in gitignore)


bl <- readRDS("inter_jens_datafiles/bloomTimingOCCCI_1998_2026.RDS")

# again merging to include the regions 
bl<-bl %>% full_join(dummy_region,by='bsierp_id')
bl<-bl[complete.cases(bl$jens_grid),]



sum_bl<- bl %>% group_by(year,bsierp_super_region ) %>% summarize(avg_peak = mean(peak_timing_all_log,na.rm=TRUE),
                                                                  sd_peak = sd(peak_timing_all_log,na.rm=TRUE))

sum_bl_sub<-subset(sum_bl,bsierp_super_region %in% c("South inner shelf","South middle shelf","South outer shelf","Offshelf")  )
head(sum_bl_sub)


# these 2 jens grids are the ones near the M2 mooring. 
m2_oc<- bl[bl$jens_grid %in% c(108,109), ]

#plot(m2_glob$year, m2_glob$gap_sizeTS)

m2_oc_agg<- m2_oc %>% group_by(year) %>% summarize(avg_peak = mean(peak_timing_all_log,na.rm=TRUE),
                                                       sd_peak = sd(peak_timing_all_log,na.rm=TRUE))

m2_oc_agg$bsierp_super_region<-rep("M2 mooring",nrow(m2_oc_agg))

  
sat<-rbind(sum_bl_sub, m2_oc_agg)
head(sat)
sat$bsierp_super_region<-factor((as.character(sat$bsierp_super_region)), levels=c("South outer shelf","South middle shelf","South inner shelf",
                                                                                    "Offshelf","M2 mooring"))  

# this is a step that is pulling bloom timing estimates from the M2 mooring - based on past papers 
# the first file 1998-2011 estimates are from Sigler 2014 - "Spring and fall phytoplankton blooms in a productive subarctic ecosystem, the eastern Bering Sea, during 1995–2011"
# 2005-2019 are from Nielsen 2024 - "Spring phytoplankton bloom phenology during recent climate warming on the Bering Sea shelf"


sigler<-read.csv("ESR/mooring_bloom_estimates/Sigler_2014_timing_data_modified_version.csv",header=TRUE, dec=".",sep=",",na.strings="NA")
sig_m2<-sigler[sigler$Moor=="M2",]

# Additional data from Nielsen 
load("ESR/mooring_bloom_estimates/timing_data_M2_mooring_peaks_30_dec2021.RData")
mor_m2<-jnd_tbl_m2
dummy_year<-data.frame(1998:2026)
colnames(dummy_year)<-'year'
mor_m2<-merge(mor_m2,dummy_year,all=T,by='year')

mor_m2$prim_hybrid<-mor_m2$primary_peak

# in cases where estimates from sigler and nielsen are both present - Sigler is used (usually simialr)

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

mooring

# WARNING here - when you add 2026 - you need to adjust the values of the rows for the hack code below! 


# additional m2 mooring bloom timing estimates were done from the Profiling crawler which reports live for the year. 
# we add those to eh M2 estimates and the south middle shelf estimates 

# personal M2 mooring inspection - peak estimation 2023
mooring$prim_hybrid[25]<-142 # day of year - based on Prawler data
mooring$prim_hybrid[53]<-142 # day of year - based on Prawler data

# setting 2024 (from Prawler)

mooring$prim_hybrid[27]<- 136 # day of year - based on Prawler data
mooring$prim_hybrid[55]<-136 # day of year - based on Prawler data

# setting 2025 - didn't hhave this so no 2025 estimate. Mabye we have it now? 
mooring$prim_hybrid[28]#<- 136 # day of year - based on Prawler data
mooring$prim_hybrid[56]#<-136 # day of year - based on Prawler data

m2_2022_2026<-c()
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


fig4<- ggplot(sat,aes(avg_peak, year)) + 
  geom_vline(data= long_termavg, aes(xintercept=avg_peak), linetype="dashed", color = "black",size=1)+
  facet_wrap(bsierp_super_region~.,ncol=3,labeller =as_labeller(jens_names_fig4)) +
  xlim(70, 190)+
  scale_y_continuous(limits = c(1997,2026), breaks = seq(1997,2026,2))+
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


windows(11,9)
fig4
# 
png(filename="ESR/2026/Fig4_satellite_Chla_ESR_EBS_bloomtiming.png",width = 1600, height = 1400,res=120)
plot(fig4)
dev.off()

