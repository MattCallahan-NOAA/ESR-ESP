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
bs_data <- readRDS("inter_jens_datafiles/mod_bs.RDS")
head(bs_data)

# newebs <- bs %>%
#   filter(Ecosystem_Area=="Eastern Bering Sea") %>%
#   dplyr::select(id,chlorophyll,date) %>%
#   bind_rows(readRDS("Updated_modis_lookup_table.RDS") %>%
#               filter(Ecosystem_Area=="Eastern Bering Sea") %>%
#               dplyr::select(id)) %>%
#   complete(id,date,fill=list(chlorophyll=NA))
# 
# newebs_join <- newebs %>%
#   inner_join(readRDS("Updated_modis_lookup_table.RDS") %>%
#                filter(Ecosystem_Area=="Eastern Bering Sea") %>%
#                dplyr::select(id,statefed,STAT_AREA,depth,nmfsarea,BSIERP_Region_Name,Ecosystem_Area,Ecosystem_Subarea,latitude,longitude)) %>%
#   mutate(month=month(date),
#          year=year(date),
#          doy=yday(date))
# 
# tail(newebs_join)
# 


#bs_data<-data.frame(newebs_join)




gc()
bs_data$doy<- yday(bs_data$date)

# bs_data_sub<-bs_data[bs_data$year==2011,]
# table(is.na(bs_data_sub$chlorophyll), bs_data_sub$doy)
unique(bs_data$BSIERP_Region_Name)

bs_data<-bs_data[!is.na(bs_data$BSIERP_Region_Name),]

bs_data$bsierp_name_nb<-NA # adding empty 
head(data.frame(bs_data))

bs_data$bsierp_name_nb[bs_data$BSIERP_Region_Name=="AK peninsula"]<-"1 AK peninsula"
bs_data$bsierp_name_nb[bs_data$BSIERP_Region_Name=="South inner shelf"]<-"2 South inner shelf"
bs_data$bsierp_name_nb[bs_data$BSIERP_Region_Name=="South middle shelf"]<-"3 South middle shelf"
bs_data$bsierp_name_nb[bs_data$BSIERP_Region_Name=="South outer shelf"]<-"4 South outer shelf"
bs_data$bsierp_name_nb[bs_data$BSIERP_Region_Name=="Pribilofs"]<-"5 Pribilofs"
bs_data$bsierp_name_nb[bs_data$BSIERP_Region_Name=="Central middle shelf"]<-"6 Central middle shelf"
bs_data$bsierp_name_nb[bs_data$BSIERP_Region_Name=="Central inner shelf"]<-"7 Central inner shelf"
bs_data$bsierp_name_nb[bs_data$BSIERP_Region_Name=="North outer shelf"]<-"8 North outer shelf"
bs_data$bsierp_name_nb[bs_data$BSIERP_Region_Name=="St. Matthew"]<-"9 St. Matthews"
bs_data$bsierp_name_nb[bs_data$BSIERP_Region_Name=="North middle shelf"]<-"10 North middle shelf"
bs_data$bsierp_name_nb[bs_data$BSIERP_Region_Name=="North inner shelf"]<-"11 North inner shelf"
bs_data$bsierp_name_nb[bs_data$BSIERP_Region_Name=="St. Lawrence"]<-"12 St. Lawrence"
bs_data$bsierp_name_nb[bs_data$BSIERP_Region_Name=="South Bering Strait"]<-"13 South Bering Strait"
bs_data$bsierp_name_nb[bs_data$BSIERP_Region_Name=="Norton Sound"]<-"14 Norton Sound"
bs_data$bsierp_name_nb[bs_data$BSIERP_Region_Name=="Off-shelf north"]<-"15 Off-shelf north"
bs_data$bsierp_name_nb[bs_data$BSIERP_Region_Name=="Off-shelf southeast"]<-"16 Off-shelf southeast"

unique(bs_data$bsierp_name_nb)
bs_data$bsierp_name_nb<-factor(bs_data$bsierp_name_nb, levels=c("1 AK peninsula", "2 South inner shelf", "3 South middle shelf","4 South outer shelf",
                                                                "5 Pribilofs","6 Central middle shelf","7 Central inner shelf","8 North outer shelf",
                                                                "9 St. Matthews","10 North middle shelf","11 North inner shelf","12 St. Lawrence","13 South Bering Strait",
                                                                "14 Norton Sound","15 Off-shelf north","16 Off-shelf southeast"))

################
### Figure 2 ###
################
bs_data$bsierp_major_areas<-NA

bs_data$bsierp_major_areas[bs_data$BSIERP_Region_Name=="AK peninsula"]<-"South middle shelf"
bs_data$bsierp_major_areas[bs_data$BSIERP_Region_Name=="South inner shelf"]<-"South inner shelf"
bs_data$bsierp_major_areas[bs_data$BSIERP_Region_Name=="South middle shelf"]<-"South middle shelf"
bs_data$bsierp_major_areas[bs_data$BSIERP_Region_Name=="South outer shelf"]<-"South outer shelf"
bs_data$bsierp_major_areas[bs_data$BSIERP_Region_Name=="Pribilofs"]<-"South middle shelf"
bs_data$bsierp_major_areas[bs_data$BSIERP_Region_Name=="Central middle shelf"]<-"South middle shelf"
bs_data$bsierp_major_areas[bs_data$BSIERP_Region_Name=="Central inner shelf"]<-"South inner shelf"
bs_data$bsierp_major_areas[bs_data$BSIERP_Region_Name=="North outer shelf"]<-"North outer shelf"
bs_data$bsierp_major_areas[bs_data$BSIERP_Region_Name=="St. Matthew"]<-"North middle shelf"
bs_data$bsierp_major_areas[bs_data$BSIERP_Region_Name=="North middle shelf"]<-"North middle shelf"
bs_data$bsierp_major_areas[bs_data$BSIERP_Region_Name=="North inner shelf"]<-"North inner shelf"
bs_data$bsierp_major_areas[bs_data$BSIERP_Region_Name=="St. Lawrence"]<-"Bering Strait & St Lawrence"
bs_data$bsierp_major_areas[bs_data$BSIERP_Region_Name=="South Bering Strait"]<-"Bering Strait & St Lawrence"
bs_data$bsierp_major_areas[bs_data$BSIERP_Region_Name=="Norton Sound"]<-"14 Norton Sound"
bs_data$bsierp_major_areas[bs_data$BSIERP_Region_Name=="Off-shelf north"]<-"Offshelf"
bs_data$bsierp_major_areas[bs_data$BSIERP_Region_Name=="Off-shelf southeast"]<-"Offshelf"

head(data.frame(bs_data))

### aggregating to each doy and major area ###
setDT(bs_data)
d_doy<-bs_data[ !is.na(year) & !is.na(bsierp_major_areas) & !is.na(doy), lapply(.SD, function(x) {
  if(is.numeric(x)) mean(x, na.rm = TRUE) else x[!is.na(x)][1L]}), by = list(year,bsierp_major_areas,doy)]
d_doy<-data.frame(d_doy)

### aggregating to each month and major area ###
setDT(d_doy)
d_month<-d_doy[ !is.na(year) & !is.na(bsierp_major_areas) & !is.na(month), lapply(.SD, function(x) {
  if(is.numeric(x)) mean(x, na.rm = TRUE) else x[!is.na(x)][1L]}), by = list(year,bsierp_major_areas,month)]
d_month<-data.frame(d_month)

levels((d_month$bsierp_major_areas) ) 

d_month$bsierp_major_areas<-factor((as.character(d_month$bsierp_major_areas)), levels=c("South inner shelf","South middle shelf","South outer shelf",
                                                                                        "North inner shelf","North middle shelf","North outer shelf",
                                                                                        "Offshelf", "Bering Strait & St Lawrence", "14 Norton Sound"))

###
###
###
spring_data<-subset(d_month,month %in% c(04,05,06))
setDT(spring_data)
Major_region_spring<-spring_data[ !is.na(year) & !is.na(bsierp_major_areas) , lapply(.SD, function(x) {
  if(is.numeric(x)) mean(x, na.rm = TRUE) else x[!is.na(x)][1L]}), by = list(year,bsierp_major_areas)]

Major_region_spring<-data.frame(Major_region_spring)

Major_region_spring$bsierp_major_areas<-factor((as.character(Major_region_spring$bsierp_major_areas)), levels=c("South inner shelf","South middle shelf","South outer shelf",
                                                                                                                "North inner shelf","North middle shelf","North outer shelf",
                                                                                                                "Offshelf", "Bering Strait & St Lawrence", "14 Norton Sound"))

Major_region_spring_sub<-subset(Major_region_spring,bsierp_major_areas %in% c("South inner shelf","South middle shelf","South outer shelf","North inner shelf",
                                                                              "North middle shelf","North outer shelf","Offshelf","Bering Strait & St Lawrence"))

### sd of same data 
###
setDT(spring_data)
Major_region_spring_SD<-spring_data[ !is.na(year) & !is.na(bsierp_major_areas) , lapply(.SD, function(x) {
  if(is.numeric(x)) sd(x, na.rm = TRUE) else x[!is.na(x)][1L]}), by = list(year,bsierp_major_areas)]

Major_region_spring_SD<-data.frame(Major_region_spring_SD)

Major_region_spring_SD$bsierp_major_areas<-factor((as.character(Major_region_spring_SD$bsierp_major_areas)), levels=c("South inner shelf","South middle shelf","South outer shelf",
                                                                                                                      "North inner shelf","North middle shelf","North outer shelf",
                                                                                                                      "Offshelf", "Bering Strait & St Lawrence", "14 Norton Sound"))



Major_region_spring_SD_sub<-subset(Major_region_spring_SD,bsierp_major_areas %in% c("South inner shelf","South middle shelf","South outer shelf","North inner shelf",
                                                                                    "North middle shelf","North outer shelf","Offshelf","Bering Strait & St Lawrence"))


colnames(Major_region_spring_SD_sub)[7]<-'sd_chlorophyll'


## mean and median lines #


a_mean<-aggregate(chlorophyll~bsierp_major_areas,data=Major_region_spring_sub,mean)
a_median<-aggregate(chlorophyll~bsierp_major_areas,data=Major_region_spring_sub,median)

#write.csv(Major_region_spring_sub, file = "data_forMean_calc.csv")

a_mean$bsierp_major_areas<-factor((as.character(a_mean$bsierp_major_areas)), levels= c("Bering Strait & St Lawrence","North outer shelf","North middle shelf","North inner shelf",
                                                                                       "Offshelf","South outer shelf",  "South middle shelf","South inner shelf"))


a_median$bsierp_major_areas<-factor((as.character(a_median$bsierp_major_areas)), levels= c("Bering Strait & St Lawrence","North outer shelf","North middle shelf","North inner shelf",
                                                                                           "Offshelf","South outer shelf",  "South middle shelf","South inner shelf"))


test_mer<-merge(x=Major_region_spring_sub,y=Major_region_spring_SD_sub,by=c('year','bsierp_major_areas'),all.x=TRUE)



test_mer$bsierp_major_areas<-factor((as.character(test_mer$bsierp_major_areas)), levels= c("Bering Strait & St Lawrence","North outer shelf","North middle shelf","North inner shelf",
                                                                                           "Offshelf","South outer shelf",  "South middle shelf","South inner shelf"))

head(test_mer)

#test_mer$chlorophyll[16]<-NA
#test_mer$sd_chlorophyll[16]<-NA

range(test_mer$chlorophyll)
range(test_mer$sd_chlorophyll)



test_mer$min_BAR<-test_mer$chlorophyll.x-test_mer$sd_chlorophyll
test_mer$max_BAR<-test_mer$chlorophyll.x+test_mer$sd_chlorophyll

range(test_mer$min_BAR)
range(test_mer$max_BAR)
# setting the bars to min and max for plotting 
test_mer$min_BAR2<-test_mer$min_BAR
test_mer$min_BAR2[test_mer$min_BAR<0.0]<-0.01

test_mer$max_BAR2<-test_mer$max_BAR
test_mer$max_BAR2[test_mer$max_BAR>10.0]<-9.9
range(test_mer$max_BAR2-test_mer$min_BAR2)

jens_names <- c("Bering Strait & St Lawrence" = "Bering Strait",
                "North outer shelf" = "north outer",
                "North middle shelf" = "north middle",
                "North inner shelf" = "north inner",
                "Offshelf"  = "off-shelf",
                "South outer shelf"  = "south outer",
                "South middle shelf" = "south middle",
                "South inner shelf" = "south inner")


levels(test_mer$bsierp_major_areas)
levels(a_mean$bsierp_major_areas)
levels(a_median$bsierp_major_areas)
length(unique(test_mer$year))
test_mer$chlorophyll<-test_mer$chlorophyll.x
color_filler<-rep(c(rep('dodgerblue',19),'blue'),8) # fix here - when more years are added # 


fig2<- test_mer %>% 
  ggplot() + 
  geom_bar(aes(year,chlorophyll),stat="identity",fill=color_filler) +
  facet_wrap(bsierp_major_areas~.,ncol=4,labeller =as_labeller(jens_names) ) + 
  geom_hline(data=a_median,aes(yintercept=chlorophyll),linetype=2) + #add horizontal mean values
  #geom_hline(data=a_median,aes(yintercept=chlorophyll),linetype=2,col='red') + #add horizontal mean values
  geom_errorbar( aes(x=year,ymin=min_BAR2, ymax=max_BAR), width=.3,col='red',alpha=0.4)+
  theme(strip.text = element_text(size=20,color="white",family="sans",face="bold"),
        strip.background = element_rect(fill='dodgerblue'), # Add the NOAA color blue to the facet strips.
        axis.title = element_text(size=20,family="sans"),
        axis.text = element_text(size=14,family="sans"),
        panel.background=element_blank(),
        panel.border=element_rect(color="black",fill=NA),
        axis.text.x=element_text(color="black")) +
  xlab("") + 
  ylab("Chlorophyll-a [ug/L]") + 
  scale_y_continuous(limits=c(0,10),expand = c(0,0.0)) # Specify a boundary and expansion to set bars on bottom black line

# look at it @ 
windows(10,8)
fig2

png(filename="Chla/ESR/2023/Fig2_satellite_Chla_ESR_EBS.png",width = 1600, height = 1200,res=120)
plot(fig2)
dev.off()


#############
### Fig 3 ### 
#############
d_doy_facet<-subset(d_doy,bsierp_major_areas %in% c("South inner shelf","South middle shelf","South outer shelf","North inner shelf",
                                                    "North middle shelf","North outer shelf","Offshelf","Bering Strait & St Lawrence")  )



d_doy_facet$bsierp_major_areas<-factor((as.character(d_doy_facet$bsierp_major_areas)), levels=c("South inner shelf","South middle shelf","South outer shelf","North inner shelf",
                                                                                                "North middle shelf","North outer shelf","Offshelf","Bering Strait & St Lawrence"))


