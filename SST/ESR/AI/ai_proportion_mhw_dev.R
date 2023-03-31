#proportion mhw figures
#Create MHW figure for PEEC workshop and ESR contribution

library(odbc)
library(tidyverse)
library(getPass)
library(lubridate)
library(zoo)

#connect to AKFIN
con <- dbConnect(odbc::odbc(), "akfin", UID=getPass(msg="USER NAME"), PWD=getPass())

#pull count of mhw per day for Aluetians
#Does not include depth filter 
mhw_ai<- dbFetch(dbSendQuery(con,
                             paste0("select  a.heatwave_category,
count(*) total_count,
a.read_date, 
b.ecosystem_sub 
from (select
crw_id, read_date, heatwave_category
from afsc.erddap_crw_sst
where extract(year from read_date)=2022) a
inner join (select id, ecosystem_sub 
from afsc.erddap_crw_sst_spatial_lookup
where ecosystem ='Aleutian Islands') b
on a.crw_id=b.id
group by a.read_date, b.ecosystem_sub
order by a.read_date, b.ecosystem_sub")))

#create percentage
mhw_ai<-mhw_ai%>%mutate(prop_mhw=MHW/TOTAL_COUNT)
#save
saveRDS(mhw_ai, "AI/Data/prop_mhw_ai.RDS")
#load RDS-start here on subsequent analyses.

mhw_ai<-readRDS("AI/Data/prop_mhw_ai.RDS")%>%
  rename_with(tolower)

#OK, let's make this prettier
count_plot<-function(x){
  ggplot() +
    geom_line(data=x,
              aes(read_date,prop_mhw), size=0.5, color="orange") +
    facet_wrap(~ecosystem_sub,nrow=1) + 
    ylab("proportion MHW") + 
    xlab("") +
    ylim(c(0,1))+
    theme_bw()+
    theme( strip.text = element_text(size=10,color="white",family="sans",face="bold"),
           strip.background = element_rect(fill='#0055A4'),
           axis.title.y = element_text(size=10,family="sans"),
           axis.text.y = element_text(size=10,family="sans"),
           panel.border=element_rect(colour="black",, fill=NA, size=0.75)) 
}
png("AI/2022/ai_mhw_count_long.png", width=6,height=3,units="in",res=300)    
count_plot(mhw_ai)
dev.off()

#smooth
count_plot_smooth<-function(x){
  ggplot() +
    geom_smooth(data=x,
              aes(read_date,prop_mhw), size=0.5, color="orange", span=0.1, se=FALSE) +
    facet_wrap(~ecosystem_sub,nrow=1) + 
    ylab("proportion MHW") + 
    xlab("") +
    ylim(c(0,1))+
    theme_bw()+
    theme( strip.text = element_text(size=10,color="white",family="sans",face="bold"),
           strip.background = element_rect(fill='#0055A4'),
           axis.title.y = element_text(size=10,family="sans"),
           axis.text.y = element_text(size=10,family="sans"),
           panel.border=element_rect(colour="black",, fill=NA, size=0.75)) 
}

png("AI/2022/ai_mhw_count_smooth.png", width=6,height=3,units="in",res=300)
count_plot_smooth(mhw_ai)
dev.off()

#query by heatwave category
mhw_ai2<- dbFetch(dbSendQuery(con,
                             paste0("select 
heatwave_category,
count(heatwave_category) mhw_count,
a.read_date, 
b.ecosystem_sub 
from (select
crw_id, read_date, heatwave_category
from afsc.erddap_crw_sst
where extract(year from read_date)=2022) a
inner join (select id, ecosystem_sub 
from afsc.erddap_crw_sst_spatial_lookup
where ecosystem ='Aleutian Islands') b
on a.crw_id=b.id
group by a.read_date, b.ecosystem_sub, heatwave_category
order by a.read_date, b.ecosystem_sub, heatwave_category")))%>%
  rename_with(tolower)

#Join in total counts... I could do this in sql but it's easier for me in R since I already have the other results

mhw_ai2<-mhw_ai2%>%left_join(mhw_ai%>%dplyr::group_by(ecosystem_sub)%>%summarise(total_count=mean(total_count)),by="ecosystem_sub")
#calculate propmhw
mhw_ai2<-mhw_ai2%>%mutate(prop_mhw=mhw_count/total_count)

#save
saveRDS(mhw_ai2, "AI/Data/prop_mhw_ai2.RDS")
mhw_ai2<-readRDS("AI/Data/prop_mhw_ai2.RDS")
#reorder ecosystems
mhw_ai2<-mhw_ai2%>%mutate(ecosystem_sub=fct_relevel(ecosystem_sub,
                                                  c("Western Aleutians", "Central Aleutians", "Eastern Aleutians")))

count_by_mhw<-function(x){
  mycolors=c("white", "#ffc866","#ff6900", "#9e0000", "#2d0000", "#0093D0", "white")
  ggplot() +
    geom_histogram(data=x,
              aes(read_date,prop_mhw, fill=heatwave_category, color=heatwave_category), 
              stat="identity") +
    facet_wrap(~ecosystem_sub,nrow=1) + 
    ylab("proportion MHW") + 
    xlab("") +
    ylim(c(0,1))+
    theme_bw()+
    scale_color_manual(values=mycolors)+
    scale_fill_manual(values=mycolors)+
    theme_bw()+
    theme( strip.text = element_text(size=10,color="white",family="sans",face="bold"),
           strip.background = element_rect(fill='#0055A4'),
           axis.title.y = element_text(size=10,family="sans"),
           axis.text.y = element_text(size=10,family="sans"),
           #panel.border=element_rect(colour="black",, fill=NA, size=0.75)
           ) 
}

count_by_mhw(mhw_ai2%>%mutate(month=month(read_date))%>%filter(month==1))

png("AI/2022/ai_mhw_by_status.png", width=6,height=3,units="in",res=300)
count_by_mhw(mhw_ai2)
dev.off()

mhw_ai2$heatwave_category<-recode(mhw_ai2$heatwave_category, "I"="0")
mhw_ai2_5<-mhw_ai2%>%
  group_by(ecosystem_sub, heatwave_category)%>%
  mutate(mean_5day = zoo::rollmean(prop_mhw, k = 5, fill=NA),
         mean_7day = zoo::rollmean(prop_mhw, k = 7, fill=NA),
         mean_10day = zoo::rollmean(prop_mhw, k = 10, fill=NA))%>%
  ungroup()

#function for a smoothed version
count_by_mhw_d<-function(x){
  mycolors=c("white", "#ffc866","#ff6900", "#9e0000", "#0093D0", "#2d0000", "#0093D0", "white")
  ggplot() +
    geom_histogram(data=x%>%mutate(category=heatwave_category),
                   aes(read_date,mean_5day, fill=category, color=category), 
                   stat="identity") +
    facet_wrap(~ecosystem_sub,nrow=1) + 
    ylab("proportion in MHW") + 
    xlab("") +
    ylim(c(0,1))+
    scale_color_manual(values=mycolors)+
    scale_fill_manual(values=mycolors)+
    theme_bw()+
    theme( strip.text = element_text(size=10,color="white",family="sans",face="bold"),
           strip.background = element_rect(fill='#0055A4'),
           axis.title.y = element_text(size=10,family="sans"),
           axis.text.y = element_text(size=10,family="sans"),
           panel.grid.major = element_blank(), 
           panel.grid.minor = element_blank()
           #panel.border=element_rect(colour="black",, fill=NA, size=0.75)
    ) 
}

png("AI/2022/ai_mhw_by_status_5day.png", width=9,height=4.5,units="in",res=300)
count_by_mhw_d(mhw_ai2_5)
dev.off()

#smooth by category (not used)
count_plot_smooth2<-function(x){
  ggplot() +
    geom_smooth(data=x%>%filter(heatwave_category!="0"),
                aes(read_date,prop_mhw, color=heatwave_category), size=0.5, span=0.5,  se=FALSE) +
   # geom_line(data=x%>%filter(heatwave_category!="0"),aes(read_date,prop_mhw, color=heatwave_category))+
    facet_wrap(~ecosystem_sub,nrow=1) + 
    ylab("proportion MHW") + 
    xlab("") +
    ylim(c(0,1))+
    theme_bw()+
    theme( strip.text = element_text(size=10,color="white",family="sans",face="bold"),
           strip.background = element_rect(fill='#0055A4'),
           axis.title.y = element_text(size=10,family="sans"),
           axis.text.y = element_text(size=10,family="sans"),
           panel.border=element_rect(colour="black",, fill=NA, size=0.75)) 
}

count_plot_smooth2(mhw_ai2)

#area plot
area_plot<-function(x){
  ggplot() +
    geom_area(data=x%>%filter(heatwave_category!="0")
              ,
                aes(read_date,mean_5day, fill=heatwave_category, color=heatwave_category)) +
    facet_wrap(~ecosystem_sub,nrow=1) + 
    ylab("proportion MHW") + 
    xlab("") +
    ylim(c(0,1))+
    theme_bw()+
    theme( strip.text = element_text(size=10,color="white",family="sans",face="bold"),
           strip.background = element_rect(fill='#0055A4'),
           axis.title.y = element_text(size=10,family="sans"),
           axis.text.y = element_text(size=10,family="sans"),
           panel.border=element_rect(colour="black",, fill=NA, size=0.75)) 
}
area_plot(mhw_ai2_5)
