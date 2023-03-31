#proportion mhw figures
#Create MHW figure for PEEC workshop and ESR contribution

library(odbc)
library(tidyverse)
library(getPass)
library(lubridate)

#connect to AKFIN
con <- dbConnect(odbc::odbc(), "akfin", UID=getPass(msg="USER NAME"), PWD=getPass())

#pull count of mhw per day for Aluetians
#Does not include depth filter 
mhw_ai<- dbFetch(dbSendQuery(con,
                             paste0("select 
count(case when a.heatwave_category in ('1', '2', '3', '4', '5') then 1 end) mhw, 
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

#merge and create percentage

mhw_ai<-mhw_ai%>%mutate(prop_mhw=MHW/TOTAL_COUNT)

#plot figure
ggplot()+
  geom_line(data=mhw_ai, aes(x=READ_DATE, y=prop_mhw))+
  facet_wrap(~ECOSYSTEM_SUB)

saveRDS(mhw_ai, "Data/prop_mhw_ai.RDS")
mhw_ai<-readRDS("Data/prop_mhw_ai.RDS")

#remove march 10
#it's already not there sooooooo why is it dipping down to zero...

#reorder ecosystems
mhw_ai<-mhw_ai%>%mutate(ECOSYSTEM_SUB=fct_relevel(ECOSYSTEM_SUB,
                                                  c("Western Aleutians", "Central Aleutians", "Eastern Aleutians")))
#OK, let's make this prettier
count_plot<-function(x){
  ggplot() +
    geom_line(data=x,
              aes(READ_DATE,prop_mhw), size=0.5, color="orange") +
    facet_wrap(~ECOSYSTEM_SUB,nrow=1) + 
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
png("ai_mhw_count.png")    
count_plot(mhw_ai)
dev.off()

#ice
ice_ai<- dbFetch(dbSendQuery(con,
                             paste0("select 
count(case when a.heatwave_category = 'I' then 1 end) ice, 
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



ice_ai<-ice_ai%>%mutate(prop_ice=ICE/TOTAL_COUNT)
#reorder ecosystems
ice_ai<-ice_ai%>%mutate(ECOSYSTEM_SUB=fct_relevel(ECOSYSTEM_SUB,
                                                  c("Western Aleutians", "Central Aleutians", "Eastern Aleutians")))
saveRDS(ice_ai, "Data/ice_ai.RDS")

ice_plot<-function(x, y){
  ggplot() +
    geom_line(data=x,
              aes(READ_DATE,prop_mhw), size=0.5, color="orange") +
    geom_line(data=y,
              aes(READ_DATE,prop_ice), size=0.5, color="blue") +
    facet_wrap(~ECOSYSTEM_SUB,nrow=1) + 
    ylab("MHW (orange), historical ice (blue)") + 
    xlab("") +
    ylim(c(0,1))+
    theme_bw()+
    theme( strip.text = element_text(size=10,color="white",family="sans",face="bold"),
           strip.background = element_rect(fill='#0055A4'),
           axis.title.y = element_text(size=10,family="sans"),
           axis.text.y = element_text(size=10,family="sans"),
           panel.border=element_rect(colour="black",, fill=NA, size=0.75)) 
}

ice_plot(mhw_ai, ice_ai)

#pull depth filtered counts for goa and bs
bsgoa<- dbFetch(dbSendQuery(con,
                            paste0("select 
count(case when a.heatwave_category in ('1', '2', '3', '4', '5') then 1 end) mhw,
count(case when a.heatwave_category = 'I' then 1 end) ice,
count(*) total_count,
a.read_date, 
b.ecosystem_sub 
from (select
crw_id, read_date, heatwave_category
from afsc.erddap_crw_sst
where extract(year from read_date)=2022) a
inner join (select id, ecosystem_sub 
from afsc.erddap_crw_sst_spatial_lookup
where ecosystem in ('Gulf of Alaska', 'Eastern Bering Sea')
and depth between -200 and -10) b
on a.crw_id=b.id
group by a.read_date, b.ecosystem_sub
order by a.read_date, b.ecosystem_sub")))


saveRDS(bsgoa, "Data/mhw_ice_bsgoa.RDS")  
bsgoa<-readRDS("EBS/Data/mhw_ice_bsgoa.RDS")
#create proportion
bsgoa<-bsgoa%>%mutate(prop_mhw=MHW/TOTAL_COUNT,
                      prop_ice=ICE/TOTAL_COUNT)
#separate bering sea
bs<-bsgoa%>%filter(ECOSYSTEM_SUB %in% c("Southeastern Bering Sea", "Northern Bering Sea"))
#plot figure
count_plot(bs)
ice_plot(bs, bs)

png("bs_mhw_count.png")
count_plot(bs)
dev.off() 

#separate goa
goa<-bsgoa%>%filter(ECOSYSTEM_SUB %in% c("Western Gulf of Alaska", "Eastern Gulf of Alaska"))%>%
  #reorder ecosystems
  mutate(ECOSYSTEM_SUB=fct_relevel(ECOSYSTEM_SUB,
                                   c("Western Gulf of Alaska", "Eastern Gulf of Alaska")))

png("goa_mhw_count.png")
count_plot(goa)
dev.off()

ice_plot(goa, goa)


#ice
ice_ai<- dbFetch(dbSendQuery(con,
                             paste0("select 
count(case when a.heatwave_category = 'I' then 1 end) ice, 
count(*) total_count,
a.read_date, 
b.ecosystem_sub 
from (select
crw_id, read_date, heatwave_category
from afsc.erddap_crw_sst
where extract(year from read_date)=2022
and extract(month from read_date)<4) a
inner join (select id, ecosystem_sub 
from afsc.erddap_crw_sst_spatial_lookup
where ecosystem ='Aleutian Islands') b
on a.crw_id=b.id
group by a.read_date, b.ecosystem_sub
order by a.read_date, b.ecosystem_sub"))) 



ice_ai<-ice_ai%>%mutate(prop_ice=ICE/TOTAL_COUNT)
#reorder ecosystems
ice_ai<-ice_ai%>%mutate(ECOSYSTEM_SUB=fct_relevel(ECOSYSTEM_SUB,
                                                  c("Western Aleutians", "Central Aleutians", "Eastern Aleutians")))
saveRDS(ice_ai, "Data/ice_ai.RDS")

ice_plot<-function(x, y){
  ggplot() +
    geom_line(data=x%>% filter(READ_DATE<=as_date("2022-03-19")),
              aes(READ_DATE,prop_mhw), size=0.5, color="orange") +
    geom_line(data=y%>% filter(READ_DATE<=as_date("2022-03-19")),
              aes(READ_DATE,prop_ice), size=0.5, color="blue") +
    facet_wrap(~ECOSYSTEM_SUB,nrow=1) + 
    ylab("MHW (orange), ice (blue)") + 
    xlab("") +
    ylim(c(0,1))+
    theme_bw()+
    theme( strip.text = element_text(size=10,color="white",family="sans",face="bold"),
           strip.background = element_rect(fill='#0055A4'),
           axis.title.y = element_text(size=10,family="sans"),
           axis.text.y = element_text(size=10,family="sans"),
           panel.border=element_rect(colour="black",, fill=NA, size=0.75)) 
}

ice_plot(mhw_ai, ice_ai)

#create proportion
mhw_bsgoa<-mhw_bsgoa%>%mutate(prop_mhw=MHW/TOTAL_COUNT)
#separate bering sea
mhw_bs<-mhw_bsgoa%>%filter(ECOSYSTEM_SUB %in% c("Southeastern Bering Sea", "Northern Bering Sea"))
#plot figure
count_plot(mhw_bs)

#separate goa
mhw_goa<-mhw_bsgoa%>%filter(ECOSYSTEM_SUB %in% c("Western Gulf of Alaska", "Eastern Gulf of Alaska"))
#reorder ecosystems
mhw_goa<-mhw_goa%>%mutate(ECOSYSTEM_SUB=fct_relevel(ECOSYSTEM_SUB,
                                                    c("Western Gulf of Alaska", "Eastern Gulf of Alaska")))


#ice for goa and bs
ice_bsgoa<- dbFetch(dbSendQuery(con,
                                paste0("select 
count(case when a.heatwave_category = 'I' then 1 end) ice, 
count(*) total_count,
a.read_date, 
b.ecosystem_sub 
from (select
crw_id, read_date, heatwave_category
from afsc.erddap_crw_sst
where extract(year from read_date)=2022
and extract(month from read_date)<4) a
inner join (select id, ecosystem_sub 
from afsc.erddap_crw_sst_spatial_lookup
where ecosystem in ('Gulf of Alaska', 'Eastern Bering Sea')
and depth between -200 and -10) b
on a.crw_id=b.id
group by a.read_date, b.ecosystem_sub
order by a.read_date, b.ecosystem_sub")))

#download spatial lookup table for map
dbFetch(dbSendQuery(con, paste0("select * from afsc.erddap_crw_sst_spatial_lookup")))%>%
  saveRDS("crw_spatial_lookup_may_2022.RDS")
