library(httr)
library(dplyr)
library(ggplot2)
# crab occci globcolour comparison
snow25 <- read.csv("ESP/2025_indicators/AMJ_Chlorophylla_Biomass_SEBS_Satellite.csv")
bbrkc25 <- read.csv("ESP/2025_indicators/Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite.csv")

snow24q<-"-- AMJ_Chlorophylla_Biomass_SEBS_Satellite
select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year, 'AMJ_Chlorophylla_Biomass_SEBS_Satellite' as indicator_name, round(avg(chla),2) data_value
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and bsierp_region_name in ('St. Matthew','Pribilofs')
group by extract(year from to_date(start_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(start_date,'YYYY-MM-DD')+4) asc"

bbrkc24q<-"
-- Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite
select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite' as indicator_name, round(avg(chla),2) data_value
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and bs_king='BBRKC'
group by extract(year from to_date(start_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(start_date,'YYYY-MM-DD')+4) asc"

snow24 <- dbGetQuery(con, snow24q)
bbrkc24 <- dbGetQuery(con, bbrkc24q)

p1<-ggplot()+
  geom_line(data=snow24%>%rename_with(tolower), aes(x=year, y=data_value), color="red")+
  geom_line(data=snow25%>%rename_with(tolower), aes(x=year, y=data_value), color="blue")+
  ggtitle("AMJ_Chlorophylla_Biomass_SEBS_Satellite: red=globcolour, blue=occci")
p1
ggsave("ESP/2025_indicators/snow_crab_glob_occci.png",
       width = 8, height = 6, dpi = 300)

p2<-ggplot()+
  geom_line(data=bbrkc24%>%rename_with(tolower), aes(x=year, y=data_value), color="red")+
  geom_line(data=bbrkc25%>%rename_with(tolower), aes(x=year, y=data_value), color="blue")+
  ggtitle("Spring_Chlorophylla_Biomass_SEBS_Inner_Shelf_Satellite: red=globcolour, blue=occci")
p2
ggsave("ESP/2025_indicators/bbrkc_glob_occci.png",
       width = 8, height = 6, dpi = 300)



