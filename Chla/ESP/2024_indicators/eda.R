STM<-dbGetQuery(con_j, "select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year, 'AMJ_Chlorophylla_Biomass_SEBS_Satellite' as indicator_name, round(avg(chla),2) data_value
from env_data.globcolour a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and bsierp_region_name in ('St. Matthew','Pribilofs')
group by extract(year from to_date(start_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(start_date,'YYYY-MM-DD')+4) asc")

GOA<-dbGetQuery(con_j, "select extract(year from to_date(start_date,'YYYY-MM-DD')+4) year, 'Spring_Chlorophylla_Biomass_GOA_Satellite' as indicator_name, round(avg(chla),2) data_value
from env_data.globcolour_2023 a
left join env_data.globcolour_spatial_lookup b on a.glob_id=b.glob_id
where extract(month from to_date(start_date,'YYYY-MM-DD')+4) in (4, 5, 6)
and NMFS_REP_AREA in ('610', '620', '630', '640', '650')
and depth <= (-10)
group by extract(year from to_date(start_date,'YYYY-MM-DD')+4)
order by extract(year from to_date(start_date,'YYYY-MM-DD')+4) asc")

library(gridExtra)
p1<-ggplot()+
  geom_line(data=STM, aes(x=YEAR, y=DATA_VALUE))+
  ggtitle("St Matt + Pribs AMJ globcolour")

p2<-ggplot()+
  geom_line(data=GOA, aes(x=YEAR, y=DATA_VALUE))+
  ggtitle("GOA AMJ globcolour")

grid.arrange(p1,p2, nrow=2)
