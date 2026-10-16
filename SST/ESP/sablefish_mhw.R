
library(keyring)
library(RJDBC)
library(magrittr)
library(dplyr)
library(heatwaveR)
library(lubridate)

# download data
options(java.parameters="-Xmx8g")

jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="../../../snippets/dbconnect/java/ojdbc8.jar")

con_j <- dbConnect(jdbcDriver, 
                   "jdbc:oracle:thin:@//tiger:2045/akfin.psmfc.org", 
                   key_list("akfin_oracle_db")$username, 
                   keyring::key_get("akfin_oracle_db", keyring::key_list("akfin_oracle_db")$username))


# try including April so that a mhw that starts in april but extends into May will be included
start_time <-Sys.time()
sst2<-dbGetQuery(con_j, paste0("--Pull SST data for heatwave indicator
--this will be slow to run...       
select read_date, round(avg(temp),2) as mean_sst
--join sst and lookup tables
from afsc.erddap_crw_sst a
inner join afsc.erddap_crw_sst_spatial_lookup b
on a.crw_id=b.id
--AMJJ
where extract(month from read_date) in (4,5,6,7)
--Area: GOA outside waters NMFS 640 and 650
and nmfsarea in (610, 620, 630, 640, 650)
--No depth filter
group by read_date
order by read_date asc
"))%>%
  rename_with(tolower)
end_time <- Sys.time()

end_time-start_time


saveRDS(sst2, "sablefish_sst_AMJJ.RDS")

# run for AMJJ
sst2 <- sst2 %>%
  mutate(year=year(read_date),
         date=as_date(read_date))
current.year<-year(Sys.Date())

climatology_start_date <- "1985-04-01"
climatology_end_date <- "2014-07-31"

climatology_start_date2 <- "1991-04-01" 
climatology_end_date2 <- "2020-07-30"
mhw <- (detect_event(ts2clm(sst2 %>%
                               rename(t=date,temp=mean_sst) %>% 
                               arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim



mhw2 <- (detect_event(ts2clm(sst2 %>%
                               rename(t=date,temp=mean_sst) %>% 
                               arrange(t), climatologyPeriod = c(climatology_start_date2, climatology_end_date2))))$clim

mhw_days <- mhw %>%
  mutate(year=year(t), month=month(t)) %>%
  filter(month %in% c(5,6) & event==TRUE) %>%
  group_by(year) %>%
  summarize(mhw_days_1985_baseline=n())

mhw_days2 <- mhw2 %>%
  mutate(year=year(t), month=month(t)) %>%
  filter(month %in% c(5,6) & event==TRUE) %>%
  group_by(year) %>%
  summarize(mhw_days_1991_baseline=n())



ind <- seq(1985, current.year, 1) %>%
  data.frame() %>%
  rename("year"=".") %>%
  left_join(mhw_days, by="year") %>%
  left_join(mhw_days2, by="year") 

ind[is.na(ind)] <- 0

write.csv(ind, "draft_sablefish_mhw_days.csv", row.names=FALSE)


# try OISST data
start_time <-Sys.time()
sst3<-dbGetQuery(con_j, paste0("--Pull SST data for heatwave indicator
--this will be slow to run...       
select read_date, round(avg(temp),2) as mean_sst
--join sst and lookup tables
from afsc.erddap_oi_sst a
inner join afsc.erddap_oi_sst_spatial_lookup b
on a.oi_id=b.id
--AMJJ
where extract(month from read_date) in (4,5,6,7)
--Area: GOA outside waters NMFS 640 and 650
and nmfsarea in ('610', '620', '630', '640', '650')
--No depth filter
group by read_date
order by read_date asc
"))%>%
  rename_with(tolower)
end_time <- Sys.time()

end_time-start_time


sst3 <- sst3 %>%
  mutate(year=year(read_date),
         date=as_date(read_date))
current.year<-year(Sys.Date())

climatology_start_date <- "1982-04-01"
climatology_end_date <- "2011-07-31"

climatology_start_date2 <- "1991-04-01" 
climatology_end_date2 <- "2020-07-30"
mhw <- (detect_event(ts2clm(sst3 %>%
                              rename(t=date,temp=mean_sst) %>% 
                              arrange(t), climatologyPeriod = c(climatology_start_date, climatology_end_date))))$clim



mhw2 <- (detect_event(ts2clm(sst3 %>%
                               rename(t=date,temp=mean_sst) %>% 
                               arrange(t), climatologyPeriod = c(climatology_start_date2, climatology_end_date2))))$clim

mhw_days <- mhw %>%
  mutate(year=year(t), month=month(t)) %>%
  filter(month %in% c(5,6) & event==TRUE) %>%
  group_by(year) %>%
  summarize(mhw_days_1982_baseline=n())

mhw_days2 <- mhw2 %>%
  mutate(year=year(t), month=month(t)) %>%
  filter(month %in% c(5,6) & event==TRUE) %>%
  group_by(year) %>%
  summarize(mhw_days_1991_baseline=n())



ind <- seq(1982, current.year, 1) %>%
  data.frame() %>%
  rename("year"=".") %>%
  left_join(mhw_days, by="year") %>%
  left_join(mhw_days2, by="year") 

ind[is.na(ind)] <- 0

write.csv(ind, "draft_sablefish_mhw_days_oisst.csv", row.names=FALSE)

library(tidyverse)
library(httr)
library(jsonlite)
library(keyring)

####public####

get_all_esp <- function() {
  jsonlite::fromJSON(
    httr::content(
      httr::GET("https://apex.psmfc.org/akfin/data_marts/akmp/esp_indicators?"),
      as="text", encoding="UTF-8")) %>%
    dplyr::bind_rows() %>%
    rename_with(tolower)
}

esp <-get_all_esp()

old<- esp %>% filter(indicator_name=="Annual_Heatwave_GOA_Model")


mc <- read.csv("draft_sablefish_mhw_days.csv")
mcoi <- read.csv("draft_sablefish_mhw_days_oisst.csv")

ggplot()+
  geom_line(data=old, aes(x=year, y=indicator_value), size=2)+
  geom_line(data=mc, aes(x=year, y=mhw_days_1985_baseline), color="red", size=1)+
  geom_line(data=mcoi, aes(x=year, y=mhw_days_1982_baseline), color="blue")+
  ggtitle("black=webservice, red=CRWSST, blue=OISST")+
  ylab("Number of may-june heatwave days with a 1982 or 1985 baseline.")

ggplot()+
  geom_line(data=old, aes(x=year, y=indicator_value), size=2)+
  geom_line(data=mc, aes(x=year, y=mhw_days_1991_baseline), color="red", size=1)+
  geom_line(data=mcoi, aes(x=year, y=mhw_days_1991_baseline), color="blue")+
  ggtitle("black=webservice, red=CRWSST, blue=OISST")+
  ylab("Number of may-june heatwave days with a 1991 baseline.")
