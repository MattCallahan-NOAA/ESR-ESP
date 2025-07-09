library(RJDBC)
library(tidyverse)
library(lubridate)
library(getPass)
library(zoo)

# calculate daily mean ice
# jdbcDriver <- RJDBC::JDBC(driverClass="oracle.jdbc.OracleDriver",
#                           classPath=system.file("driver", "ojdbc8.jar", package = "akfinupload") )
# server<-"jdbc:oracle:thin:@akfin"
# 
# keyring_service <-"akfin_oracle_db"
# 
# con<-DBI::dbConnect(jdbcDriver, server,
#                     keyring::key_list(keyring_service)$username,
#                     keyring::key_get(keyring_service, keyring::key_list(keyring_service)$username))
# 
# 
# query <- paste0("select read_date, avg(sea_ice_fraction) mean_ice from afsc.erddap_crw_sst a
# left join afsc.erddap_crw_sst_spatial_lookup b on a.crw_id = b.id
# where b.bsierp_id in (4,5,6,8)
# group by read_date")
# 
# dbGetQuery(con, query)  %>%
#   saveRDS("ESP/2025_indicators/bsierp_daily_ice.RDS")


# load saved data plot
ice <- readRDS("ESP/2025_indicators/bsierp_daily_ice.RDS")

ice <- ice %>%
  rename_with(tolower) %>%
  mutate(year=year(read_date),
         doy=yday(read_date))

ggplot()+
  geom_line(data=ice, aes(x=doy, y=mean_ice))+
  facet_wrap(~year)

# filter out - values
ice <- ice %>%
  filter(mean_ice >= 0)

# plot again
ggplot()+
  geom_line(data=ice, aes(x=doy, y=mean_ice))+
  facet_wrap(~year) +
  ylim(c(0,1))+
  xlim(c(1,200))

# save the plot
ggsave("ESP/2025_indicators/bsierp_daily_ice.png",
       width = 8, height = 6, dpi = 300)

# calculate 8 day mean
ice <- ice %>%
  arrange(read_date) %>%
  mutate(mean_8day = zoo::rollmean(mean_ice, k = 8, fill=NA))


ggplot()+
  geom_line(data=ice, aes(x=doy, y=mean_8day))+
  geom_line(data=ice, aes(x=doy, y=mean_ice), color="red")+
  geom_hline(yintercept=0.15)+
  facet_wrap(~year) +
  ylim(c(0,0.25))+
  xlim(c(1,200))+
  theme_bw()


# which years are never over 15%
# 1987, 1996, 2001, 2018, 2019, 2021, 2025
ice %>%
  group_by(year) %>%
  summarize(max_ice=max(mean_8day, na.rm=T)) %>%
  print(n=Inf)


  
retreat <- ice %>% 
    filter(!is.na(mean_8day) & doy<200) %>%
  group_by(year) %>%
    summarise(
      retreat_doy = if (any(mean_8day >= 0.15)) max(doy[mean_8day >= 0.15]) else NA) %>%
    print(n=Inf)

ggplot()+
  geom_line(data=retreat, aes(x=year, y=retreat_doy))+
  theme_bw()
ggsave("ESP/2025_indicators/tanner_retreat_doy.png",
       width = 8, height = 6, dpi = 300)
