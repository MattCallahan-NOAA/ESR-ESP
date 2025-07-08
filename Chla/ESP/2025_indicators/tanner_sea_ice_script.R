library(RJDBC)
library(tidyverse)
library(lubridate)
library(getPass)


jdbcDriver <- RJDBC::JDBC(driverClass="oracle.jdbc.OracleDriver",
                          classPath=system.file("driver", "ojdbc8.jar", package = "akfinupload") )
server<-"jdbc:oracle:thin:@akfin"

keyring_service <-"akfin_oracle_db"

con<-DBI::dbConnect(jdbcDriver, server,
                    keyring::key_list(keyring_service)$username,
                    keyring::key_get(keyring_service, keyring::key_list(keyring_service)$username))


query <- paste0("select read_date, avg(sea_ice_fraction) mean_ice from afsc.erddap_crw_sst a
left join afsc.erddap_crw_sst_spatial_lookup b on a.crw_id = b.id
where b.bsierp_id in (3,4,5,6,8)
group by read_date")

dbGetQuery(con, query)  %>%
  saveRDS("ESP/2025_indicators/bsierp_daily_ice.RDS")


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
