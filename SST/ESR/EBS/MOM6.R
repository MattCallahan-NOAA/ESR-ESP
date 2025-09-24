library(tidyverse)
library(tidync)
library(lubridate)
library(sf)
library(akgfmaps)

# new mom6 data
file <- "EBS/Data/MOM6/mom6nep_hc202507_selected_daily_20250701.nc"


grid <-  tidync("EBS/Data/MOM6/mom6nep_hc202507_ocean_static.nc") %>%
  hyper_tibble()


dat <- dl %>%
  dplyr::select(tob, time, ih, jh) %>%
  left_join(grid %>% dplyr::select(geolon, geolat, ih, jh), by=c("jh"="jh", "ih"="ih"))



dat <- dat %>%
  mutate(date=as.POSIXct(time*86400, origin=datesince, tz="UTC"),
         year=year(date),
         month=month(date),
         doy=yday(date))


max(dat$time)
min(dat$time)

t<- as_date(as_datetime(11869.5,origin="1990-01-01 00:00:00", tz = "UTC"))

# convert the number to datetime (input should be in second while the time is in unit of days)
datesince <- '1993-01-01'
datetime_var <- as.POSIXct(11921.5*86400, origin=datesince, tz="UTC")
datetime_var


# OK, now let's try for a full year 
datesince <- '1993-01-01'

grid <-  tidync("EBS/Data/MOM6/mom6nep_hc202507_ocean_static.nc") %>%
  hyper_tibble()

gridak <-  tidync("EBS/Data/MOM6/mom6nep_hc202507_ocean_static_ak.nc") %>%
  hyper_tibble()
#plot grid
ggplot()+
  geom_point(grid, mapping=aes(x=geolon, y=geolat))+
  
  ggplot()+
  geom_point(gridak, mapping=aes(x=geolon_c, y=geolat_c))+
    geom_sf(data=akmarineareas2::ak_dd %>%sf::st_shift_longitude(), color="red", fill=NA)
  
# get subset 
  esr_dd <- get_esr_regions(select.region="esr_subarea", set.crs=4326)
gridebs <- grid %>%
  mutate(lat=geolat, lon=geolon-360) %>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326, agr = 'constant') %>%
  st_join(esr_dd, join = st_within) %>%
  filter(AREA_NAME %in% c("Southeastern Bering Sea", "Northern Bering Sea") & deptho >= 10 & deptho <= 200) %>%
  data.frame() %>%
  rename_with(tolower) %>%
  dplyr::select(deptho, geolon, geolat, ih, jh, area_name)

ggplot()+
  geom_point(gridebs, mapping=aes(x=geolon, y=geolat), size=0.1)+
  geom_sf(data=akmarineareas2::ak_dd %>%sf::st_shift_longitude(), color="red", fill=NA)
  

# 
read_mom6 <-function(file, grid) {
  datesince <- '1993-01-01'
  tidync(file) %>%
    hyper_tibble(force=TRUE) %>%
    dplyr::select(tob, time, ih, jh) %>%
    inner_join(gridebs, by=c("jh"="jh", "ih"="ih")) %>%
    mutate(date=as.POSIXct(time*86400, origin=datesince, tz="UTC"),
           year=year(date),
           month=month(date),
           doy=yday(date),
           domain=case_when(deptho >= 10 & deptho <= 50 ~ "inner",
                            deptho > 50 & deptho <= 100 ~ "middle",
                            deptho > 100 & deptho <= 200~"outer",
                            .default="you messed up your domain assignments"))
  
  
}

late25<-read_mom6(file)
ggplot()+
  geom_point(late25%>%filter(time==11870.5), mapping=aes(x=geolon, y=geolat, color=tob))+
  geom_sf(data=akmarineareas2::ak_dd %>%sf::st_shift_longitude(), color="red", fill=NA)

ggplot()+
  geom_point(late25%>%filter(time==11870.5), mapping=aes(x=geolon, y=geolat, color=domain))+
  geom_sf(data=akmarineareas2::ak_dd %>%sf::st_shift_longitude(), color="red", fill=NA)


myyear<-1993:2025

df<-lapply(myyear, FUN=function(x) read_mom6(paste0("EBS/Data/MOM6/mom6nep_hc202507_selected_daily_",x,"0101.nc")) %>% 
         group_by(date, area_name, domain) %>%
         summarize(mean_bt = mean(tob))) %>%
  bind_rows()


df725<- read_mom6(paste0("EBS/Data/MOM6/mom6nep_hc202507_selected_daily_",2025,"0701.nc")) %>% 
  group_by(date, area_name, domain) %>%
  summarize(mean_bt = mean(tob))

df <- df %>%
  bind_rows(df725)

df <- df %>%
  mutate(year=year(date),
         month=month(date),
         doy=yday(date))

saveRDS(df, "EBS/Data/MOM6/domain_averages.RDS")

