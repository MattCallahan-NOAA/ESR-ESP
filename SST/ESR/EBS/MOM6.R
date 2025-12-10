library(tidyverse)
library(tidync)
library(lubridate)
library(sf)
library(akgfmaps)

# new mom6 data
file <- "EBS/Data/MOM6/mom6nep_hc202507_selected_daily_20250701.nc"


grid <-  tidync("EBS/Data/MOM6/mom6nep_hc202507_ocean_static.nc") %>%
  hyper_tibble()

dl <- tidync(file) %>%
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

gridak_old <-  tidync("EBS/Data/MOM6/mom6nep_hc202507_ocean_static_ak.nc") %>%
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
read_mom6 <-function(file) {
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



#### Compare using the AK grid ####
### follow up using netcdf4
library(ncdf4)

file <- "EBS/Data/MOM6/mom6nep_hc202507_selected_daily_20250701.nc"
static_np <- "EBS/Data/MOM6/mom6nep_hc202507_ocean_static.nc"
static_ak <- "EBS/Data/MOM6/mom6nep_hc202507_ocean_static_ak.nc"

datesince <- '1993-01-01'

grid <-  nc_open(static_np)
names(grid$var)

grid_ak <-  nc_open(static_ak)
names(grid_ak$var)

dl <- nc_open(file)
names(dl$var)
names(dl$dim)

all_vars <- lapply(names(grid_ak$var), function(vname) {
  list(
    name  = vname,
    value = ncvar_get(grid_ak, vname)
  )
})

names(all_vars) <- sapply(all_vars, `[[`, "name")

df <- expand.grid(geolon = geolon, geolat = geolat, time = time)

# Add each variable
for (v in varnames) {
  arr <- ncvar_get(nc, v)
  df[[v]] <- as.vector(arr)
}

dims <- grid_ak$dim

# Specify the OPeNDAP server URL (using regular grid output)
url <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/northwest_atlantic/hist_run/ocean_monthly.199301-201912.sos.nc"
url_static <- "http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/northwest_atlantic/hist_run/ocean_static.nc"

# Open a NetCDF file lazily and remotely
ncopendap <- nc_open(url)
ncstaticopendap <- nc_open(url_static)

# Read the data into memory
timeslice = 1
lon <- ncvar_get(ncstaticopendap, "geolon")
lat <- ncvar_get(ncstaticopendap, "geolat")
i <- ncvar_get(ncopendap, "ih")
j <- ncvar_get(ncopendap, "jh")
time <- ncvar_get(ncopendap, "time",start = c(timeslice), count = c(1))

# Read a slice of the data into memory
sos <- ncvar_get(ncopendap, "sos", start = c(1, 1, timeslice), count = c(-1, -1, 1))


#### use the correct grid dimensions
grid_ak <- tidync(static_ak) %>%
  activate("D3,D2") %>%
  hyper_tibble()


ebs_grid <- grid_ak%>% filter(mask_esr_area==3 & deptho>=10 & deptho <=200) %>%
  dplyr::select(ih, jh, geolat, geolon, deptho, mask_esr_subarea) %>%
  mutate(domain=case_when(deptho >= 10 & deptho <= 50 ~ "inner",
                          deptho > 50 & deptho <= 100 ~ "middle",
                          deptho > 100 & deptho <= 200~"outer",
                          .default="you messed up your domain assignments"))
  

ggplot()+
  geom_point(ebs_grid %>% filter(deptho>=10), mapping=aes(geolon, geolat, color=mask_esr_subarea))

read_mom6 <-function(file) {
  datesince <- '1993-01-01'
  tidync(file) %>%
    hyper_tibble(force=TRUE) %>%
    dplyr::select(tob, time, ih, jh) %>%
    inner_join(ebs_grid, by=c("jh"="jh", "ih"="ih")) %>%
    mutate(date=as.POSIXct(time*86400, origin=datesince, tz="UTC"),
           year=year(date),
           month=month(date),
           doy=yday(date),
           domain=case_when(deptho >= 10 & deptho <= 50 ~ "inner",
                            deptho > 50 & deptho <= 100 ~ "middle",
                            deptho > 100 & deptho <= 200~"outer",
                            .default="you messed up your domain assignments"))
  
  
}

df725<- read_mom6(paste0("EBS/Data/MOM6/mom6nep_hc202507_selected_daily_",2025,"0701.nc")) %>% 
  group_by(date, mask_esr_subarea, domain) %>%
  summarize(mean_bt = mean(tob))


# compare results
original <- readRDS("EBS/Data/MOM6/domain_averages.RDS") %>%
  mutate(bt_og = mean_bt,
         mask_esr_subarea=case_match(area_name,
                                     "Southeastern Bering Sea" ~ 4,
                                     "Northern Bering Sea" ~ 3)) %>%
  dplyr::select(date, domain, mask_esr_subarea, bt_og)


df725_comp <- df725 %>%
  inner_join(original, by=c("date", "domain", "mask_esr_subarea")) %>%
  mutate(diff=bt_og-mean_bt)
