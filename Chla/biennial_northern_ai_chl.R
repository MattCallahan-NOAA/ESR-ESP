# test biennial pattern seen in OCCCI with VIIRS and MODIS

# download latest VIIRS and MODIS data for the region of interest

xmin <- 170
xmax <- 190
ymin <- 52
ymax <- 55

# modis -May


options(timeout=6000)
myyear <- 2003:2021
for(i in myyear){
  file_name <- paste0("data/ai_modis_may_",i,".nc")
  download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMH1chlamday_Lon0360.nc?chlorophyll%5B(",i,"-05-16T00:00:00Z):1:(",i,"-05-16T00:00:00Z)%5D%5B(52):1:(55)%5D%5B(170):1:(190)%5D"),
                method = "libcurl", mode="wb",destfile = file_name)
}   

tidy_chl<-function(file) {
  x<-tidync(file) %>% 
    hyper_tibble()
  t<-tidync(file) %>% 
    activate("D0") %>%
    hyper_tibble()
  x$time<-t$time
  x %>%
    mutate(date=as.POSIXct(time, origin = "1970-01-01", tz = "UTC"),
             year=year(date))
}


nai<-lapply(paste0("data/ai_modis_may_",myyear,".nc"), tidy_chl) %>% 
  bind_rows() 

nai2 <- nai %>%
  group_by(year) %>%
  summarize(mean_nai_modis=mean(chlorophyll, na.rm=TRUE))
ggplot(nai2)+
  geom_line(aes(x=year, y=mean_nai_modis))

# viirs -May
"https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQchlaMonthly"

myyear <- 2013:2026
for(i in myyear){
  file_name <- paste0("data/ai_viirs_may_",i,"_e.nc")
  download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQchlaMonthly.nc?chlor_a%5B(",i,"-05-01T12:00:00Z):1:(",i,"-05-01T12:00:00Z)%5D%5B(0.0):1:(0.0)%5D%5B(55):1:(52)%5D%5B(-179.98125):1:(-170)%5D"),
                method = "libcurl", mode="wb",destfile = file_name)
}  

for(i in myyear){
  file_name <- paste0("data/ai_viirs_may_",i,"_w.nc")
  download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/nesdisVHNSQchlaMonthly.nc?chlor_a%5B(",i,"-05-01T12:00:00Z):1:(",i,"-05-01T12:00:00Z)%5D%5B(0.0):1:(0.0)%5D%5B(55):1:(52)%5D%5B(170):1:(179.98125)%5D"),
                method = "libcurl", mode="wb",destfile = file_name)
}  

tidy_chl<-function(file) {
  x<-tidync(file) %>% 
    hyper_tibble()
  t<-tidync(file) %>% 
    activate("D0") %>%
    hyper_tibble()
  x$time<-t$time
  x %>%
    mutate(date=as.POSIXct(time, origin = "1970-01-01", tz = "UTC"),
           year=year(date))
}



nai_e<-lapply(paste0("data/ai_viirs_may_",myyear,"_e.nc"), tidy_chl) %>% 
  bind_rows() 

nai_w<-lapply(paste0("data/ai_viirs_may_",myyear,"_w.nc"), tidy_chl) %>% 
  bind_rows() 

nai<-nai_e %>%
  bind_rows(nai_w)

nai2 <- nai %>%
  group_by(year) %>%
  summarize(mean_nai_viirs=mean(chlor_a, na.rm=TRUE))
ggplot(nai2)+
  geom_line(aes(x=year, y=mean_nai_viirs))
