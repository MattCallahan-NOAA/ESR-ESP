##### Start here if you already downloaded the data #####
library(tidync)
library(tidyverse)
library(lubridate)
# function to tidy chla data
tidy_chl<-function(file) {
  tidync(file) %>% 
    hyper_tibble()%>% 
    mutate(date=as_datetime(time),
           latc=as.numeric(substr(latitude, 1, 7)),
           longc=as.numeric(substr(longitude, 1, 9)),
           chlorophyll=round(chlor_a,3),
           year=year(date),
           month=month(date))
}

#bring in all files with for loop
datalist = list()

for (i in myyear){
  dat <- tidy_chl(paste0("ESP/tanner/nc/occ8_",i,".nc"))
  datalist[[i]] <- dat 
}

#convert list to data frame
occ <- dplyr::bind_rows(datalist)


# from https://github.com/MattCallahan-NOAA/chla-indicator-comparison/blob/main/chla-indicator-comparison-data.RMD
occ_grid<-readRDS("ESP/tanner/occ_chl_spatial_lookup.RDS")

occ_esp<-occ%>%
  inner_join(occ_grid, by=c("longc"="longitude", "latc"="latitude"))

# plot BSIERP regions
occ_esp %>%
  filter(dplyr::filter(BSIERP_ID %in% c(3,4,5,6,8))) %>%
  group_by(BSIERP_ID, year) %>%
  summarize(meanchla = mean(chlorophyll, na.rm=T))%>%
  ggplot()+
  geom_line(aes(x=year, y=meanchla))+
  facet_wrap(~BSIERP_ID)+
  theme(text=element_text(size=15))
ggsave("BSIERP_OCCCI_AMJJ.PNG")