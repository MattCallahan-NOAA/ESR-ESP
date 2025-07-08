library(tidync)
library(tidyverse)
library(lubridate)
library(sf)
library(httr)



ext <- read.csv("ESP/tanner/tannerbox.csv")
ext <- ext$x
current_year<-year(Sys.Date())

myyear<-1998:current_year

for(i in myyear){
  file_name <- paste0("ESP/tanner/nc/occ8_",i,".nc")
  download.file(url = paste0("https://coastwatch.pfeg.noaa.gov/erddap/griddap/pmlEsaCCI60OceanColor8Day.nc?chlor_a%5B(",
                             i,"-04-01T00:00:00Z):(", i,"-07-31T00:00:00Z)%5D%5B(",ext[2],"):(",ext[4],")%5D%5B(",ext[1],"):(",ext[3],")%5D&.draw=surface&.vars=longitude%7Clatitude%7Cchlor_a&.colorBar=%7C%7C%7C%7C%7C&.bgColor=0xffccccff"),
                method = "libcurl", mode="wb",destfile = file_name)
}

