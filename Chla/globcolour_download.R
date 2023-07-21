#### Script to download globcolour data
# Matt Callahan
# 7/20/2023

library(tidyverse)
library(tidync)
library(lubridate)
library(httr)
library(RCurl)
library(purrr)
library(ncdf4)
library(here)

#### Load 1998-2022 data East of the meridian ####
# These data were previously downloaded in the chla-indicator-comparison repo
# eg.
glob1998_e<-readRDS("data/2022_e/1998_GlobColour_AV.RDS")

#### Download 2023 data east of the meridian####
# use their online tool. 
# ymin=47.5, ymax=69, xmin=-180, xmax=-130
# startdate 20221125, enddate 20230718

order_number<-773565676
url<-paste0("ftp://ftp_hermes@ftp.hermes.acri.fr/",order_number,"/") # my full ftp link

#userpwd<- "ftp_gc_MCallahan:MCallahan_5113"
userpwd <- "ftp_hermes:hermes%"
  filenames <- getURL(url, userpwd = userpwd,
                      ftp.use.epsv = FALSE,dirlistonly = TRUE)
# unlisting all files
filenames<-unlist(strsplit(filenames, "\r\n"))

## downloading all the nc files from the ftp server - saving names
for(i in 1:length(filenames)) {
  GET((paste0(url, filenames[i])),
      authenticate("ftp_hermes","hermes%"),
      write_disk(path=paste0("data/2023_e/",filenames[i])))
}


 saveRDS(filenames,"data/2023_e/filenames_2023_e.RDS")


###
### unpacking nc and converting to rds objects
###
 
filenames_extract<-readRDS("data/2023_e/filenames_2023_e.RDS")
filenames_extract<-filenames_extract[!grepl(".txt", filenames_extract)] # don't extract the text file 

# loading each nc file and making them data frames.
for(y  in 1:(length(filenames_extract))) {
  input_nc_fileName<-filenames_extract[y]
  d<-tidync(paste0("data/2023_e/",input_nc_fileName)) %>%
    hyper_tibble(na.rm=FALSE)
  d$id<-paste(input_nc_fileName) # you can then get the date
  rds_file_name<-paste0("data/2023_e/",str_sub(input_nc_fileName, end=-4), ".RDS")
  saveRDS(d, file = rds_file_name)    # assign(paste("df", y, sep=""), df)
  rm(d)
  #file.remove(paste0("data/2023_e/",input_nc_fileName)) # you can take this away if you want to keep the nc files. I usually delete them
}


# select AV filenames
all_RDS<-list.files(path=here("data", "2023_e"), pattern = ".RDS")[grepl(".RDS", list.files(path=here("data", "2023_e"), pattern = ".RDS"))]


all_RDS_AV<-all_RDS[grepl("AV", all_RDS)] # picking all the AV chla estimates (AV and AVW)

#set up data frame to combine 2023 data
glob_2023_e <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(glob_2023_e) <- c("CHL1_mean",  "CHL1_flags", "CHL1_error", "lon",        "lat",        "id" )
glob_2023_e<-glob_2023_e %>%
  mutate(across(c(CHL1_mean, CHL1_error, lon, lat), as.numeric),
         CHL1_flags=as.integer(CHL1_flags),
         id=as.character(id))
#combine
for(y  in 1:(length(all_RDS_AV))) {
  input_rds_fileName<-all_RDS_AV[y]
  glob_2023_e<-glob_2023_e %>%
    bind_rows(readRDS(paste0("data/2023_e/",input_rds_fileName)))
}

#saveRDS
saveRDS(glob_2023_e, file = 'data/2023_e/globcolour_2023_e_update.RDS') #

#delete other files
file.remove(paste0("data/2023_e/",all_RDS))

#### Download 1998-2023 data West of the meridian ####
# ymin=47.5, ymax=60, xmin=167, xmax=180
# startdate 19980101, enddate 20230720

order_number<-203450380
url<-paste0("ftp://ftp_hermes@ftp.hermes.acri.fr/",order_number,"/") # my full ftp link

#userpwd<- "ftp_gc_MCallahan:MCallahan_5113"
userpwd <- "ftp_hermes:hermes%"
filenames <- getURL(url, userpwd = userpwd,
                    ftp.use.epsv = FALSE,dirlistonly = TRUE)
# unlisting all files
filenames<-unlist(strsplit(filenames, "\r\n"))

## downloading all the nc files from the ftp server - saving names
for(i in 1:length(filenames)) {
  GET((paste0(url, filenames[i])),
      authenticate("ftp_hermes","hermes%"),
      write_disk(path=paste0("data/2023_w/",filenames[i])))
}


saveRDS(filenames,"data/2023_w/filenames_2023_w.RDS")


###
### unpacking nc and converting to rds objects
###

filenames_extract<-readRDS("data/2023_w/filenames_2023_w.RDS")
filenames_extract<-filenames_extract[!grepl(".txt", filenames_extract)] # don't extract the text file 
filenames_extract_av <- filenames_extract[grepl("AV", filenames_extract)] # only need to save the av files as RDS

# loading each nc file and making them data frames.
for(y  in 1:(length(filenames_extract_av))) {
  input_nc_fileName<-filenames_extract_av[y]
  d<-tidync(paste0("data/2023_w/",input_nc_fileName)) %>%
    hyper_tibble(na.rm=FALSE)
  d$id<-paste(input_nc_fileName) # you can then get the date
  rds_file_name<-paste0("data/2023_w/",str_sub(input_nc_fileName, end=-4), ".RDS")
  saveRDS(d, file = rds_file_name)    # assign(paste("df", y, sep=""), df)
  rm(d)
}

#pulling this out of the loop so I can get rid of the non AV files too
#file.remove(paste0("data/2023_w/",filenames_extract)) 

# select filenames

all_RDS<-list.files(path=here("data", "2023_w"), pattern = ".RDS")

# Combine into annual files 

rds_fx<-function(year){
  saveRDS(purrr:: map_dfr(all_RDS[substr(all_RDS, 5, 8)==year], readRDS), file = paste0("data/2023_w/",year,'_glob_w.RDS')) #
}