# This script combines elements of the globcolour download and processing scripts
# It downloads, appends spatial information, and updates the globcolour table in the akfin database.

library(tidyverse)
library(tidync)
library(lubridate)
library(httr)
library(RCurl)
library(purrr)
library(ncdf4)
library(here)
library(RJDBC)
library(getPass)

options(java.parameters="-Xmx8g")

# download files from Hermes
download_glob <- function(order_number, year, dateline) {
  
  url<-paste0("ftp://ftp_hermes@ftp.hermes.acri.fr/",order_number,"/") # my full ftp link
  
  userpwd <- "ftp_hermes:hermes%"
  
  filenames <- getURL(url, userpwd = userpwd,
                      ftp.use.epsv = FALSE,dirlistonly = TRUE)
  
  filenames<-unlist(strsplit(filenames, "\r\n"))
  
  for(i in 1:length(filenames)) {
    GET((paste0(url, filenames[i])),
        authenticate("ftp_hermes","hermes%"),
        write_disk(path=paste0("data/",year,"_",dateline,"/",filenames[i])))
  }
  saveRDS(filenames,paste0("data/",year,"_",dateline,"/filenames_",year,"_",dateline,".RDS"))
  
}


download_glob(order_number = 772431832, year=2024, dateline = "w")
download_glob(order_number = 549302826, year=2024, dateline = "e")

# combine into RDS file
glob_combine <- function(year, dateline) {

filenames_extract<-readRDS(paste0("data/",year,"_",dateline,"/filenames_",year,"_",dateline,".RDS"))
filenames_extract<-filenames_extract[!grepl(".txt", filenames_extract)] # don't extract the text file 



# loading each nc file and making them data frames.
for(y  in 1:(length(filenames_extract))) {
  input_nc_fileName<-filenames_extract[y]
  d<-tidync(paste0("data/",year,"_",dateline,"/",input_nc_fileName)) %>%
    hyper_tibble(na.rm=FALSE)
  d$id<-paste(input_nc_fileName) # you can then get the date
  rds_file_name<-paste0("data/",year,"_",dateline,"/",str_sub(input_nc_fileName, end=-4), ".RDS")
  saveRDS(d, file = rds_file_name)    # assign(paste("df", y, sep=""), df)
  rm(d)
  #file.remove(paste0("data/2023_e/",input_nc_fileName)) # you can take this away if you want to keep the nc files. I usually delete them
}

all_RDS<-list.files(path=here("data", paste0(year,"_",dateline)), pattern = ".RDS")[grepl(".RDS", list.files(path=here("data", paste0(year,"_",dateline)), pattern = ".RDS"))]


all_RDS_AV<-all_RDS[grepl("AV", all_RDS)] # picking all the AV chla estimates (AV and AVW)

#set up data frame to combine 2023 data
data <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(data) <- c("CHL1_mean",  "CHL1_flags", "CHL1_error", "lon",        "lat",        "id" )
data<-data %>%
  mutate(across(c(CHL1_mean, CHL1_error, lon, lat), as.numeric),
         CHL1_flags=as.integer(CHL1_flags),
         id=as.character(id))
#combine
for(y  in 1:(length(all_RDS_AV))) {
  input_rds_fileName<-all_RDS_AV[y]
  data<-data %>%
    bind_rows(readRDS(paste0("data/",year,"_",dateline,"/",input_rds_fileName)))
}

#saveRDS
saveRDS(data, file = paste0("data/",year,"_",dateline,"/globcolour_",year,"_",dateline,"_update.RDS")) #

#delete other files
file.remove(paste0("data/",year,"_",dateline,"/",all_RDS))

}

glob_combine(year=2024, dateline="e")
glob_combine(year=2024, dateline="w")

# formart and combine ew

load_combine_ew <- function(file1, file2) {
  process <- function(file){
    readRDS(file)%>%
      rename_with(tolower) %>%
      rename(chla=chl1_mean) %>%
      filter(!is.na(chla)) %>%
      mutate(start_date=ymd(substr(id, 5, 12))) %>% 
      dplyr::select(start_date, lon, lat, chla)
  }
  bind_rows(process(file1), process(file2))
}


glob_2024<-load_combine_ew(file1="data/2024_e/globcolour_2024_e_update.RDS",
                           file2="data/2024_w/globcolour_2024_w_update.RDS")

#test ... looks reasonable
ggplot(data=glob_2024%>%
         filter(start_date==as.Date("2023-07-20"))%>%
         mutate(lon2=ifelse(lon>0, lon, lon+360)))+
  geom_point(aes(x=lon2, y=lat, color=chla))

# load lookup table
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="../../snippets/dbconnect/java/ojdbc8.jar")

con_j <- dbConnect(jdbcDriver, 
                   "jdbc:oracle:thin:@//tiger:2045/akfin.psmfc.org", 
                   getPass(msg="USER NAME"), getPass())

lkp <- dbGetQuery(con_j, paste0("select * from env_data.globcolour_spatial_lookup")) %>%
  rename_with(tolower)

glob_trim<-function(data) {
  data %>%
    mutate(join_lat=as.numeric(substr(lat, 1, 6)),
           join_lon=ifelse(lon>0, as.numeric(substr(lon, 1, 8)), as.numeric(substr(lon, 1, 9)))) %>% 
    dplyr::select(start_date, join_lon, join_lat, chla)%>%
    inner_join(lkp %>% mutate(join_lat=as.numeric(join_lat),
                              join_lon=as.numeric(join_lon)), 
               by=c("join_lon"="join_lon", "join_lat"="join_lat"))%>%
    dplyr::select(glob_id, start_date, chla)
}
  
glob_2024 <- glob_2024 %>%
  glob_trim()

#test again, still looks good.
ggplot(data=glob_2024%>%
         filter(start_date==as.Date("2023-07-20"))%>%
         left_join(lkp, by=c("glob_id"="glob_id")) %>%
         mutate(join_lat=as.numeric(join_lat),
                join_lon=as.numeric(join_lon),
                lon2=ifelse(join_lon>0, join_lon, join_lon+360)))+
  geom_point(aes(x=lon2, y=join_lat, color=chla))
  
  # update globcolour data


glob_2024 <- glob_2024%>%
  mutate(start_date=as.character(start_date)) %>%
  dplyr::select(glob_id, start_date, chla)%>% 
  rename_with(toupper)

  dbWriteTable(con_j, "GLOBCOLOUR", glob_2024, append=TRUE)
  
