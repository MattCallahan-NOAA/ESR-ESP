# here some code to make the ESR EBS stuff 
library(tidyverse)
library(tidync)
library(lubridate)
library(maps)
library(mapdata)
library(viridis)
library(data.table)
library(gridExtra)
library(RColorBrewer)
library(viridis)


gl <- readRDS("inter_jens_datafiles/bloomTimingGlob_1998_2024.RDS")
oc <- readRDS("inter_jens_datafiles/bloomTimingOCCCI_1998_2024.RDS")


head(gl)
table(gl$bsierp_id,gl$year)
head(oc)
table(oc$bsierp_id)



