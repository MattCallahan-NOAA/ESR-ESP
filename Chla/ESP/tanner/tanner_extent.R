# get extent of bsierp regions
# devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
library(akgfmaps)
library(sf)
library(dplyr)

bsierp <- get_bsierp_regions(set.crs=4326)

tannerbox <- bsierp %>%
  filter(BSIERP_ID %in% c(3,4,5,6,8)) %>%
  st_bbox()

tannerbox

write.csv(tannerbox, "ESP/tanner/tannerbox.csv", row.names=FALSE)
