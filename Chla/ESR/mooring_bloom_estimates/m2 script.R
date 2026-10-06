library(tidyverse)
library(tidync)
library(ncdf4)

m2<-"ESR/mooring_bloom_estimates/26bspr2a_eco_0001m.nc"

nc <- nc_open(m2)

df <- data.frame(
  time               = ncvar_get(nc, "time"),
  depth              = ncvar_get(nc, "depth"),
  latitude           = ncvar_get(nc, "latitude"),
  longitude          = ncvar_get(nc, "longitude"),
  chlor_fluorescence = ncvar_get(nc, "chlor_fluorescence"),
  turbidity          = ncvar_get(nc, "turbidity"),
  cdom               = ncvar_get(nc, "cdom")
)
time_units <- ncatt_get(nc, "time", "units")$value
nc_close(nc)

df <- df %>% 
  mutate(datetime = as.POSIXct(time * 86400, origin = "1900-01-01", tz = "UTC"))

summary(df$datetime
        )


ggplot(df, aes(x = datetime, y = chlor_fluorescence, color=depth)) +
  geom_line() +
  ggtitle("M2 Chlorophyll")

mf<-max(df$chlor_fluorescence, na.rm=T)

df%>%filter(chlor_fluorescence==mf)
