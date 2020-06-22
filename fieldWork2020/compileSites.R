# Create site data for Lw and Fdi

library(tidyverse)
library(here)
library(sf)
library(mapview)

# Load csv
x<-  read.csv(here("fieldWork2020","sites2.csv"))

# Create DBH and Height Summaries
treeMetrics<-
  x %>% 
  group_by(Opening) %>% 
  summarise(DBH.2010=round(mean(DBH.2010,na.rm=T),1),
            Height.2010=round(mean(Height.2010,rm=T),1))

# Create list
siteList<-
  x %>% 
  distinct(Opening,.keep_all = TRUE) %>%
  dplyr::select(-DBH.2010,-Height.2010) %>% 
  left_join(treeMetrics,by="Opening") %>% 
  dplyr::select(-Plot.ID) %>% 
  dplyr::select(-Spp) %>% 
  rename(Species=Target.Species) %>% 
  arrange(Species,desc(DBH.2010))

# Write to csv
write.csv(siteList,file=here("fieldWork2020","sitesSkeena_FdiLw.csv"))

# Mapping
sites<-
  siteList %>% 
  st_as_sf(coords=c("Longitude","Latitude"),crs=4326)

mapview(sites,zcol="Target.Species")

st_write(sites, "sitesSkeena_FdiLw.kml", driver='kml', update=TRUE)
