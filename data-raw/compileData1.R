# Script to compile raw tree and site data
library(readr)
library(tidyverse)

# Load data
treeData <- 
  read_csv("data-raw/Tree_data_raw.csv") %>% 
  mutate_each_(funs(factor(.)),1:4) %>%  # convert first four columns to factors
  mutate(Comms=X14) %>% # rename 14th column
    dplyr::select(-X14) %>% # and delete
  dplyr::select(1:7,Crown.Status=`FG/C/C`,everything()) %>% # rename column 'FG/C/C' 
  # Put a dummy variable in Crown.Status where not recorded
  mutate(Crown.Status=replace(Crown.Status,is.na(Crown.Status),0))


siteData <- 
  read_csv("data-raw/Site_data_raw.csv") %>% 
  mutate_each_(funs(factor(.)),vars(c("Opening","Plot","BGC.unit","SMR","SNR","Site.series"))) # convert first two columns to factors

# Calculate plot basal area
PlotBA<-
  treeData %>% 
  filter(Crown.Status!=4) %>% # remove all suppressed trees from basal area calculation
  group_by(Opening,Plot) %>% 
  summarise(PlotBA=sum(DBH)*200*0.0001)   # multiply by 200 to get per hectare basal area

# Calculate tree density per plot
plotDensity<-
  treeData %>% 
  filter(Crown.Status!=4) %>% # remove all suppressed trees from basal area calculation
  group_by(Opening,Plot) %>% 
  summarise(PlotD=n())   # 

# Concatenate data into 1 data set
dat<-
  treeData %>% 
  left_join(siteData,by=c("Opening","Plot")) %>% 

# Bring in planting year
  left_join(read_csv("data-raw/plantingYear.csv"),by="Opening") %>% 
  
# Join with PlotBA
  left_join(PlotBA,by=c("Opening","Plot")) %>% 
  
# Join with Plot Density
  left_join(plotDensity,by=c("Opening","Plot"))
  

  


# Save in data-raw folder
# Use data screening to fix/format/remove outliers, etc.. and save into 'data' folder
save(dat,file="data-raw/datRaw.RData")
