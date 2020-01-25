# Script to compile raw tree and site data
library(readr)
library(tidyverse)

# Load data
treeData <- 
  read_csv("data-raw/Tree_data_raw.csv") %>% 
  mutate_each_(funs(factor(.)),1:4) %>%  # convert first four columns to factors
  mutate(Comms=X14) %>% # rename 14th column
    dplyr::select(-X14) # and delete
  

siteData <- 
  read_csv("data-raw/Site_data_raw.csv") %>% 
  mutate_each_(funs(factor(.)),1:2) # convert first two columns to factors

# Concatenate data into 1 data set
dat<-
  treeData %>% 
  left_join(siteData,by=c("Opening","Plot"))

# Save
save(dat,file="data/dat.RData")
