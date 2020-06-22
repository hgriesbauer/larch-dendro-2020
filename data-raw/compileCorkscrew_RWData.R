# Script to compile and prepare Corkscrew larch and pine tree ring data

# Load library
library(tidyverse)
library(dplR)

# Set working directory to get files
setwd("C:/Users/hgriesba/Sync/Mackenzie_corkscrew")

coreComp<-function(folderInd=1) {
  
  # get folder structure
  runFolders<-c("Run1","Run2","Run3","Run4")
  
  # Change folder to different run folders
  setwd(paste("C:/Users/hgriesba/Sync/Mackenzie_corkscrew",runFolders[folderInd],"/",sep="/"))
  
# List files in the folder that match pattern
coreList<-
  list.files(pattern=fixed("CS-*")) 

# Now, for each core in coreList:
for (k in 1:length(coreList)) {

coreID=coreList[k] # for each core in coreList, extract core

# First extract site, plot species and core information
site=str_split_fixed(coreID,pattern="-",n=4)[1]
plot=str_split_fixed(coreID,pattern="-",n=4)[2]
species=str_split_fixed(coreID,pattern="-",n=4)[3]
tree=str_split_fixed(coreID,pattern="-",n=4)[4]

# Read in different variables and add rownames as column
rw=read.rwl(fname=paste(getwd(),"/",coreID,"/TUCSON/",coreID,".TXT",sep=""))
ew=read.rwl(fname=paste(getwd(),"/",coreID,"/TUCSON/EARLYWID/",coreID,".TXT",sep=""))
lw=read.rwl(fname=paste(getwd(),"/",coreID,"/TUCSON/LATEWIDT/",coreID,".TXT",sep=""))
lwd=read.rwl(fname=paste(getwd(),"/",coreID,"/TUCSON/LATEDENS/",coreID,".TXT",sep=""))
ewd=read.rwl(fname=paste(getwd(),"/",coreID,"/TUCSON/EARLYDEN/",coreID,".TXT",sep=""))
rwd=read.rwl(fname=paste(getwd(),"/",coreID,"/TUCSON/RINGDENS/",coreID,".TXT",sep=""))
maxd=read.rwl(fname=paste(getwd(),"/",coreID,"/TUCSON/MAXDENSI/",coreID,".TXT",sep=""))

# Create function to create dataframe for each variable prior to rbind
varPrep<-function(varData="rw") {
  
    get(varData) %>% # get varData from string passed to function
    rownames_to_column("year") %>% # create year column
    setNames(c("year","value")) %>% # rename value column
    mutate(site=site,plot=plot,species=species,tree=tree) %>% # create columns
    mutate(type=varData) %>%  # create type
    dplyr::select(site,plot,species,tree,type,year,value) %>% # reorder
    return() # return 
}

# Create dataframe of all variables for core
coreData<-
  rbind(varPrep("rw"),varPrep("ew"),varPrep("lw"),
        varPrep("rwd"),varPrep("ewd"),varPrep("lwd"),
        varPrep("maxd"))

if (k==1) {
  coreData.Comp=coreData
} else {
    coreData.Comp=rbind(coreData,coreData.Comp)
    }

} # close loop

return(coreData.Comp)
} # close function


x<-rbind(
  coreComp(1),
  coreComp(2),
  coreComp(3),
  coreComp(4)
)

ckscrewData<-x

setwd("~/Documents/Git/larch-dendro-2020")
save(ckscrewData,file="./data/corkscrewData.RData")
     