# Function to interactively compile tree density data 
# This is for Pli mortality data

library(dplR)
library(tidyverse)
library(here)

# create function to compile data
  dataCompile<-function(folderID,siteID) {
  
  # Define subfunction
  
  coreFormat<-function(corePath,coreID,coreType="rw") {
    
    invisible(read.rwl(paste(corePath,coreID,sep="/"))) %>% # read in rwl data
      setNames("value") %>% 
      rownames_to_column("year") %>% 
      mutate(site=siteID,coreID=coreID,coreType) %>% 
      mutate(coreID=str_replace(coreID,pattern=fixed(".TXT"),"")) %>%  # remove ".TXT"
      dplyr::select(site,coreID,year,coreType,value) %>% 
      return()
    
  }
  
  
  # Come up with list of folders within folderID
  fList<-
    list.files(here::here("data-raw","density_files",folderID)) %>% 
    .[str_detect(string=.,pattern=fixed("Plexi"),negate=TRUE)] # remove plexi ladder from folder list
  
  # for each folder
  for (i in 1:length(fList)) {
    
    # Take the folder
    fpath=fList[i]
    print(fpath) 
    
    # Identify how many cores are in the folder
    coreList= list.files(here::here("data-raw","density_files",folderID,fpath,"Tucson"),pattern=fixed(".TXT"))
    
    for (k in 1:length(coreList)) { # for each core in the folder
      
      # set up data frame at start of run
      if (k==1 & i==1) crnData=NULL
      
      rw<-coreFormat(here("data-raw","density_files",folderID,fpath,"Tucson"),coreList[k],coreType="rw")
      ew<-coreFormat(here("data-raw","density_files",folderID,fpath,"Tucson","EARLYWID"),coreList[k],coreType="ew")
      lw<-coreFormat(here("data-raw","density_files",folderID,fpath,"Tucson","LATEWIDT"),coreList[k],coreType="lw")
      mxd<-coreFormat(here("data-raw","density_files",folderID,fpath,"Tucson","MAXDENSI"),coreList[k],coreType="mxd")
      mnd<-coreFormat(here("data-raw","density_files",folderID,fpath,"Tucson","MINDENSI"),coreList[k],coreType="mnd")
      rwd<-coreFormat(here("data-raw","density_files",folderID,fpath,"Tucson","RINGDENS"),coreList[k],coreType="rwd")
      
      crnData<-
        rbind(crnData,rw,ew,lw,mxd,mnd,rwd)
        
      
    } # close k loop
    
    
  } # close i loop
 
  return(crnData)
}

X<-
  rbind(
    
    # dataCompile(folderID="3A-7 Run 1",siteID="3A-7"),
    # dataCompile(folderID="3A-7 Run 2",siteID="3A-7"),
    # dataCompile(folderID="3A-7 Run 1",siteID="3A-7"),
    # dataCompile(folderID="154-75 Run 1",siteID="154-75"),
    # dataCompile(folderID="154-75 Run 2",siteID="154-75"),
    # dataCompile(folderID="154-75 Run 3",siteID="154-75"),
    # dataCompile(folderID="292-67 Run 1",siteID="292-67"),
    # dataCompile(folderID="292-67 Run 2",siteID="292-67"),
    # dataCompile(folderID="292-67 Run 3",siteID="292-67"),
    # dataCompile(folderID="409-3 Run 1",siteID="409-3"),
    # dataCompile(folderID="409-3 Run 2",siteID="409-3"),
    # dataCompile(folderID="409-3 Run 3",siteID="409-3"),
    
    
    
    
  )


 X<-
   X %>% 
   mutate(plot=str_sub(coreID,start=-4,end=-3)) %>% 
   mutate(plot=as.factor(plot)) %>% 
    mutate(treeID=str_sub(coreID,start=-1,end=-1)) %>% 
   mutate(treeID=as.factor(treeID)) %>% 
   arrange(site,plot,treeID,coreType,year)

X %>% 
  group_by(site,plot) %>% 
  distinct(treeID) %>% 
  arrange(site,plot) %>% 
 summarise(n()) %>% 
  View()

pliDat<-X
save(pliDat,file="pliDat.RData")
