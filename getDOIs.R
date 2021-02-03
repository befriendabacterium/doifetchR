# START -------------------------------------------------------------------
rm(list=ls())
set.seed(1234)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# LOAD PACKAGES -----------------------------------------------------------

#install.packages('rcrossref')
library(rcrossref)
library(revtools)
library(stringdist)

# LOAD BIBLIOGRAPHY -------------------------------------------------------

#read in bibliography file (e.g. ris, bib)
bib<-revtools::read_bibliography('ENTER_FILENAME')
#add blank doi column
bib$doi<-''

# GET DOIS FROM CROSSREF -----------------------------------------------

alldists<-c()

for (a in 1:nrow(bib)){
  #send the title of the reference as a query to CrossRef and get reuslts back
  results<-tryCatch(
    rcrossref::cr_works(query=bib$title[a], sort = 'relevance'), 
    error=function(e) NULL)
  
  #check if no results, if so, return NA then iterate the loop
  if(is.null(results)==T){
    print(paste(a,') ', NA, sep=''))
    bib$doi[a]<-NA
    alldists<-c(alldists,NA)
    a<-a+1
    
  } 
  
  #if there are results, return them, find nearest match to title and get the doi
  else
  {
  #extract the bibliographic info
  results_data<-results$data
  #generate vector of distances between your bibliography title and that of crossref
  dists<-stringdist::stringdist(bib$title[a],
  results_data$title)
  #For those where the distance is over 20 characters, coerce to NA (based on brief tests, this is the threshold of similarity)
  dists[dists>20]<-NA
  #find the index of the nearest matching title in the CrossRef table
  nearestmatch<-which(dists==min(dists, na.rm = T))
  #snatch its DOI and add to your bibliographic dataframe
  bib$doi[a]<-results_data[nearestmatch,]$doi[1]
  #optional print of doi result, good for bug testing/development
  #print(paste(a,') ', results_data[nearestmatch,]$doi[1], sep=''))
  }       
  
}

#write the new bibliography with the DOIs
#write_bibliography(bib, 'ENTER_FILENAME_wRDOIs')
