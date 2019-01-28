library(tidyverse)
library(MplusAutomation)
make.key <- function(){
domains <- c("Agreeableness", "Conscientiousness", "Extraversion", "Neuroticism", "Openness")
little <- c("agree", "con", "e", "n", "open")
key <- tibble(domains=character(),
              facets=character(),
              abrev=character())

this.table <- key
for(i in 1:5){
  setwd(here(paste("data/Sample_USA_EFA+CFA/CFA/", domains[i], "/5 Items", sep="")))
  files.here <- list.files()
  files.here <- files.here[grep("out", files.here)]
  files.here <- if(any(grep("gesamt", files.here))){files.here[-grep("gesamt", files.here)]}else{files.here}
  files.here <- if(any(grep("seld", files.here))){files.here[-grep("seld", files.here)]}else{files.here}
  
  facets <- files.here
  for(j in 1:length(facets)){
    facets[j] <- sub(paste("CFA ",domains[i], " - ", sep=""), "", facets[j])
    facets[j] <- sub("_5 items.out", "", facets[j])
    facets[j] <- sub("_3 items.out", "", facets[j])
  }
  abrev <- as.character(NA)
  for(j in 1:length(files.here)){
    abrev[j] <- MplusAutomation::readModels(files.here[j])$savedata_info$fileName
    
    this.table[j,1]<-domains[j]
    this.table[j,2]<-facets[j]
    this.table[j,3] <- abrev[j]
  }
  key <- rbind(key, this.table)
}


for(j in 1:length(key$abrev)){
  key[j,1]<-paste(strsplit(sub("cfa ","",key$facets), "")[[j]][1:4], collapse="")
  key[j,3] <- sub(".txt", "", key$abrev[j]) 
}

key$facets[44] <- "cfa openness - intellect"
x <- strsplit(key$facets, " ") 
x <- lapply(x,function(x)sub("sample", "", x))
x <- lapply(x, function(x)x[x!=""])
x <- lapply(x, function(x)x[-1:-3])
x <- lapply(x, function(x)x[x != "-"])
x <- unlist(lapply(x, paste, collapse=" "))
key$facets <- x
key$facets[key$facets=="openness to actions and activities"] <- "openness to action and activity"
key <- key[-which(duplicated(key$facets)),]
return(key)
}
key <- make.key()

#setwd(here("/functions"))
