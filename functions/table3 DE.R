
create.table3.5items.DE <- function(){
  
  library(here)
  library(MplusAutomation)
  library(tidyverse)
  domains <- c("Gewissenhaftigkeit", "Neurotizismus", "Offenheit", "Verträglichkeit", "Extraversion")
  little <- c("con","n", "open","agree", "e")
  
  mytable <- tibble(facets=character(),
                    items = as.numeric(NA),
                    chisq = as.numeric(NA),
                    pvalue = as.numeric(NA),
                    cfi=as.numeric(NA),
                    rmsea=as.numeric(NA))
  
  colnames(mytable)[3]<-"chisq(df)"
  this.table <- mytable
  
  facets <- as.character(NA)
  for(i in 1:5){
    setwd(here(paste("data/Sample_Deutsch_CFA/CFA/", domains[i], sep="")))
    files.here <- list.files()
    files.here <- files.here[grep("out", files.here)]
    files.here <- if(any(grep("gesamt", files.here))){files.here[-grep("gesamt", files.here)]}else{files.here}
    
    for(j in 1:length(files.here)){
      
      facets[j] <- paste(unlist(strsplit(files.here[j], ""))[5:6], collapse="")
      
      model <- readModels(files.here[j])
      this.table[j, 3] <- paste(model$summaries$ChiSqM_Value, "(", model$summaries$ChiSqM_DF, ")", sep="")
      this.table[j, 4] <- if(model$summaries$ChiSqM_PValue < 0.001){"< 0.001"}else{
        round(model$summaries$ChiSqM_PValue, 3)}
      this.table[j,5:6] <- print(c(model$summaries$CFI,model$summaries$RMSEA_Estimate))
      this.table[j,1] <- facets[j]
      this.table[j,2] <- model$summaries$NDependentVars
    }
    mytable <- rbind(mytable, this.table)
  }
  mytable <- mytable[-which(duplicated(mytable)),]
  mytable$facets <- tolower(mytable$facets)
  return(mytable)
}

z <- create.table3.5items.DE()

setwd(here("/functions"))
