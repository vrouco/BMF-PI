
create.table3.5items <- function(){
  library(tidyverse)
  library(here)
  library(foreign)
  library(lavaan)
  library(MplusAutomation)
  domains <- c("Agreeableness", "Conscientiousness", "Extraversion", "Neuroticism", "Openness")
  little <- c("agree", "con", "e", "n", "open")
  
  mytable <- tibble(facets=character(),
                    items = as.numeric(NA),
                    chisq = as.numeric(NA),
                    pval = as.numeric(NA),
                    cfi=as.numeric(NA),
                    rmsea=as.numeric(NA))
  
  for(i in 1:5){
    setwd(here(paste("data/Sample_USA_EFA+CFA/CFA/", domains[i], sep="")))
    assign(domains[i], read.spss(list.files()[grep(".sav", list.files())], to.data.frame = T))
    assign(domains[i], as.data.frame(lapply(get(domains[i])[,grep(paste(little[i]), 
                                                                  colnames(get(domains[i])))], ordered)))
  }
  colnames(Extraversion) <- sub("extra", "e", colnames(Extraversion))
  colnames(Neuroticism) <- sub("neuro", "n", colnames(Neuroticism))
  #agree[,grep("agre", colnames(agree))] <- lapply(agree[,grep("agre", colnames(agree))], ordered)
  
  this.table <- mytable
  for(i in 1:5){
    setwd(here(paste("data/Sample_USA_EFA+CFA/CFA/", domains[i], "/5 Items", sep="")))
    files.here <- list.files()
    files.here <- files.here[grep("out", files.here)]
    files.here <- files.here[-grep("gesamtmodell", files.here)]
    
    facets <- files.here
    for(j in 1:length(facets)){
      facets[j] <- sub(paste("CFA ",domains[i], " - ", sep=""), "", facets[j])
      facets[j] <- sub("_5 items.inp", "", facets[j])
      facets[j] <- sub("_3 items.inp", "", facets[j])
    }
    tableformodel <- tibble(facets=NA,
                            items=NA,
                            chisq=NA,
                            df=NA,
                            pvalue=NA,
                            cfi=NA,
                            rmsea=NA)
    
    
    for(j in 1:length(files.here)){
      fit <- readModels(files.here[j])
      
      fit <- fit$summaries
      tableformodel[j,3:7] <- c(round(fit$ChiSqM_Value,2), 
                                fit$ChiSqM_DF, 
                                round(fit$ChiSqM_PValue,2),
                                round(fit$CFI,2),
                                round(fit$RMSEA_Estimate,2))
      
      tableformodel[j,1] <- facets[j]
      
      
      
      tableformodel$chisq[j] <- paste(tableformodel$chisq[j], "(", tableformodel$df[j], ")", sep="")
    }
    
    this.table <- tibble(facets=tableformodel$facets,
                         items=tableformodel$items,
                         chisq=tableformodel$chisq,
                         pvalue=tableformodel$pvalue,
                         cfi=tableformodel$cfi,
                         rmsea=tableformodel$rmsea)
    
    mytable <- rbind(mytable, this.table)
    
  }
  mytable$facets <- tolower(mytable$facets)
  mytable$facets <- sub("_3 items.out", "", 
                        sub("- ", "", 
                            sub(" -", "", 
                                sub("_5 items.out", "", 
                                    sub(" - ", "", 
                                        sub("extraversion  sample - ", "", 
                                            sub("openness | conscientiousness | agreeableness | extraversion | neuroticism", "", 
                                                sub("cfa", "", 
                                                    mytable$facets))))))))
  mytable<-mytable[-31, ]#remove duplicated but typo seld-assuredness
  return(mytable[,-2])
}


y <-create.table3.5items()

source(here("functions/keys facet abbrev.R"))

y<- merge(y, key, by="facets")

source(here("tables/reliability.R"))


facets.rel[,2:3]<-as.data.frame(lapply(facets.rel[,2:3], round, 2))
facets.rel$facets <- tolower(facets.rel$facets)

y<-merge(y, facets.rel, by.x= "abrev", by.y="facets")

source(here("functions/ESEM USA.R"))

y<-merge(y, this.table, by.x= "abrev", by.y="param")

colnames(y)[10]<-"ESEM"

crit.table<-y[,c("domains","facets", "alpha", "omega", "chisq", "pvalue", "cfi", "rmsea", "ESEM")]

#write.csv(crit.table, here("tables/construct validity and rel usa.csv"))
