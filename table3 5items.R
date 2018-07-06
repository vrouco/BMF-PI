## @knitr table3


create.table3.5items <- function(){

library(here)
library(foreign)
library(lavaan)
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
                                                              colnames(get(domains[i])))], ordered)))}
colnames(Extraversion) <- sub("extra", "e", colnames(Extraversion))
colnames(Neuroticism) <- sub("neuro", "n", colnames(Neuroticism))
#agree[,grep("agre", colnames(agree))] <- lapply(agree[,grep("agre", colnames(agree))], ordered)

this.table <- mytable
for(i in 1:5){
  setwd(here(paste("data/Sample_USA_EFA+CFA/CFA/", domains[i], "/5 Items", sep="")))
  files.here <- list.files()
  files.here <- files.here[grep("inp", files.here)]
  files.here <- files.here[-grep("Gesamtmodell", files.here)]

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
    model <- mplus2lavaan(files.here[j], run = F)$model
    model <- tolower(model)
    tableformodel[j,3:7] <- round(fitMeasures(cfa(model, get(domains[i]), 
                                                  ordered=domains[i]), c("chisq", "df", "pvalue",
                                                                         "cfi","rmsea")), digits=3)
    tableformodel[j,1] <- facets[j]
    #this.table[j,1] <- domains[i]  #i put no domains in this table
    tableformodel[j,2] <- length(fitted(cfa(model, get(domains[i]), 
                                            ordered=domains[i]))$mean)
    
    
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
return(mytable)
}


y <-create.table3.5items()
