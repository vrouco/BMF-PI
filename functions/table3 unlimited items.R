## @knitr table3


create.table3 <- function(){
  library(here)
  library(foreign)
  library(lavaan)
  library(tidyverse)
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
    setwd(here(paste("data/Sample_USA_EFA+CFA/CFA/", domains[i], sep="")))
    files.here <- list.files()
    files.here <- files.here[grep("inp", files.here)]
    files.here <- files.here[-grep("Gesamtmodell", files.here)]
    if(any(grep("ARI", files.here))){
    files.here <- files.here[-grep("ARI", files.here, fixed=T)]}
    if(any(grep("-", files.here, invert=T))){
      files.here <- files.here[-grep("-", files.here, invert=T)]}
    
    
    facets <- files.here
    for(j in 1:length(facets)){
      facets[j] <- sub(paste("CFA ",domains[i], " - ", sep=""), "", facets[j])
      facets[j] <- sub("_5 items.inp", "", facets[j])
      facets[j] <- sub("_3 items.inp", "", facets[j])
      facets[j] <- sub("Messmodell ", "", facets[j])
      facets[j] <- sub("Messmodel ", "", facets[j])
      facets[j] <- sub(".inp", "", facets[j])
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
  mytable$pvalue <- if(mytable$pvalue < 0.001){"< 0.001"}else{
    round(mytable$pvalue, 3)}
  
  #inconsistencies with models mit 5 facets
  mytable <- mytable[-grep("ohne", mytable$facets, fixed = F),]
  mytable$facets <- sub("Extraversion CFA Sample - ", "", mytable$facets)
  mytable$facets <- sub("Excitement Seeking", "sensation seeking", mytable$facets)
  mytable$facets <- sub("Gregariousness", "sociability gregariousness", mytable$facets)
  mytable$facets <- sub("positive emo", "positive emotions", mytable$facets)
  mytable$facets <- sub("Lethargy", "Lethargia", mytable$facets)
  mytable$facets <- sub("Love of Learning_Intellectual Growth", "Love of Learning", mytable$facets)
  mytable$facets <- sub("Openmindedness_Judgement", "Openmindedness Judgement", mytable$facets)
  mytable$facets[mytable$facets=="Openness to Action"]<-"Openness to Actions and activity"
  #mytable$facets <- sub("Openness to Action", "Openness to Actions and activities", mytable$facets)
  mytable$facets[mytable$facets=="F8"]<-"self-serving attention"
  mytable <- mytable[-which(mytable$facets=="F7"),]
  mytable$facets <- tolower(mytable$facets)
  
  return(mytable)
}


x <-create.table3()
setwd(here("/functions"))
