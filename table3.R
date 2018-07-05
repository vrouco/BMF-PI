

create.table3.5items <- function(){
  
  
  library(here)
  library(foreign)
  library(lavaan)
  domains <- c("Agreeableness", "Conscientiousness", "Extraversion", "Neuroticism", "Openness")
  little <- c("agree", "con", "e", "n", "open")
  
  mytable <- tibble(domains=character(),
                    facets=character(),
                    cfi=as.numeric(NA),
                    rmsea=as.numeric(NA),
                    srmr=as.numeric(NA),
                    items = as.numeric(NA))
  
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
    for(j in 1:length(files.here)){
      model <- mplus2lavaan(files.here[j], run = F)$model
      this.table[j,3:5] <- round(fitMeasures(cfa(model, get(domains[i]), 
                                                 ordered=domains[i]), c("cfi", "rmsea", "srmr")), digits=3)
      this.table[j,2] <- facets[j]
      this.table[j, 1] <- domains[i]
      this.table[j,6] <- length(fitted(cfa(model, get(domains[i]), 
                                           ordered=domains[i]))$mean)
    }
    mytable <- rbind(mytable, this.table)
  }
  mytable <- mytable[-which(duplicated(mytable)),]
  mytable$facets <- tolower(mytable$facets)
  return(mytable)
}



y <-create.table3.5items()

grep(key$facets[1], y$facets, ignore.case = T)


create.table3 <- function(){
  
  
  
  library(here)
  library(foreign)
  library(lavaan)
  library(tidyverse)
  domains <- c("Agreeableness", "Conscientiousness", "Extraversion", "Neuroticism", "Openness")
  little <- c("agree", "con", "e", "n", "open")
  
  mytable <- tibble(domains=character(),
                    facets=character(),
                    cfi=as.numeric(NA),
                    rmsea=as.numeric(NA),
                    srmr=as.numeric(NA),
                    items = as.numeric(NA))
  
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
    for(j in 1:length(files.here)){
      model <- mplus2lavaan(files.here[j], run = F)$model
      model <- tolower(model)
      this.table[j,3:5] <- round(fitMeasures(cfa(model, get(domains[i]), 
                                                 ordered=domains[i]), c("cfi", "rmsea", "srmr")), digits=3)
      this.table[j,2] <- facets[j]
      this.table[j, 1] <- domains[i]
      this.table[j,6] <- length(fitted(cfa(model, get(domains[i]), 
                                           ordered=domains[i]))$mean)
    }
    mytable <- rbind(mytable, this.table)
  }
  
  mytable <- mytable[-which(duplicated(mytable)),]
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


table3 <- merge(x,y,by=c("domains","facets"))

library(kableExtra)


kable(table3, "latex", align = "c", booktabs = T)%>%
  kable_styling("striped") %>%
  kable_styling(font_size = 7) %>% 
  add_header_above(c(" " = 2, "Full items" = 4, "5 items" = 4))
# print(xtable(table1, caption="Model fit for each facet"),include.rownames=FALSE,
#       size="\\fontsize{9.5pt}{9pt}\\selectfont", caption.placement = "top")
