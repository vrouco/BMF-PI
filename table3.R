
create.table3 <- function(){


library(here)
library(foreign)
library(lavaan)
domains <- c("Agreeableness", "Conscientiousness", "Extraversion", "Neuroticism", "Openness")
little <- c("agree", "con", "e", "n", "open")

mytable <- tibble(domains=character(),
                  facets=character(),
                cfi=as.numeric(NA),
                rmsea=as.numeric(NA),
                srmr=as.numeric(NA))

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
  }
  mytable <- rbind(mytable, this.table)
}
mytable <- mytable[-which(duplicated(mytable)),]
return(mytable)
}


table3 <- create.table3()
