## @knitr table3


create.sumscore.cor <- function(abrev = c("a", "c", "e", "n", "o")){
  library(here)
  library(foreign)
  library(lavaan)
  domains <- c("Agreeableness", "Conscientiousness", "Extraversion", "Neuroticism", "Openness")
  if(abrev=="a"){
    i <- 1
  }else if(abrev=="c"){
    i <- 2
  }else if(abrev=="e"){
    i <- 3
  }else if(abrev=="n"){
    i <- 4
  }else if(abrev=="o"){
    i <- 5
  }
  little <- c("agree", "con", "e", "n", "open")
  
    setwd(here(paste("data/Sample_USA_EFA+CFA/CFA/", domains[i], sep="")))
    assign(domains[i], read.spss(list.files()[grep(".sav", list.files())], to.data.frame = T))
    assign(domains[i], as.data.frame(lapply(get(domains[i])[,grep(paste(little[i]), 
                                                                  colnames(get(domains[i])))], ordered)))
  if(abrev=="e"){
  colnames(Extraversion) <- sub("extra", "e", colnames(Extraversion))
  } else if(abrev=="n"){
  colnames(Neuroticism) <- sub("neuro", "n", colnames(Neuroticism))
  }else {nada <- NA}
  #agree[,grep("agre", colnames(agree))] <- lapply(agree[,grep("agre", colnames(agree))], ordered)
  

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
    sumscores <- as.tibble(matrix(nrow = 361,
                        ncol=length(facets)))
    
    
    for(j in 1:length(files.here)){
      model <- mplus2lavaan(files.here[j], run = F)$model
      model <- tolower(model)
      sumscores[,j] <- lavPredict(cfa(model, get(domains[i]),ordered=domains[i]), "lv",get(domains[i]))
      
    }
    colnames(sumscores) <- facets
    
    return(assign(paste(domains[i], "sumscores", sep=""), sumscores))

}


setwd(here("/functions"))
