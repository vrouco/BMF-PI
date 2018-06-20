pathfromhere <- "data/Sample_USA_EFA+CFA/CFA/Conscientiousness/5 Items"




get_fscores <- function(pathfromhere){
library(here)
library(MplusAutomation)
library(tidyverse)

setwd(here(pathfromhere))
Afacets <- list.files()[grep("out",list.files())]
Afacets <- Afacets[-grep("gesamtmodel",Afacets)]

out=NULL
for(i in 1:length(Afacets)){
         h <-readModels(Afacets[i])$parameters$stdyx.standardized
         #h$number <- max(grep("BY",h$paramHeader))
         h <- h[grep("BY", h$paramHeader),]
         out <- rbind(out, h)
}
return(out)
# 
# out <- as.tibble(out[grep("BY",out$paramHeader), c("param","est","se","pval", "number")])
}

A <- get_fscores(pathfromhere)
