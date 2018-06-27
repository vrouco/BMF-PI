require(here)
library(tidyverse)
library(xtable)

setwd(here("data/Sample_USA_EFA+CFA/CFA/Full model/ESEM"))

#let's use the model without three facets, althought the 3 factes out and 3 correlated errors works best

library(MplusAutomation)

this.model <- readModels("esem_all facets on all domains without o8, a5, a4, e2.out")
mytable <- this.model$parameters$stdyx.standardized

for(i in 1:length(mytable$paramHeader)){
if(mytable$pval[i]<0.001){
  mytable$new[i] <- paste(mytable$est[i], "***", sep="")
}else if(mytable$pval[i]<0.01){
  mytable$new[i] <- paste(mytable$est[i], "**", sep="")
}else if(mytable$pval[i]<0.05){
  mytable$new[i] <- paste(mytable$est[i], "*", sep="")
}else {mytable$new[i] <- mytable$est[i]}}

mytable <- mytable[,c("paramHeader", "param", "new")]
mytable <- mytable[1:190, ]#remove all information concerning factor correlations, intercepts, res. variances etc

mytable <- spread(mytable, key="paramHeader", value="new")
#there must be an error between NEURO.BY and OPEN.BY, they seem to be crossed

mytable[,c(1,2,3,4,6,5)]

colnames(mytable) <- c("", "Agreeableness", "Conscientiousness", "Extraversion", "Openness", "Neuroticism")


print(xtable(mytable))