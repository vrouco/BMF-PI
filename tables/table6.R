require(here)
library(tidyverse)
library(xtable)

setwd(here("data/Sample_Deutsch_CFA/CFA/Full model"))


library(MplusAutomation)

this.model <- readModels("esem_all facets on all without o8, a5, a4, e2.out")

mytable <- this.model$parameters$stdyx.standardized

for(i in 1:length(mytable$paramHeader)){
  if(mytable$pval[i]<0.001){
    mytable$new[i] <- paste(mytable$est[i], "***", sep="")
  }else if(mytable$pval[i]<0.01){
    mytable$new[i] <- paste(mytable$est[i], "**", sep="")
  }else if(mytable$pval[i]<0.05){
    mytable$new[i] <- paste(mytable$est[i], "*", sep="")
  }else {mytable$new[i] <- sprintf("%.3f", round(mytable$est[i],3))}}

mytable <- mytable[,c("paramHeader", "param", "new")]


mytable <- mytable[1:190, ]#remove all information concerning factor correlations, intercepts, res. variances etc

mytable <- spread(mytable, key="paramHeader", value="new")
#there must be an error between NEURO.BY and OPEN.BY, they seem to be crossed

mytable <- mytable[,c(1,2,3,4,6,5)]

colnames(mytable) <- c("", "Agreeableness", "Conscientiousness", "Extraversion", "Neuroticism","Openness")



mytable$Agreeableness[1:6] <- paste("\\textbf{",mytable$Agreeableness[1:6], "}", sep="")
mytable$Conscientiousness[7:15] <- paste("\\textbf{",mytable$Conscientiousness[7:15], "}", sep="")
mytable$Extraversion[16:23] <- paste("\\textbf{",mytable$Extraversion[16:23], "}", sep="")
mytable$Neuroticism[24:30] <- paste("\\textbf{",mytable$Neuroticism[24:30], "}", sep="")
mytable$Openness[31:38] <- paste("\\textbf{",mytable$Openness[31:38], "}", sep="")


print(xtable(mytable),sanitize.text.function=Hmisc::latexTranslate)

