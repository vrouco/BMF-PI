library(here)
library(readxl)

#####alldata

setwd(here("/data/ETS_IPIP/Datensätze"))
alldataUSA <- read_excel("ETS.xlsx")

############EFA subsample 1

setwd(here("/data/Sample_USA_EFA+CFA/EFA/MAP_PA"))

domains <- c("agree", "consc", "extra", "neuro", "open")

agree<-read_excel("ARI Daten Agreeableness EFA.xls")
consc<-read_excel("conneu.xlsx")
extra<-read.csv("ARI Daten Extraversion EFA.csv", header=TRUE, sep=";")
neuro<-read.csv("ARI Daten Neuroticism EFA.csv", header=TRUE, sep=";")
open<-read_excel("ARI Daten Openness EFA.xls")

x <- which(consc$respid %in% agree$respid)
consc <- consc[x,]

efasample <- agree
for(i in 2:length(domains)){
efasample <- cbind(efasample, get(domains[i]))}

efasample<-efasample[,-which(duplicated(colnames(efasample)))]#remove duplicates of ids and filter vars


x <- which(alldataUSA$id %in% efasample$respid)
efasamplefull <- alldataUSA[x,]

agree <- agree[,grep("agree", colnames(agree))]
trash <- fa(agree, 8, fm="ml", rotate = "geominT")
trash <- print.psych(trash, cut=0.3)
