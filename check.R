library(here)

key <- read.csv(here("longkey.csv"), sep = ";")

domains <- c("Agreeableness", "Conscientiousness", "Extraversion", "Neuroticism", "Openness")


prob <- read.table(here("data/Sample_USA_EFA+CFA/CFA/Full Model/ESEM/factorscores_CFA.txt"))
prob <- read.table(here("data/Measurement Invariance/Domänen mit ESEM/factorscores_combined.txt"))
prob <- prob[prob$V44==1,]

for(i in 2:43){
  print(all(prob[i]%in%prob2[i]))
}

#these two are the same


for(i in 1:5){
  fileshere <- grep(pattern = ".txt", list.files(here(paste0("data/Sample_USA_EFA+CFA/CFA/"), domains[i], "5 items")), value=T)
  fileshere <- fileshere[-grep("CFA", fileshere)]
  for(j in 1:length(fileshere)){
    localdata <- read.table(here("data/Sample_USA_EFA+CFA/CFA", domains[i],"5 items", fileshere[j]))
    localdata <- localdata[,dim(localdata)[2]]
    names(localdata) <- sub(".txt", "", fileshere[j])
    colnames(prob)[sapply(prob, function(x) all(x %in% localdata))]<-names(localdata)[1]
  }
}

colnames_facets <- colnames(prob)
colnames_facets[10]<-"c1"
colnames_facets[44]<-"country"
colnames_facets[1]<-"id"
