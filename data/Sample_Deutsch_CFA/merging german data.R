library(here)

key <- read.csv(here("longkey.csv"), sep = ";")

domains <- c("Extraversion", "Gewissenhaftigkeit", "Neurotizismus", "Offenheit", "Verträglichkeit")

germandata <- data.frame(myids =seq(1:387))
for(i in 1:5){
  fileshere <- grep(pattern = ".txt", list.files(here(paste0("data/Sample_Deutsch_CFA/CFA/"), domains[i])), value=T)
  fileshere <- fileshere[-grep("IQIPIP", fileshere)]
  for(j in 1:length(fileshere)){
    localdata <- read.table(here("data/Sample_Deutsch_CFA/CFA", domains[i], fileshere[j]))
    localdata <- localdata[,-dim(localdata)[2]]
    colnames(localdata) <- key$items[key$facet == sub(".txt", "", fileshere[j])]
    germandata <- cbind(germandata, localdata)
  }
}

#write.csv(germandata, here("data/Sample_Deutsch_CFA/German data merged Victor.csv"))


#test if german data in ESEM invariance is well merged

prob <- read.table(here("data/Measurement Invariance/Domänen mit ESEM/factorscores_combined.txt"))
prob <- prob[prob$V44==0,]

target <- read.table(here("data/Sample_Deutsch_CFA/CFA/Extraversion/e1.txt"))
target <- target$V6

colnames(prob)[sapply(prob, function(x) all(x %in% target))]



gdata <- data.frame(myids =seq(1:387))
for(i in 1:5){
  fileshere <- grep(pattern = ".txt", list.files(here(paste0("data/Sample_Deutsch_CFA/CFA/"), domains[i])), value=T)
  fileshere <- fileshere[-grep("IQIPIP", fileshere)]
  for(j in 1:length(fileshere)){
    localdata <- read.table(here("data/Sample_Deutsch_CFA/CFA", domains[i], fileshere[j]))
    localdata <- localdata[,dim(localdata)[2]]
    names(localdata) <- sub(".txt", "", fileshere[j])
    colnames(prob)[sapply(prob, function(x) all(x %in% localdata))]<-names(localdata)
  }
}
#In german data they are ok